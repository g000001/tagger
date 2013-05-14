;;;-*- Package: FSA-TOKENIZER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1991 by Xerox Corporation

#|(cl:defpackage :fsa-tokenizer
  (:use :common-lisp :cl-extensions :ssb :string-resource :tdb
	:fsa :fsa-standard :standard-states)
  (:nicknames :fsa-t)
  (:export * + ? - / or ! sequence)
  (:export fsa-tokenizer tokenizer-fsa make-tokenizer-fsa def-tokenizer-fsa)
  (:export print-token-stream)
  (:shadow space))|#

(cl:in-package :fsa-tokenizer)


;;;; tokenizer rules and compiler
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass tokenizer-fsa (fsa-standard)
   ;; A character-labelled FSA whose final arcs are integers naming
   ;; the rule to fire when the input matches the preceding path.
   ((fsa-types :accessor fsa-types)    ; table with type of each rule
    (fsa-backups :accessor fsa-backups)	; number of characters to unread
    )))

;; subclasses and methods required for FSA calculus to work on the above
(defclass tokenizer-nfa (tokenizer-fsa nfa) ())
(defclass tokenizer-dfa (tokenizer-fsa dfa) ())
(defmethod make-nfa ((fsa tokenizer-fsa))
  (make-instance 'tokenizer-nfa))
(defmethod make-dfa ((fsa tokenizer-fsa))
  (make-instance 'tokenizer-dfa))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-tokenizer-fsa (rules &optional augment)
    ;; Builds and returns an FSA for RULES.  When AUGMENT is provided, it
    ;; should a tokenizer FSA.  The result is then as if RULES were added to
    ;; the end of the rules used to create AUGMENT.
    (let* ( ;; destructure rules into three lists
           (new-types (mapcar #'first rules))
           (new-bodies (mapcar #'second rules))
           (new-backups (mapcar #'(lambda (rule)
                                    (etypecase (third rule)
                                      ((integer 0 *) (third rule))
                                      (null 0)))
                                rules))
           ;; construct the new FSA
           (rule-number (1- (if augment (length (fsa-types augment)) 0)))
           (proto (make-instance 'tokenizer-fsa))
           (fsa (minimize
                 (apply #'fsa-union
                        (nconc
                         (mapcar
                          #'(lambda (body)
                              ;; add a final arc with number of rule
                              (fsa-concat
                               (regexp-to-fsa body proto)
                               (fsa-symbol (incf rule-number) proto)))
                          new-bodies)
                         (and augment (list augment)))))))
      (setf (fsa-types fsa)
            (concatenate 'vector (and augment (fsa-types augment)) new-types))
      (setf (fsa-backups fsa)
            (concatenate 'vector (and augment (fsa-backups augment)) new-backups))
      fsa)))

(defmacro def-tokenizer-fsa (name form &key augment)
  ;; build the FSA at macroexpand (i.e. compile) time
  #|(let ((tokenizer-fsa
	 (make-tokenizer-fsa (eval form)
			     (and augment (symbol-value augment)))))
    `(defparameter ,name 
	 #-cmu ',tokenizer-fsa
	 #+cmu ,(fsa::make-load-sx tokenizer-fsa)))|#
  `(defparameter ,name 
     (load-time-value 
      (make-tokenizer-fsa ,form ,augment))))



;;;; TT (Tokenizer Tables): a very fast representation for tokenizer FSAs

(defstruct (tokenizer-tables (:conc-name tt-))
  (start-state 0 :type fixnum)
  ;; 2D array in 1D, maps from <state,char> into <state>
  (delta (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (finals #() :type simple-vector))

;; Caution: to be portable should use (INTEGER-LENGTH CL:CHAR-CODE-LIMIT)
(defconstant +char-code-bits+ 7)

(defmacro delta-ref (delta state symbol)
  `(aref (the (simple-array fixnum (*)) ,delta)
	 (the fixnum
	   (+ (the fixnum (ash (the fixnum ,state) +char-code-bits+))
	      (the fixnum (char-code ,symbol))))))

(defmethod make-tt ((fsa fixnum-states))
  (let* ((state-count (1+ (last-state fsa)))
	 (finals (make-array state-count :initial-element nil))
	 (delta (make-array (ash state-count +char-code-bits+)
			    :element-type 'fixnum :initial-element -1)))
    (relation-map
     #'(lambda (state trans)
	 (relation-map
	  #'(lambda (symbol dest)
	      (if (typep symbol 'fixnum)
		  (unless (svref finals state) ; preserve rule ordering
		    (setf (svref finals state) symbol))
		(setf (delta-ref delta state symbol) dest)))
	  trans))
     (fsa-delta fsa))
    ;; fill in finals and delta
    (make-tokenizer-tables
     :delta delta
     :finals finals
     :start-state (fsa-start-state fsa))))


;;;; STREAM-HEAD: a buffer of the head of a stream to enable fast backup.

(defstruct (stream-head
	     (:conc-name sh-)
	     (:constructor make-stream-head ()))
  (ssb (make-ssb))
  (read-pointer 0 :type fixnum)
  (pos 0 :type byte28)
  (end 0 :type byte28)
  stream)

(defun initialize-stream-head (stream start end sh)
  (setf (ssb-pointer (sh-ssb sh)) 0)
  (setf (sh-read-pointer sh) 0)
  (setf (sh-stream sh) stream)
  (setf (sh-pos sh) start)
  (setf (sh-end sh) end)
  sh)

(defun adjust-stream-head (length sh)
  (declare (fixnum length))
  ;; slide buffered characters back over first LENGTH characters
  (do* ((ssb (sh-ssb sh))
	(buffer (ssb-buffer ssb))
	(pos-1 0 (1+ pos-1))
	(pos-2 length (1+ pos-2))
	(end-2 (ssb-pointer ssb)))
      ((= pos-2 end-2)
       (setf (ssb-pointer ssb) (- end-2 length))
       (setf (sh-read-pointer sh) 0))
    (declare (fixnum pos-1 pos-2 end-2))
    (setf (schar buffer pos-1) (schar buffer pos-2))))


(defun sh-read-char (sh)
  (let* ((ssb (sh-ssb sh))
	 (fill-pointer (ssb-pointer ssb))
	 (read-pointer (sh-read-pointer sh)))
    (declare (fixnum fill-pointer read-pointer))
    (prog1 (cond ((< read-pointer fill-pointer)
		  ;; read from the buffer
		  (schar (ssb-buffer ssb) read-pointer))
		 ((= read-pointer fill-pointer)
		  ;; add to the buffer
		  (let ((char (fast-read-char (sh-stream sh))))
		    (incf (sh-pos sh))
		    (if (> (sh-pos sh) (sh-end sh))
			(return-from sh-read-char nil)
			(ssb-push-extend char ssb))))
		 (t (error "~S: Read pointer past fill pointer!" sh)))
      (incf (sh-read-pointer sh)))))


;;;; Protocol implementation

(defvar *english-tokenizer-fsa*)

(defclass fsa-tokenizer (char-ts)
    ((token-end-pos :reader token-end-pos :type fixnum :initarg :start)
     (token-start-pos :reader token-start-pos :type fixnum)
     (tokenizer-fsa :reader tokenizer-fsa :initarg :tokenizer-fsa
		    :initform *english-tokenizer-fsa*)
     (tokenizer-tt :reader tokenizer-tt)
     (stream-head :initform (make-stream-head))))

(defmethod initialize-instance :after ((ts fsa-tokenizer)
				       &key &allow-other-keys)
  (with-slots (tokenizer-fsa tokenizer-tt) ts
    (setf tokenizer-tt (make-tt tokenizer-fsa))))

(defmethod shared-initialize :after ((ts fsa-tokenizer) slots
				     &key char-stream start end
				     &allow-other-keys)
  (declare (ignore slots))
  (with-slots (stream-head) ts
    (initialize-stream-head char-stream start end stream-head)))

(defmethod next-token ((ts fsa-tokenizer))
  (declare #.tdb:*highly-optimized*)
  (with-slots (stream-head token-start-pos token-end-pos tokenizer-tt) ts
    (let ((pos token-end-pos)
	  (fsa (tokenizer-fsa ts))
	  (sh stream-head))
      (declare (fixnum pos))
      (when (> pos (sh-end sh))
	(setf token-start-pos (sh-end sh))
	(return-from next-token nil))
      (loop 
	(multiple-value-bind (rule length) (apply-tokenizer sh tokenizer-tt)
	  (declare (fixnum length))
	  (cond
	   ((zerop length)
	    (when (eq rule t)
	      (setf token-end-pos (sh-pos sh))
	      (setf token-start-pos pos)
	      (return (values (alloc-string 0) :para))) ; trigger para
	    (incf pos)
	    (sh-read-char sh)
	    (adjust-stream-head 1 sh))
	   (t (decf length (the fixnum (svref (fsa-backups fsa) rule)))
	      (setf token-end-pos (the fixnum (+ pos length)))
	      (setf token-start-pos pos)
	      (let ((token (simple-string-copy
			    (ssb-buffer (sh-ssb sh)) length)))
		(adjust-stream-head length sh)
		(return (values token (svref (fsa-types fsa) rule)))))))))))

(defun apply-tokenizer (sh tt)
;;; Attempts to read the next token in SH, returning (1) the rule number of the
;;; token, NIL if there is no match, and T if at EOF, and (2) the length of the
;;; token read.
  (declare #.tdb:*highly-optimized*)
  (let ((finals (tt-finals tt))
	(delta (tt-delta tt))
	(state (tt-start-state tt))
	(last-rule nil)
	(last-pos 0) (pos 0)
	(eof-p nil))
    (declare (fixnum state pos last-pos))
    (loop
      (let ((char (sh-read-char sh))
	    (final (svref finals state)))
	(when final
	  (setq last-pos pos last-rule final))
	(if char
	    (setq state (delta-ref delta state char))
	  (setq eof-p t)))
      (incf pos)
      (when (or (minusp state) eof-p)
	(return
	  (if last-rule
	      (values last-rule last-pos)
	    (values eof-p 0)))))))


(defmethod (setf token-end-pos) (new-pos (ts fsa-tokenizer))
  (with-slots ((sh stream-head) token-end-pos) ts
    (setf (ssb-pointer (sh-ssb sh)) 0)
    (setf (sh-read-pointer sh) 0)
    (file-position (sh-stream sh) new-pos)
    (setf (sh-pos sh) new-pos)
    (setf token-end-pos new-pos)))

(defmethod get-text-string (start-pos end-pos (ts fsa-tokenizer))
  (with-slots (stream-head) ts
    (let* ((char-stream (sh-stream stream-head))
	   (old-file-pos (file-position char-stream))
	   (length (- end-pos start-pos))
	   (string (alloc-string length)))
      (declare (fixnum length))
      (unwind-protect
	   (progn (file-position char-stream start-pos)
		  (dotimes (i length)
		    (declare (fixnum i))
		    (setf (schar string i) (fast-read-char char-stream)))
		  string)
	;; restore the position of the char stream
	(file-position char-stream old-file-pos)))))

(defmethod ts-types ((ts fsa-tokenizer))
  (delete-duplicates (coerce (fsa-types (tokenizer-fsa ts)) 'list)))



;;;; PRINT-TOKEN-STREAM: useful for debugging tokenizers

(defvar *line-length* 79)

(defmethod print-token-stream ((ts ts) output &optional limit)
  (let ((count 0)
	(line-length 0)
	(term "") (type nil)
	(text ()) (types ())
	(last-token-start (token-end-pos ts))
	(ssb (make-ssb))
	(*print-case* :downcase))
    (flet ((output-line ()
	     (incf count (length text))
	     (dolist (string (nreverse text) (terpri output))
	       (write-string string output))
	     (dolist (string (nreverse types) (terpri output))
	       (write-string string output))
	     (terpri output)
	     (setq text () types ())))
      (loop (unless term
	      (output-line) (return))
	(when (and limit (> count limit)) (return))
        (multiple-value-bind (next-term next-type) (next-token ts)
	  (let* ((token-start (token-start-pos ts))
		 (text-string
		  (clean-text-string
		   (get-text-string last-token-start token-start ts)
		   :ssb ssb :endsp t))
		 (text-length (length text-string))
		 (type-string (if type (format nil "~A " type) ""))
		 (type-length (length type-string)))
	    (cond ((< text-length type-length)
		   (incf line-length type-length)
		   (setq text-string
		     (format nil "~A~VA"
			     text-string (- type-length text-length) "")))
		  ((< type-length text-length)
		   (incf line-length text-length)
		   (setq type-string
		     (format nil "~A~VA"
			     type-string (- text-length type-length) "")))
		  (t (incf line-length text-length)))
	    (when (>= line-length *line-length*)
	      (output-line)
	      (setq line-length (length text-string)))
	    (push text-string text)
	    (push type-string types)
	    (setq term next-term type next-type)
	    (setq last-token-start token-start)))))))
