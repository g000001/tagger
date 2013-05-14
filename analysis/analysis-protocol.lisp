;;;-*- Package: TDB; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1988, 1989, 1990, 1991 by Xerox Corporation


;;;; Token Protocol

(cl:in-package :tdb)

#|(eval-when (compile eval load)

  (use-package :cl-extensions)
  (use-package :ssb)
  (use-package :string-resource)

  (export '(analysis *analysis-directory*

	    ts char-ts char-stream ts-char-stream ts-doc-id
	    make-ts

	    token-filter
	    next-token token-start-pos token-end-pos
	    sentence-start-pos paragraph-start-pos
	    get-text-string ts-types

	    do-tokens

	    clean-text-string print-paragraph)))|#



;;;; basics

(defvar *analysis-directory* (tdb-dirpath "data" "english"))

(defclass analysis () ())

(defclass ts ()
  ((doc-id :accessor ts-doc-id :initarg :doc-id)))

(defclass char-ts (ts)
  ((char-stream :accessor ts-char-stream :initarg :char-stream)))

(defmethod shared-initialize :around ((stream char-ts) slots &rest rest
				      &key char-stream start end
				      &allow-other-keys)
  (declare (dynamic-extent rest))
  (cond
    (char-stream
     (if start				; default START to current position
	 (file-position char-stream start) ; reposition to START if provided
	 (setq start (file-position char-stream)))
     (unless end			; default END to end of CHAR-STREAM
       #-allegro (setq end (cl-extensions::file-length char-stream))
       #+allegro (setq end (file-length char-stream)))
     (apply #'call-next-method stream slots :start start :end end rest))
    (t (call-next-method))))

;;;; Analyzers must provide token streams through two means: (1) given a
;;;; character stream, as from a query, and (2) given a document ID, from a
;;;; mixed-in corpus.  For plain-text corpora the latter is usually implemented
;;;; in terms of the former.  The default methods do this when
;;;; MAKE-TS is defined to return a subclass of CHAR-TS.

(defgeneric make-ts (char-stream analysis &key start end))

;;; Token streams returned by MAKE-TS must also support
;;; REINITIALIZE-INSTANCE with args :CHAR-STREAM :START and :END.  It is
;;; suggested that this be supported through methods on SHARED-INITIALIZE.


;;;;
;;;; Token streams are instances of classes defined as follows:
;;;;
;;;;   (DEFCLASS <class> (<filter1> <filter2> ... <source>) ())
;;;;

(defclass token-filter () ())

;;;; Sources & Filters must support the following methods:


;;;; Tokenizers implement NEXT-TOKEN directly.
;;;; Filters implement NEXT-TOKEN by processing the results of CALL-NEXT-METHOD.

;;; Returns the next token from TS, or NIL if at end of stream.
;;; Tokens are simple strings which are not elsewhere pointed at, i.e. clients
;;; may alter them.  (Use ALLOC-STRING, -COPY and -FREE).
(defgeneric next-token (ts))

;;; Tokenizers implement the following directly.
;;; Filters must ensure these are maintained in the face of elisions and/or
;;; buffering of tokens.  Positions are assumed to be monotonically increasing.

;;; Returns the position of the start of the last token returned by NEXT-TOKEN.
;;; Undefined before the first token is read or after stream is repositioned,
;;; i.e. only valid immediately after calls to NEXT-TOKEN.
;;; Defined at eos to be the position at end of stream.
(defgeneric token-start-pos (ts))

;;; Returns the position of the end of the last token returned by NEXT-TOKEN.
;;; Initially returns the position of the beginning of the text.
;;; Undefined at eos.
(defgeneric token-end-pos (ts))

;;; Resets the position of a token stream.  This should only be called with
;;; positions previously returned by TOKEN-END-POSITION.  Note that
;;; SENTENCE-START-POS must also be restored when repositioning.
(defgeneric (setf token-end-pos) (ts pos))


;;; Returns the position of the beginning of the sentence containing the last
;;; token returned by NEXT-TOKEN.  Initially returns the beginning of the text.
;;; Defined at eos to be the position at end of stream.
(defgeneric sentence-start-pos (ts))

;;; Must be called with appropriate position when stream is repositioned.
(defgeneric (setf sentence-start-pos) (ts pos))

;;; Returns the position of the beginning of the paragraph containing the last
;;; token returned by NEXT-TOKEN.  Initially returns the beginning of the text.
;;; Defined at eos to be the position at end of stream.
(defgeneric paragraph-start-pos (ts))

;;; Must be called with appropriate position when stream is repositioned.
(defgeneric (setf paragraph-start-pos) (ts pos))


;;;; Tokenizers implement the following directly w/o filter intervention.

;;; Returns a string containing the text between the named positions.
(defgeneric get-text-string (start-pos end-pos ts))


;;;; Protocol for typed token streams
;;;; Types are keywords.  The types :WORD and :SENT and :PARA are reserved.

;;; Returns the list of types that may be seen.
(defgeneric ts-types (ts))



;;; DO-TOKENS: a handy macro

(defmacro do-tokens ((vars ts &optional value) &body body)
  (let ((ts-var (gensym "TS"))
	(vars (if (consp vars) vars (list vars))))
    `(let ((,ts-var ,ts))
       (block do-tokens
	 (loop (multiple-value-bind ,vars (next-token ,ts-var)
		 (unless ,(car vars) (return ,value))
		 ,@body))))))


;;; the following two functions properly belong elsewhere...

(defun clean-text-string (string
			  &key (end (length string)) (ssb (make-ssb)) endsp)
;;; Replaces sequences of whitespace characters with a single space.  Unless
;;; ENDSP is true leading & trailing whitespace is eliminated too.
  (declare (simple-string string) (type byte28 end))
  (check-type string simple-string)
  (setf (ssb-pointer ssb) 0)
  (let ((last-char-was-whitespace-p (not endsp)))
    (dotimes (i end)
      (declare (type byte28 i))
      (let ((char (schar string i)))
	(case char
	  ((#\space #\tab #\linefeed #\return #\page)
	   (unless last-char-was-whitespace-p
	     (ssb-push-extend #\space ssb)
	     (setq last-char-was-whitespace-p t)))
	  (t (ssb-push-extend char ssb)
	     (setq last-char-was-whitespace-p nil)))))
    (when (and last-char-was-whitespace-p
	       (not endsp)
	       (not (zerop (ssb-pointer ssb))))
      (decf (ssb-pointer ssb)))
    (simple-string-copy (ssb-buffer ssb) (ssb-pointer ssb))))

(defvar *line-width* 79)
    
(defun print-paragraph (string indent stream)
;;; Breaks lines in STRING, printing to STREAM.  Lines (except the first) are
;;; prefixed with INDENT.  Returns the number of lines printed.
  (let ((line-count 0)
	(column indent)
	(line-start 0)
	(last-space nil)
	(limit (length string))
	(pos 0))
    (loop
     (when (= pos limit)
       (when (or (zerop limit) (/= line-start limit))
	 (format stream "~A~%" (subseq string line-start limit))
	 (incf line-count))
       (return line-count))
     (let ((char (schar string pos)))
       (when (char= char #\space)
	 (setf last-space pos))
       (incf pos) (incf column)
       (when (= column *line-width*)
	 (format stream "~A~%~VA"
		 (subseq string line-start (or last-space pos))
		 indent "")
	 (incf line-count)
	 (setq line-start (if last-space (1+ last-space) pos)
	       column (+ indent (- pos line-start))))))))




#|

(defmethod next-sentence ((stream ts))
;;; Returns a list of the tokens in the next sentence on STREAM.
  (let ((sentence-start (sentence-start-pos stream))
	(last-token-end (token-end-pos stream))
	(tokens ()))
    (declare (type byte28 sentence-start last-token-end) (list tokens))
    (do-tokens (token stream)
      (unless (= (sentence-start-pos stream) sentence-start)
	;; back up one token to the begining of the new sentence
	(setf (token-end-pos stream) last-token-end)
	;; SENTENCE-START-POS is ok where it is
	(return))
      (%push token tokens)
      (setq last-token-end (token-end-pos stream)))
    (nreverse tokens)))

(defmethod next-paragraph ((stream ts))
;;; Returns a list of the sentences in the next paragraph on STREAM.
  (let ((paragraph-start (paragraph-start-pos stream))
	(sentences ()))
    (declare (type byte28 paragraph-start) (list sentences))
    (loop
     (let ((sentence (next-sentence stream)))
       (unless sentence (return))
       (%push sentence sentences)
       (unless (= (paragraph-start-pos stream) paragraph-start)
	 (return))))
    (nreverse sentences)))

(defun stems-byte-offsets (function tokens char-stream tdb &key start end)
;;; Calls (FUNCTION <token> <start pos> <length>) for occurence of a token in
;;; TOKENS in STREAM between START and END.
  (let ((table (make-hash-table :test 'equal :size (length tokens))))
    (dolist (token tokens)
      (setf (gethash token table) token))
    (let ((stream (make-ts char-stream (tdb-corpus tdb))))
      (do-tokens (token stream)
	(let ((start-pos (token-start-pos stream)))
	  (declare (type byte28 start-pos))
	  (when (and end (> start-pos (the byte28 end)))
	    (return))
	  (when (and (gethash token table)
		     (or (null start) (>= start-pos (the byte28 start))))
	    (funcall function (gethash token table)
		     start-pos (token-end-pos stream))))))))

|#
