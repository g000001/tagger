;;;-*- Package: TAG-BROWN; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1991, 1992 by Xerox Corporation

(cl:defpackage :tag-brown
  (:use :common-lisp :cl-extensions :string-resource :ssb :trie :tdb
	:vector-lexicon :tag-basics :fsa-tokenizer :tag-analysis
	:class-guesser)
  (:export brown-tokenizer evaluate-brown))

(cl:in-package :tag-brown)

;;; define a lexicon

(defvar *brown-open-class*
    '(:jj :jjr :jjs :jjt
      :nn :nn$ :nns :nns$ :np :np$ :nps :nps$ 
      :nr :nr$ :nrs
      :rb :rbr :rbt
      :rn
      :uh
      :vb :vbd :vbg :vbn :vbz))

(defclass brown-lexicon (vector-lexicon suffix-lexicon)
  ()
  (:default-initargs
    :lexicon-open-class *brown-open-class*
    :lexicon-pathname (tdb-pathname "data" "brown" "lexicon.txt")
    :lexicon-size 150000
    :suffix-pathname (tdb-pathname "data" "brown" "suffix.trie")))


;;; define a tokenizer

(def-tokenizer-fsa *brown-tokenizer-fsa*
    (let ((all-chars
	   `(/ #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\0
	       #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@ 
	       #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
	       #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\` 
	       #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p
	       #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\| #\} #\~))
	  (white `(/ #\space #\tab #\page #\newline)))
      ;; Should ":" be an ambiguous token?
      `((:word (+ ,all-chars))
	;; #\+ is introduced at headline boundaries
	(:sent (! (/ #\. #\! #\? #\; #\] "++") ,white) 1)
	;; various character classes
	(:cm (! #\, ,white) 1)
	(:- (! (/  #\( #\) "--" #\: #\[) ,white) 1))))

;;; note some transition biases

(defvar *brown-transition-biases*
    '( ;; `to' infinitive marker is typically followed by a verb form
      (:valid :to :be :bed :bedz :beg :bem :ben :ber :bez :do :dod :doz :hv 
       :hvd :hvg :hvn :hvz :vb :vbd :vbg :vbn :vbz)
      ;; Determiners are typically not followed by verb forms
      ;; Don't necessarily want to do it for all det-like objects....
      (:invalid :at :be :bed :bedz :beg :bem :ben :ber :bez :do :dod :doz :hv 
       :hvd :hvg :hvn :hvz :vb :vbd :vbg :vbn :vbz)
      (:invalid :dt :be :bed :bedz :beg :bem :ben :ber :bez :do :dod :doz :hv 
       :hvd :hvg :hvn :hvz :vb :vbd :vbg :vbn :vbz)
      (:invalid :dti :be :bed :bedz :beg :bem :ben :ber :bez :do :dod :doz :hv 
       :hvd :hvg :hvn :hvz :vb :vbd :vbg :vbn :vbz)
      (:invalid :dts :be :bed :bedz :beg :bem :ben :ber :bez :do :dod :doz :hv 
       :hvd :hvg :hvn :hvz :vb :vbd :vbg :vbn :vbz)
      ;; From Julian's "inhibit" list
      (:valid :ql :ql :qlp :jj :jjr :jjs :jjt :rb :rbr :rbt :rn)
      (:invalid :abl :in :jj :nns :nn :cs)
      (:invalid :rbr :nn :nns)
      (:invalid :in :cm :vbd :vb :vbz :sent :bed :be :bedz :beg :ben
       :bem :ber :bez)
      (:invalid :jj :in :-)
      (:invalid :hv :vbd :hvd)
      (:invalid :hvd :hvd)
      (:invalid :hvz :vbd :hvd)
      (:invalid :hvn :ben)
      (:invalid :be :vbd :hvd)
      (:invalid :ben :vbd)
      (:invalid :bedz :vbd :hvd)
      (:invalid :ber :vbd)
      (:invalid :beg :vbd)
      (:invalid :bem :vbd)
      (:invalid :bez :vbd)
      (:invalid :nn :rp)
      (:invalid :nns :rp)
      (:invalid :ex :at :sent :cs :in)
      (:invalid :rp :cs :nn :dt :at :dts)
      (:invalid :wps :at :ex :ap :pps :np :* :in :cs :nns :abn :pp$ :dt)
      (:invalid :np :np :nn)
      (:invalid :rbt :nn :nns)
      ;; (:invalid :to :bez :cm :cs :sent :wrb :ppo)
      (:valid :uh :uh)
      (:valid :fw :fw)
      ;; (:invalid :at :np)
      ))

;;; combine all of the above into a tag-tokenizer specification

(defvar *brown-lexicon* nil)

(defclass brown-tokenizer (basic-tag-ts) ()
  (:default-initargs
    :lexicon (or *brown-lexicon*
		 (setq *brown-lexicon* (make-instance 'brown-lexicon)))
    :tagger-pathname (tdb-pathname "data" "brown" "brown")
    :tokenizer-fsa *brown-tokenizer-fsa*
    :transition-biases *brown-transition-biases*
    :symbol-biases `((:valid (:nn :np) :nn)
		     (:valid (:jj :np) :jj)
		     (:valid (:ap :ql :rb) :ap :ql)
		     (:valid (:cs :in) :in)
		     (:valid ,*brown-open-class* :nn :nns :np :nps))))

(setq *tokenizer-class* (find-class 'brown-tokenizer))

#|
;;;; to train brown:
(pdefsys:compile-system :tag-trainer)
(pdefsys:load-system :tag-trainer)
(use-package :tag-trainer)
#+excl(setf (sys:gsgc-parameter :generation-spread) 10)
(train-on-files (list (tdb-pathname "data" "brown" "training.txt"))
		:quantum 10000 :iterations 5)
|#



;;;; Code for Brown tagger evaluation

#|
;;; to evaluate brown:


|#


;;; Tokenizer for tagged brown corpus character stream

;;; Doesn't support token-start-pos, token-end-pos, sentence-start-pos,
;;; paragraph-start-pos, or get-text-string

(defclass brown-truth-ts (char-ts)
    ((end-pos :initarg :end :type fixnum)
     (pos :initarg :start :type fixnum)
     (ssb1 :initform (make-ssb))
     (ssb2 :initform (make-ssb))))

(defmethod next-token ((stream brown-truth-ts))
  (with-slots (char-stream end-pos pos ssb1 ssb2)
      stream
    (loop (multiple-value-bind (token tag end)
	      (parse-token char-stream pos end-pos ssb1 ssb2)
	    (setf pos end)
	    (unless (eq tag :sent)
	      (return (values token tag)))))))

(defmacro fast-alpha-char-p (char)      ; the normal ALPHA-CHAR-P is too slow
  `(let ((char ,char))                  ; assume ASCII
     (or (and (char<= #\a char) (char<= char #\z))
         (and (char<= #\A char) (char<= char #\Z)))))

(defun parse-token (stream pos end ssb1 ssb2)
  (declare (fixnum pos end))
  (setf (ssb-pointer ssb1) 0)
  (setf (ssb-pointer ssb2) 0)
  (let ((collecting-tag-p nil))
    (loop (when (>= pos end) (return))
      (let ((char (fast-read-char stream)))
	(incf pos)
	(cond ((eql char #\newline) (return))
	      ((eql char #\space) (setq collecting-tag-p t))
	      (t (if collecting-tag-p
		     (ssb-push-extend char ssb2)
		   (ssb-push-extend char ssb1))))))
    (if collecting-tag-p
	(values (simple-string-copy (ssb-buffer ssb1) (ssb-pointer ssb1))
		(make-tag ssb1 ssb2)
		pos)
      (values nil nil pos))))		; fell off the end of the file

(defun make-tag (token-ssb tag-ssb)
  (let* ((string
	  (nstring-upcase
	   (simple-string-copy (ssb-buffer tag-ssb) (ssb-pointer tag-ssb))))
	 (symbol
	  (if (= 1 (length string))
	      (case (schar (ssb-buffer token-ssb) 0)
		((#\. #\! #\? #\; #\] #\+) :sent)
		(#\, :cm)
		((#\( #\) #\- #\: #\[) :-)
		(t (or (find-symbol string *the-keyword-package*)
		       (intern (copy-seq string) *the-keyword-package*))))
	    (or (find-symbol string *the-keyword-package*)
		(intern (copy-seq string) *the-keyword-package*)))))
    (free-string string)
    symbol))

      
;;;; Specialized I/O for diagnostics and verification

(defclass brown-evaluation-ts (tagging-ts)
  ((truth-ts :accessor brown-truth-ts :initarg :truth-ts)
   (ts-output-p :initarg :output-p :accessor ts-output-p)
   (ts-total :initform 0 :accessor ts-total :type fixnum)
   (ts-ambiguous :initform 0 :accessor ts-ambiguous :type fixnum)
   (ts-errors :initform 0 :accessor ts-errors :type fixnum)
   (ts-confusion-table :initarg :confusion-table :accessor ts-confusion-table)
   (ts-error-trie :initarg :error-trie :accessor ts-error-trie)))

(defmethod next-token ((ts brown-evaluation-ts))
  (multiple-value-bind (token tag) (call-next-method)
    (multiple-value-bind (token2 true-tag) (next-token (brown-truth-ts ts))
      (when (and token token2)
	(assert (string= token token2))
	(free-string token2)
	(let ((arity (length (token-class ts))))
	  (incf (ts-total ts))
	  (unless (eq true-tag tag)
	    (incf (ts-errors ts))
	    (let ((table (ts-confusion-table ts))
		  (trie (ts-error-trie ts)))
	      (when table (incf (getf (gethash true-tag table) tag 0)))
	      (when trie (incf (st-get token trie 0)))))
	  (unless (= arity 1) (incf (ts-ambiguous ts)))
	  (values token
		  (when (ts-output-p ts)
		    (format nil "~A~[~*~:;/~D~]~:[*~A~;~*~]"
			    tag (1- arity) arity
			    (eq tag true-tag) true-tag))))))))

(defmacro do-plist ((key val plist) &body body)
  (let ((tail (gensym)))
    `(do* ((,tail ,plist (cddr ,tail))
	   (,key (car ,tail) (car ,tail))
	   (,val (cadr ,tail) (cadr ,tail)))
	 ((null ,tail))
       ,@body)))

(defun evaluate-brown (source truth &key (output nil)
					 (threshold 100)
					 limit)
  (with-open-file (source-stream source)
    (with-open-file (truth-stream truth)
      (let ((ts
	     (make-instance 'brown-evaluation-ts
	       :char-stream source-stream
	       :truth-ts (make-instance 'brown-truth-ts
			   :char-stream truth-stream)
	       :output-p (and output t)
	       :confusion-table (and threshold (make-hash-table :test 'eq))
	       :error-trie (and threshold (make-string-trie)))))
	(if (ts-output-p ts)
	    (print-token-stream ts output limit)
	  (time (loop (free-string (or (next-token ts) (return)))
		  (when (and limit (> (ts-total ts) limit)) (return))
		  (when (zerop (mod (ts-total ts) 1000)) (format t ".")))))
	(format t "~%    total: ~7:D (~4F% correct)~%"
		(ts-total ts)
		(- 100.0 (/ (* (ts-errors ts) 100.0) (ts-total ts))))
	(format t "ambiguous: ~7:D (~4F% correct)~%"
		(ts-ambiguous ts)
		(- 100.0 (/ (* (ts-errors ts) 100.0) (ts-ambiguous ts))))
	(when (ts-confusion-table ts)
	  (format t "Errors by tag, cut at ~D errors:~%" threshold)
	  (let ((*print-case* :downcase))
	    (format t "~:{~4D ~A~%~:{  ~4D ~A~%~}~}"
		    (sort
		     (with-collection
		       (maphash
			#'(lambda (tag errors)
			    (let* ((sum 0)
				   (errors 
				    (with-collection
				      (do-plist (tag count errors)
					(incf sum count)
					(when (>= count threshold)
					  (collect (list count tag)))))))
			      (when (>= sum threshold)
				(collect 
				 (list sum tag
				       (sort errors #'> :key #'car))))))
			(ts-confusion-table ts)))
		     #'> :key #'car))))
	(when threshold
	  (format t "Terms with more than ~D errors:~%" threshold)
	  (format t "~:{~4D ~A~%~}"
		  (sort (with-collection
			  (map-st
			   #'(lambda (term freq)
			       (when (>= freq threshold)
				 (collect (list freq (copy-seq term)))))
			   (ts-error-trie ts)))
			#'> :key #'car)))))))
  
