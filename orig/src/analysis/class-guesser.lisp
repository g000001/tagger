;;;-*- Package: CLASS-GUESSER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992 by Xerox Corporation

(cl:defpackage :class-guesser
  (:use :common-lisp :cl-extensions :cons-resource :string-resource
	:sv-resource :trie
	:tdb :tag-basics)
  (:export write-string-trie read-string-trie
	   train-guesser-on-files suffix-lexicon suffix-pathname))

(cl:in-package :class-guesser)

;;; Lexicon which guesses ambiguity classes based upon word suffixes.

(defclass suffix-lexicon (lexicon)
  ((suffix-trie :initform nil :accessor suffix-trie)
   (suffix-pathname :accessor suffix-pathname
		    :initarg :suffix-pathname :initform nil)))

(defmethod shared-initialize :after ((lexicon suffix-lexicon)
				     slots &key &allow-other-keys)
  (declare (ignore slots))
  (let ((pathname (suffix-pathname lexicon)))
    (cond
     ((and pathname (probe-file pathname))
      (format *error-output* "~&;;; Reading ~A ..." (namestring pathname))
      (with-open-file (stream pathname)
	(setf (suffix-trie lexicon) (read-string-trie stream)))
      (format *error-output* " Done.~%"))
     (t (format *error-output* "~&;;; Suffix file does not exist: ~A"
		pathname)))))

(defparameter *suffix-limit* 5)

(defmethod lexicon-lookup (token (lexicon suffix-lexicon))
  (declare (simple-string token))
  (let ((trie (suffix-trie lexicon)))
    (cond ((or (null trie) (< (length token) *suffix-limit*))
	   (call-next-method))
	  (t (nreverse token)
	     (let ((class nil))
	       #+franz(declare (special class)) ; avoid closure consing
	       (flet ((note-class (node offset)
			(declare (ignore offset))
			(when (trie-node-value node)
			  (setq class (trie-node-value node)))))
		 (declare (dynamic-extent #'note-class))
		 (walk-string #'note-class token trie *suffix-limit*))
	       (nreverse token)
	       (if class
		   (values (simple-string-copy token) (sv-copy class))
		 (call-next-method)))))))


;;;; Use existing lexicon and training text to infer a good suffix table.

(defclass suffix-training-ts (token-filter)
	  ((dict-ts :accessor dict-ts)
	   (suffix-trie :initform (make-string-trie) :accessor suffix-trie)
	   (count :accessor suffix-ts-count :initform 0)
	   (closed-class-table :accessor closed-class-table)))

(defmethod initialize-instance :after ((ts suffix-training-ts)
				       &key char-stream start end
				       &allow-other-keys)
  (let* ((dict-ts (make-instance *tokenizer-class* :char-stream char-stream
				 :start start :end end))
	 (open-class (lexicon-open-class dict-ts))
	 (table (make-array (class-count dict-ts) :initial-element nil)))
    (setf (dict-ts ts) dict-ts)
    (dotimes (class-number (length table))
      (let ((class (number->class class-number dict-ts)))
	(dotimes (i (length class))
	  (unless (find (svref class i) open-class)
	    (setf (svref table class-number) t)
	    (return)))))
    (setf (closed-class-table ts) table)))
  

(defmethod reinitialize-instance :after ((stream suffix-training-ts)
					 &key char-stream start end
					 &allow-other-keys)
  (reinitialize-instance (dict-ts stream) :char-stream char-stream
			 :start start :end end))

(defmethod next-token ((ts suffix-training-ts))
  (with-slots (dict-ts suffix-trie closed-class-table) ts
    (loop
      (multiple-value-bind (stems class surface) (next-token dict-ts)
	(unless stems (return))
	(if (simple-string-p stems)
	    (free-string stems)
	  (dotimes (i (length (the simple-vector stems)) (free-sv stems))
	    (free-string (svref stems i))))
	(let ((class-number (class->number class dict-ts)))
	  #+franz(declare (special class-number)) ; dynamic extent
	  (free-sv class)
	  (if (or (svref closed-class-table class-number)
		  (<= (length surface) *suffix-limit*))
	      (free-string surface)
	    (flet ((count-class (node offset)
		     (declare (ignore offset))
		     (incf (getf (trie-node-value node) class-number 0))))
	      (declare (dynamic-extent #'count-class))
	      (nreverse surface)	;Assume here that nreverse returns
					;argument 
	      (walk-string #'count-class surface suffix-trie *suffix-limit*)
	      (free-string surface)
	      (return (incf (suffix-ts-count ts))))))))))

(defun train-guesser-on-files (files &key token-limit quiet-p
					  (freq-limit 10) (coverage 0.9))
  (let ((ts nil))
    (dolist (f files)
      (with-open-file (stream f)
	(if ts
	    (reinitialize-instance ts :char-stream stream)
	  (setq ts (make-instance 'suffix-training-ts :char-stream stream)))
	(loop (let ((count (next-token ts)))
		(when (or (null count)
			  (and token-limit (>= count token-limit)))
		  (return))))
	(let ((trie (suffix-trie ts))
	      (pathname (suffix-pathname (ts-lexicon (dict-ts ts)))))
	  (sort-trie trie)
	  (map-st #'(lambda (suffix counts)
		      (setf (st-get (copy-seq suffix) trie)
			(sort-fixnum-plist counts)))
		  trie)
	  (multiple-value-bind (truncated-trie accounted-for)
	      (truncate-trie ts :limit freq-limit :coverage coverage)
	    (with-open-file (stream pathname
			     :direction :output :if-exists :new-version)
	      (write-string-trie truncated-trie stream))
	    (unless quiet-p
	      (format t "~&~d% token accounted for.~%" accounted-for))))))
    ts))

;;; Trie I/O
(defun write-string-trie (trie stream)
  (let ((*print-array* t))
    (map-st #'(lambda (string value) (format stream "~s ~s~%" string value))
	    trie)))

(defun read-string-trie (stream)
  (let ((trie (make-string-trie)))
    (loop (let ((string (or (read stream nil nil) (return)))
		(value (read stream)))
	    (setf (st-get string trie) value)))
    trie))
    
;;; Trie truncation

(define-list-sorter sort-fixnum-plist
    :key cadr :next cddr :order > :key-type fixnum)

(defmacro do-plist ((key value plist) &body body)
  (let ((tail (gensym)))
    `(do ((,tail ,plist))
         ((null ,tail))
       (let ((,key (pop ,tail))
             (,value (pop ,tail)))
         ,@body))))

(defun truncate-trie (ts &key (limit 10) (coverage 0.9))
  (let* ((dict-ts (dict-ts ts))
	 (suffix-trie (suffix-trie ts))
	 (result (make-string-trie))
	 (match nil)
	 (tag-table (make-array (tag-count dict-ts)))
	 (discarded 0)
	 (kept 0))
    (map-st
     #'(lambda (suffix counts)
	 (unless (and match (> (length suffix) (length match))
		      (string= match suffix :end2 (length match)))
	   (let ((total 0))
	     (do-plist (class count counts)
	       (declare (ignore class))
	       (incf total count))
	     (if (< total limit)
		 (when (= (length suffix) *suffix-limit*)
		   (incf discarded total))
	       (let ((enough (ceiling (* total coverage)))
		     (class-count 0)
		     (covered 0))
		 (fill tag-table nil)
		 (do-plist (class count counts)
		   (incf covered count)
		   (incf class-count)
		   (mask-tag-table tag-table class dict-ts)
		   (when (>= covered enough)
		     (let ((covering-class (covering-class tag-table dict-ts)))
		       (if covering-class
			   (setf (st-get suffix result) covering-class
				 match (copy-seq suffix)
				 kept (+ kept total))
			 (when (= (length suffix) *suffix-limit*)
			   (incf discarded total))))
		     (return))))))))
     suffix-trie)
    (values result (round (* 100 kept) (+ discarded kept)))))

(defun mask-tag-table (tag-table class-number ts)
  (let ((class (number->class class-number ts)))
    (dotimes (i (length class))
      (setf (svref tag-table (tag->number (svref class i) ts)) t))))

(defun covering-class (tag-table ts)
  (do ((class (alloc-sv (count t tag-table)))
       (limit (length tag-table))
       (i 0 (1+ i))
       (j 0))
      ((= i limit)
       (sort class #'string<)
       (when (class->number class ts nil)
	 (prog1 (copy-seq class) (free-sv class))))
    (when (svref tag-table i)
      (setf (svref class j) (number->tag i ts))
      (incf j))))

#|

;;; Diagnostic output

(defun print-suffix-trie (ts &key (output *standard-output*)
				  (limit 10)
				  (coverage 0.9))
  (let* ((dict-ts (dict-ts ts))
	 (suffix-trie (suffix-trie ts))
	 (*print-case* :downcase)
	 (match nil)
	 (tag-table (make-array (tag-count dict-ts)))
	 (discarded 0)
	 (kept 0))
    (map-st
     #'(lambda (suffix counts)
	 (unless (and match (string= match suffix :end2
				     (min (length match) (length suffix))))
	   (let ((total 0))
	     (do-plist (class count counts)
	       (declare (ignore class))
	       (incf total count))
	     (if (< total limit)
		 (when (= (length suffix) *suffix-limit*)
		   (incf discarded total))
	       (let ((enough (ceiling (* total coverage)))
		     (class-count 0)
		     (covered 0))
		 (fill tag-table nil)
		 (do-plist (class count counts)
		   (incf covered count)
		   (incf class-count)
		   (mask-tag-table tag-table class dict-ts)
		   (when (>= covered enough)
		     (let ((covering-class (covering-class tag-table dict-ts)))
		       (if covering-class
			   (progn
			     (format
			      output "~& ~V@A ~5D ~3D% in ~D ~{ ~A~}~%"
			      *suffix-limit* (reverse suffix)
			      total (round (* covered 100) total) class-count
			      (coerce (number->class covering-class dict-ts)
				      'list))
			     (setq match (copy-seq suffix))
			     (incf kept total))
			 (when (= (length suffix) *suffix-limit*)
			   (incf discarded total))))
		     (return))))))))
     suffix-trie)
    (format output "~%~D total words, ~D% accounted for.~%"
	    (+ discarded kept) (round (* 100 kept) (+ discarded kept)))))
    
|#
