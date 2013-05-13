;;;-*- Package: TAG-BASICS; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992 by Xerox Corporation

#|(cl:defpackage :tag-basics
  (:use :common-lisp :cl-extensions :trie :tdb :fsa-tokenizer)
  (:export basic-tag-ts
	   tagger-pathname tagger-symbol-biases tagger-transition-biases
	   tag->number number->tag tag-count
	   class->number number->class class-count
	   write-tag-state check-tag-state))|#

(cl:in-package :tag-basics)


;;;; BASIC-TAG-TS: token stream behaviour needed by both tagging and training.
;;;; Provides tokenization, lexicon lookup, and class and tag numbering. 

(defclass basic-tag-ts (lexicon-filter fsa-tokenizer)
  ((ts-tag-vector :accessor ts-tag-vector :initarg :tags)
   (ts-tag-trie :accessor ts-tag-trie)
   (ts-class-vector :accessor ts-class-vector :initarg :classes)
   (ts-class-trie :accessor ts-class-trie)
   ;; exported slots for specification with lexicon/tagset
   (tagger-pathname :accessor tagger-pathname :initarg :tagger-pathname)
   (tagger-symbol-biases :accessor tagger-symbol-biases
			 :initarg :symbol-biases :initform '())
   (tagger-transition-biases :accessor tagger-transition-biases
			     :initarg :transition-biases :initform '())))

(defmethod initialize-instance :after ((ts basic-tag-ts)
				       &key &allow-other-keys)
  ;; Construct TAG-VECTOR and TAG-TRIE in accordance with lexicon.
  ;; Note: :para is made equivalent to :sent in the number mapping
  (let ((tags (sort (delete :para (lexicon-tags ts)) #'string<))
	(tag-trie (make-string-trie)))
    (dotimes (i (length tags))
      (setf (st-get (symbol-name (svref tags i)) tag-trie) i))
    (setf (st-get (symbol-name :para) tag-trie)
      (or (st-get (symbol-name :sent) tag-trie)
	  (error "Lexicon does not generate ~s" :sent)))
    (setf (ts-tag-vector ts) tags)
    (setf (ts-tag-trie ts) tag-trie))
  ;; Construct CLASS-VECTOR and CLASS-TRIE in accordance with lexicon. 
  (let ((classes
	 (sort (delete '#(:para) (lexicon-classes ts) :test #'equalp)
	       #'class-order))
	(class-trie
	 (make-simple-vector-trie
	  :key-fn #'(lambda (tag) (tag->number tag ts))
	  :inverse-key-fn #'(lambda (number) (number->tag number ts)))))
    (dotimes (i (length classes))
      (setf (svt-get (svref classes i) class-trie) i))
    (setf (ts-class-vector ts) classes)
    (setf (ts-class-trie ts) class-trie)))



(defun tag->number (tag ts &optional (error-p t))
  (or (st-get (symbol-name tag) (ts-tag-trie ts))
      (and error-p (error "No such tag: ~S." tag))))
(defun number->tag (number ts)
  (svref (ts-tag-vector ts) number))
(defun tag-count (ts)
  (length (ts-tag-vector ts)))
				
(defun class->number (class ts &optional (error-p t))
  (or (svt-get class (ts-class-trie ts))
      (and error-p (error "No such class: ~S" class))))
(defun number->class (number ts)
  (svref (ts-class-vector ts) number))
(defun class-count (ts)
  (length (ts-class-vector ts)))

(defmethod write-tag-state ((ts basic-tag-ts))
  (let ((*print-array* t)
	(*print-readably* t)
	(*package* *the-keyword-package*))
    (with-open-file (stream
		     (make-pathname :type "classes"
				    :defaults (tagger-pathname ts))
		     :direction :output :if-exists :new-version)
      (prin1 (ts-class-vector ts) stream))
    nil))

(defmethod check-tag-state ((ts basic-tag-ts))
  (let* ((*package* *the-keyword-package*)
	 (class-vector
	  (with-open-file (stream (make-pathname
				   :type "classes"
				   :defaults (tagger-pathname ts)))
	    (read stream))))
    (unless (equalp class-vector (ts-class-vector ts))
      (let ((i (mismatch class-vector (ts-class-vector ts) :test #'equalp)))
	(error "Lexicon and saved classes mismatch at ~d: ~s vs ~s"
	       i (svref class-vector i)
	       (svref (ts-class-vector ts) i))))))
