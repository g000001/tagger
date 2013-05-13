;;;-*- Package: TAG-ANALYSIS; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1991, 1992 by Xerox Corporation

(cl:defpackage :tag-analysis
  (:use :common-lisp :cl-extensions :cons-resource :string-resource :svb :ssb
	:sv-resource :hmm :tdb :fsa-tokenizer :tag-basics)
  (:export :tag-analysis :tagging-ts :token-class
	   :tag-file :tag-string :tokenize-file))

(cl:in-package :tag-analysis)

;;; Tag Analysis

(defclass tag-analysis (analysis) ())

(defmethod make-ts (char-stream (analysis tag-analysis) &key start end)
  (make-instance 'tagging-ts :char-stream char-stream :start start :end end))

;;; Tag Token stream

(defclass tagging-ts (ts)
   ((ts-hmm-est :accessor ts-hmm-est :initarg :hmm-est)
    (sentence-start-pos :type fixnum :accessor sentence-start-pos
			:initarg start)
    (paragraph-start-pos :type fixnum :accessor paragraph-start-pos
			 :initarg start)
    (sent-svb :initform (make-svb))
    (hmm-svb :initform (make-svb))
    (sent-pointer :type fixnum :initform 0)
    (dict-ts :initarg :dict-ts :accessor dict-ts)))

(defmethod initialize-instance :after
	   ((ts tagging-ts) &key char-stream start end &allow-other-keys)
  ;; from file: check tags, classes & class-trie w/ defaults, read hmm
  (setf (dict-ts ts)
    (make-instance *tokenizer-class* :char-stream char-stream
		   :start start :end end))
  (check-tag-state (dict-ts ts))
  (setf (ts-hmm-est ts)
    (make-hmm-est
     (hmm-read-file
      (make-pathname :type "hmm"
		     :defaults (tagger-pathname (dict-ts ts)))))))


(defmethod reinitialize-instance :after ((stream tagging-ts)
					 &key char-stream start end
					 &allow-other-keys)
  (with-slots (sent-pointer sent-svb dict-ts) stream
    (reinitialize-instance
     dict-ts :char-stream char-stream :start start :end end)
    (dotimes (i (svb-pointer sent-svb))
      (free-bt (svbref sent-svb i)))
    (setf (svb-pointer sent-svb) 0)
    (setf sent-pointer 0)))

;;; Buffered Tokens

(defstruct (buffered-token
	    (:conc-name bt-)
	    (:constructor make-bt (form start-pos end-pos class)))
  (start-pos 0 :type byte28)
  (end-pos 0 :type byte28)
  ;; A vector of strings, or a single string, or NIL
  form
  ;; The chosen tag, or NIL
  tag
  ;; A vector of tags
  class)

(defvar *free-bt-head* nil)

(defun alloc-bt (form start-pos end-pos class)
  (let ((bt *free-bt-head*))
    (cond (bt (setq *free-bt-head* (bt-form bt))
	      (setf (bt-form bt) form)
	      (setf (bt-start-pos bt) start-pos)
	      (setf (bt-end-pos bt) end-pos)
	      (setf (bt-class bt) class)
	      bt)
	  (t (make-bt form start-pos end-pos class)))))

(defun free-bt (bt)
  (when (simple-string-p (bt-form bt))
    (free-string (bt-form bt)))
  (free-sv (bt-class bt))
  (setf (bt-form bt) *free-bt-head*)
  (setq *free-bt-head* bt)
  nil)

;;; Token stream protocol

(defmethod token-start-pos ((stream tagging-ts))
  (with-slots (sent-svb sent-pointer dict-ts) stream
    (if (zerop (svb-pointer sent-svb))		  ; at eof
	(token-start-pos dict-ts)
      (bt-start-pos (svbref sent-svb (1- sent-pointer))))))

(defmethod token-end-pos ((stream tagging-ts))
  (with-slots (sent-svb sent-pointer dict-ts) stream
    (if (zerop sent-pointer)		  ; at bof
	(token-end-pos dict-ts)
      (bt-end-pos (svbref sent-svb (1- sent-pointer))))))

(defmethod token-class ((stream tagging-ts))
  (with-slots (sent-pointer sent-svb) stream
    (bt-class (svbref sent-svb (1- sent-pointer)))))

(defmethod get-text-string (start end (stream tagging-ts))
  (with-slots (dict-ts) stream
    (get-text-string start end dict-ts)))

(defmethod next-token ((stream tagging-ts))
  (with-slots (sent-svb hmm-svb ts-hmm-est sent-pointer dict-ts) stream
    (unless (< sent-pointer (svb-pointer sent-svb))
      (multiple-value-bind (start-pos class)
	  (tag-next-sentence dict-ts ts-hmm-est sent-svb hmm-svb)
	(unless start-pos
	  ;; eos
	  (setf (sentence-start-pos stream)
	    (token-start-pos dict-ts))
	  (setf (paragraph-start-pos stream)
	    (token-start-pos dict-ts))
	  (return-from next-token nil))
	(setf (sentence-start-pos stream) start-pos)
	(when (equalp class '#(:para))
	  (setf (paragraph-start-pos stream) start-pos))
	(setf sent-pointer 0)))
    (let ((bt (svbref sent-svb sent-pointer)))
      (incf sent-pointer)
      (values (simple-string-copy (bt-form bt)) (bt-tag bt)))))

(defun tag-next-sentence (ts hmm-est sent-svb hmm-svb)
  (let ((sent-class-number (class->number '#(:sent) ts)))
    ;; Free storage from allocated in the last call to tag-next-sentence
    (dotimes (i (svb-pointer sent-svb))
      (free-bt (svbref sent-svb i)))
    (setf (svb-pointer sent-svb) 0)
    (setf (svb-pointer hmm-svb) 0)
    ;; Prime the observation vector with a sentence end marker
    (svb-push-extend sent-class-number hmm-svb)
    (let ((sent-start-pos (token-end-pos ts))
	  end-class)
      (loop
	(multiple-value-bind (stems class surface) (next-token ts)
	  ;; Stems is a either a vector of strings, or a single string
	  ;; Class is a vector of tags
	  (when (null stems) (return-from tag-next-sentence)) ; end of stream
	  (free-string surface)
	  (let ((class-number (class->number class ts)))
	    (if (= class-number sent-class-number)
		(unless (= (svb-pointer sent-svb) 0)
		  ;; Full sentence collected
		  (setq end-class class)
		  (return))
	      (progn
		(svb-push-extend
		 (alloc-bt stems (token-start-pos ts) (token-end-pos ts) class)
		 sent-svb)
		(svb-push-extend class-number hmm-svb))))))
      ;; Terminate the observation vector with a sentence end marker
      (svb-push-extend sent-class-number hmm-svb)
      ;; Call the viterbi algorithm on the observation vector
      (hmm-maximal-path
       (svb-buffer hmm-svb) hmm-est (svb-buffer hmm-svb) (svb-pointer hmm-svb))
      (dotimes (i (svb-pointer sent-svb))
	(declare (fixnum i))
	(let* ((bt (svbref sent-svb i))
	       (tag (svbref hmm-svb (1+ i)))
	       (form (bt-form bt))
	       (class (bt-class bt)))
	  (declare (fixnum tag))
	  (setf (bt-tag bt) (number->tag tag ts))
	  (unless (simple-string-p form)
	    ;; find the appropriate lexical form corresponding to tag
	    (dotimes (i (length class) (free-sv form))
	      (when (= tag (tag->number (svref class i) ts))
		(setf (bt-form bt) (simple-string-copy (svref form i))))
	      (free-string (svref form i))))))
      (values sent-start-pos end-class))))


;;; diagnostics

(defun count-tags (pathname)
  (with-open-file (stream pathname)
    (let ((count 0)
	  (ts (make-ts stream (make-instance 'tag-analysis))))
      (time (do-tokens (token ts)
		       (free-string token)
		       (incf count)))
      count)))

(defclass print-tagging-ts (tagging-ts) ())
(defmethod next-token ((ts print-tagging-ts))
  (multiple-value-bind (term tag) (call-next-method)
    (when term
      (values term
	      (let ((arity (length (token-class ts))))
		(if (= arity 1)		; annotate tags w/ degree of ambig.
		    tag			; unambiguous
		  (format nil "~A/~D" tag arity)))))))

(defun tag-file (pathname &optional (output *standard-output*))
  (with-open-file (stream pathname)
    (print-token-stream
     (make-instance 'print-tagging-ts :char-stream stream) output))
  (values))

(defun tag-string (string &optional (output *standard-output*))
  (with-input-from-string (stream string)
    (print-token-stream
     (make-instance 'print-tagging-ts :char-stream stream) output))
  (values))

(defun tokenize-file (pathname &optional (output *standard-output*))
  (with-open-file (stream pathname)
    (print-token-stream
     (make-instance 'fsa-tokenizer
       :char-stream stream
       :tokenizer-fsa (tokenizer-fsa (make-instance *tokenizer-class*)))
     output))
  (values))

