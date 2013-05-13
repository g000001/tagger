;;;-*- Package: TAG-TRAINER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1991, 1992 by Xerox Corporation

#|(cl:defpackage :tag-trainer
  (:use :common-lisp :cl-extensions :cons-resource :string-resource
	:sv-resource :hmm :float-vector
	:tdb :tag-basics)
  (:shadow common-lisp:pi)
  (:export train-on-docs train-on-files train-combination))|#

(cl:in-package :tag-trainer)

;;;; Training

(defclass training-ts (token-filter)
   ((dict-ts :initarg :dict-ts :accessor dict-ts)
    (ts-hmm :accessor ts-hmm)))

(defmethod initialize-instance :after
	   ((ts training-ts) &key char-stream start end &allow-other-keys)
  (let ((dict-ts (make-instance *tokenizer-class* :char-stream char-stream
				:start start :end end)))
    (setf (dict-ts ts) dict-ts)
    (setf (ts-hmm ts)
      (make-training-hmm dict-ts
			 (tagger-symbol-biases dict-ts)
			 (tagger-transition-biases dict-ts)))))

(defmethod reinitialize-instance :after ((stream training-ts)
					 &key char-stream start end
					 &allow-other-keys)
  (reinitialize-instance (dict-ts stream) :char-stream char-stream
			 :start start :end end))

(defun make-training-hmm (dict-ts symbol-biases transition-biases)
  (let* ((n (tag-count dict-ts))
	 (m (class-count dict-ts))
	 (pi (make-sfv n (/ 1.0 n)))
	 (a (make-sfm n n (/ 1.0 n)))
	 (b (make-sfm n m 0.0))
	 (hmm (make-hmm :n n :m m :pi pi :a a :b b)))
    (dotimes (class-number m)
      (let ((class (number->class class-number dict-ts)))
	(dotimes (j (length class))
	  (setf (sfmref b (tag->number (svref class j) dict-ts) class-number)
	    1.0))))
    (dotimes (i n)
      (let ((b-at-i (svref b i))
	    (sum 0.0))
	(declare (single-float sum))
	(dotimes (j m)
	  (incf sum (sfvref b-at-i j)))
	(setq sum (/ 1.0 sum))
	(dotimes (j m)
	  (unless (zerop (sfvref b-at-i j))
	    (setf (sfvref b-at-i j) sum)))))
    (bias-hmm hmm (convert-transition-biases transition-biases dict-ts)
	      (convert-symbol-biases symbol-biases dict-ts))))

(defun convert-transition-biases (transition-biases dict-ts)
  ;; Transition-Biases is a list of lists of the form
  ;; ({:valid :not-valid} start-tag end-tag*)
  (with-collection
    (dolist (clause transition-biases)
      (destructuring-bind (type start . end) clause
	(when (eq type :invalid)
	  (setq end
	    (set-difference (coerce (lexicon-tags dict-ts) 'list) end)))
	(collect
	 (cons (tag->number start dict-ts)
	       (delete-duplicates
		(mapcar #'(lambda (tag) (tag->number tag dict-ts)) end))))))))


(defun convert-symbol-biases (symbol-biases dict-ts)
  ;; Symbol-Biases is a list of lists of the form
  ;; ({:valid :not-valid} class tag*)
  (with-collection
    (dolist (clause symbol-biases)
      (destructuring-bind (type class . end) clause
	(when (eq type :invalid)
	  (setq end
	    (set-difference (coerce (lexicon-tags dict-ts) 'list) end)))
	(collect
	 (list* (class->number
		 (sort (map 'simple-vector #'identity class) #'string<)
		 dict-ts)
		(map 'list #'(lambda (tag) (tag->number tag dict-ts)) class)
		(delete-duplicates
		 (mapcar #'(lambda (tag) (tag->number tag dict-ts)) end))))))))

(defun bias-hmm (hmm transition-biases symbol-biases)
  ;; Transition-Biases is a list of lists of the form
  ;; (start-tag end-tag*)
  ;; Symbol-Biases is a list of the forms
  ;; (class tag*)
  (let ((n (hmm-n hmm)))
    (dolist (clause transition-biases)
      (let* ((clause-tags (cdr clause))
	     (vector (svref (hmm-a hmm) (car clause)))
	     (epsilon (compute-epsilon (length clause-tags) n))
	     (rlength (/ 1.0 (length clause-tags))))
	(declare (single-float epsilon rlength))
	(dotimes (i n)
	  (if (member i clause-tags)
	      (setf (sfvref vector i) rlength)
	    (setf (sfvref vector i) epsilon)))))
    (dolist (clause symbol-biases)
      (let* ((class-number (car clause))
	     (class-tags (cadr clause))
	     (clause-tags (cddr clause))
	     (b (hmm-b hmm)))
	(dolist (tag class-tags)
	  (unless (member tag clause-tags)
	    (let* ((b-at-tag (svref b tag))
		   (factor (/ 1.0  (1- (count-if-not #'zerop b-at-tag)))))
	      (declare (single-float factor))
	      (dotimes (i (length b-at-tag))
		(unless (zerop (sfvref b-at-tag i))
		  (if (= i class-number)
		      (setf (sfvref b-at-tag i) single-float-epsilon)
		    (setf (sfvref b-at-tag i) factor)))))))))
    hmm))

(defun compute-epsilon (m n)
  (/ single-float-epsilon (- n m)))

;;;; Next token just returns the class number

(defmethod next-token ((ts training-ts))
  (with-slots (dict-ts) ts
    (multiple-value-bind (stems class surface) (next-token dict-ts)
      (when stems
	(free-string surface)
	(if (simple-string-p stems)
	    (free-string stems)
	  (dotimes (i (length (the simple-vector stems)) (free-sv stems))
	    (free-string (svref stems i))))
	(prog1 (class->number class dict-ts)
	  (free-sv class))))))


(defmethod write-tag-state ((ts training-ts))
  ;; write trained hmm
  (hmm-write-file (ts-hmm ts)
		  (make-pathname :type "hmm"
				 :defaults (tagger-pathname (dict-ts ts))))
  (write-tag-state (dict-ts ts)))
	    
;;;; training utilities

(defun stream-to-tokens (ts limit)
  (let ((count 0)
	(empty-p nil)
	(sent-number (class->number '#(:sent) (dict-ts ts))))
    (values 
     (%with-collection
      (do-tokens (token ts (setq empty-p t))
	(incf count)
	(%collect token)
	(when (and limit (>= count limit) (= token sent-number))
	  (return))))
     count
     (unless empty-p ts))))

(defun tokens-to-input-seq (tokens)
  (let* ((length (length tokens))
	 (vector (make-array length)))
    (dotimes (i length)
      (setf (svref vector i) (%pop tokens)))
    vector))

(defvar *tag-iterations* 5)
(defvar *tag-quantum* 5000)

(defun train-on-streams (next-open-stream close-stream
			 &key ts iterations quantum quiet-p)
  (multiple-value-bind (char-stream start end)
      (funcall next-open-stream 0)
    (let* ((ts (if ts
		   (reinitialize-instance ts :char-stream char-stream
					  :start start :end end)
		 (make-instance 'training-ts
		   :char-stream char-stream :start start :end end)))
	   (q-count 0)
	   #+excl(generations (sys:gsgc-parameter :generation-spread)))
      #+excl(setf (sys:gsgc-parameter :generation-spread) 10)
      (unwind-protect
	  (setf (ts-hmm ts)
	    (hmm-train-multiple
	     #'(lambda ()
		 (let ((count 0)
		       (tokens ())) 
		   (loop
		     (multiple-value-bind (new-tokens length ts-p)
			 (stream-to-tokens ts (- quantum count))
		       (incf count length)
		       (setq tokens (nconc tokens new-tokens))
		       (unless ts-p
			 (funcall close-stream char-stream)
			 (multiple-value-setq (char-stream start end)
			   (funcall next-open-stream q-count))
			 (if char-stream
			     (reinitialize-instance ts :char-stream char-stream
						    :start start :end end)
			   (return nil))))
		     (when (>= count quantum)
		       (unless quiet-p
			 (format *error-output*
				 "~&;;; Training on ~D tokens.~%" count))
		       (incf q-count)
		       (return (tokens-to-input-seq
				(%cons (tag->number :sent (dict-ts ts))
				       tokens)))))))
	     (ts-hmm ts) :iterations iterations))
	#+excl(setf (sys:gsgc-parameter :generation-spread) generations))
      ts)))


;;; Train on separate files

(defun train-on-files (files &key (quantum *tag-quantum*)
				  (iterations *tag-iterations*) quiet-p)
  (write-tag-state
   (train-on-files-internal files quantum iterations quiet-p)))

(defun train-on-files-internal (files quantum iterations quiet-p &optional ts)
  (train-on-streams
   #'(lambda (count)
       (declare (ignore count))
       (let ((file (pop files)))
	 (when file
	   (unless quiet-p
	     (format *error-output* ";;; Opening ~a~%" (namestring file)))
	   (open file))))
   #'(lambda (stream) (close stream))
   :ts ts
   :iterations iterations :quantum quantum :quiet-p quiet-p))


;;; Train on doc's from a corpus

(defun train-on-docs (corpus &key (quanta 5) (quantum *tag-quantum*)
				  (iterations *tag-iterations*) quiet-p
				  (random-state (make-random-state)))
  (multiple-value-bind (ts random-state)
      (train-on-docs-internal corpus random-state quanta quantum
			      iterations quiet-p)
    (write-tag-state ts)
    random-state))

(defun train-on-docs-internal (corpus random-state quanta quantum
			       iterations quiet-p &optional ts)
  (let ((*random-state* (make-random-state random-state))
	(seen-docs (make-array (corpus-id-limit corpus)
			       :element-type 'bit :initial-element 0))
	id)
    (values
     (train-on-streams
      #'(lambda (count)
	  (when (< count quanta)
	    (setq id (get-random-id corpus seen-docs))
	    (unless quiet-p
	      (format *error-output* ";;; Opening ~s~%" (doc-title id corpus)))
	    (open-doc id corpus)))
      #'(lambda (stream)
	  (close-doc stream id corpus))
      :ts ts
      :iterations iterations :quantum quantum :quiet-p quiet-p)
     random-state)))

(defun get-random-id (corpus seen-docs)
  (loop
    (let ((id (random (corpus-id-limit corpus))))
      (when (and (valid-id-p id corpus) (zerop (sbit seen-docs id)))
	(setf (sbit seen-docs id) 1)
	(return id)))))

(defun valid-id-p (id corpus)
  (let ((result nil))
    (map-docs #'(lambda (id) (declare (ignore id)) (setq result t))
	      corpus :start id :end (1+ id))
    result))


;;;; combination training

(defun train-combination (corpus files
			  &key (quanta 5) (quantum *tag-quantum*)
			       (iterations *tag-iterations*) quiet-p
			       (random-state (make-random-state)))
  (multiple-value-bind (ts random-state)
      (train-on-docs-internal corpus random-state quanta quantum
			      iterations quiet-p)
    (write-tag-state
     (train-on-files-internal files quantum iterations quiet-p ts))
    random-state))
    
