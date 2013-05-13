;;;-*- Package: LIST-SETS; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
 
;;; Copyright (c) 1992 by Xerox Corporation

;;;; list implementation of FSA set protocol

#|(cl:defpackage :list-sets
  (:use :common-lisp :fsa)
  (:export :list-sets :list-set))|#

(cl:in-package :list-sets)

(defclass list-set (ordered-set)
	  ((order :accessor list-order :initarg :order-fn)
	   (head :accessor list-head :initarg :contents
		 :initform '() :type list)))

(defmethod print-object ((set list-set) stream)
  (format stream "#<list-set: ~s>" (list-head set)))

(defclass list-sets () ())

(defmethod make-ordered-set (order (fsa list-sets))
  (make-instance 'list-set :order-fn order))
	  
(defmethod set-order-fn ((set list-set))
  #'(lambda (set-1 set-2)
      (let ((tail-1 (list-head set-1))
	    (tail-2 (list-head set-2))
	    (order (list-order set)))
	(loop
	  (when (null tail-1) (return (and (null tail-2) :equal)))
	  (when (null tail-2) (return t))
	  (let* ((value (funcall order (car tail-1) (car tail-2))))
	    (unless (eq value :equal)
	      (return value)))
	  (setq tail-1 (cdr tail-1)
		tail-2 (cdr tail-2))))))

(defmethod set-length ((set list-set))
  (length (list-head set)))

(defmethod set-insert (element (set list-set))
  (do ((tail (list-head set) (cdr tail))
       (prev nil tail)
       (order (list-order set)) pred)
      ((or (null tail) (setq pred (funcall order element (car tail))))
       (unless (eq pred :equal)
	 (let ((cell (cons element tail)))
	   (if prev (setf (cdr prev) cell) (setf (list-head set) cell))))))
  element)

(defmethod set-clear ((set list-set)) (setf (list-head set) '()) set)

(defmethod set-copy ((set list-set))
  (make-instance 'list-set :order-fn (list-order set)
		 :contents (copy-list (list-head set))))
		 
(defmethod set-union ((set-1 list-set) (set-2 list-set))
  (let ((order (list-order set-1))
	(head ())
	(last ())
	(tail-1 (list-head set-1))
	(tail-2 (list-head set-2)))
    (loop (when (or (null tail-1) (null tail-2)) (return))
      (let ((ordering (funcall order (car tail-1) (car tail-2)))
	    (cell tail-1))
	(cond ((eq ordering :equal)
	       (setq tail-1 (cdr tail-1))
	       (setq tail-2 (cdr tail-2)))
	      (ordering
	       (setq tail-1 (cdr tail-1)))
	      (t (setq cell tail-2)
		 (setq tail-2 (cdr tail-2))))
	(if last
	    (setf last (setf (cdr last) cell))
	  (setf head (setf last cell)))))
    (if last
	(setf (cdr last) (or tail-1 tail-2))
      (setq head (or tail-1 tail-2)))
    (setf (list-head set-1) head)
    set-1))

(defmethod set-intersect ((set-1 list-set) (set-2 list-set))
  (let ((order (list-order set-1))
	(head ())
	(last ())
	(tail-1 (list-head set-1))
	(tail-2 (list-head set-2)))
    (loop (when (or (null tail-1) (null tail-2)) (return))
      (let ((ordering (funcall order (car tail-1) (car tail-2))))
	(cond ((eq ordering :equal)
	       (if last
		   (setf last (setf (cdr last) tail-1))
		 (setf head (setf last tail-1)))
	       (setq tail-1 (cdr tail-1))
	       (setq tail-2 (cdr tail-2)))
	      (ordering
	       (setq tail-1 (cdr tail-1)))
	      (t (setq tail-2 (cdr tail-2))))))
    (when last (setf (cdr last) '()))
    (setf (list-head set-1) head)
    set-1))

(defmethod set-minus ((set-1 list-set) (set-2 list-set))
  (let ((order (list-order set-1))
	(head ())
	(last ())
	(tail-1 (list-head set-1))
	(tail-2 (list-head set-2)))
    (loop (when (or (null tail-1) (null tail-2)) (return))
      (let ((ordering (funcall order (car tail-1) (car tail-2))))
	(cond ((eq ordering :equal)
	       (setq tail-1 (cdr tail-1))
	       (setq tail-2 (cdr tail-2)))
	      (ordering
	       (if last
		   (setf last (setf (cdr last) tail-1))
		 (setf head (setf last tail-1)))
	       (setq tail-1 (cdr tail-1)))
	      (t (setq tail-2 (cdr tail-2))))))
    (if last
	(setf (cdr last) tail-1)
      (setq head tail-1))
    (setf (list-head set-1) head)
    set-1))

(defmethod set-map (fn (set list-set))
  (dolist (elt (list-head set))
    (funcall fn elt)))

(defmethod set-member-p (elt (set list-set))
  (let ((order (list-order set)))
    (dolist (x (list-head set))
      (when (eq (funcall order x elt) :equal)
	(return t)))))

(defmethod set-empty-p ((set list-set))
  (null (list-head set)))
