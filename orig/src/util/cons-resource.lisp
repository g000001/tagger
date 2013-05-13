;;;-*- Package: CONS-RESOURCE; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1990, 1991 by Xerox Corporation

(cl:defpackage :cons-resource
  (:use :common-lisp :cl-extensions)
  (:export %cons %list %make-list %copy-tree %copy-list
	   %push %pop %with-collection %collect %delete
	   free-cell free-list free-tree))

(cl:in-package :cons-resource)

;;;; cons cell storage management

(defvar *cons-cells* () "Free list of CONS cells linked by CDR")

(defun %cons (car cdr)
  (declare #.tdb:*highly-optimized*)
  (let ((cell *cons-cells*))
    (cond
      (cell
       (setq *cons-cells* (cdr *cons-cells*))
       (setf (car cell) car)
       (setf (cdr cell) cdr)
       cell)
      (t (cons car cdr)))))

(defmacro %list (&rest elements)
  (if elements
      `(%cons ,(first elements) (%list ,@(rest elements)))
      ()))


#|
(put '%with-collection 'fi:common-lisp-indent-hook '(like progn))
|#
(defmacro %with-collection (&body body)
  `(let ((.result. ())
	 (.tail. ()))
     (macrolet ((%collect (form)
		  (once-only (form)
		    `(if .tail.
			 (rplacd .tail. (setq .tail. (%list ,form)))
			 (setq .result. (setq .tail. (%list ,form)))))))
       ,@body
       .result.)))

(defun %make-list (length)
  (declare (type fixnum length) #.tdb:*highly-optimized*)
  (do* ((last nil tail)
	(tail *cons-cells* (cdr tail))
	(head tail)
	(i 0 (1+ i)))
       ((or (null tail) (= i length))
	(setq *cons-cells* tail)
	(if (= i length)
	    (when last
	      (rplacd last nil)
	      head)
	    (if last
		(progn (rplacd last (make-list (the fixnum (- length i))))
		       head)
		(make-list length))))
    (declare (type fixnum i))))    


(defun %copy-tree (tree)
  (if (consp tree)
      (%cons (%copy-tree (car tree))
	     (%copy-tree (cdr tree)))
    tree))

(defun %copy-list (list) (%with-collection (dolist (x list) (%collect x))))

(defmacro %pop (var)
  `(let* ((cell ,var)
	  (car (car cell))
	  (cdr (cdr cell)))
     (setf ,var cdr)
     (free-cell cell)
     car))

(defmacro %push (value place)
  (let ((val-var (gensym "VALUE")))
    `(let ((,val-var ,value))
       (setf ,place (%cons ,val-var ,place)))))

(defun %delete (x l)
  (do* ((first l)
	(prev nil)
	(cell first (cdr prev)))
      ((null cell) first)
    (if (eql (car cell) x)
	(progn (if prev
		   (setf (cdr prev) (cdr cell))
		 (setq first (cdr cell)))
	       (free-cell cell))
      (setq prev cell))))

(defun free-cell (cell)
  (declare #.tdb:*highly-optimized*)
  (when (consp cell)
    (setf (car cell) nil)
    (setf (cdr cell) *cons-cells*)
    (setq *cons-cells* cell)
    nil))

(defun free-list (list)
  (declare #.tdb:*highly-optimized*)
  (do ((last nil cell)
       (cell list (cdr cell)))
      ((not (consp cell))
       (when last
	 (rplacd last *cons-cells*)
	 (setq *cons-cells* list)))
    (rplaca cell nil)))

(defun free-tree (tree)
  ;; recursive on CARs, iterative on CDRs
  (let ((tree tree))
    (loop
     (cond
       ((consp tree)
	(let ((car (car tree))
	      (cdr (cdr tree)))
	  (free-tree car)
	  (free-cell tree)
	  (setq tree cdr)))
       (t (return))))))
