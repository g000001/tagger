;;;-*- Package: SV-RESOURCE; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992 by Xerox Corporation

#|(cl:defpackage :sv-resource
  (:use :common-lisp)
  (:export alloc-sv %vector sv-copy free-sv))|#

(cl:in-package :sv-resource)

;;;; simple-vector storage management

(defconstant +max-sv-length+ (1- (expt 2 8)))

(defvar *sv-table* (make-array (1+ +max-sv-length+) :initial-element nil))
(defvar *sv-fill-pointers*
    (make-array (1+ +max-sv-length+) :initial-element 0))
(defvar *sv-table-lengths*
    (make-array (1+ +max-sv-length+) :initial-element 0))

(defmacro sv-fill-pointer (length)
  `(the fixnum (svref *sv-fill-pointers* ,length)))
(defmacro sv-table-length (length)
  `(the fixnum (svref *sv-table-lengths* ,length)))

(defun alloc-sv (length)
  (declare (fixnum length) #.tdb:*highly-optimized*)
  (if (or (> length +max-sv-length+)
	  (zerop (sv-fill-pointer length)))
      (make-array length)		; really have to cons a new one
      (svref (svref *sv-table* length)	; can reuse an old sv
	     (decf (sv-fill-pointer length)))))

(defmacro %vector (&rest args)
  (let ((var (gensym)))
    `(let ((,var (alloc-sv ,(length args))))
       ,@(do ((body ())
	      (i 0 (1+ i))
	      (arg-tail args (cdr arg-tail)))
	     ((null arg-tail) (nreverse body))
	   (push `(setf (svref ,var ,i) ,(car arg-tail)) body))
       ,var)))

(defun sv-copy (old &optional (length (length old)) (new (alloc-sv length)))
  (declare (simple-vector old new) (fixnum length))
  (dotimes (i length new)
    (declare (fixnum i))
    (setf (svref new i) (svref old i))))

(defun free-sv (sv)
  (declare (simple-vector sv))
  (check-type sv simple-vector)
  (let ((length (length sv)))
    (declare (fixnum length))
    (unless (> length +max-sv-length+)
      (let ((vector (svref *sv-table* length))
	    (fill-pointer (sv-fill-pointer length)))
	(declare (fixnum fill-pointer))
	(when (= fill-pointer (sv-table-length length))
	  (let* ((new-length (* 2 (1+ (sv-table-length length))))
		 (new-vector (make-array new-length)))
	    (and vector (replace new-vector vector))
	    (setf (sv-table-length length) new-length)
	    (setf (svref *sv-table* length) new-vector)
	    (setq vector new-vector)))
;;;	(dotimes (i fill-pointer)
;;;	  (when (eq (svref vector i) sv) (error "already free!")))
	(setf (sv-fill-pointer length) (the fixnum (1+ fill-pointer)))
	(setf (svref vector fill-pointer) sv)))))
