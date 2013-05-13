;;;-*- Package: STRING-RESOURCE; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1990, 1991 by Xerox Corporation

(cl:defpackage :string-resource
  (:use :common-lisp :cl-extensions)
  (:export +max-string-length+
	   alloc-string string-copy simple-string-copy free-string))

(cl:in-package :string-resource)

;;;; string storage management

(defconstant +max-string-length+ (1- (expt 2 8)))

(defparameter *string-table*
  (make-array (1+ +max-string-length+) :initial-element nil))
(defparameter *string-fill-pointers*
  (make-array (1+ +max-string-length+) :initial-element 0))
(defparameter *string-table-lengths*
  (make-array (1+ +max-string-length+) :initial-element 0))

(defmacro string-fill-pointer (length)
  `(the fixnum (svref *string-fill-pointers* ,length)))
(defmacro string-table-length (length)
  `(the fixnum (svref *string-table-lengths* ,length)))

(defun alloc-string (length)
  (declare (fixnum length) #.tdb:*highly-optimized*)
  (if (or (> length +max-string-length+)
	  (zerop (string-fill-pointer length)))
      (make-string length)		   ; really have to cons a new one
      (svref (svref *string-table* length) ; can reuse an old string
	     (decf (string-fill-pointer length)))))

(defun string-copy (old &optional
			(length (length old))
			(new (alloc-string length)))
  (declare (fixnum length))
  (dotimes (i length new)
    (declare (fixnum i))
    (setf (schar new i) (char old i))))

#+allegro
;; Lie to the compiler to get word moves.
;; Assume 2-word alignment, 1 word header.
(defun simple-string-copy (old &optional
			       (length (length (the simple-string old)))
			       (new (alloc-string length)))
  (declare (fixnum length) (optimize (speed 3) (safety 0)))
  (macrolet ((ref (a i) `(the fixnum (svref ,a ,i))))
    (setf (ref new 0) (ref old 0))	; there's always at least one word
    (do ((end (ash (+ length 3) -2))
	 (i 1 (1+ i)))
	((>= i end) new)
      (declare (fixnum end i))
      (setf (ref new i) (ref old i)))))
#-allegro
(defun simple-string-copy (old &optional
			       (length (length (the simple-string old)))
			       (new (alloc-string length)))
  (declare (fixnum length) (simple-string old))
  (dotimes (i length new)
    (declare (fixnum i))
    (setf (schar new i) (schar old i))))

(defun free-string (string)
  (declare (simple-string string))
  (check-type string simple-string)
  (let ((length (length string)))
    (declare (fixnum length))
    (unless (> length +max-string-length+)
      (let ((vector (svref *string-table* length))
	    (fill-pointer (string-fill-pointer length)))
	(declare (fixnum fill-pointer))
	(when (= fill-pointer (string-table-length length))
	  (let* ((new-length (* 2 (1+ (string-table-length length))))
		 (new-vector (make-array new-length)))
	    (and vector (replace new-vector vector))
	    (setf (string-table-length length) new-length)
	    (setf (svref *string-table* length) new-vector)
	    (setq vector new-vector)))
;	(dotimes (i fill-pointer)
;	  (when (eq (svref vector i) string) (error "already free!")))
	(setf (string-fill-pointer length) (the fixnum (1+ fill-pointer)))
	(setf (svref vector fill-pointer) string)))))
