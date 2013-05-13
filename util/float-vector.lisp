;;;-*- Package: FLOAT-VECTOR; Syntax: Common-Lispfi; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1991 by Xerox Corporation

#|(cl:defpackage :float-vector
  (:use :common-lisp :cons-resource :binary-io :variable-storage
	:vector-resource) 
  (:export #:single-float-vector #:make-sfv #:sfvref #:make-sfv #:alloc-sfv 
	   #:adjust-sfv #:free-sfv #:copy-sfv #:let-sfv
	   #:fill-sfv #:sfv-add #:sfv-div #:sfv-max
	   #:sfv-read #:sfv-write #:sfv-size
	   #:clear-sfv-storage))|# 

(cl:in-package :float-vector)

;;;; I/O

#-(and allegro big-endian)
(defun sfv-write (sfv stream &optional (length (length sfv) length-p))
  (declare (type single-float-vector sfv) (fixnum length))
  (unless length-p
    (int29-write length stream))
  (dotimes (i length)
    (single-float-write (aref sfv i) stream))
  sfv)

#+(and allegro big-endian)
(defun sfv-write (sfv stream &optional (length (length sfv) length-p))
  (declare (type (simple-array (unsigned-byte 8) (*)) sfv) (fixnum length)
	   (optimize (speed 3) (safety 0)))
  (unless length-p
    (int29-write length stream))
  (dotimes (i (the fixnum (ash length 2)))
    (declare (fixnum i))
    (byte8-write (aref sfv i) stream))
  sfv)

#-(and allegro big-endian)
(defun sfv-read (stream &optional (length (int29-read stream))
				  (sfv (make-sfv length)))
  (declare (type single-float-vector sfv) (fixnum length))
  (dotimes (i length)
    (setf (aref sfv i) (single-float-read stream)))
  sfv)

#+(and allegro big-endian)
(defun sfv-read (stream &optional (length (int29-read stream))
				  (sfv (make-sfv length)))
  (declare (type (simple-array (unsigned-byte 8) (*)) sfv) (fixnum length)
	   (optimize (speed 3) (safety 0)))
  (dotimes (i (the fixnum (ash length 2)))
    (declare (fixnum i))
    (setf (aref sfv i) (byte8-read stream)))
  sfv)

(defun sfv-size (sfv &optional (length (length sfv) length-p))
  (declare (type single-float-vector sfv) (fixnum length))
  (the fixnum
    (+ (if length-p 0 (int29-size length))
       (the fixnum (* length 4)))))
