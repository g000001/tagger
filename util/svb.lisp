;;;-*- Package: SVB ; Syntax: Common-Lisp; Mode: Lisp; -*-

;;; Copyright (c) 1991 by Xerox Corporation

;;; Simple Vector Buffer: fast, extendable vectors

#|(cl:defpackage :svb
  (:use :common-lisp :cl-extensions :string-resource)
  (:export svb make-svb copy-svb svb-buffer svb-pointer svb-push-extend
	   copy-svb-to-string copy-string-to-svb
	   svbref svb-size extend-svb))|#

(cl:in-package :svb)

;;;; svb basics

(defparameter *default-svb-size* 80)

(defstruct (svb
	    (:constructor %make-svb (buffer size))
	    (:print-function %print-svb)
	    (:copier nil))
  (buffer (make-array 0) :type simple-vector)
  (pointer 0 :type fixnum)
  (size *default-svb-size* :type fixnum))

(defmacro svbref (svb i) `(svref (svb-buffer ,svb) ,i))

(defun %print-svb (svb stream depth)
  (declare (ignore depth))
  (format stream "#<SVB ~S>" (subseq (svb-buffer svb) 0 (svb-pointer svb))))

(defun make-svb (&optional (size *default-svb-size*))
  (declare (fixnum size))
  (%make-svb (make-array size :initial-element nil) size))

(defmacro svb-push-extend (elt svb)
  (once-only (elt svb)
    `(let ((pointer (svb-pointer ,svb)))
       (declare (fixnum pointer))
       (when (= pointer (svb-size ,svb))
	 (extend-svb ,svb))
       (setf (svref (svb-buffer ,svb) pointer) ,elt)
       (incf (svb-pointer ,svb))
       ,elt)))

(defun extend-svb (svb &optional new-size)
  (let* ((buffer (svb-buffer svb))
	 (size (svb-size svb))
	 (new-size (or new-size (if (zerop size) 2 (the fixnum (ash size 1)))))
	 (new-buffer (make-array new-size :initial-element nil)))
    (declare (fixnum size new-size))
    (sv-replace buffer new-buffer size)
    (setf (svb-size svb) new-size)
    (setf (svb-buffer svb) new-buffer)))

(defun copy-svb (svb1 &optional (svb2 (make-svb (svb-pointer svb1))))
  (let ((length (svb-pointer svb1))
	(buffer1 (svb-buffer svb1))
	(buffer2 (svb-buffer svb2)))
    (declare (fixnum length))
    (when (< (svb-size svb2) length)
      (setf (svb-buffer svb2) (setq buffer2 (make-array length)))
      (setf (svb-size svb2) length))
    (setf (svb-pointer svb2) length)
    (sv-replace buffer1 buffer2 length)
    svb2))

(defun sv-replace (old new length)
  (declare (fixnum length) #.tdb:*highly-optimized*)
  (dotimes (i length new)
    (declare (fixnum i))
    (setf (svref new i) (svref old i))))

(defun copy-svb-to-string (svb &optional
			       (length (svb-pointer svb))
			       (string (alloc-string length)))
  (declare (fixnum length) (simple-string string))
  (let ((buffer (svb-buffer svb)))
    (dotimes (i length string)
      (declare (fixnum i))
      (setf (schar string i) (svref buffer i)))))

(defun copy-string-to-svb (string &optional
				  (svb (make-svb (length string)))
				  (length (length string)))
  (declare (fixnum length) (simple-string string) #.tdb:*highly-optimized*)
  (unless (>= (svb-size svb) length)
    (setf (svb-buffer svb) (make-array length))
    (setf (svb-size svb) length))
  (let ((buffer (svb-buffer svb)))
    (dotimes (i length)
      (declare (fixnum i))
      (setf (svref buffer i) (schar string i))))
  (setf (svb-pointer svb) length)
  svb)
