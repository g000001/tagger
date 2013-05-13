;;;-*- Package: SSB ; Syntax: Common-Lisp; Mode: Lisp; -*-

;;; Copyright (c) 1990 by Xerox Corporation


;;; Optimized versions of some string operations.  Equivalent things can be
;;; accomplished with adjustable arrays & sequence functions, but usually
;;; accompanied by a performance penalty.

(cl:defpackage :ssb
  (:use :common-lisp :cl-extensions)
  (:export make-ssb ssb-buffer ssb-pointer ssb-push-extend ssb-string
	   copy-ssb reinitialize-ssb ssb-read-line ssb-string=)) 

(cl:in-package :ssb)


;;;; ssb basics

(defparameter *default-ssb-size* 80)

(defstruct (sstring-buffer
	     (:constructor %make-ssb (buffer size))
	     (:copier nil)
	     (:print-function %print-ssb)
	     (:conc-name ssb-))
  (buffer (make-string 0) :type simple-string)
  (pointer 0 :type fixnum)
  (size *default-ssb-size* :type fixnum))

(defun %print-ssb (ssb stream depth)
  (declare (ignore depth))
  (format stream "#<SSB ~S>" (ssb-string ssb)))

(defun make-ssb (&optional (size *default-ssb-size*))
  (%make-ssb (make-string size) size))

(defmacro ssb-push-extend (char ssb)
  (once-only (char ssb)
	     `(let ((pointer (ssb-pointer ,ssb)))
		(declare (fixnum pointer))
		(when (= pointer (ssb-size ,ssb))
		  (extend-ssb ,ssb))
		(setf (schar (ssb-buffer ,ssb) pointer) ,char)
		(incf (ssb-pointer ,ssb))
		,char)))

(defun extend-ssb (ssb)
  (let* ((buffer (ssb-buffer ssb))
	 (size (ssb-size ssb))
	 (new-size (the fixnum (ash size 1)))
	 (new-buffer (make-string new-size)))
    (sstring-replace buffer new-buffer size)
    (setf (ssb-size ssb) new-size)
    (setf (ssb-buffer ssb) new-buffer)))

(defun ssb-string (ssb)
  (let ((pointer (ssb-pointer ssb)))
    (sstring-replace (ssb-buffer ssb) (make-string pointer) pointer)))

(defun sstring-replace (old new length)
  (declare (fixnum length))
  (dotimes (i length new)
    (declare (fixnum i))
    (setf (schar new i) (schar old i))))


;;;; ssb utilities

(defun reinitialize-ssb (ssb sstring &optional (length (length sstring)))
  (declare (fixnum length) (simple-string sstring))
  (setf (ssb-pointer ssb) 0)
  (dotimes (i length)
    (declare (fixnum i))
    (ssb-push-extend (schar sstring i) ssb)))

(defun ssb-read-line (stream ssb)
  (setf (ssb-pointer ssb) 0)
  (do ((i 0 (1+ i))
       (char (fast-read-file-char stream) (fast-read-file-char stream)))
      ((eql char #\newline) ssb)
    (declare (fixnum i))
    (when (eq char :eof) (return (if (zerop i) nil ssb)))
    (ssb-push-extend char ssb)))

(defun ssb-string= (ssb sstring)
  (declare (simple-string sstring))
  (let ((pointer (ssb-pointer ssb))	
	(length (length sstring)))
    (declare (fixnum pointer length))
    (when (= pointer length)
      (let ((buffer (ssb-buffer ssb)))
	(dotimes (i pointer t)
	  (declare (fixnum i))
	  (unless (char= (schar buffer i) (schar sstring i))
	    (return)))))))

(defun copy-ssb (ssb1 &optional (ssb2 (make-ssb (ssb-pointer ssb1))))
  (let ((length (ssb-pointer ssb1))
	(buffer1 (ssb-buffer ssb1))
	(buffer2 (ssb-buffer ssb2)))
    (declare (fixnum length))
    (when (< (ssb-size ssb2) length)
      (setf (ssb-buffer ssb2) (setq buffer2 (make-string length)))
      (setf (ssb-size ssb2) length))
    (setf (ssb-pointer ssb2) length)
    (sstring-replace buffer1 buffer2 length)
    ssb2))
