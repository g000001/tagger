;;;-*- Package: VARIABLE-STORAGE; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1990, 1991 by Xerox Corporation

;;; Resource facility for variable sized objects

#|(cl:defpackage :variable-storage
  (:use :common-lisp :cons-resource)
  (:use :tagger.internal)
  (:export #:make-variable-storage #:clear-variable-storage
	   #:count-variable-storage #:print-variable-storage
	   #:alloc-item #:free-item
	   #:with-storage-balance))|# 

(cl:in-package :variable-storage)


;;;BIT MANIPULATION FUNCTIONS

;;;fast integer length

(cltl1-eval-when (compile eval load)
  (deftype fixnum-vector () `(simple-array fixnum (*)))

  (defmacro fash (x y)
    `(the fixnum (ash (the fixnum ,x) (the fixnum ,y))))

  (defmacro f+ (x y)
    `(the fixnum (+ (the fixnum ,x) (the fixnum ,y))))

  (declaim (ftype (function (fixnum) fixnum) fast-integer-length))
  (let ((length-table (make-array #X100 :element-type 'fixnum)))
    (declare (type fixnum-vector length-table))
    (dotimes (i #X100)
      (declare (fixnum i))
      (setf (aref length-table i) (integer-length i)))
    
    (defun fast-integer-length (x)
      (declare (fixnum x))
      (let ((ans 0))
	(declare (fixnum ans))
	#-(or x86_64 x86-64)
        (progn
          (unless (zerop (fash x -16))
            (setf ans 16
                  x (fash x -16)))
          (unless (zerop (fash x -8))
            (setf ans (f+ ans 8)
                  x (fash x -8)))
          (f+ ans (aref length-table x)))
	#+(or x86_64 x86-64)
        (progn
          (unless (zerop (fash x -32))
            (setf ans 32
                  x (fash x -32)))
          (unless (zerop (fash x -16))
            (setf ans (f+ ans 16)
                  x (fash x -16)))
          (unless (zerop (fash x -8))
            (setf ans (f+ ans 8)
                  x (fash x -8)))
          (f+ ans (aref length-table x)))))))

;;ash-1 is like ash, except it shift in ones rather than zeros if you shift
;;left. 
(cltl1-eval-when (compile load eval)
  (defmacro int (n)
    `(the fixnum ,n))
  (defmacro ash-1 (i count)
    `(int (lognot (int (ash (lognot ,i) ,count)))))
  (defconstant +significant-bits+ 1)
  (defconstant +significant-bit-mask+
      (ash-1 0 +significant-bits+))
  (defconstant +significant-bit-shift+ (1+ +significant-bits+)))

;;;plan is for hi-bits to be the <significant-bits> bits following the msb of
;;;the input number n.  these form the low end of an index whose high end is
;;;the integer length of the number.
(cltl1-eval-when (compile eval load)

  (defun size-bucket (n)
    (declare (fixnum n))
    (let* ((len (fast-integer-length n))
	   (hi-bits (logand (int (ash n (- +significant-bit-shift+ len)))
			    +significant-bit-mask+))
	   (index (logior (int (ash len +significant-bits+)) hi-bits))) 
      (declare (fixnum len hi-bits index))
;;;    (format t "~&~4d: len ~d hi-bits ~d index ~d" n len hi-bits index)
      index)))

;;;the largest number which can end up in a bucket is one whose msb is the one
;;;defined by the length field (high order bits) of the bucket, and whose next
;;;lower bits are the ones specified in the low field of the bucket, and all of
;;;whose other bits are one.
(defun bucket-size (b)
  (declare (fixnum b))
  (let* ((hi-bits (logand b +significant-bit-mask+))
	 (len (ash b (- +significant-bits+)))
	 (num (int (ash-1 (logior hi-bits (int (ash 1 +significant-bits+)))
			  (- len +significant-bit-shift+))))) 
    (declare (fixnum hi-bits len  num))
;;    (format t "~&~4d: len ~d hi-bits ~d num ~d" b len hi-bits num)
    num))

(defun round-bucket (size)
  (declare (fixnum size))
  (logior size 
	  (ash-1 0 (the fixnum
		     (- (the fixnum (fast-integer-length size))
			(the fixnum +significant-bit-shift+))))))

(defmacro fits-bucket-p (size)
  `(= (the fixnum ,size) (the fixnum (round-bucket ,size))))

;;;STORAGE ALLOCATOR

(cltl1-eval-when (compile load eval)
  (defconstant +largest-bucket+ (size-bucket most-positive-fixnum))
  (defconstant +end-buckets+ (1+ +largest-bucket+)))
(defvar *all-buckets* nil)

(declaim (fixnum *storage-count*))
(defvar *storage-count* 0)

(defstruct (storage-bucket 
	    (:conc-name sb-)
	    (:print-function print-storage-bucket))
  name
  (buckets (make-array +end-buckets+) :type (simple-array t *))
  (out-size 0 :type fixnum)
  (out-count 0 :type fixnum)
  (cons-fn #'default-cons :type (function (fixnum &optional t) t)))

(defun print-storage-bucket (sb &optional (stream t) depth)
  (declare (ignore depth))
  (let* ((counts (map 'vector #'(lambda (x) (if (listp x) (length x) 0))
		      (sb-buckets sb)))
	 (in-count 0)
	 (in-size 0))
    (declare (fixnum in-count in-size)
	     #|(type (vector fixnum *) counts)|#)
    (dotimes (i +end-buckets+)
      (declare (fixnum i))
      (incf in-count (aref counts i))
      (incf in-size (* (aref counts i) (bucket-size i))))
    (format stream "~&#<Storage bucket ~A." (sb-name sb))
    (format stream "~&Free: items ~d, size ~d.  Used: items ~d,  size ~d."
	    in-count in-size (sb-out-count sb) (sb-out-size sb))
    (format stream "~&Bucket counts ~S>" counts)))

(defun default-cons (size &optional will-reclaim-p)
  (declare (ignore will-reclaim-p))
  (make-array size))

(defun make-variable-storage (cons-fn &optional name)
  (let ((bucket (make-storage-bucket :name name :cons-fn cons-fn)))
    (push bucket *all-buckets*)
    bucket))

(defun clear-variable-storage (&optional sb)
  (if sb
      (let ((buckets (sb-buckets sb)))
	(setf (sb-out-count sb) 0)
	(setf (sb-out-size sb) 0)
	(dotimes (i +end-buckets+)
	  (declare (fixnum i))
	  (setf (svref buckets i) nil)))
    (progn
      (setf *storage-count* 0)
      (mapc #'clear-variable-storage *all-buckets*))))

(defun print-variable-storage (&optional (sb *all-buckets*))
  (format t "Storage outstanding: ~D~&" *storage-count*)
  (print sb)
  nil)
    
;;;ITEM ALLOCATOR

(defvar *hash-storage-p* nil)
(defvar *allocated-items* (make-hash-table :test #'eq))

;;;BUG---exact allocs will get consed in static space and never reclaimed.
;;;SOLUTION: give cons-fn an optional argument consisting of whether requested
;;;object will be dropped on the floor.
(defun alloc-item (size sb &key (exact nil))
  (declare (fixnum size))
  (let ((result
	 (let ((rounded-size (round-bucket size)))
	   (declare (fixnum rounded-size))
	   (if (and exact (/= size rounded-size))
	       (progn 
		 (incf *storage-count* size)
		 (funcall (sb-cons-fn sb) size nil)) ;signal won't reclaim
	     (let ((bucket (size-bucket size)))
	       (declare (fixnum bucket))
	       (incf *storage-count* rounded-size)
	       (incf (sb-out-count sb))
	       (incf (sb-out-size sb) rounded-size)
	       (or (%pop (svref (sb-buckets sb) bucket))
		   (funcall (sb-cons-fn sb) rounded-size)))))))
    (when *hash-storage-p*
      (setf (gethash result *allocated-items*) result))
    result))
    
(defun free-item (item size sb)
  (declare (fixnum size))
  (decf *storage-count* size)
  (when *hash-storage-p*
    ;;(assert (gethash item *allocated-items*) (item)
    ;;    "attempt to free non-allocated storage ~S" item)
    (unless (gethash item *allocated-items*)
      (error "attempt to free non-allocated storage ~S" item))
    (remhash item *allocated-items*))
  (when (= size (int (round-bucket size)))
    (let ((bucket (size-bucket size)))
      (declare (fixnum bucket))
      (decf (sb-out-count sb))
      (decf (sb-out-size sb) size)
      (%push item (svref (sb-buckets sb) bucket)))))

#-nodebug
(defmacro with-storage-balance (&body body)
  (let ((start-storage (gensym "STORAGE")))
    `(let ((,start-storage *storage-count*))
       (declare (fixnum ,start-storage))
       (prog1 (progn ,@body)
	 (assert (= ,start-storage (the fixnum *storage-count*)) ()
	   "unbalanced storage allocation")))))

#+nodebug
(defmacro with-storage-balance (&body body)
  `(progn ,@body))
