;;;-*- Package: BINARY-IO; Syntax: Common-Lisp; Mode: Lisp -*-

;;; Copyright (c) 1993 by Xerox Corporation


(cl:defpackage :binary-io
  (:use :common-lisp :cl-extensions :string-resource :cons-resource)
  (:export open-byte8-stream *default-byte8-stream-buffer-size*
	   close-byte8-stream with-open-byte8-stream flush-byte8-stream
	   get-byte8-stream-position set-byte8-stream-position
	   byte8-stream-length pad-byte8-stream)
  (:export io-fns io-fns-p io-fns-name ordered-io-fns ordered-io-fns-p
	   find-io-fns make-io-fns make-ordered-io-fns
	   io-size-fn io-read-fn io-write-fn io-free-fn io-copy-fn io-order-fn
	   define-io-fns define-io-struct
	   debug-io-fns undebug-io-fns
	   io-fns-arg)
  (:export byte8 byte8-write byte8-read
	   byte16 byte16-read byte16-write byte16-order
	   byte32 read-byte32 write-byte32
	   swapped-byte32 byte32-read byte32-write)
  (:export null)
  (:export integer integer-write integer-read integer-size integer-order)
  (:export int29 int29-write int29-read int29-size int29-order)
  (:export string stringp simple-string 
	   string-write string-read string-size
	   string-order stringp-order simple-string-order simple-stringp-order
	   simple-string-size)
  (:export single-float single-float-write single-float-read
	   single-float-size single-float-order))

(cl:in-package :binary-io)



;;;; I/O Functions: SIZE, WRITE, READ, FREE, COPY and ORDER.
;;;; (Probably the last three belong elsewhere.)

(eval-when (compile eval load)

(defstruct (io-fns (:print-function %print-io-fns) (:conc-name nil))
  (io-fns-name nil :read-only t)

  (io-size-fn-name nil :type (or symbol list))
  (io-size-fn				; returns number of bytes to write OBJ
   #'(lambda (obj) (declare (ignore obj)) 0)
   :type function)
  (io-read-fn-name nil :type (or symbol list))
  (io-read-fn				; reads an object from STREAM
   #'(lambda (stream) (declare (ignore stream)) nil)
   :type function)
  (io-write-fn-name nil :type (or symbol list))
  (io-write-fn				; writes OBJ to STREAM
   #'(lambda (obj stream) (declare (ignore obj stream)) nil)
   :type function)
  (io-free-fn-name nil :type (or symbol list))
  (io-free-fn				; free's storage for OBJ
   nil :type (or null function))
  (io-copy-fn-name nil :type (or symbol list))
  (io-copy-fn				; returns a fresh copy of OBJ
   nil :type (or null function)))

(defstruct (ordered-io-fns
	     (:print-function %print-io-fns)
	     (:conc-name io-)
	     (:include io-fns))
  (order-fn-name nil :type (or symbol list))
  (order-fn				; returns T, NIL or :EQUAL
   #'(lambda (x y) (declare (ignore x y)) :equal)
    :type function))

(defun %print-io-fns (fns stream depth)
  (declare (ignore depth))
  (let ((*print-case* :capitalize))
    (format stream "#<I/O functions for ~A>" (io-fns-name fns))))

)					; end EVAL-WHEN


(eval-when (compile eval load)

(defvar *io-fns* (make-hash-table :test 'equal))
  
(defmacro find-io-fns (name) `(gethash ,name *io-fns*))

(defvar *fns-being-debugged* (make-hash-table :test 'equal))

)					; end EVAL-WHEN

(defmacro define-io-fns (name &key write-fn read-fn size-fn
				   (free-fn nil free-fn-p)
				   (copy-fn nil copy-fn-p)
				   (order-fn nil order-fn-p))
  `(progn
     (eval-when (compile)
       (setf (find-io-fns ',name)
	 (,(if order-fn-p 'make-ordered-io-fns 'make-io-fns)
	     :io-fns-name ',name
	   :io-size-fn-name ',size-fn
	   :io-read-fn-name ',read-fn
	   :io-write-fn-name ',write-fn
	   ,@(when free-fn-p `(:io-free-fn-name ',free-fn))
	   ,@(when copy-fn-p `(:io-copy-fn-name ',copy-fn))
	   ,@(when order-fn-p `(:order-fn-name ',order-fn)))))
     (setf (find-io-fns ',name)
       (,(if order-fn-p 'make-ordered-io-fns 'make-io-fns)
	   :io-fns-name ',name
	 :io-size-fn-name ',size-fn :io-size-fn #',size-fn
	 :io-read-fn-name ',read-fn :io-read-fn #',read-fn
	 :io-write-fn-name ',write-fn :io-write-fn #',write-fn
	 ,@(when free-fn-p
	     `(:io-free-fn-name ',free-fn :io-free-fn #',free-fn))
	 ,@(when copy-fn-p
	     `(:io-copy-fn-name ',copy-fn :io-copy-fn #',copy-fn))
	 ,@(when order-fn-p
	     `(:order-fn-name ',order-fn :order-fn #',order-fn))))
     (remhash ',name *fns-being-debugged*)
     ',name))


(defstruct (parsed-io-struct
	    (:conc-name pis-)
	    (:constructor make-pis (slot-names slot-types accessors))
	    (:type list))
  slot-names
  slot-types
  accessors)



(defmacro define-io-struct (name&options &rest slots)
;;; Defines a structure and associated i/o functions.  The syntax is mostly
;;; like defstruct, with the following additions & restrictions:
;;; 1. The constructor is always a BOA constructor which takes all slots in
;;;    the order they are given.
;;; 2. Slots must always be specified with lists, a default and :TYPE must be
;;;    specified, moreover, the :TYPE must named a defined binary-io type.
;;; 3. The :ALLOCATOR and :FREER may be named in the options.
;;; 4. Slots to order by may be named with the :ORDER-BY option.
  (macrolet ((getf-option (key format-string)
	       `(let ((option (assoc ,key options)))
		  (cond
		   (option (setq options (delete option options))
			   (second option))
		   (t (format-name ,format-string name-string))))))
    (let* ((name (if (consp name&options) (car name&options) name&options))
	   (name-string (symbol-name name))
	   (options (and (consp name&options) (copy-list (cdr name&options))))
	   (prefix-option (assoc :conc-name options))
	   (prefix (if prefix-option
		       (string (or (second prefix-option) ""))
		     (format nil "~A-" name-string)))
	   (include-name (second (assoc :include options)))
	   (include-pis
	    (when include-name
	      (or (get include-name 'parsed-io-struct)
		  (error "~S not a defined I/O struct." include-name))))
	   (local-slot-names (mapcar #'car slots))
	   (slot-names (append (pis-slot-names include-pis) local-slot-names))
	   (fns (mapcar #'io-fns-arg
			(append (pis-slot-types include-pis)
				(mapcar #'(lambda (slot) (getf slot :type))
					slots))))
	   (accessors
	    (append (pis-accessors include-pis)
		    (mapcar #'(lambda (name) (format-name "~A~A" prefix name))
			    local-slot-names)))
	   (resource-var (format-name "*~A-RESOURCE*" name-string))
	   (allocator (getf-option :allocator "ALLOC-~A"))
	   (freer (getf-option :freer "FREE-~A"))
	   (constructor (getf-option :constructor "MAKE-~A"))
	   (copier (getf-option :copier "COPY-~A"))
	   (read-fn (format-name "~A-READ" name-string))
	   (write-fn (format-name "~A-WRITE" name-string))
	   (size-fn (format-name "~A-SIZE" name-string))
	   (order-by-slots (cdr (assoc :order-by options)))
	   (order-fn
	    (when order-by-slots
	      (setq options (delete (assoc :order-by options) options))
	      (format-name "~A-ORDER" name-string)))
	   (v1 (gensym)) (v2 (gensym)))
      `(eval-when (compile eval load)
	 (defstruct (,name
		     (:copier nil)
		     (:constructor ,constructor ,slot-names)
		     ,@options)
	   ,@slots)
	 (defparameter ,resource-var nil)
	 (defun ,allocator ,slot-names
	   (let ((,v1 (%pop ,resource-var)))
	     (cond
	      (,v1
	       ,@(mapcar #'(lambda (accessor val) `(setf (,accessor ,v1) ,val))
			 accessors slot-names)
	       ,v1)
	      (t (,constructor ,@slot-names)))))
	 (defun ,freer (,v1)
	   ,@(mapcan #'(lambda (io accessor)
			 (when (io-free-fn io)
			   `((,(io-free-fn-name io) (,accessor ,v1)))))
		     fns accessors)
	   (%push ,v1 ,resource-var)
	   nil)
	 (defun ,copier (,v1)
	   (,allocator
	    ,@(mapcar
	       #'(lambda (io accessor)
		   (if (io-copy-fn io)
		       `(,(io-copy-fn-name io) (,accessor ,v1))
		     `(,accessor ,v1)))
	       fns accessors)))
	 (defun ,read-fn (stream)
	   (,allocator
	    ,@(mapcar #'(lambda (io) `(,(io-read-fn-name io) stream))
		      fns)))
	 (defun ,write-fn (,v1 stream)
	   ,@(mapcar #'(lambda (io accessor)
			 `(,(io-write-fn-name io) (,accessor ,v1) stream))
		     fns accessors)
	   ,v1)
	 (defun ,size-fn (,v1)
	   (let ((,v2 0))
	     (declare (type byte16 ,v2))
	     ,@(mapcar
		#'(lambda (io accessor)
		    `(incf ,v2 (,(io-size-fn-name io) (,accessor ,v1))))
		fns accessors)
	     ,v2))
	 ,@(when order-by-slots
	     (labels ((order-body (x)
			(let* ((slot (if (consp (car x)) (caar x) (car x)))
			       (position  (position slot slot-names))
			       (accessor (nth position accessors))
			       (order (if (consp (car x))
					  (cadar x)
					(io-order-fn-name (nth position fns))))
			       (compare-form
				`(,order (,accessor ,v1) (,accessor ,v2))))
			  (if (null (cdr x))
			      compare-form
			    `(case ,compare-form
			       ((nil) nil)
			       (:equal ,(order-body (cdr x)))
			       (t t))))))
	       `((defun ,order-fn (,v1 ,v2) ,(order-body order-by-slots)))))
	 (define-io-fns
	     ,name :read-fn ,read-fn :write-fn ,write-fn :size-fn ,size-fn
	     :copy-fn ,copier :free-fn ,freer
	     ,@(when order-by-slots `(:order-fn ,order-fn)))
	 (setf (get ',name 'parsed-io-struct)
	   (make-pis ',slot-names ',(mapcar #'io-fns-name fns) ',accessors))
	 ',name))))

(defun io-fns-arg (spec &optional ordered)

;;; If SPEC is a IO or names a IO then it (the IO) is returned,
;;; otherwise an error is signalled.

  (let ((io (if (typep spec 'io-fns)
		   spec
		   (find-io-fns spec))))
    (unless io
      (error "~S not defined for BINARY-IO" spec))
    (when ordered
      (unless (ordered-io-fns-p io)
	(error "~S not defined for ordered BINARY-IO" spec)))
    io))


(define-io-fns null 
    :write-fn (lambda (null stream) (declare (ignore null stream)) nil)
    :read-fn (lambda (stream) (declare (ignore stream)) nil)
    :size-fn (lambda (null) (check-type null null) 0))
