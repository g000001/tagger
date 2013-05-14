;;;-*- Package: VECTOR-RESOURCE; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1991 by Xerox Corporation

#|(cl:defpackage :vector-resource
  (:use :common-lisp :cl-extensions :variable-storage) 
  ;;may want to add binary-io  later
  (:export lert lert* free-fn resource-type))|#


(cl:in-package :vector-resource)

;;; constructors & accessors
(defmacro free-fn (name)
  `(get ,name 'free-fn))

(defsetf free-fn (name) (fn)
  `(setf (get ,name 'free-fn) ,fn))

(defmacro resource-type (name)
  `(get ,name 'resource-type))

(defsetf resource-type (name) (type)
  `(setf (get ,name 'resource-type) ,type))

(defmacro define-vector-resource
    (element-type 
     &key (resource-type (format-name "~A-VECTOR" element-type))
	  (conc-name resource-type)
	  (fill-default 0)
	  (arithmetic-p nil))
  (macrolet ((build-name (str) `(format-name ,str conc-name)))
    (let ((make-fn (build-name "MAKE-~A"))
	  (storage-var (build-name "*~A-STORAGE*"))
	  (%alloc-fn (build-name "%ALLOC-~A"))
	  (clear-fn (build-name "CLEAR-~A-STORAGE"))
	  (alloc-fn (build-name "ALLOC-~A"))
	  (alloc-lfn (build-name "ALLOC-L~A"))
	  (free-fn (build-name "FREE-~A"))
	  (adjust-fn (build-name "ADJUST-~A"))
	  (adjust-lfn (build-name "ADJUST-L~A"))
	  (fill-fn (build-name "FILL-~A"))
	  (fill-lfn (build-name "FILL-L~A"))
	  (copy-fn (build-name "COPY-~A"))
	  (length-macro (build-name "~A-LENGTH"))
	  (let-macro (build-name "LET-~A"))
	  (ref-macro (build-name "~AREF"))
	  (add-fn (build-name "~A-ADD"))
	  (div-fn (build-name "~A-DIV"))
	  (max-fn (build-name "~A-MAX")))
      `(progn

	 (cltl1-eval-when (compile eval load)
	   (export '(,make-fn ,clear-fn ,alloc-fn ,alloc-lfn ,adjust-fn 
		     ,adjust-lfn ,free-fn ,length-macro ,conc-name
		     ,fill-fn ,fill-lfn ,copy-fn ,let-macro))

	   (setf (free-fn ',conc-name) ',free-fn)
	   (setf (resource-type ',conc-name) ',resource-type))
	 
	 ,@(unless (member resource-type ; don't redefine builtin types
			   '(simple-vector simple-bit-vector bit-vector))
	     `((deftype ,resource-type () `(simple-array ,',element-type (*)))
	       (cltl1-eval-when (compile eval load)
		 (export ',resource-type))))

	 (defun ,make-fn (size &optional initial-element)
	   (declare (fixnum size))
	   (if initial-element
	       (make-array size :element-type ',element-type
			   :initial-element initial-element)
	     (make-array size :element-type ',element-type)))
	 
	 (defun ,%alloc-fn (size &optional (will-reclaim-p t))
	   (declare (fixnum size))
	   #+(and excl (not :ansi-cl)) ;; use static arrays whenever possible
	   ,(let ((type (upgraded-array-element-type element-type)))
	      (if (etypecase type
		    (symbol
		     (case type
		       ((bit fixnum string-char single-float double-float) t)))
		    (cons
		     (case (first type)
		       ((signed-byte unsigned-byte) (<= (second type) 32)))))
		  `(if will-reclaim-p
		       (excl:make-static-array size :element-type 
					       ',element-type)
		     (make-array size :element-type ',element-type))
		`(make-array size :element-type ',element-type)))
	   #-(and excl (not :ansi-cl))
	   (make-array size :element-type ',element-type))

	 (defvar ,storage-var 
	     (make-variable-storage #',%alloc-fn ',storage-var))

	 (defun ,clear-fn ()
	   (clear-variable-storage #',%alloc-fn))

	 ,(if (subtypep 'fixnum element-type)
	      `(defmacro ,length-macro (v)
		 (once-only (v)
		   `(aref (the ,',resource-type ,v)
			  (the fixnum
			    (1- (length (the ,',resource-type ,v)))))))
	    `(defmacro ,length-macro (v)
	       (once-only (v)
		 `(coerce
		   (aref (the ,',resource-type ,v)
			 (the fixnum (1- (length (the ,',resource-type ,v)))))
		   'fixnum))))
	 
	 ,(if (subtypep 'fixnum element-type)
	      `(defsetf ,length-macro (v) (new-length)
		 (once-only (v)
		   `(setf (aref (the ,',resource-type ,v) 
				(the fixnum 
				  (1- (length (the ,',resource-type ,v))))) 
		      ,new-length)))
	    `(defsetf ,length-macro (v) (new-length)
	       (once-only (v)
		 `(setf (aref (the ,',resource-type ,v) 
			      (the fixnum 
				(1- (length (the ,',resource-type ,v)))))
		    (coerce ,new-length ',',element-type)))))
	 

	 (defun ,alloc-fn (length &key (initial-element ,fill-default ie-p))
	   (declare (fixnum length))
	   (let ((v (alloc-item length ,storage-var)))
	     (declare (type ,resource-type v))
	     (when ie-p
	       (,fill-fn v initial-element))
	     v))

	 (defun ,alloc-lfn (length &key (initial-element ,fill-default ie-p))
	   (declare (fixnum length))
	   (let ((v (alloc-item (the fixnum (1+ length)) ,storage-var)))
	     (declare (type ,resource-type v))
	     (when ie-p
	       (,fill-fn v initial-element))
	     (setf (,length-macro v) length)
	     v))
	 
	 (defun ,free-fn (v)
	   (declare (type ,resource-type v))
	   (free-item v (length v) ,storage-var)
	   nil)

	 (defun ,adjust-fn (v length)
	   (declare (type ,resource-type v) (fixnum length))
	   (let ((v-length (length v)))
	     (declare (fixnum v-length))
	     (if (<= length v-length)
		 v
	       (progn
		 (let ((w (,alloc-fn length)))
		   (declare (type ,resource-type w))
		   (dotimes (i v-length)
		     (declare (fixnum i))
		     (setf (aref w i) (aref v i)))
		   (,free-fn v)
		   w)))))

	 (defun ,adjust-lfn (v length)
	   (declare (type ,resource-type v) (fixnum length))
	   (let ((v (,adjust-fn v (1+ length))))
	     (setf (,length-macro v) length)
	     v))

	 (defun ,copy-fn (v1 &optional (v2 (,alloc-fn (length v1))))
	   (declare (type ,resource-type v1 v2))
	   (let ((n (length v1)))
	     (declare (fixnum n))
	     (assert (= n (length v2)))
	     (dotimes (i n v2)
	       (declare (fixnum i))
	       (setf (aref v2 i) (aref v1 i)))
	     v2))

	 (defun ,fill-fn (vector &optional (value ,fill-default))
	   (declare (type ,resource-type vector) 
		    (type ,element-type value))
	   (dotimes (i (length vector))
	     (declare (fixnum i))
	     (setf (aref vector i) value))
	   vector)

	 (defun ,fill-lfn (vector &optional (value ,fill-default))
	   (declare (type ,resource-type vector) 
		    (type ,element-type value))
	   (dotimes (i (,length-macro vector))
	     (declare (fixnum i))
	     (setf (aref vector i) value))
	   vector)
	 
	 (defmacro ,let-macro (binds &body body)
	   (let ((vars (mapcar #'car binds)))
	     `(let ,binds
		(declare
		 ,@(mapcar #'(lambda (var) `(type ,',resource-type ,var))
			   vars))
		(prog1 (progn ,@body)
		  ,@(mapcar #'(lambda (var) `(,',free-fn ,var)) vars)))))
	 
	 ,@(unless (eq ref-macro 'svref)
	     `((eval-when (compile eval load)
		 (export ',ref-macro))
	       (defmacro ,ref-macro (var index)
		 `(the ,',element-type
		    (aref (the ,',resource-type ,var) ,index)))))
	 
	 ,@(when arithmetic-p
	     `( 
	       (eval-when (compile eval load)
		 (export '(,add-fn ,div-fn ,max-fn)))
	       
	       (defun ,add-fn (v1 v2)
		 (declare (type ,resource-type v1 v2))
		 (let ((n (length v1)))
		   (declare (fixnum n))
		   (assert (= n (length v2)))
		   (dotimes (i n)
		     (declare (fixnum i))
		     (setf (,ref-macro v1 i) 
		       (+ (,ref-macro v1 i) (,ref-macro v2 i))))
		   v1))
	 
	       ;;finds index of maximum value in vector
	       (defun ,max-fn (vec &optional (count (length vec)))
		 (declare (type ,resource-type vec) (fixnum count))
		 (let ((best (aref vec 0))
		       (index 0))
		   (declare (,element-type best) (fixnum index))
		   (dotimes (i count)
		     (declare (fixnum i))
		     (when (> (aref vec i) best)
		       (setf best (aref vec i)
			     index i)))
		   index))
	 
	       (defun ,div-fn (v const)
		 (declare (type ,resource-type v) (,element-type const))
		 (let ((n (length v)))
		   (declare (fixnum n))
		   (dotimes (i n v)
		     (declare (fixnum i))
		     (setf (,ref-macro v i) (/ (,ref-macro v i) const)))))))
	 ',resource-type))))

(defmacro lert (binds &body body)
  (let* ((resource-binds 
	  (mapcan #'(lambda (bind) (when (third bind) (list bind))) binds))
	 (resource-vars (mapcar #'car resource-binds))
	 (resource-names (mapcar #'third resource-binds))
	 (resource-types (mapcar #'(lambda (name) (resource-type name))
				 resource-names))
	 (resource-frees (mapcar #'(lambda (name) (free-fn name)) 
				 resource-names))
	 (decls (list nil)))
    (loop
      (unless (equal (caar body) 'declare)
	(return))
      (setf (cdr (last decls)) (list (pop body))))
    (pop decls)
    `(let ,(mapcar #'(lambda (bind) (subseq bind 0 2)) binds)
       (declare ,@(mapcar #'(lambda (type var) `(type ,type ,var))
			  resource-types resource-vars))
       ,@(cdr decls)
       (prog1 (progn ,@body)
	 ,@(mapcar #'list resource-frees resource-vars)))))

(defmacro lert* (binds &body body)
  (let* ((resource-binds 
	  (mapcan #'(lambda (bind) (when (third bind) (list bind))) binds))
	 (resource-vars (mapcar #'car resource-binds))
	 (resource-names (mapcar #'third resource-binds))
	 (resource-types (mapcar #'(lambda (name) (resource-type name))
				 resource-names))
	 (resource-frees (mapcar #'(lambda (name) (free-fn name))
				 resource-names))
	 (decls (list nil)))
    (loop
      (unless (equal (caar body) 'declare)
	(return))
      (setf (cdr (last decls)) (list (pop body))))
    `(let* ,(mapcar #'(lambda (bind) (subseq bind 0 2)) binds)
       (declare ,@(mapcar #'(lambda (type var) `(type ,type ,var))
			  resource-types resource-vars))
       ,@(cdr decls)
       (prog1 (progn ,@body)		;allow declarations
	 ,@(mapcar #'list resource-frees resource-vars)))))

(define-vector-resource t
    :conc-name sv :resource-type simple-vector :fill-default nil)

(define-vector-resource fixnum
    :conc-name fixv)

(define-vector-resource single-float
    :conc-name sfv :arithmetic-p t :fill-default 0.0)

(define-vector-resource byte32 :conc-name byte32v)

#|
(put 'let-sfv 'fi:common-lisp-indent-hook '(like let))
(put 'let-sv 'fi:common-lisp-indent-hook '(like let))
(put 'let-fixv 'fi:common-lisp-indent-hook '(like let))
(put 'lert 'fi:common-lisp-indent-hook '(like let))
(put 'lert* 'fi:common-lisp-indent-hook '(like let))
|#
