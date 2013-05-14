;;;-*- Package: CL-EXTENSIONS; Syntax: Common-Lisp; Mode: Lisp; -*-

;;; Copyright (c) 1988-1994 by Xerox Corporation

#|(cl:defpackage :cl-extensions
  (:use :common-lisp #+(and allegro (version>= 4 1)) :clos)
  (:use :tagger.internal)
  (:export format-name)
  (:export with-collection collect)
  (:export once-only)
  (:export make-lock with-lock with-locks without-interrupts
	   process-block add-process
	   kill-process this-process)
  (:export byte8 byte16 byte32 int29 byte28 byte7)
  (:export define-list-sorter get-qua
	   effective-function with-effective-function)
  (:export fast-read-char fast-read-file-char)
  (:export make-directory remove-directory))|#

(cl:in-package :cl-extensions)



;;;; format-name
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun format-name (format &rest args)
    (let ((*print-case* :upcase))
      (intern (apply #'format nil format args)))))
  
;;;; with-collection

#|
(put 'with-collection 'fi:common-lisp-indent-hook '(like progn))
|#

(cltl1-eval-when (compile load eval)
  (defmacro with-collection (&body body)
    `(let (($with-collection-result$ nil)
	   $with-collection-tail$)
       (macrolet
	   ((collect (form)
                
	      ;;  The FORM is evaluated first so that COLLECT nests
	      ;; properly, i.e., The test to determine if this is
	      ;; the first value collected should be done after the
	      ;; value itself is generated in case it does
	      ;; collection as well.
	      `(let (($collectable$ ,form))
		 (if $with-collection-tail$
		     (rplacd $with-collection-tail$
			     (setq $with-collection-tail$
				   (list $collectable$)))
		     (setq $with-collection-result$
			   (setq $with-collection-tail$
				 (list $collectable$))))
		 $with-collection-tail$)))
	 ,@body $with-collection-result$))))


;;;; once-only

#|
(put 'once-only 'fi:common-lisp-indent-hook '(like let))
|#

(cltl1-eval-when (compile load eval)
  (defmacro once-only (vars &body body)
 
    ;;  ONCE-ONLY assures that the forms given as vars are evaluated in
    ;; the proper order, once only. Used in the body of macro
    ;; definitions.
 
    ;; Taken from Zeta Lisp.
    (let*
	((gensym-var (gensym))
	 (run-time-vars (gensym))
	 (run-time-vals (gensym))
	 (expand-time-val-forms
	  (mapcar #'(lambda (var)
		      `(if (or (symbolp ,var)
			       (constantp ,var))
			   ,var
			   (let ((,gensym-var (gensym)))
			     (push ,gensym-var ,run-time-vars)
			     (push ,var ,run-time-vals)
			     ,gensym-var)))
		  vars)))
      `(,'let* (,run-time-vars
		,run-time-vals
		(wrapped-body
		 (,'let ,(with-collection
			     (do ((var vars (cdr var))
				  (expand-time-val-form
				   expand-time-val-forms
				   (cdr expand-time-val-form)))
				 ((null var))
			       (collect (list (car var)
					      (car expand-time-val-form)))))
		   ,@body)))
	 `(,'let ,(with-collection (do ((run-time-var
					 (reverse ,run-time-vars)
					 (cdr run-time-var))
					(run-time-val
					 (reverse ,run-time-vals)
					 (cdr run-time-val)))
				       ((null run-time-var))
				     (collect (list (car run-time-var)
						    (car run-time-val)))))
	    ,wrapped-body)))))


;;;; Process Stuff

#+excl (eval-when (compile eval load) (require :process))

(defmacro make-lock (&optional name)
#+xerox `(il:create.monitorlock ,name)
#+excl  `(mp:make-process-lock :name ,name)
#-(or xerox excl) (declare (ignore name))
#-(or xerox excl) nil
)

(defmacro with-lock (place &body body)
#+xerox `(il:with.monitor ,place ,@body)
#+excl  `(mp:with-process-lock (,place) ,@body)
#+lucid `(lcl:with-process-lock (,place) ,@body)

#-(or xerox excl lucid) (declare (ignore place))
#-(or xerox excl lucid) `(progn ,@body)
)

(defmacro with-locks (locks &body body)
  (if locks
      `(with-lock ,(first locks)
	 (with-locks ,(rest locks)
	   ,@body))
      `(progn ,@body)))

(defmacro without-interrupts (&body body)
  #+excl`(excl:without-interrupts ,@body)
  #+lucid`(lcl:with-scheduling-inhibited ,@body)
  #-(or excl lucid) `(progn ,@body))


(defmacro process-block ()
#+xerox `(il:block)
#+excl `(mp:process-allow-schedule)
#-(or xerox excl) nil)


(defmacro add-process (name function &rest args)
#+xerox `(il:add.process
		`'(apply ,',function
		   ',(list ,@args))
		'il:name ,name)
#+excl  `(mp:process-run-function ,name ,function ,@args)
#+lucid `(lcl:make-process :name ,name
			   :function ,function
			   :args (list ,@args))
#-(or xerox excl lucid) (declare (ignore name))
#-(or xerox excl lucid) `(funcall ,function ,@args)
)

(defmacro kill-process (process)
#+xerox `(il:del.process ,process)
#+excl  `(mp:process-kill ,process)
#+lucid `(lcl:kill-process ,process)
#-(or xerox excl lucid) (declare (ignore process))
#-(or xerox excl lucid) nil
)

(defmacro this-process ()
  #+xerox `(il:this.process)
  #+excl  `mp:*current-process*)



;;;; handy type declarations


(deftype byte8 () '(unsigned-byte 8))
(deftype byte16 () '(unsigned-byte 16))
(deftype byte32 () '(unsigned-byte 32))

(deftype int29 () `(signed-byte 29))
(deftype byte28 () `(unsigned-byte 28))
(deftype byte7 () `(unsigned-byte 7))



;;;; DEFINE-LIST-SORTER defines a function named NAME which sorts linked lists.

;;; :NEXT names the accessor for the link.  CDR is used by default.
;;; Lists must be terminated with NIL.

;;; :KEY names the key reader.  By default the cell itself is passed to the
;;; order function.

;;; :ORDER names the order function.  If :ORDER is not provided, the defined
;;; sort function will take an order function as its second argument, otherwise
;;; it takes just one argument, the list to be sorted.

;;; Note that :NEXT, :KEY and :ORDER may name macros, as they are not
;;; funcalled, but rather inserted directly in the code.

(defmacro define-list-sorter (name &key (key 'progn) (next 'cdr)
				   order key-type)
  `(defun ,name (list ,@(if order () '(order)))
     (labels ((sort-list (head)
		(if (null (,next head))
		    head
		    (merge-lists (sort-list (split-list head))
				 (sort-list head))))
	      (split-list (list)	; break LIST at midpoint, return tail
		(let ((slow list)
		      (fast (,next (,next list))))
		  (loop (when (null fast)
			  (return (prog1 (,next slow)
				    (setf (,next slow) nil))))
			(setq slow (,next slow))
			(setq fast (,next fast))
			(and fast (setq fast (,next fast))))))
	      (merge-lists (list-1 list-2) ; merge two sorted lists
		(let ((first-cell nil)
		      (last-cell nil)
		      (tail-1 list-1)
		      (key-1 (,key list-1))
		      (tail-2 list-2)
		      (key-2 (,key list-2)))
		  ,@(and key-type `((declare (type ,key-type key-1 key-2))))
		  (macrolet ((collect-cell (key-var tail-var other-tail-var)
			       `(progn
				  (cond (last-cell
					 (setf (,',next last-cell) ,tail-var)
					 (setq last-cell ,tail-var))
					(t (setq first-cell ,tail-var)
					   (setq last-cell ,tail-var)))
				  (setq ,tail-var (,',next ,tail-var))
				  (if ,tail-var
				      (setq ,key-var (,',key ,tail-var))
				      (progn (setf (,',next last-cell)
						   ,other-tail-var)
					     (return))))))
		    (loop (if ,(if order
				   `(,order key-1 key-2)
				   '(funcall order key-1 key-2))
			      (collect-cell key-1 tail-1 tail-2)
			      (collect-cell key-2 tail-2 tail-1)))
		    first-cell))))
       (when list
	 (sort-list list)))))


;;;; GET-QUA -- a disembodied CALL-NEXT-METHOD

;#-(or (and allegro (version>= 4)) lucid pcl mcl)
;(warn "Don't know how to get qua!")

#+(or (and allegro (version>= 4)) lucid)
(eval-when (compile eval load) (use-package :clos))
#+pcl
(eval-when (compile eval load) (import 'pcl::class-precedence-list))
#+mcl
(eval-when (compile eval load) (import 'ccl:class-precedence-list))

#+(or (and allegro (version>= 4)) lucid pcl mcl sbcl)
(defun get-qua-internal (generic-function class superclass)
  ;; This assumes standard method combination, one argument generic
  ;; functions, and primary methods only.
  (let ((methods 
	 (remove nil
		 (mapcar
		  #'(lambda (class)
		      (find-method generic-function () (list class) nil))
		  (cdr (member superclass (c2mop:class-precedence-list class)))))))
    #+pcl
    (pcl::make-effective-method-function
     generic-function
     (pcl::compute-effective-method
      generic-function pcl::*standard-method-combination* methods))
    #+(and allegro (version>= 4) (not (version>= 8)))
    (clos::compute-effective-method-standard-mc generic-function methods)
    #+(and allegro (version>= 8) )
    (excl::compute-effective-method-standard-mc generic-function methods)
    #+lucid
    (clos::make-effective-method-function 
     generic-function
     (clos::compute-effective-method-body generic-function methods))
    #+mcl
    (ccl::compute-effective-method 
     generic-function
     (ccl::generic-function-method-combination generic-function)
     methods)
    #+sbcl
    (sb-pcl::fast-method-call-function
     (sb-pcl::make-effective-method-function    
      generic-function
      (sb-pcl:compute-effective-method generic-function
                                       sb-pcl::*standard-method-combination*
                                       methods)))))


(defmacro get-qua (gf obj super)
#-cmu17
  `(get-qua-internal #',gf (class-of ,obj) (find-class ',super))
#+cmu17
  `(let ((call
	  (get-qua-internal #',gf
			    (kernel:class-pcl-class (class-of ,obj))
			    (kernel:class-pcl-class (find-class ',super)))))
     #'(lambda (arg)
	 (funcall
	  (pcl::fast-method-call-function call)
	  (pcl::fast-method-call-pv-cell call)
	  (pcl::fast-method-call-next-method-call call)
	  arg))))

#+cmu17
(defun find-method (&rest args) (apply #'pcl::get-method args))

;(defmethod initialize-instance :after ((stream my-class) &key)
;  (setf (my-qua-slot stream) (get-qua my-method stream my-class)))


;;;; WITH-EFFECTIVE-FUNCTION -- for loop optimization of method calls

;;; first: EFFECTIVE-FUNCTION, the runtime support

#+(and allegro (version>= 4 1))
(progn

(defvar *ef-cache* (make-hash-table :test #'equal))

(defclass ef-dependent () ((key :accessor key :initarg :key)))

(defun effective-function (&rest gf-and-classes)
  (declare (dynamic-extent gf-and-classes))
  (or (gethash gf-and-classes *ef-cache*)	  ; hit case does not cons
      (destructuring-bind (gf &rest classes) gf-and-classes
	(let* ((key (copy-list gf-and-classes))
	       (dep (make-instance 'ef-dependent :key key)))
	  (dolist (metaobj gf-and-classes)	  ; note dependencies
	    (add-dependent metaobj dep))
	  (setf (gethash key *ef-cache*)	  ; cache lookup
	    (compute-effective-function gf classes))))))

(defmethod update-dependent (metaobject (d ef-dependent) &rest args)
  (declare (ignore args metaobject))		  ; METAOBJECT has changed
  (let ((key (key d)))
    (remhash key *ef-cache*)			  ; invalidate cache
    (dolist (metaobj key)			  ; clear dependencies
      (remove-dependent metaobj d))))

(defun compute-effective-function (gf classes)
  (let ((effective
	 (compute-effective-method
	  gf (generic-function-method-combination gf)
	  (compute-applicable-methods-using-classes gf classes))))
    (if (and (consp effective)
	     (eq (car effective) 'call-method)
	     (null (caddr effective)))
	(method-function (cadr effective))  ; use primary method
      gf)))					  ; punt to generic function
)

#-(and allegro (version>= 4 1))
(defun effective-function (gf &rest classes)
  (declare (ignore classes))
  gf)						  ; punt to generic function


(defvar *the-class-t* (find-class t))	; reduce calls to find-class


;;; finally, the macro itself

(defmacro with-effective-function ((gf &rest args) &body body)
  (let ((classes (mapcar #'(lambda (arg)
			     (if (eq arg t) `*the-class-t* `(class-of ,arg)))
			 args))
	(var (gensym)))
    `(let ((,var (effective-function #',gf ,@classes)))
       (macrolet ((,gf (&rest args) `(funcall ,',var ,@args)))
	 ,@body))))




;;;; FAST-READ-CHAR: returns :EOF at eof

(defmacro fast-read-char (stream)
  ;; #+(and allegro (version>= 4)) `(stream:stream-read-char ,stream)
  #+lucid`(lcl:fast-read-char ,stream nil :eof)
  #+mcl`(or (ccl:stream-tyi ,stream) :eof)
  #-(or lucid mcl) `(read-char ,stream nil :eof))

(defmacro fast-read-file-char (stream) `(fast-read-char ,stream))

#+(and allegro-foo (version>= 4))
(define-compiler-macro fast-read-file-char (stream)
  `(macrolet ((stream-slots (stream)
		`(the simple-vector (svref ,stream 1)))
	      (stream-buffer (slots)
		`(the simple-string (svref ,slots 11)))
	      (stream-buffpos (slots)
		`(the fixnum (svref ,slots 12)))
	      (stream-maxbuffpos (slots)
		`(the fixnum (svref ,slots 13))))
     #|(declare (optimize (speed 3) (safety 0)))|#
     (let* ((stream ,stream)
	    (slots (stream-slots stream))
	    (buffpos (stream-buffpos slots))
	    (maxbuffpos (stream-maxbuffpos slots)))
       (declare (fixnum buffpos maxbuffpos))
       (if (>= buffpos maxbuffpos)
	   (stream:stream-read-char stream)
	 (prog1 (schar (stream-buffer slots) buffpos)
	   (when (= (setf (stream-buffpos slots) (the fixnum (1+ buffpos)))
		    maxbuffpos)
	     (setf (stream-maxbuffpos slots) 0)))))))

#+mcl
(define-compiler-macro fast-read-file-char (stream)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (or (ccl::%ftyi (svref ,stream 3)) :eof)))

;;;; Insure string input streams are repositionable.  In particular, file-length
;;;; and file-position should work as expected.

;; No fix needed for franz 4.x

#+sbcl
(defmethod file-length ((stream stream))
  (cl:file-length stream))


#+sbcl
(defmethod file-length ((stream string-stream))
  (sb-impl::string-input-stream-end stream))


#+mcl
(defmethod file-length ((stream string-stream) &optional new)
  (declare (ignore new))
  (slot-value stream 'ccl::end))

#+lucid
(progn
  (lcl:defadvice (file-position tdb) (stream &optional position)
    (if (lucid::string-input-stream-p stream)
	(if position
	    (if (<= 0 position (lucid::string-input-stream-limit stream))
		(setf (lucid::string-input-stream-index stream) position)
		(error "Illegal position: ~s" position))
	    (lucid::string-input-stream-index stream))
	(lcl:advice-continue stream position)))
  (lcl:defadvice (file-length tdb) (stream)
    (if (lucid::string-input-stream-p stream)
	(lucid::string-input-stream-limit stream)
	(lcl:advice-continue stream))))


;;; MAKE-DIRECTORY
#+excl
(progn
  (eval-when (compile eval load) (require :foreign))
  #-svr4
  (load ""
	:unreferenced-lib-names `(,(ff:convert-to-lang "mkdir")
				  ,(ff:convert-to-lang "rmdir"))
	:verbose t)
  (ff:defforeign-list
      `((%mkdir :entry-point ,(ff:convert-to-lang "mkdir"))
	(%rmdir :entry-point ,(ff:convert-to-lang "rmdir"))))
  (defvar *%umask* #o775)
  (defun make-directory (pathname)
    (let ((directory (pathname-directory pathname)))
      (unless (eq (first directory) :absolute)
	(error "~A not an absolute pathname" pathname))
      (unless (zerop (%mkdir (format nil "~{/~A~}" (rest directory)) *%umask*))
	(error "Can't make directory ~S" pathname))))
  (defun remove-directory (pathname)
    (let ((directory (pathname-directory pathname)))
      (unless (eq (first directory) :absolute)
	(error "~A not an absolute pathname" pathname))
      (unless (zerop (%rmdir (format nil "~{/~A~}" (rest directory))))
	(error "Can't remove directory ~S" pathname))))
  )
;#-excl(warn "MAKE-DIRECTORY not implemented.")


#|
(put 'defclass 'fi:common-lisp-indent-hook '((1 2 quote) (0 t 2)))
(put 'reinitialize-instance 'fi:common-lisp-indent-hook '(like make-instance))
|#

;; Avoid bug in CMU destructuring of dotted pairs
#+cmu
(defmacro destructuring-bind (&rest rest) `(pcl::destructuring-bind ,@rest))

;; Allow floating-point underflow (generates denormalized floats or zero) 
;; and overflow (generates infinity)
#+cmu
(extensions:set-floating-point-modes :traps '(:invalid :divide-by-zero))
