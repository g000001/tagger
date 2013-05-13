;;; -*- Mode: LISP; Syntax: Common-lisp; Package: COMMON-LISP; Base: 10; Lowercase: Yes -*-
;;;
;;; *************************************************************************
;;;
;;; Copyright (c) 1989 by Xerox Corporation.
;;; All rights reserved.
;;;
;;; *************************************************************************
;;;
;;; Provide a condition system adhering to the CL standard, as specified
;;; in the Common Lisp Condtion System spec, Revision #18, and the X3J13
;;; proposal CLOS-CONDITIONS:INTEGRATE.  This later proposal specifies
;;; how the Condition system is to be integrated with CLOS.

#|

** Catalog of items defined in the Condition System Spec

Here is the tree of predefined condition types:

Condition---Simple-Condition
         |
         |--Warning-------------Simple-Warning
         |
         |--Serious-Condition---Storage-Condition
                             |
                             |--Error-------Simple-Error
                                     |
                                     |------Control-Error
                                     |
                                     |------Type-Error---------Simple-Type-Error
                                     |
                                     |------Program-Error
                                     |
                                     |------Package-Error
                                     |
                                     |------Stream-Error-------End-Of-File
                                     |
                                     |------File-Error
                                     |
                                     |------Cell-Error---------Unbound-Variable
                                     |                |
                                     |                |--------Undefined-Function
                                     |
                                     |------Arithmetic-Error---Division-By-Zero
                                                            |
                                                            |--Floating-Point-Overflow
                                                            |
                                                            |--Floating-Point-Underflow


The spec also provides the following structure info:

TYPE			SLOTS
---------------------------------
Simple-Condition	format-string format-arguments
Simple-Warning		format-string format-arguments
Simple-Error		format-string format-arguments
Type-Error		datum expected-type
Simple-Type-Error	format-string format-arguments
Package-Error		package
Stream-Error		stream
File-Error		pathname
Cell-Error		name
Arithmetic-Error	operation operands


The following functions/macros are the defined interface to the condition system:

DEFINE-CONDITION name (supers) [({slot}*) {option}*]			[Macro]
MAKE-CONDITION type &rest slot-initializations				[Function]
ERROR datum &rest arguments						[Function]
CERROR continue-format-string datum &rest arguments			[Function]
SIGNAL datum &rest arguments						[Function]
WARN datum &rest arguments						[Function]
HANDLER-BIND ({(type handler)}*) {form}*				[Macro]
HANDLER-CASE expression {(type ([var]) {form}*)}*			[Macro]
IGNORE-ERRORS {form}*							[Macro]
RESTART-CASE expression {(case-name arglist {keyword value}* {form}*)}*	[Macro]
RESTART-BIND ({(name function {keyword value}*)}*) {form}*		[Macro]
WITH-SIMPLE-RESTART (name format-string {format-argument}*) {form}*	[Macro]
RESTART-NAME restart							[Function]
COMPUTE-RESTARTS							[Function]
FIND-RESTART identifier							[Function]
INVOKE-RESTART restart &rest arguments					[Function]
INVOKE-RESTART-INTERACTIVELY restart					[Function]
ABORT									[Function]
CONTINUE								[Function]
MUFFLE-WARNING								[Function]
STORE-VALUE value							[Function]
USE-VALUE value								[Function]
BREAK &optional format-string &rest format-arguments			[Function]
INVOKE-DEBUGGER datum &rest arguments					[Function]


** Compatiblity Note

Some other specified functions (ASSERT, ECASE, ...) present no special
problem once all the other machinery is in place.

ERROR, CERROR, WARN, and BREAK are defined in CLtL, which presents us
with a bit of a package problem.  We take care of this by providing a
package to replace the LISP package.  The COMMON-LISP package is
"plug-compatible" with the LISP package, and contains our def's of these
conflicting functions.


Lucid provides the following additional condition types:
 Stack-Overflow (subtype of Storage-Condition)
 Storage-Exhausted (subtype of Storage-Condition)
 Illegal-Throw (subtype of Error) slots: tag
 System-Error (subtype of Simple-Error)

Franz provides the following additional condition types:
 Stack-Overflow (subtype of Storage-Condition)
 Storage-Exhausted (subtype of Storage-Condition)
 Simple-Break (subtype of Simple-Condition)
 Case-Failure (subtype of Type-Error) slots: name possibilities

For the nonce, we ignore these extra condition types, since we couldn't
deal with them in a portable way anyway.


** CLOSified condtions

Currently, the lisps we are using implement approximate versions of the
Condition System, but are not integrated with CLOS.  There are a number
of problems with extending these existing implementations to use CLOS
condition objects.

1) Since existing system code (like the debugger) is no doubt dependent
on the structure of condition objects, we can't just pass around CLOS
objects as conditions.  Whenever there is any possibility of a CLOS
condition object being passed to the underlying system, it has to be
"wrapped" in a system-dependent condtion.

2) Since user code expects condition objects to be CLOS objects, system
generated condtion objects need to be wrapped in a CLOS condition object
before they are made visible to user code.

3) CLOS supports multiple-inheritance, but the existing condition
systems do not allow it.  For example, it should be possible to define a
new condition type that is a subtype of, say, type-error and file-error.
This implementation does not support multiple-inheritance where on of
the supertypes is a primitive condtion type.


Before getting into details of the implementation, let's settle on some
terminology.  Condition objects that are defined by and understood by
the underlying Lisp are called "primitive conditions".

For each primitive condition type, we define a corresponding CLOS
condition type.  Instances of this CLOS condition type have a slot that
holds an instance of the primitive condition type.

For each primitive condition type in the spec, we define the following:

1) A CLOS condtion that serves to "wrap" instances of the primitive
condition type in CLOS condition objects.  These CLOS wrapper types have
the same name as the primitive condtion type, and are the types that are
finally made visible to the user.  Instances of this CLOS condition type
have a slot that contains the primitive condtion type.  Accessors on the
CLOS condtion type work by calling the appropriate accessors on the
wrapped primitive condition.  Since CLOS condition types specialize the
wrapper for the primitive condition type CONDITION, all CLOS conditions
wrap some primitive condition.

2) A new primitive condition that is used to wrap CLOS condition
objects.  This is used in situations where a CLOS condition object has to
be handled by some primtive condtion handling functions.  While it might
seem that (1) implies we could use the wrapped primtive condition in the
CLOS condition object, there is no way to go from this wrapped primitive
condition back to the CLOS condtion object.

3) ??

|#


(in-package "COMMON-LISP")

(eval-when (eval compile load)
  
(defconstant primitive-condition-package
  (find-package #+lucid "LCL"
		#+(or allegro cmu) "CONDITIONS"
		#+xerox "XCL"))

) ; eval-when

;;; Define CLOS conditions for each predefined condition type.

;; Wrapper-info objects are used to store the associations between
;; primitive condition types, the CLOS conditions that wrap them, and
;; the primitive conditions that in turn wrap them.
(defstruct (wrapper-info (:type list))
  primitive-condition			;The primitive condition
  wrapper				;The CLOS condition that wraps it
  surrogate-class
  extractor-fn
  slot-info)

(defvar *condition-wrappers* nil
  "A list of (clos-condition wrapper-info)")

(defvar *unwrapper-functions* (make-hash-table))

;; Internals, used in the expansion of the MAKE-CONDITION-WRAPPER 
(defgeneric clos-condition-wrapper-type (condition))

(defgeneric clos-condition-wrapper-initialization-args
	    (condition &rest init-list))

(defmacro make-condition-wrapper (type supers &rest slots)
  (let* ((c (make-symbol "CONDTION"))
	 (*package* (symbol-package type))
	 (*print-case* :upcase)
 	 (wrapper-name (intern (format nil "~S-WRAPPER" type)
			       (find-package "CL")))
	 (surrogate-class (intern (format nil "%SURROGATE-~S-CONDITION" type)
				  (find-package "CL")))
	 (define-condition (intern "DEFINE-CONDITION"
				   primitive-condition-package))
	 (make-condition (intern "MAKE-CONDITION"
				 primitive-condition-package))
	 (keyword-package (find-package "KEYWORD"))
	 (primitive-condition (intern (symbol-name type) primitive-condition-package))
	 (extractor-fn (intern (format nil"~S-CLOS-CONDITION" wrapper-name)
			       (find-package "CL")))
	 (slot-info '()))
    `(progn
       ;; Define the wrapper condition
       (,define-condition ,wrapper-name
	  (,primitive-condition)
	  (clos-condition)
	  (:report (lambda (condition stream)
		     (format stream "~A" (,extractor-fn condition)))))
       ;; Define the CLOS condition type to shadow the standard condition
       (defclass ,type ,supers
	   (,@(when (null supers)	;Only need the pointer in the root class
		'((%condition-wrapper :initarg :condition-wrapper
		                      :accessor clos-condition-%condition-wrapper)))))
       (defmethod clos-condition-wrapper-type ((c ,type))
	  ',wrapper-name)
       ,@(if (null supers)
	   `((defmethod initialize-instance :after
			((,c ,type) &rest slots &key ,@slots &allow-other-keys)
	       (unless (slot-boundp ,c '%condition-wrapper)
		 (setf (slot-value ,c '%condition-wrapper)
		       (apply #',make-condition
			      (clos-condition-wrapper-type ,c)
			      :clos-condition ,c
			      (apply #'clos-condition-wrapper-initialization-args
				     ,c slots))))))
	   ;; This next is bogus, required to get around PCL initialization arg checking
	   #+pcl
	   `((defmethod initialize-instance :after
			((,c ,type) &key ,@slots &allow-other-keys)
	       (declare (ignore ,@slots))
	       ,c)))
       (defmethod clos-condition-wrapper-initialization-args
		  ((,c ,type) &key ,@slots &allow-other-keys)
	 (list* ,@(mapcan
		    #'(lambda (slot)
			`(',(intern (symbol-name slot) keyword-package)
			   ,slot))
		    slots)
		,(if (null supers)
		     'nil
		     '(call-next-method))))
       ;; Make the classes delegate the appropriate set of functions to the
       ;; condition object.
       ,@(mapcar
	   #'(lambda (slot)
	       (let* ((accessor (intern (format nil "~S-~S" type slot)
					(find-package "CL")))
		      (raw-accessor (intern (symbol-name accessor)
					    primitive-condition-package)))
		 (push `(,(intern (symbol-name slot) keyword-package)
			  ,raw-accessor)
		       slot-info)
		 `(progn
		    (defmethod ,accessor ((,c ,type))
		      (,raw-accessor (slot-value ,c '%condition-wrapper)))
		    (defmethod ,accessor ((,c t))
		      (,raw-accessor ,c)))))
	   slots)
       ;; Make a surrogate class to convert primitve conditions into CLOS conditions
       (defclass ,surrogate-class (,type) ())
       (defmethod print-object ((,c ,surrogate-class) stream)
	 (if *print-escape*
	     (call-next-method)
	     (format stream "~A" (slot-value ,c '%condition-wrapper))))
       ;; Update the wrapper info
       (setf (gethash ',wrapper-name *unwrapper-functions*) #',extractor-fn)
       (let ((old-info (assoc ',type *condition-wrappers* :test #'eq))
	     (info (make-wrapper-info :wrapper ',wrapper-name
				      :primitive-condition ',primitive-condition
				      :surrogate-class ',surrogate-class
				      :extractor-fn ',extractor-fn
				      :slot-info ',slot-info)))
	 (if old-info
	     (setf (cadr info) info)
	     (push (list ',type info) *condition-wrappers*)))
       ',type)))

(make-condition-wrapper condition ())

(defmethod print-object ((condition condition) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "A condition of type ~S occurred."
	      (or (class-name (class-of condition))
		  (class-of condition)))))


(make-condition-wrapper simple-condition (condition)
  format-string format-arguments)

(defmethod print-object ((condition simple-condition) stream)
  (if *print-escape*
      (call-next-method)
      (apply #'format stream
	     (simple-condition-format-string condition)
	     (simple-condition-format-arguments condition))))


(make-condition-wrapper serious-condition (condition))


(make-condition-wrapper storage-condition (serious-condition))


(make-condition-wrapper warning (condition))


(make-condition-wrapper simple-warning (warning simple-condition))


(make-condition-wrapper error (serious-condition))


(make-condition-wrapper simple-error (error simple-condition))


(make-condition-wrapper control-error (error))


(make-condition-wrapper type-error (error) datum expected-type)

(defmethod print-object ((condition type-error) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "~S should be a ~S."
	      (type-error-datum condition)
	      (type-error-expected-type condition))))


(make-condition-wrapper simple-type-error (type-error simple-condition))


(make-condition-wrapper program-error (error))


(make-condition-wrapper package-error (error) package)


(make-condition-wrapper stream-error (error) stream)

(defmethod print-object ((condition stream-error) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "Error on stream ~S."
	      (stream-error-stream condition))))


(make-condition-wrapper end-of-file (stream-error))

(defmethod print-object ((condition end-of-file) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "End of file on stream ~S."
	      (stream-error-stream condition))))


(make-condition-wrapper file-error (error) pathname)

(defmethod print-object ((condition file-error) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "Some error involving file ~A."
	      (file-error-pathname condition))))


(make-condition-wrapper cell-error (error) name)


(make-condition-wrapper unbound-variable (cell-error))

(defmethod print-object ((condition unbound-variable) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "The symbol ~S has no global value."
	      (cell-error-name condition))))


(make-condition-wrapper undefined-function (cell-error))

(defmethod print-object ((condition undefined-function) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "The function ~S is unbound."
	      (cell-error-name condition))))


(make-condition-wrapper arithmetic-error (error) operation operands)


(make-condition-wrapper division-by-zero (arithmetic-error))


(make-condition-wrapper floating-point-overflow (arithmetic-error))


(make-condition-wrapper floating-point-underflow (arithmetic-error))

;;; Define functions to shadow existing Condition system functions

;; Creating and raising conditions

(defmacro define-condition (name supers slots &rest options)
  (let ((report (cadr (assoc :report options :test #'eq)))
	(doc (cadr (assoc :documentation options :test #'eq))))
    `(progn (defclass ,name (,@supers condition)
		;; The "official" syntax for slot specs is the same as CLOS.
		;; However old code will (probably) be around, and it expects
		;; DEFSTRUCT like syntax.  It's easy to tell them apart, so we
		;; allow both.
		,(mapcar #'(lambda (s)
			     (if (and (consp s)
				      (= (length s) 2))
				 ;; DEFSTRUCT style
				 (let ((slot-name (car s)))
				   `(,slot-name
				       ,@(if (and (consp s) (not (null (cdr s))))
					     `(:initform ,(cadr s))
					     nil)
				       :reader ,(intern (format nil "~A-~A" name slot-name)
							(find-package "CL"))
				       :initarg ,(intern (symbol-name slot-name)
							 (find-package "KEYWORD"))))
				 ;; Otherwise it's CLOS style.  Note that CLOS
				 ;; style slot specs don't define accessors or initargs.
				 s))
			 slots)
	      ,@(when doc
		  `((:documentation ,doc))))
	    ,@(when report
		(when (stringp report)
		  (setq report `(lambda (condition stream)
				 (declare (ignore condition))
				 (write-string ,report stream))))
		`((defmethod print-object ((x ,name) stream)
		    (funcall #',report x stream)))))))

(defun make-condition (datum &rest args)
  ;; This should never be passed "raw" conditions as understood by the
  ;; underlying lisp (or the type of such raw conditions), since we shadow all
  ;; those primitive conditions with CLOS conditions.  But it can't hurt to
  ;; allow it, so we do.  It only clutters the code a little.
  (cond ((typep datum 'condition)
	 datum)
	((typep datum (intern "CONDITION" primitive-condition-package))
	 (funcall (gethash (type-of datum) *unwrapper-functions* #'identity)
		  datum))
	((symbolp datum)
	 (let ((class (find-class datum nil)))
	   ;; Recurse here to avoid duplicating the code that converts a "raw"
	   ;; condition to a CLOSified condition.
	   (if class
	       (make-condition (apply #'make-instance class args))
	       (make-condition (apply
				(intern "MAKE-CONDITION" primitive-condition-package)
				 datum args)))))
	(t (error 'type-error :datum datum :expected-type 'condition))))

(defun make-condition-internal (datum &rest args)
  (clos-condition-%condition-wrapper
    (apply #'make-condition datum args)))

(defun error (datum &rest args)
  (lisp:error (if (stringp datum)
		  (make-condition-internal 'simple-error
					   :format-string datum
					   :format-arguments args)
		  (apply #'make-condition-internal datum args))))

(defun cerror (continue-format-string datum &rest args)
  (lisp:cerror continue-format-string
	       (if (stringp datum)
		   (make-condition-internal 'simple-error
					    :format-string datum
					    :format-arguments args)
		   (apply #'make-condition-internal datum args))))

(defun signal (datum &rest args)
  (#+lucid lcl:signal
   #+xerox xcl:signal
   #+(or allegro cmu) lisp:error
	   (if (stringp datum)
	       (make-condition-internal 'simple-condition
					:format-string datum
					:format-arguments args)
	       (apply #'make-condition-internal datum args))))

(defun warn (datum &rest args)
  (let ((condition (if (stringp datum)
		       (make-condition-internal 'simple-warning
						:format-string datum
						:format-arguments args)
		       (apply #'make-condition-internal datum args))))
    (if (typep condition #+lucid 'lcl:warning
	                 #+xerox 'xcl:warning
	                 #+allegro 'conditions:condition
			 #+cmu 'lisp:condition)
	(lisp:warn condition)
	(error 'type-error :datum datum :expected-type 'warning))))

(defun invoke-debugger (datum &rest args)
  (let ((c (if (stringp datum)
	       (make-condition-internal 'simple-error
					:format-string datum
					:format-arguments args)
	       (apply #'make-condition-internal datum args))))
    #+lucid
    (lcl:invoke-debugger c)
    #+xerox
    (xcl:invoke-debugger c)
    #+allegro
    (conditions:invoke-debugger c)
    #+cmu(lisp:invoke-debugger c)))

;; Handlers

(defmacro handler-bind (bindings &body body)
  (let* ((c (gensym))
	 (clos-bindings '())
	 wrapper-info
	 handler-type handler-fn)
    (dolist (b bindings)
      (setq handler-type (car b)
	    handler-fn (cadr b))
      (setq wrapper-info (cadr (assoc handler-type *condition-wrappers* :test #'eq)))
      (cond ((and (symbolp handler-type)
		  (eq primitive-condition-package (symbol-package handler-type)))
	     ;; Direct reference to a primitive condition type.  Let it pass.
	     (push b clos-bindings))
	    (wrapper-info
	     ;; Reference to a shadowed condition type.  We need to catch both
	     ;; CLOS conditions and primitive conditions.  Before a primitive
	     ;; condition can be processed, it has to be wrapped in a CLOS
	     ;; object.  In any case, it is the object and not the condition
	     ;; wrapper that gets passed to the handler-fn.
	     (push `(,(wrapper-info-wrapper wrapper-info)
		      #'(lambda (,c)
			  (funcall ,handler-fn
				   (,(wrapper-info-extractor-fn wrapper-info)
				     ,c))))
		   clos-bindings)
	     (push `(,(wrapper-info-primitive-condition wrapper-info)
		      #'(lambda (,c)
			  (funcall ,handler-fn
				   (make-instance
				     ',(wrapper-info-surrogate-class wrapper-info)
				      :condition-wrapper ,c
				      ,@(mapcan #'(lambda (s)
						    `(,(first s) (,(second s) ,c)))
						(wrapper-info-slot-info wrapper-info))))))
		   clos-bindings))
	    (t
	     ;; Default case is (presumably) a reference to a specialized type
	     ;; of CLOS condition.  We can't predict at compile-time what
	     ;; wrapper type might be used to deal with this CLOS condition, so
	     ;; we have to check all of them at run time.  Ugh.
	     (let ((fn (gensym)))
	       (push `(,(intern "CONDITION" primitive-condition-package)
			#'(lambda (,c)
			    (let ((,fn (gethash (type-of ,c)
						*unwrapper-functions*)))
			      (when ,fn
				(setq ,c (funcall ,fn ,c))
				(when (typep ,c ',handler-type)
				  (funcall ,handler-fn ,c))))))
		     clos-bindings)))))
      ;; Finally cons up the result.
    `(,(intern "HANDLER-BIND" primitive-condition-package)
          ,(reverse clos-bindings)
	,@body)))

(defmacro handler-case (expression &rest handlers)
  (let* ((error-return (gensym))
	 (normal-return (gensym))
	 (condition (gensym))
	 (no-error-clause (assoc ':no-error handlers :test #'eq))
	 (clauses (if no-error-clause
		      (remove no-error-clause handlers)
		      handlers))
	 (clause-descrs
	  ;; The clause-descrs are lists of the form
	  ;; (condition-type tag var body).
	  ;; This is computed here since we need to make the association between
	  ;; handler and tag in two places in the expansion.
	  (mapcar #'(lambda (h)
		      `(,(car h)
			 ,(gensym)
			 ,(caadr h)
			 ,(cddr h)))
		  clauses))
	 (c (gensym)))
    `(block ,error-return
       (,@(if no-error-clause
	      `(multiple-value-call #'(lambda ,@(cdr no-error-clause)))
	      '(progn))
	  (block ,normal-return
	    (let (,condition)
	      (tagbody
		 (handler-bind
		   ,(mapcar #'(lambda (d)
				`(,(first d) #'(lambda (,c)
					       (setq ,condition ,c)
					       (go ,(second d)))))
			    clause-descrs)
		   (return-from ,normal-return ,expression))
		 ,@(mapcan #'(lambda (d)
			       `(,(second d)
				  (return-from ,error-return
				    (let ,(if (third d)
					      `((,(third d) ,condition))
					      nil)
				      ,@(fourth d)))))
			   clause-descrs))))))))

;;; Restarts
#+allegro (progn

(defun store-value (value)
  (let ((restart (find-restart 'store-value)))
    (if restart
	(invoke-restart restart value)
	nil)))

(defun use-value (value)
  (let ((restart (find-restart 'use-value)))
    (if restart
	(invoke-restart restart value)
	nil)))
	    
) ;+allegro

;;; Integrate with the debugger.  Or at least try to.

#+lucid
;;; Get the debugger to skip over some symbols in the backtrace.  Since these
;;; are just shadowed versions of standard Commonlisp functions that end up
;;; calling the original function, the resulting stack will look right.
(eval-when (eval load)
  (dolist (s '(error cerror signal warn))
    (declare (special lucid::*debugger-hide-names*))
    (pushnew s lucid::*debugger-hide-names*)))

#|

Test code

(handler-case
    (form)
  (type1 (var1)
    (body1 var1)
    (asdf))
  (type2 (var2) (body2 var2))
  (type3 () (body3))
  (:no-error (v) (bar v)))

(handler-bind
  ((type1
     #'(lambda (condition)
	 (fum condition)))
   (simple-error
     #'(lambda (condition)
	 (bazoola condition))))
  (return-from no-error-block
    (form)))

;; An example from the spec

(defvar *food-colors*
  '((milk white brown)
    (cheese yellow white blue)))

(defun bad-food-color-p (food color)
  (not (member color (cdr (assoc food *food-colors* :test #'string-equal))
	       :test #'string-equal)))

(defclass bad-food-color (error)
    ((food :initarg :food)
      (color :initarg :color))
  )

(defun use-food (new-food)
  (invoke-restart 'use-food new-food))

(defun use-color (new-color)
  (invoke-restart 'use-color new-color))

(let ((my-food 'milk)
      (my-color 'greenish-blue))
  (do ()
      ((not (bad-food-color-p my-food my-color)))
    (restart-case (error 'bad-food-color :food my-food :color my-color)
      (use-food (new-food)
	:report "Use another food."
	:interactive
	(lambda ()
	  (list (progn
		  (format *query-io* "~%Enter new food: ")
		  (read *query-io*))))
	(setq my-food new-food))
      (use-color (new-color)
	:report "Use another color."
	:interactive
	(lambda ()
	  (list (progn
		  (format *query-io* "~%Enter new color: ")
		  (read *query-io*))))
	(setq my-color new-color))))
  (list my-food my-color))
  

;; Another example

(define-condition foo-error (error)
    ())

(handler-bind
    ((foo-error
       #'(lambda (ignore)
	   (use-value 7)))
     (fum-error
       #'(lambda (ignore)
	   (use-value 9))))
  (restart-case (error 'foo-error)
    (use-value (x) (* x x))))

 => 49

;; A couple of more examples

(ignore-errors
  (big-hairy-computation)
  (still-more-hair))

(defun read-eval-print-loop (level)
  (with-simple-restart
      (abort "Exit command level ~d." level)
    (loop
      (with-simple-restart (abort "Return to command level ~D." level)
	(let ((form (prog2 (fresh-line) (read) (fresh-line))))
	  (prin1 (eval form)))))))

(defun foo (x)
  (restart-case (/ 1.0 x)
    (sorry () (return-from foo "Sorry"))))

(defun fnord (x)
  (restart-bind
      ((so-sorry
	   #'(lambda ()
	       (return-from fnord "So Sorry"))
	 :report-function
	 #'(lambda (s) (format s "Return the string -So Sorry-")))
       (most-positive-float #'(lambda ()
				(return-from fnord most-positive-single-float))
	 :report-function
	 #'(lambda (s) (format s "Return the most positive float"))))
    (/ 1.0 x)))

(defun fum (x)
  (handler-bind
      ((division-by-zero #'(lambda (c)
			     (declare (ignore c))
			     (return-from fum "Sorry"))))
    (/ 1.0 x)))

(fum 0)
 => "Sorry"

|#

#|
The following can be used in GnuEmacs to tell cl-indent how to indent the macros
defined in this file

(put 'define-condition 'common-lisp-indent-hook
     '(2 9 4 &rest 2))
(put 'handler-bind 'common-lisp-indent-hook
     '((&whole 4 &rest (&whole 1 2)) &body))
(put 'handler-case 'common-lisp-indent-hook
     '(4 &rest (&whole 2 4 &rest 2)))
(put 'ignore-errors 'common-lisp-indent-hook
     '0)
(put 'restart-case 'common-lisp-indent-hook
     '(4 &rest (&whole 2 4 &rest 2)))
(put 'restart-bind 'common-lisp-indent-hook
     '((&whole 4 (&whole nil 4 &rest 2)) &body))
(put 'with-simple-restart 'common-lisp-indent-hook
     '((&whole 4 &body) &body))

|#


;;;
;;; When editing with GnuEmacs, make sure the file of helpful stuff is loaded
;;;
;;; Local Variables:
;;; eval: (require 'common-lisp-indent (expand-file-name "common-lisp-indent.el" (file-name-directory buffer-file-name)))
;;; End:
