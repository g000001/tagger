 ;;; -*- Mode: Lisp; Package: COMMON-LISP; Base: 10.; Syntax: Common-Lisp -*-
;;;
;;; Defines the COMMON-LISP package, our version of CommonLisp as it will exist one
;;; day in the far distant future.
;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved.
;;;

(in-package "COMMON-LISP")


;; PRINT-UNREADABLE-OBJECT

#+lucid
(defmacro print-unreadable-object (&rest rest)
  `(system:printing-random-object ,@rest))

#+cmu-v15
(defmacro print-unreadable-object ((thing stream &rest ignore) &body body)
  (declare (ignore ignore))
  `(pcl::printing-random-thing (,thing ,stream) ,@body))

;; DECLAIM

#-cmu
(defmacro declaim (&rest decls)
  `(eval-when (compile eval load)
     ,@(mapcar #'(lambda (decl) `(proclaim ',decl)) decls)))

;;; Fixes and extensions to PCL

;; These redefinitions are only due to "bugs" in PCL.

#+pcl
(progn

(pcl:defmethod type-of (x)
  (lisp:type-of x))

(pcl:defmethod type-of ((x pcl:standard-object))
  (class-name (class-of x)))

(defun subtypep (type1 type2)
  (multiple-value-bind (value are-you-sure-p)
      (lisp:subtypep type1 type2)
    (if (or are-you-sure-p (not (symbolp type1)) (not (symbolp type2)))
	(values value are-you-sure-p)
	(let ((c1 (find-class type1 nil))
	      (c2 (find-class type2 nil)))
	  (if (and c1 c2)
	      (values (and (member c2 (pcl::class-precedence-list c1)
				   :test #'eq)
			   t)
		      t)
	      (values value are-you-sure-p))))))
) ;+pcl

;; The following top-level forms are shadowed to give us a way to tell the
;; compiler how to tell us what it being compiled.  This has nothing to do with
;; the COMMON-LISP package, but it does make life a lot nicer for users.

(defmacro make-named-top-level-form (name form)
  #+(and lucid (not lcl4.0))
  `(compiler-let ((system:*compiler-message-string*
		    (or system:*compiler-message-string*
			(format nil "~S" ',name))))
     ,form)
  #-(and lucid (not lcl4.0))
  (declare (ignore name))
  #-(and lucid (not lcl4.0))
  form)


;; In the Mardi-Gras release, DEFGENERIC exists, but does little or nothing.
;; If the gf is of the form (SETF ...) it does at least define the SETF method,
;; and it accepts the :documentation option, but that's all.  This definition
;; teaches it to accept the :method option, too.
#+pcl
(defmacro defgeneric (function-specifier lambda-list &rest options)
  (let ((expansion nil)
	(setfp (and (consp function-specifier)
		    (eq (car function-specifier) 'setf)))
	(methods nil)
	(docstring nil))
    (unless setfp
      ;; Try to convince the lisp not to print out warning msgs about
      ;; references to undefined functions that have been declared in a
      ;; defgeneric.  This is supposed to work in Lucid at least.
      (push `(proclaim
	      '(function ,function-specifier
		,(let ((key-p nil))
		   (mapcar
		    #'(lambda (a)
			(cond ((member a lambda-list-keywords :test #'eq)
			       (setq key-p (eq a '&key))
			       a)
			      #+cmu
			      (key-p
			       (list (if (listp a)
					 (car a)
				       (intern (symbol-name a)
					       (find-package :keyword)))
				     t))
			      (t t)))
		    lambda-list))))
	    expansion))
    (dolist (option options)
      (case (car option)
	(:documentation (setq docstring (second option)))
	(:method
	 (push `(defmethod ,function-specifier ,@(cdr option))
	       methods))))
    (when docstring
      (if setfp
	  (push `(setf (documentation ',(cadr function-specifier) 'setf)
		       ',docstring)
		expansion)
	  (push `(setf (documentation ',function-specifier 'function) ',docstring)
		expansion)))
    `(make-named-top-level-form (generic-function ,function-specifier)
				(progn
				  (pcl:defgeneric ,function-specifier ,lambda-list)
				  ,@expansion
				  ,@methods))))

;; And the function FIND-METHOD is named GET-METHOD
#+pcl
(setf (symbol-function 'pcl:find-method) (symbol-function 'pcl::get-method))

;;; A definition of DESTRUCTURING-BIND, as accepted by the X3J13 committee.
;;; It does no optimizations, presuming that the compiler is smart enough to
;;; share side-effect-free code.  Possibly a bad assumption.

#+allegro (progn

(eval-when (eval compile)		;Don't need these after the file is compiled

(defmacro push-binding (sym &optional (val nil val-p))
  `(locally
       (declare (special *bindings*))
     ,(if val-p
	  `(push (list ,sym ,val) *bindings*)
	  `(push ,sym *bindings*))))

(defmacro push-form (form)
  `(locally
       (declare (special *forms*))
     (push ,form *forms*)))

) ; eval-when

(defun destructure-bindings (pattern value)
  ;; Pattern is the pattern to destructure.
  ;; Value is the value form that corresponds to the entire pattern.
  ;; Returns two values -- a binding list and a list of additional forms to
  ;; evaluate after the bindings.
  (let ((*bindings* nil)
	(*forms* nil))
    (declare (special *bindings* *forms*))
    (destructure-bindings-1 pattern value)
    (values (reverse *bindings*) (reverse *forms*))))

(defun destructure-bindings-1 (pattern value)
  (when (and (consp pattern)
	     (eq (car pattern) '&whole))
    (push-binding (cadr pattern) value)
    (setq pattern (cddr pattern)))
  (destructure-required-args pattern value))
  
(defun destructure-required-args (pattern value)
  ;; Pattern is the pattern to destructure.
  ;; Value is the value form that corresponds to the entire pattern.
  (cond ((null pattern))
	((symbolp pattern) (push-binding pattern value))
	((member (car pattern) lambda-list-keywords :test #'eq)
	 (destructure-lambda-keywords pattern value))
	(t (destructure-bindings-1 (car pattern)
				   `(car ,value))
	   (destructure-required-args (cdr pattern)
				      `(cdr ,value)))))

(defun destructure-lambda-keywords (pattern value)
  ;; Deal with lamda-list-keywords in the pattern.
  ;; Pattern is a list whose car in a lambda-list-keyword.
  (let ((*&rest-seen-p* nil))
    (declare (special *&rest-seen-p*))
    (ecase (car pattern)
      (&whole
       (lisp:error "~S only allowed at the beginning of a lambda-list." '&whole))
      ((&rest &body)
       (if *&rest-seen-p*
	   (lisp:error "Only one ~S or ~S allowed in lambda-list." '&rest '&body)
	   (setq *&rest-seen-p* t))
       (push-binding (cadr pattern) value)
       (destructure-lambda-keywords (cddr pattern) value))
      (&key
       (destructure-keywords (cdr pattern) value))
      (&allow-other-keys
       (lisp:error "~S not preceeded by ~S." '&allow-other-keys '&key))
      (&aux
       (destructure-aux-args (cdr pattern)))
      (&optional
       (if *&rest-seen-p*
	   (lisp:error "~S not the first keyword in the lambda-list." '&optional)
	   (destructure-optional-args (cdr pattern) value))))))

(defun destructure-keywords (pattern value)
  ;; The remainging elements of pattern (up to the next lambda-list-keyword) are
  ;; keyword args.
  (do* ((keyword-package (find-package "KEYWORD"))
	(keywords nil)
	(allow-other-keys-p nil)
	(new-value-var nil)
	(supplied-p-var)
	(pattern pattern (cdr pattern)))
       ((null pattern)
	(unless allow-other-keys-p
	  (push-form
	    `(do ((l ,value (cddr l)))
		 ((null l))
	       (cond ((eq (car l) :allow-other-keys)
		      (return))
		     ((member (car l) ',keywords))
		     (t (lisp:error "Keyword ~S not one of"
				    (car l) ',keywords)))))))
    (cond ((not (consp pattern))
	   (lisp:error "~S after ~S not a proper list." pattern '&key))
	  ((eq (car pattern) '&allow-other-keys)
	   (unless (or (null (cdr pattern))
		       (eq (cadr pattern) '&aux))
	     (lisp:error "~S not allowed after ~S." (cadr pattern) '&aux))
	   (setq allow-other-keys-p t))
	  ((eq (car pattern) '&aux)
	   (destructure-aux-args (cdr pattern))
	   (setq pattern nil))
	  ((member (car pattern) lambda-list-keywords :test #'eq)
	   (lisp:error "Keyword ~S not allowed after ~S." (car pattern) '&key))
	  (t (let* ((spec (car pattern))
		    (var (cond ((symbolp spec) spec)
			       ((consp (car spec)) (caar spec))
			       (t (car spec))))
		    (keyword (cond ((symbolp spec)
				    (intern spec keyword-package))
				   ((consp (car spec)) (cadar spec))
				   (t (intern (car spec) keyword-package)))))
	       (pushnew keyword keywords)
	       (cond ((or (symbolp spec)
			  (null (cdr spec)))
		      ;; No default value, so this is easy
		      (push-binding var `(getf ,value ',keyword)))
		     ((and (null (cddr spec))
			   (or (constantp (cadr spec))
			       (symbolp (cadr spec))))
		      ;; Default value is a side-effect-free and there is no
		      ;; supplied-p arg
		      (push-binding var `(getf ,value ',keyword ,(cadr spec))))
		      (t ;; Have to be very careful about evaluating the
			 ;; default-value form.  First get our hands on a
			 ;; brand-new value.
			(unless new-value-var
			  (setq new-value-var (gensym))
			  (push-binding new-value-var '(cons nil nil)))
			;; Now bind a supplied-p arg.  If there is one
			;; specified, use it.  Otherwise gensym up one.
			(setq supplied-p-var (if (null (cddr spec))
						 (gensym)
						 (caddr spec)))
			(push-binding supplied-p-var
				      `(not (eq ,new-value-var
						(getf ,value ',keyword
						      ,new-value-var))))
			(push-binding var
				      `(if ,supplied-p-var
					   (getf ,value ',keyword)
					   ,(cadr spec))))))))))

(defun destructure-aux-args (pattern)
  (do ((pattern pattern (cdr pattern)))
      ((null pattern))
    (cond ((not (consp pattern))
	   (lisp:error "~S after ~S not a proper list" pattern '&aux))
	  ((member (car pattern) lambda-list-keywords :test #'eq)
	   (lisp:error "Keyword ~S not allowed after ~S." (car pattern) '&aux))
	  ((consp (car pattern))
	   (push-binding (caar pattern) (cadar pattern)))
	  (t (push-binding (car pattern))))))

(defun destructure-optional-args (pattern value)
  ;; The remaining elements of pattern (up to the next lambda-list-keyword) are
  ;; optional args.
  (cond ((null pattern))
	((not (consp pattern))
	 (lisp:error "~S after ~S not a proper list." pattern '&optional))
	((consp (car pattern))
	 (when (not (null (cddar pattern)))
	   (push-binding (caddar pattern)
			 `(not (null ,value))))
	 (push-binding (caar pattern)
		       `(if (null ,value)
			    ,(cadar pattern)
			    (car ,value)))
	 (destructure-optional-args (cdr pattern) `(cdr ,value)))
	((eq (car pattern) '&optional)
	 (lisp:error "~S can only occur once in the lambda-list." '&optional))
	((member (car pattern) lambda-list-keywords :test #'eq)
	 (destructure-lambda-keywords pattern value))
	(t (push-binding (car pattern)
			 `(car ,value))
	   (destructure-optional-args (cdr pattern) `(cdr ,value)))))

(defmacro destructuring-bind (pattern form &body body)
  (let ((value (gensym)))
    (multiple-value-bind (bindings forms)
	(destructure-bindings pattern value)
      `(let* ((,value ,form)
	      ,@bindings)
	 ;; Not right yet -- need to parse out the decls from the body and stick
	 ;; them before the forms.  But the additional forms only appear to
	 ;; check keywords, so the typical cases work just fine.
	 ,@forms
	 (locally ,@body)))))

) ;+allegro

;;; IN-PACKGE and DEFPACKAGE

#+ignore
(defmacro in-package (name)
  (let ((package-name (etypecase name
			(symbol (symbol-name name))
			(string name))))
    `(eval-when (eval compile load)
       (setq *package*
	     (or (find-package ,package-name)
		 (error "Package ~s does not exist." ,package-name))))))

(defmacro defpackage (package-name &rest options)
  (flet ((stringify (x)
	   (etypecase x
	     (string x)
	     (symbol (symbol-name x)))))
    (macrolet ((push-strings (list loc)
		 `(dolist (x ,list)
		    (push (stringify x) ,loc))))
      (let ((name (stringify package-name))
	    (nicknames nil)
	    (shadow-names nil)
	    (shadowing-import-lists nil)
	    (use-list nil)
	    (use-list-present-p nil)
	    (import-names nil)
	    (intern-list nil)
	    (export-list nil)
	    (size nil))
	(dolist (option options)
	  (etypecase option
	    (cons (ecase (car option)
		    (:nicknames (push-strings (cdr option) nicknames))
		    (:use (setq use-list-present-p t)
			  (push-strings (cdr option) use-list))
		    (:shadow (push-strings (cdr option) shadow-names))
		    (:shadowing-import-from
		     (let ((l nil))
		       (push-strings (cddr option) l)
		       (push (cons (stringify (cadr option)) l)
			     shadowing-import-lists)))
		    (:import-from 
		     (let ((l nil))
		       (push-strings (cddr option) l)
		       (push (cons (stringify (cadr option)) l)
			     import-names)))
		    (:intern (push-strings (cdr option) intern-list))
		    (:export (push-strings (cdr option) export-list))
		    (:size
		     (if (null size)
			 (setq size (cadr size))
			 (lisp:error "~S option specified multiple times"
				     ':size)))))))
	(when (not (null (intersection intern-list export-list :test #'string=)))
	  (lisp:error "The lists of exported and interned symbols are not disjoint"))
	;; Also need to verify that the :shadow, :intern, :import-from, and
	;; :shadowing-import-from all need to be disjoint.
	`(eval-when (eval load compile)
	   ;; First make sure the package exists
	   (cond ((find-package ,name)
		  )
		 (t (make-package ,name :use nil :nicknames ',nicknames)))
	   ;; Shadow stuff next
	   ,@(when shadow-names
	       `((shadow (mapcar #'(lambda (s) (intern s ,name))
				 ',shadow-names)
			 ,name)))
	   ,@(mapcar #'(lambda (l)
			 `(shadowing-import (mapcar #'(lambda (s)
							(intern s ',(car l)))
						    ',(cdr l))
					    ,name))
		     shadowing-import-lists)
	   ;; Then use appropriate packages
	   (use-package ',(if use-list-present-p
			      use-list
			      '("COMMON-LISP"))
			,name)
	   ;; Import other symbols
	   ,@(mapcar #'(lambda (l)
			 `(import (mapcar #'(lambda (s)
					      (intern s ',(car l)))
					  ',(cdr l))
				  ,name))
		     import-names)
	   ;; Intern anything required
	   ,@(mapcar #'(lambda (s) `(intern ',s ,name)) intern-list)
	   ;; Finally export stuff
	   ,@(when export-list
	       `((export (mapcar #'(lambda (s) (intern s ,name)) ',export-list)
			 ,name))))))))

;;; Hash table access functions

#+allegro (progn

(defun hash-table-size (htable)
  (excl::hash-table-buckets htable))

(defun hash-table-test (htable)
  (excl::hash-table-kind htable))

) ;+allegro

#+CMU (progn

(defun hash-table-size (htable)
  (lisp::hash-table-size htable))

(defun hash-table-test (htable)
  (lisp::hash-table-kind htable))

) ;+allegro

#+xerox (progn

(defun hash-table-size (htable)
  (il:harrayprop htable 'il:size))

(defun hash-table-test (htable)
  (il:harrayprop htable 'il:equivfn))

(defun hash-table-rehash-size (htable)
  (il:harrayprop htable 'il:overflow))

(defun hash-table-rehash-threshold (htable)
  ;; I dunno if this is anywhere near "correct", but it'll work for now
  (declare (ignore htable))
  1.0)

) ;+xerox

;;; Dumping and Loading random objects

(defgeneric make-load-form (object)
  (:method ((object t))
    (lisp:error "No ~S method defined for ~S" 'make-load-form object)))

(eval-when (compile eval load)
  (import #+pcl '(pcl::slot-definition-name pcl::class-slots)
	  #-pcl '(clos::slot-definition-name closs::class-slots)))

(defun make-load-form-saving-slots (object &optional (slots nil slotsp))
  (cond
   ((typep object 'standard-object)
    (let ((class-name (class-name (class-of object))))
      (values
       `(make-instance ',class-name)
       `(progn
	  ,@(mapcar
	     #'(lambda (slot &aux (s (slot-definition-name slot)))
		 `(setf (slot-value ',object ',s)
		    ',(slot-value object s)))
	     (let ((all-slots (class-slots (class-of object))))
	       (if slotsp 
		   (remove-if-not #'(lambda (name) (member name slots))
				  all-slots :key #'slot-definition-name)
		 all-slots)))))))
   (slotsp (%structure-make-load-form-saving-slots object slots))
   (t (%structure-make-load-form-saving-slots object))))


;; There are four functions that each Lisp needs to define.
;;
;; %CREATE-STRUCTURE type
;;	create an uninitialized instance of the type
;; %STRUCTURE-SLOTS object
;;	return a list of the names of the slots of object
;; %STRUCTURE-SLOT-VALUE object slot-name
;;	return the value of the slot slot-name in object
;; %SET-STRUCTURE-SLOT-VALUE object slot-name value
;;	change the value of the slot slot-name in object to value
;;
;; Having each lisp define the same internal functions is what lets the
;; different lisps share dump files.  This isn't part of the LOAD-OBJECTS issue,
;; but it sure it a nice bit of functionality.

(defun %structure-make-load-form-saving-slots
    (object &optional (slots (%structure-slots object)))
  (values `(%create-structure ',(lisp:type-of object))
	  `(progn ,@(mapcar #'(lambda (s)
				`(%set-structure-slot-value
				   ',object ',s
				   ',(%structure-slot-value object s)))
			    slots))))

#+lucid (progn

(defun %object-info (object index)
  (system:structure-ref object index
			(system:structure-type object)))
  
(defun %struct-info (object index)
  (declare (special lucid::*defstructs*))
  (%object-info (or (gethash (system:structure-type object)
				     lucid::*defstructs*)
			    (lisp:error "~S not a structure." object))
		index))

(defun %structure-slot-index (object slot-name)
  (position slot-name (%struct-info object 7)
	    :test #'eq
	    :key #'(lambda (s) (%object-info s 0))))

(defun %structure-index-slot (object index)
  (%object-info (elt (%struct-info object 7) index) 0))

(defun %create-structure (type)
  (declare (special lucid::*defstructs*))
  (funcall (%object-info
	     (or (gethash type lucid::*defstructs*)
		 (lisp:error "~S is not the name of a structure." type))
			 3)))

(defun %structure-slots (object)
  (let* ((head (cons nil nil))
	 (tail head))
    (dotimes (i (system:structure-length object (system:structure-type object)))
      (setf (cdr tail) (cons (%structure-index-slot object i) nil)
	    tail (cdr tail)))
    (cdr head)))

(defun %structure-slot-value (object slot-name)
  (%object-info object (%structure-slot-index object slot-name)))

(defun %set-structure-slot-value (object slot-name new-value)
  (setf (system:structure-ref object
			      (%structure-slot-index object slot-name)
			      (system:structure-type object))
	new-value))

) ;+lucid


#+allegro (progn

(defun %type-info (object)
  (or (get (lisp:type-of object) 'excl::%structure-definition)
      (lisp:error "~S not a structure." object)))

(defun %create-structure (type)
  (funcall (%structure-slot-value
	     (or (get type 'excl::%structure-definition)
		 (lisp:error "~S is not the name of a structure." type))
	     'excl::constructor)))

(defun %structure-slots (object)
  (mapcar #'excl::dsd-name
	  (excl::dd-slots (%type-info object))))

(defun %structure-slot-value (object slot-name)
  (funcall (excl::dsd-accessor
	     (find slot-name (excl::dd-slots (%type-info object))
		   :key #'excl::dsd-name))
	   object))

(defun %set-structure-slot-value (object slot-name new-value)
  (funcall (get (excl::dsd-accessor
		  (find slot-name (excl::dd-slots (%type-info object))
			:key #'excl::dsd-name))
		'excl::setf-inverse)
	   object new-value))

) ;+allegro


#+xerox (progn

(defun %type-info (object)
  (gethash (lisp:type-of object) cl::*parsed-defstructs*))

(defun %create-structure (type)
  (or (gethash type cl::*parsed-defstructs*)
      (lisp:error "~S is not the name of a structure." type))
  (il:ncreate type))

(defun %structure-slots (object)
  (cl::structure-slot-names (lisp:type-of object)))

(defun %structure-slot-value (object slot-name)
  (il:fetchfield (nth 4 (assoc slot-name (nth 15 (%type-info object))))
		 object))

(defun %set-structure-slot-value (object slot-name slot-value)
  (il:replacefield (nth 4 (assoc slot-name (nth 15 (%type-info object))))
		   object slot-value))

(defun %structure-make-load-form-saving-slots
    (object &optional (slots (%structure-slots object)))
  (values `(%create-structure ',(lisp:type-of object))
	  `(progn ,@(mapcar #'(lambda (s)
				`(%set-structure-slot-value
				   ',object ',s
				   ',(%structure-slot-value object s)))
			    slots))))

) ;+xerox

#+cmu (progn

(defun %type-info (object)
  (or (extensions:info type defined-structure-info (lisp:type-of object))
      (lisp:error "~S not a structure." object)))

(defun %create-structure (type)
  (let* ((info (extensions:info type defined-structure-info type))
	 (object (kernel:make-structure (c::dd-length info))))
    (setf (kernel:structure-ref object 0) (c::dd-name info))
    object))

(defun %structure-slots (object)
  (mapcar #'c::dsd-name (c::dd-slots (%type-info object))))

(defun %structure-slot-value (object slot-name)
  (funcall (c::dsd-accessor
	     (find slot-name (c::dd-slots (%type-info object))
		   :key #'c::dsd-name))
	   object))

(defun %set-structure-slot-value (object slot-name new-value)
  (funcall (gethash (c::dsd-accessor
		     (find slot-name (c::dd-slots (%type-info object))
			   :key #'c::dsd-name))
		    lisp::*setf-functions*)
	   object new-value))

) ;+cmu
