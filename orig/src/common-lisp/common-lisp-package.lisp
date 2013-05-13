;;; -*- Mode: Lisp; Package: Lisp; Base: 10.; Syntax: Common-Lisp -*-
;;;
;;; *************************************************************************
;;;
;;; Copyright (c) 1990 by Xerox Corporation.
;;; All rights reserved.
;;;
;;; *************************************************************************
;;;
;;; Set up the COMMON-LISP package structure.
;;; All the package stuff is in this file, rather than scattered about
;;; in seperate files, to minimize the possibility of getting scrod by
;;; accidentally interning symbols that we don't really want.  This is rather
;;; important since we have to shadow some symbols that live in the LISP package.
;;;
;;;

#+cmu
(eval-when (compile eval load)
  (when (eq (find-package :common-lisp) (find-package :lisp))
    (rename-package :common-lisp :lisp))
  (when (eq (find-package :common-lisp-user) (find-package :user))
    (rename-package :common-lisp-user :user)))

(in-package "USER")

#-(or allegro lucid xerox cmu)
(error "COMMON-LISP has not been ported to ~A." (lisp-implementation-type))



(eval-when (compile load)


(unless (find-package :common-lisp)
  (make-package "COMMON-LISP" :use '("LISP") :nicknames '("CL")))

#-cmu
(shadow '(;; There are some subtle differences in the condition system that
	  ;; require replacing some of the standard error functions
	  error cerror warn
	  ;; Since Lucid bitches if the first form in a file isn't
	  ;; lisp:in-package, we can't replace in-package without causing a
	  ;; lot of grief.  Not only that, the new in-package isn't
	  ;; syntactically compatible with the old one, so replacing it would
	  ;; break old files.
	  #+ignore in-package
	  )
	(find-package "CL"))

#-lcl4.0
(shadow '(pcl:defgeneric
	  ;; These get shadowed because of PCL shortcomings that force
	  ;; us to define our own versions of them.
	  type-of subtypep)
	(find-package "CL"))


(use-package (find-package #+lcl4.0 "CLOS" #-lcl4.0 "PCL")
	     (find-package "CL"))



;; Include the XP package, too.  Someday this might work in lisps aside from
;; Lucid.

(when (find-package "XP")
  (funcall (intern "INSTALL" (find-package "XP"))
    :package (find-package "CL")))


(shadowing-import '(pdefsys:make-pathname pdefsys:pathname-directory)
	(find-package "CL"))

#+ignore
;; These should be external symbols in PCL, but they aren't?
(import '(pcl::slot-value-using-class pcl::slot-boundp-using-class
	  pcl::slot-exists-p-using-class pcl::describe-object
	  pcl::funcallable-standard-class)
	(find-package "CL"))


#+lucid
;; Lucid already implements these, so just import them
(import '(lcl:destructuring-bind
	  lcl:hash-table-rehash-size lcl:hash-table-rehash-threshold
	  lcl:hash-table-size lcl:hash-table-test
	  lcl:restart-case lcl:restart-bind lcl:with-simple-restart
	  lcl:ignore-errors
	  lcl:restart-name lcl:compute-restarts lcl:find-restart
	  lcl:invoke-restart lcl:invoke-restart-interactively
	  lcl:abort lcl:continue lcl:muffle-warning
	  lcl:store-value lcl:use-value
	  lcl:dynamic-extent)
	(find-package "CL"))

#+allegro
;; Allegro already implements these, so just import them
(import '(excl::hash-table-rehash-size excl::hash-table-rehash-threshold
	  cond:restart-case cond:restart-bind cond:with-simple-restart
	  cond:ignore-errors
	  cond:restart-name cond:compute-restarts cond:find-restart
	  cond:invoke-restart cond:invoke-restart-interactively
	  cond:abort cond:continue cond:muffle-warning
	  excl:dynamic-extent)
	(find-package "CL"))

#+Xerox
;; XeroxLisp already implements these, so just import them
(import '(xcl:destructuring-bind
	  xcl:restart-case xcl:restart-bind xcl:with-simple-restart
	  xcl:ignore-errors
	  xcl:restart-name xcl:compute-restarts xcl:find-restart
	  xcl:invoke-restart xcl:invoke-restart-interactively
	  xcl:abort xcl:continue xcl:muffle-warning
	  xcl:store-value xcl:use-value)
	(find-package "CL"))




(let ((cl-package (find-package "CL")))

  ;; Export versions of the external symbols from the LISP and PCL (or CLOS) packages
  (flet ((export-visible-symbols (package)
	   (do-external-symbols (var package)
	     (let ((cl-var (intern (symbol-name var) cl-package)))
	       (export (if (null cl-var) (list cl-var) cl-var)
		       cl-package)))))
    (export-visible-symbols (find-package "LISP"))
    (export-visible-symbols (find-package #+lcl4.0 "CLOS" #-lcl4.0 "PCL")))
  
  ;; The macrolet is used so that the binary doesn't intern a lot of
  ;; symbols in the current package.  Instead it translates the symbols
  ;; into strings, and the strings appear in the binary and are interned
  ;; in the cl package at load time.
  (macrolet ((export-cl-symbols (&rest s-list)
	       `(export (list ,.(mapcar #'(lambda (s)
					    `(intern ,(symbol-name s) cl-package))
					s-list))
			cl-package)))
    (export-cl-symbols
     ;; Conditions
     condition
     simple-condition
     simple-condition-format-string simple-condition-format-arguments
     serious-condition
     storage-condition
     warning
     simple-warning
     error
     simple-error
     control-error
     type-error type-error-datum type-error-expected-type
     simple-type-error
     program-error
     package-error package-error-package
     stream-error stream-error-stream
     end-of-file
     file-error file-error-pathname
     cell-error cell-error-name
     unbound-variable
     undefined-function
     arithmetic-error arithmetic-error-operation arithmetic-error-operands
     division-by-zero
     floating-point-overflow
     floating-point-underflow
     ;; PCL
     slot-value-using-class slot-boundp-using-class
     slot-exists-p-using-class
     standard-class built-in-class
     structure-class funcallable-standard-class
     describe-object
     declaim print-unreadable-object
     ;; Issue DESTRUCTURING-BIND
     destructuring-bind
     ;; Issue IN-PACKGE-FUNCTIONALITY
     #+ignore in-package
     ;; Issue DEFPACKAGE
     defpackage
     ;; Issue HASH-TABLE-ACCESS
     hash-table-rehash-size hash-table-rehash-threshold
     hash-table-size hash-table-test
     ;; Issue LOAD-OBJECTS
     make-load-form make-load-form-saving-slots
     ;; Issue CLOS-CONDITIONS
     define-condition make-condition
     error cerror signal warn
     handler-bind handler-case ignore-errors
     compute-restarts restart-name find-restart
     invoke-restart invoke-restart-interactively
     abort continue muffle-warning store-value
     use-value
     break invoke-debugger
     with-simple-restart restart-case restart-bind
     ;; Issue DYNAMIC-EXTENT
     dynamic-extent)))



;; Make a nice user package for people to play around in.

(unless (find-package "COMMON-LISP-USER")
  (make-package "COMMON-LISP-USER" :use '("COMMON-LISP")
		:nicknames '("CL-USER")))


) ;eval-when load
