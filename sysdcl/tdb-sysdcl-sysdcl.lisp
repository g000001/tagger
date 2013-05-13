;;;-*- Package: TDB; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1990, 1991, 1992 by Xerox Corporation.  All rights reserved.

;;;; Bootstrap for TDB use of PDEFSYS.

;;; New TDB systems should be defined with DEF-TDB-SYSTEM.  Such definitions
;;; should either be added to tdb-sysdcl.lisp, or in a file in this directory
;;; named <system>-sysdcl.lisp.  A good example of the latter style is
;;; fsa-sysdcl.lisp.  Such sysdcl files should be added to the system
;;; definition for :TDB-SYSDCL at the end of this file if they are to be
;;; compiled.

;; #-(or cltl2 ansi-90 cmu)
#|(eval-when (compile eval load)
  (pdefsys:load-system :common-lisp))|#

#|(cl:defpackage :tdb
  (:use :common-lisp)
  (:use :tagger.internal)
  (:use :cl-extensions)
  (:export #:tdb #:*tdb-root* #:tdb-pathname #:tdb-dirpath
	   #:def-tdb-system #:tdb-compile
	   #:*normally-optimized* #:*highly-optimized*))|#

(cl:in-package :tdb)


;;; All pathnames should be relative to this.

;; #-(or cltl2 ansi-90 cmu)
#|(defvar *tdb-root*
    (probe-file (format nil "~A/"
			(pdefsys:get-environment-variable "TDBRELEASE"))))|#
#+(or cltl2 ansi-90 cmu)
(cltl1-eval-when (load)
  (defparameter *tdb-root*
     (make-pathname
      :directory (butlast (pathname-directory (truename *load-pathname*)) 3))))
#+(or cltl2 ansi-90 cmu)
(cltl1-eval-when (eval)
  (defparameter *tdb-root*
    (make-pathname
     :directory (butlast (pathname-directory (truename *load-pathname*)) 2))))


;;; A couple of handy functions.

(defun tdb-pathname (&rest names)
  #|(pdefsys:make-pathname
   :directory (cons :relative (butlast names))
   :defaults (pdefsys:make-pathname
	      :directory (pathname-directory *tdb-root*)
	      :defaults (car (last names))))|#
  (merge-pathnames (make-pathname :directory `(:relative ,@(butlast names))
                                  :defaults (car (last names)))
                   (asdf:system-source-directory :tagger)))

(defun tdb-dirpath (&rest names)
  #|(pdefsys:make-pathname :directory (cons :relative names)
			 :defaults *tdb-root*)|#
  (asdf:system-source-directory :tagger))


(defvar *normally-optimized*
  #+excl'(optimize (speed 3) (safety 1))
  #+cmu '(optimize (speed 3) (safety 0)
	  (extensions:debug 1) (extensions:inhibit-warnings 3))
  #-(or excl cmu) '(optimize (speed 3) (safety 0)))
(defvar *highly-optimized*
  '(optimize (speed 3) (safety 0) (space 0)
             #+cmu(extensions:inhibit-warnings 1)))

(defparameter *tdb-systems* '())

(defvar *tdb-binary-subdirectory*
    #+mcl "mcl" #+sgi "sgi" #+sun "sun"
    #-(or mcl sgi sun) "other")

;;; Location of lisp.h file for excl 
#+(and excl allegro-v4.1)
(defvar *lisp-header* "-I/import/franz/lib/misc")
#+(and excl allegro-v4.2 sun)
(defvar *lisp-header* "-I/import/franz-4.2.beta/lib/misc")
#+(and excl sgi )
(defvar *lisp-header* "-I/usr/local/lib/cl/misc")

;;; A more convenient version of the DEFSYSTEM macro for our purposes
(defmacro def-tdb-system (name options &rest modules)
  (let ((dir (cdr (assoc :dir options)))
	(sub-systems (cdr (assoc :sub-systems options))))
    `(progn
       #|(pdefsys:defsystem ,name
	   (:default-pathname (tdb-dirpath "src" ,@dir)
	    :default-binary-pathname
	    (tdb-dirpath "lib" *tdb-binary-subdirectory* ,@dir)
	    ,@(when sub-systems
		`(:needed-systems ,sub-systems
				  :load-before-compile t))
	    :default-optimizations ,(cdr *normally-optimized*)
	    :default-package :keyword)
	 ,@(mapcar
	    #'(lambda (m)
		(unless (consp m) (setq m (list m)))
		#+excl
		(when (find :language m)
		  (let ((optimizations (member :optimizations m)))
		    (when optimizations
		      (setf (cadr optimizations)
			(append (list *lisp-header*) (cadr optimizations))))))
		m)
	    modules))|#
       (pushnew ',name *tdb-systems* :test #'string-equal)
       ',name)))

#|(pdefsys:defsystem :tdb-sysdcl
    (:default-pathname (tdb-dirpath "src" "sysdcl")
     :default-binary-pathname
     (tdb-dirpath "lib" *tdb-binary-subdirectory* "sysdcl"))
  ("tdb-sysdcl-sysdcl")
  ("skip-list-sysdcl" :load-before-compile t)
  ("util-sysdcl" :load-before-compile t)
  ("fsa-sysdcl" :load-before-compile t)
  ("corpus-sysdcl" :load-before-compile t)
  ("analysis-sysdcl" :load-before-compile t)
  ("tag-analysis-sysdcl" :load-before-compile t)
  #+parc("index-sysdcl" :load-before-compile t)
  #+parc("tdb-sysdcl" :load-before-compile t)
  #+parc("cluster-sysdcl" :load-before-compile t)
  #+parc("summary-sysdcl" :load-before-compile t)
  #+(and excl parc)("ilu-tdb-sysdcl" :load-before-compile t))|#

(defun tdb-compile (&key recompile)
  #|(pdefsys:compile-system :tdb-sysdcl :recompile recompile)|#
  #|(pdefsys:load-system :tdb-sysdcl)|#
  #|(pdefsys:compile-systems (reverse *tdb-systems*) :recompile recompile)|#)

;;;default: if it's franz, then it boxes floats across function calls
;;;we assume that our box handler is franz specific, so boxes-float-calls
;;;should be set only if excl is.
#+excl
(pushnew :boxes-float-calls *features*)
