;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Lowercase: Yes -*-

(in-package "USER")

;;;
;;; System declaration for the COMMON-LISP system.  This is supposed to be like
;;; the X3J13 version of CommonLisp.  It defines the package COMMON-LISP
;;; (nickname CL) that includes CLOS, and a CLOS-ified condition system, and
;;; some implementations of some other X3J13 accepted proposals.
;;;

#-cmu
(defvar *cl-root* (pathname "/import/commonlisp-library/common-lisp/"))
#+cmu
(defvar *cl-root* 
    (make-pathname
     :directory (append
		 (butlast (pathname-directory (truename *load-pathname*)))
		 (list "common-lisp"))))

(defun cl-dirpath (&rest names)
  (pdefsys:make-pathname :directory (cons :relative names)
			 :defaults *cl-root*))

(pdefsys:defsystem common-lisp
    (:default-pathname (cl-dirpath)
     :default-binary-pathname
     #+lcl4.0 (cl-dirpath "bin" "lcl-4.0")
     #+(and lcl3.0 (not lcl4.0)) (cl-dirpath "bin" "lcl-3.0")
     #+allegro-v3.1 (cl-dirpath "bin" "allegro-v3.1")
     #-(or lcl3.0 allegro-v3.1) (cl-dirpath "bin" "other")
     :default-package "USER")
  
  ;; Lucid4.0 has its own "native" CLOS, all the others use PCL
  ("clos" :pathname "/import/lisp/lucid4.0/"
	  :binary-only t
	  :features (and :lcl4.0 (not :clos)))
  ("ensure-pcl-loaded" :features (and (not :lcl4.0) (not :cmu)))

  ("common-lisp-package" :load-before-compile (#+lcl4.0 "clos"
					       #-lcl4.0 "ensure-pcl-loaded"))
  ("common-lisp" :load-before-compile ("common-lisp-package"))

  ("lucid-condition-patches" :features :lucid)

  ;; We need an explicit load-after arg to control when the precom file gets
  ;; loaded and compiled.
  ("conditions"
   :load-after ("common-lisp" "common-lisp-precom" "lucid-condition-patches")
   :load-before-compile ("common-lisp" "lucid-condition-patches")
   :features (not :cmu))

  ;; The precom file is a pcl hack -- it precompiles the "flavor methods".
  ;; Note that it gets compiled after everything else is loaded, and loaded
  ;; before everthing (except PCL) is loaded.
  ("common-lisp-precom" :load-after ("ensure-pcl-loaded")
			:load-before-compile ("conditions")
			:optimizations ((compilation-speed 3))
			:features :pcl)

  )
