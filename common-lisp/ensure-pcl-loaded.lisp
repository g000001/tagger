;;; -*- Mode: Lisp; Package: USER; Base: 10.; Syntax: Common-Lisp -*-
;;;
;;; Make sure that PCL is loaded
;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved.
;;;

(in-package "USER")

(eval-when (load compile eval)
  (cond ((null (find-package "PCL"))
	 (let ((directory "/import/pcl/may-day/"))
	   (load (make-pathname :name "defsys" :defaults directory))
	   (let ((pcl-package (find-package "PCL")))
	     (set (intern "*PCL-DIRECTORY*" pcl-package)
		  (pathname directory))
	     (funcall (symbol-function (intern "LOAD-PCL" pcl-package))))))
	((not (string-equal (symbol-value (intern "*PCL-SYSTEM-DATE*"
						  (find-package "PCL")))
			    "5/1/90  May Day PCL (REV 2)"))
	 (error "The loaded version of PCL, ~A, is not appropriate for loading ~
                 this file." (symbol-value (intern "*PCL-SYSTEM-DATE*"
						   (find-package "PCL")))))
	(t t))
  )					; eval-when


#+excl(in-package "PCL")		;work around bug

;; Add a comment to the banner noting the version of PCL loaded in the current
;; system.
#+lucid
(lcl:defadvice (lucid::lucid-banner pcl) ()
    (concatenate 'string (lcl:advice-continue)
		 ";;; This image contains the PCL release - "
		 pcl::*pcl-system-date* ".~2%"))
#+excl
(setq excl:*restart-actions*
      `(,.excl:*restart-actions*
	(:eval . (format t "~&;;; This image contains the PCL release - ~a"
			 pcl::*pcl-system-date*))))


;; Interesting, silly code to get PCL to deal with class definitions and methods
;; in the same file.

(pushnew 'compile pcl::*defclass-times*)
(pushnew 'compile pcl::*defmethod-times*)
