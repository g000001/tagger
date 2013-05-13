;;; -*- Mode: Lisp; Package: LUCID-COMMON-LISP-PATCHES; Base: 10.; Syntax: Common-Lisp -*-
;;;
;;; *************************************************************************
;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.
;;; All rights reserved.
;;;
;;; *************************************************************************
;;;
;;; Bring the existing conditon system into line with the spec.
;;;

(in-package "LUCID-COMMON-LISP-PATCHES" :use '("LISP"))


;; Lucid 3.0 is missing the PACKAGE-ERROR condition

#-lcl4.0
(lucid::migrate-for-export "LUCID-COMMON-LISP-PATCHES" "LCL"
			   '(package-error package-error-package))

#-lcl4.0
(lcl:define-condition package-error (error)
  (package))


;; There are some symbols that are defined in the LUCID package, and should be
;; made available in the LCL package...

(lucid::migrate-for-export "LUCID" "LCL"
			   '(type-error-datum type-error-expected-type
			     file-error-pathname))


;; And a few other misc items which, although they are exported from LCL, don't
;; have function defintions.

#-lcl4.0
(setf (symbol-function 'lcl:simple-condition-format-string)
      (symbol-function 'lucid::new-simple-condition-format-string))

#-lcl4.0
(setf (symbol-function 'lcl:simple-condition-format-arguments)
      (symbol-function 'lucid::new-simple-condition-format-arguments))


