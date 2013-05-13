;;; -*- Mode: Lisp; Package: PCL; Base: 10.; Syntax: Common-Lisp -*-
;;;
;;; Precompile certain PCL code to speed up loadup.
;;;
;;; Copyright (c) 1988 by Xerox Corporation.  All rights reserved.
;;;

(in-package "PCL")


(precompile-random-code-segments)
