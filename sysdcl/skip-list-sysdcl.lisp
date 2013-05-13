;;;-*- Package: CL-USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

(cl:eval-when (cl:compile cl:eval cl:load)
  (pdefsys:load-system-def :tdb-sysdcl))

(cl:in-package :tdb)

(def-tdb-system :skip-list ((:dir "util")) "skip-list")
