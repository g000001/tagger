;;;-*- Package: USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

(cl:eval-when (cl:compile cl:eval cl:load)
  (pdefsys:load-system-def :tdb-sysdcl))

(cl:in-package :tdb)

(def-tdb-system :fsa ((:dir "fsa")) "fsa-basics")

(def-tdb-system :fsa-calculus
    ((:dir "fsa")
     (:sub-systems :fsa))
  ("fsa-calculus"))

(def-tdb-system :standard-states
    ((:dir "fsa")
     (:sub-systems :fsa))
  ("standard-states"))

(def-tdb-system :standard-symbols
    ((:dir "fsa")
     (:sub-systems :fsa))
  ("standard-symbols"))

(def-tdb-system :alist-relations
    ((:dir "fsa")
     (:sub-systems :fsa))
  ("alist-relations"))

(def-tdb-system :skip-list-relations
    ((:dir "fsa")
     (:sub-systems :fsa :skip-list))
  ("skip-list-relations"))

(def-tdb-system :list-sets
    ((:dir "fsa")
     (:sub-systems :fsa))
  ("list-sets"))

(def-tdb-system :fsa-standard
    ((:dir "fsa")
     (:sub-systems :fsa-calculus :standard-symbols :standard-states
		   :list-sets :skip-list-relations))
  ("fsa-standard"))

(def-tdb-system :fsa-test
    ((:dir "fsa")
     (:sub-systems :fsa-standard))
  ("fsa-test"))
