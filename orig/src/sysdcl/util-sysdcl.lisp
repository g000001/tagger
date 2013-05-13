;;;-*- Package: TDB; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992 by Xerox Corporation

;;;; the utilities library

(cl:eval-when (cl:compile cl:eval cl:load)
  (pdefsys:load-system-def :tdb-sysdcl))

(cl:in-package :tdb)

(def-tdb-system :util			; the basic library
    ((:sub-systems :cl-extensions :string-resource :ssb :cons-resource)))

(def-tdb-system :cl-extensions ((:dir "util"))
  "cl-extensions")

(def-tdb-system :string-resource ((:dir "util") (:sub-systems :cl-extensions))
  "string-resource")

(def-tdb-system :cons-resource ((:dir "util") (:sub-systems :cl-extensions))
  "cons-resource")

(def-tdb-system :sv-resource ((:dir "util")) "sv-resource")

(def-tdb-system :sgml-parser ((:dir "util") (:sub-systems :ssb)) "sgml-parser")

(def-tdb-system :date-parser ((:dir "util")) "date-parser")

(def-tdb-system :lru-cache ((:dir "util")) "lru-cache")

(def-tdb-system :float-box ((:dir "util") 
			    (:sub-systems :cl-extensions))
  ;;; Note a dependency on Float_box in sparse-vector
   #+(and excl boxes-float-calls)
   ("float-box" :language #-(and sun (not svr4)) :c
		#+(and sun (not svr4)) :gcc :optimizations ("-O"))
  "float-box")

(def-tdb-system :binary-io ((:dir "util")
			    (:sub-systems :string-resource :cons-resource))
  "io-structs"
  #+excl
  ("io-byte8" :language #-(and sun (not svr4)) :c
		#+(and sun (not svr4)) :gcc  :optimizations ("-O"))
  (#+excl "io-byte8-c"  #-excl "io-byte8" :load-before-compile t)
  ("io-builtin" :load-before-compile t))

(def-tdb-system :b-tree
    ((:dir "util")
     (:sub-systems :cl-extensions :binary-io :cons-resource :skip-list))
  "b-tree")

(def-tdb-system :heap-file
    ((:dir "util")
     (:sub-systems :cl-extensions :binary-io :cons-resource :skip-list))
  "heap-file")


(def-tdb-system :priority-queue ((:dir "util") 
				 (:sub-systems :cons-resource :simple-vector)) 
  "priority-queue")

(def-tdb-system :float-pq ((:dir "util")
			    (:sub-systems :cl-extensions :cons-resource
					  :vector-resource))
  "float-pq")

(def-tdb-system :trie ((:dir "util") (:sub-systems :cl-extensions :svb))
  "trie")

(def-tdb-system :ssb ((:dir "util") (:sub-systems :cl-extensions))
  "ssb")

(def-tdb-system :svb
    ((:dir "util")
     (:sub-systems :cl-extensions :string-resource))
  "svb")

(def-tdb-system :timers ((:dir "util")) "timers")

;#+excl
;(def-tdb-system :eval-service ((:dir "util"))
;  ("eval-service" :load-before-compile t))

(pdefsys:set-system-source-file
 :fsm-calculus (tdb-pathname "fsm" "fsm-calculus-sysdcl.lisp"))

(pdefsys:set-system-source-file
 :fsm-runtime (tdb-pathname "fsm" "fsm-runtime-sysdcl.lisp"))

(def-tdb-system :fst-lookup
    ((:dir "util")
     (:sub-systems :string-resource :svb :fsm-runtime))
  "fst-lookup")

(def-tdb-system :vector-resource
    ((:dir "util")
     (:sub-systems :cons-resource :cl-extensions :variable-storage) )
  "vector-resource")

(def-tdb-system :float-vector
    ((:dir "util")
     (:sub-systems :cl-extensions :binary-io :cons-resource :variable-storage
		   :vector-resource))
  "float-vector")

(def-tdb-system :float-matrix
    ((:dir "util")
     (:sub-systems :cl-extensions :float-vector :simple-vector))
  "float-matrix")

(def-tdb-system :hmm
    ((:dir "util")
     (:sub-systems :cl-extensions :cons-resource :binary-io :float-vector)) 
  "hmm")

(def-tdb-system :fixnum-vector
    ((:dir "util")
     (:sub-systems
      :cl-extensions :cons-resource :binary-io :variable-storage :debug-alloc
      :vector-resource))
  "fixnum-vector")

(def-tdb-system :simple-vector
    ((:dir "util")
     (:sub-systems :cl-extensions :cons-resource :variable-storage
		   :vector-resource))
  "simple-vector")

#+cltl2
(def-tdb-system :storage ((:dir "util"))
  "storage")
		
(def-tdb-system :debug-alloc
    ((:dir "util")
     (:sub-systems :cl-extensions))
  ("debug-alloc"))

(def-tdb-system :sparse-vector
    ((:dir "util")
     (:sub-systems
      :cl-extensions :cons-resource :float-vector :fixnum-vector
      :simple-vector :debug-alloc :binary-io :float-box)) 
  #+excl
  ("sparse-vector" :language #-(and sun (not svr4)) :c
		   #+(and sun (not svr4)) :gcc
		   :optimizations ("-O" #+boxes-float-calls "-DFLOAT_BOX")
		   :libraries ("/usr/lib/libm.a"))
  ("sparse-vector" :load-before-compile t))


(def-tdb-system :variable-storage
    ((:dir "util")
     (:sub-systems :cons-resource))
  "variable-storage")
