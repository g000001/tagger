;;;-*- Package: TDB; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992 by Xerox Corporation.  All rights reserved.

;;; TDB Analysis components

(cl:eval-when (cl:compile cl:eval cl:load)
  (pdefsys:load-system-def :tdb-sysdcl))

(cl:in-package :tdb)

(def-tdb-system :analysis
    ((:dir "analysis") (:sub-systems :util))
  "analysis-protocol")

;;;; analysis pipeline elements

'(def-tdb-system :simple-tokenizer
    ((:dir "analysis") (:sub-systems :analysis))
  ("simple-tokenizer"))


'(def-tdb-system :fsm-tokenizer
    ((:dir "analysis") (:sub-systems :analysis :fsm-runtime))
  ("fsm-tokenizer"))

'(def-tdb-system :fsm-tokenizer-rules
    ((:dir "analysis") (:sub-systems :fsm-tokenizer :fsm-calculus))
  ("fsm-tokenizer-rules"))

(def-tdb-system :fsa-tokenizer
    ((:dir "analysis") (:sub-systems :analysis :fsa :fsa-standard))
  ("fsa-tokenizer"))


'(def-tdb-system :stop-list
    ((:dir "analysis") (:sub-systems :analysis :trie))
  ("stop-list"))

'(def-tdb-system :downcase-filter
    ((:dir "analysis") (:sub-systems :analysis))
  ("downcase-filter"))


'(def-tdb-system :twol-stemmer
    ((:dir "analysis")
     (:sub-systems :analysis #-excl :fst-lookup))
  #-excl "twol-stemmer"

  #+excl("stem"
	 :language :gcc :optimizations ("-O2")
	 :binary-pathname  #+iris4d "stem-iris4d.o" #+sun4 "stem-sun4.o")
  #+excl("c-twol-stemmer" :load-before-compile t)
  )

'(def-tdb-system :fsm-filter
    ((:dir "analysis")
     (:sub-systems :analysis :string-resource :fsm-calculus))
  ("fsm-filter"))



;;;; analysis mixins

(def-tdb-system :simple-analysis
    ((:dir "analysis")
     (:sub-systems :simple-tokenizer :downcase-filter))
  "simple-analysis")

'(def-tdb-system :stop-analysis
    ((:dir "analysis")
     (:sub-systems :simple-tokenizer :downcase-filter :stop-list))
  "stop-analysis")

'(def-tdb-system :twol-analysis
    ((:dir "analysis")
     (:sub-systems
      :simple-tokenizer :downcase-filter :stop-list :twol-stemmer))
  "twol-analysis")

'(def-tdb-system :fsa-analysis
    ((:dir "analysis")
     (:sub-systems :fsa-tokenizer :downcase-filter
		   :stop-list :ssd-analysis :twol-stemmer))
  "fsa-analysis")

'(def-tdb-system :ssd-analysis
    ((:dir "analysis")
     (:sub-systems :twol-analysis))
  ("ssd-analysis"))


'(def-tdb-system :chink-chunk-analysis
    ((:dir "analysis")
     (:sub-systems :analysis :cons-resource :stop-list :twol-stemmer
		   :downcase-filter :simple-tokenizer))
  ("chink-chunk-analysis"))

'(def-tdb-system :dink-analysis
    ((:dir "analysis")
     (:sub-systems :analysis :chink-chunk-analysis))
  ("dink-analysis"))

'(def-tdb-system :couple-analysis
    ((:dir "analysis")
     (:sub-systems :fsm-tokenizer-rules :fsm-filter :cons-resource
		   :stop-list :downcase-filter :twol-stemmer))
  ("couple-analysis"))

'(def-tdb-system :is-a
    ((:dir "search")
     (:sub-systems
      :chink-chunk-analysis :couple-analysis :interval-ps))
  ("around-chunks")
  ("is-a"))

(def-tdb-system :token-grep
    ((:dir "search")
     (:sub-systems :fsm-tokenizer-rules :fsm-filter :downcase-filter))
  ("token-grep"))
