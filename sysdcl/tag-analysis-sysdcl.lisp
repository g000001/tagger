;;;-*- Package: TDB; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992, 1993 by Xerox Corporation

;;;; the hmm part-of-speech tagging utility

(cl:eval-when (cl:compile cl:eval cl:load)
  (pdefsys:load-system-def :tdb-sysdcl))

(cl:in-package :tdb)

;;; tagging

(def-tdb-system :lexicon-protocol
    ((:dir "analysis")
     (:sub-systems :sv-resource :analysis))
  "lexicon-protocol")

(def-tdb-system :vector-lexicon
    ((:dir "analysis")
     (:sub-systems
      :cl-extensions :string-resource :cons-resource :sv-resource
      :lexicon-protocol :svb :ssb :trie :skip-list :binary-io))
  "vector-lexicon")

#+parc
(def-tdb-system :fst-lexicon
    ((:dir "analysis")
     (:sub-systems :cl-extensions :string-resource :sv-resource :svb 
		   :lexicon-protocol :fst-lookup))
  "fst-lexicon")

(def-tdb-system :tag-basics
    ((:dir "analysis")
     (:sub-systems
      :cl-extensions :trie :analysis :lexicon-protocol :fsa-tokenizer))
  "tag-basics")
 
(def-tdb-system :tag-trainer
    ((:dir "analysis")
     (:sub-systems :cl-extensions :cons-resource :hmm :float-vector
		   :corpus :analysis :tag-basics))
  "tag-trainer")
 
(def-tdb-system :class-guesser
    ((:dir "analysis")
     (:sub-systems :cl-extensions :cons-resource :string-resource :sv-resource
		   :trie :analysis :tag-basics))
  "class-guesser")

(def-tdb-system :tag-analysis
    ((:dir "analysis")
     (:sub-systems :cl-extensions :cons-resource :string-resource :svb :ssb
		   :sv-resource :hmm :analysis :tag-basics))
  "tag-analysis")

(def-tdb-system :tag-brown
    ((:dir "analysis")
     (:sub-systems :tag-trainer :tag-analysis :vector-lexicon :class-guesser))
  "tag-brown")

(def-tdb-system :tag-english
    ((:dir "analysis") (:sub-systems :tag-brown))
  "tag-english")

(def-tdb-system :tag-twol
    ((:dir "analysis") (:sub-systems :tag-english))
  "tag-twol")

(def-tdb-system :np-recognizer
    ((:dir "analysis") (:sub-systems :fsa-filter :tag-analysis))
  "np-recognizer")


