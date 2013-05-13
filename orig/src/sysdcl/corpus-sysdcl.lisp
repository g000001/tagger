;;;-*- Package: TDB; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992, 1993 by Xerox Corporation.  All rights reserved.

(cl:eval-when (cl:compile cl:eval cl:load)
  (pdefsys:load-system-def :tdb-sysdcl))

(cl:in-package :tdb)

(def-tdb-system :corpus
    ((:dir "corpus")
     (:sub-systems :util))
  "corpus-protocol")

;;;; generic corpora

(def-tdb-system :persistent-corpus
    ((:dir "corpus")
     (:sub-systems :corpus :b-tree))
  ("persistent-corpus"))

(def-tdb-system :append-corpus
    ((:dir "corpus")
     (:sub-systems :corpus :cons-resource))
  ("append-corpus"))

(def-tdb-system :directory-corpus
    ((:dir "corpus")
     (:sub-systems :corpus :persistent-corpus))
  ("directory-corpus"))

(def-tdb-system :file-corpus
    ((:dir "corpus")
     (:sub-systems :corpus :persistent-corpus))
  ("file-corpus"))

(def-tdb-system :brs
    ((:dir "corpus")
     (:sub-systems :ssb :file-corpus))
  ("brs"))
	       
(def-tdb-system :rmail
    ((:dir "corpus")
     (:sub-systems :ssb :directory-corpus))
  ("rmail"))

(def-tdb-system :ircolls
    ((:dir "corpus")
     (:sub-systems :ssb :file-corpus))
  ("ircolls"))

(def-tdb-system :context
    ((:dir "corpus")
     (:sub-systems :tdb))
  ("context-corpus"))

(def-tdb-system :sentence-corpus
    ((:dir "corpus")
     (:sub-systems :cons-resource :tdb :ssb))
  ("sentence-corpus"))


;;;; specific corpora

(def-tdb-system :grolier
    ((:dir "corpus")
     (:sub-systems :brs))
  ("grolier"))

(def-tdb-system :nytimes
    ((:dir "corpus")
     (:sub-systems :brs :append-corpus))
  ("nytimes"))

(def-tdb-system :ssl-bio
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("ssl-bio"))

(def-tdb-system :moby-dick
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("moby-dick"))

(def-tdb-system :aesop
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("aesop"))

(def-tdb-system :alice-in-wonderland
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("alice-in-wonderland"))

(def-tdb-system :through-the-looking-glass
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("through-the-looking-glass"))

(def-tdb-system :peter-pan
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("peter-pan"))

(def-tdb-system :o-pioneers
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("o-pioneers"))

(def-tdb-system :far-from-the-madding-crowd
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("far-from-the-madding-crowd"))

(def-tdb-system :fbis
    ((:dir "corpus")
     (:sub-systems :corpus :date-parser :sgml-parser :lru-cache
		   :persistent-corpus))
  ("fbis"))

(def-tdb-system :tipster
    ((:dir "corpus")
     (:sub-systems :corpus :analysis :sgml-parser :directory-corpus))
  ("tipster"))

(def-tdb-system :sec
    ((:dir "corpus")
     (:sub-systems :corpus :sgml-parser :directory-corpus))
  ("sec"))

(def-tdb-system :cacm
    ((:dir "corpus")
     (:sub-systems :ircolls))
  ("cacm"))

(def-tdb-system :cranfield
    ((:dir "corpus")
     (:sub-systems :ircolls))
  ("cranfield"))

(def-tdb-system :collected-works
    ((:dir "corpus")
     (:sub-systems :file-corpus))
  ("collected-works"))

(def-tdb-system :reuters
    ((:dir "corpus")
     (:sub-systems :directory-corpus))
  ("reuters"))

(def-tdb-system :brown
    ((:dir "corpus")
     (:sub-systems :directory-corpus :file-corpus))
  ("brown"))
