(cl:in-package :asdf)


(defsystem :tagger
  :description "The Xerox Part-of-Speech Tagger Version 1.2"
  :author "Doug Cutting and Jan Pedersen of the Xerox Palo Alto Research Center"
  :license "Use, reproduction, and distribution of this software is permitted, but only for non-commercial research or educational purposes. see the tagger/COPYRIGHT file for more information."
  :serial t
  :depends-on (:closer-mop)
  :components ((:file "package")
               (:file "cltl1-compat")
               (:module "sysdcl"
                        :components ((:file "tdb-sysdcl-sysdcl")))
               (:module "util"
                        :components ((:file "cl-extensions")
                                     (:file "string-resource")
                                     (:file "cons-resource")
                                     (:file "sv-resource")
                                     (:file "io-structs")
                                     (:file "io-byte8")
                                     (:file "io-builtin")
                                     (:file "svb")
                                     (:file "trie")
                                     (:file "ssb")
                                     (:file "variable-storage")
                                     (:file "vector-resource")
                                     (:file "float-vector")
                                     (:file "hmm")
                                     ;; 
                                     (:file "skip-list")
                                     ))
               (:module "corpus"
                        :components ((:file "corpus-protocol")))
               (:module "fsa"
                        :components ((:file "fsa-basics")
                                     (:file "standard-states")
                                     (:file "standard-symbols")
                                     (:file "list-sets")
                                     (:file "skip-list-relations")
                                     (:file "fsa-standard")
                                     (:file "fsa-calculus")))
               (:module "analysis"
                        :components ((:file "analysis-protocol")
                                     (:file "lexicon-protocol")
                                     (:file "tag-basics")
                                     (:file "tag-analysis")
                                     (:file "class-guesser")
                                     (:file "fsa-tokenizer")
                                     (:file "vector-lexicon")
                                     (:file "tag-brown")
                                     (:file "tag-english")
                                     (:file "tag-trainer")))))


#|(defmethod perform ((o test-op) (c (eql (find-system :cool))))
  (load-system :cool)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :co-test :co-test))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))|#
