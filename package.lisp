;;;; package.lisp

(cl:in-package :cl-user)


(defpackage :tagger
  (:use)
  (:export))


(defpackage :tagger.internal
  (:use :cl)
  (:export :cltl1-eval-when))


(cl:defpackage :cl-extensions
  (:use :common-lisp #+(and allegro (version>= 4 1)) :clos)
  (:use :tagger.internal)
  (:shadow #:file-length)
  (:export #:format-name)
  (:export #:with-collection #:collect)
  (:export #:once-only)
  (:export #:make-lock #:with-lock #:with-locks #:without-interrupts
	   #:process-block #:add-process
	   #:kill-process #:this-process)
  (:export #:byte8 #:byte16 #:byte32 #:int29 #:byte28 #:byte7)
  (:export #:define-list-sorter #:get-qua
	   #:effective-function #:with-effective-function)
  (:export #:fast-read-char #:fast-read-file-char)
  (:export #:make-directory #:remove-directory))


(cl:defpackage :ssb
  (:use :common-lisp :cl-extensions)
  (:export #:make-ssb #:ssb-buffer #:ssb-pointer #:ssb-push-extend #:ssb-string
	   #:copy-ssb #:reinitialize-ssb #:ssb-read-line #:ssb-string=))


(cl:defpackage :string-resource
  (:use :common-lisp :cl-extensions)
  (:export #:+max-string-length+
	   #:alloc-string #:string-copy #:simple-string-copy #:free-string))


(cl:defpackage :finite-state-automata
  (:nicknames :fsa)
  (:use :common-lisp)
  #+clos(:import-from clos slot-definition-name class-slots)
  #+pcl (:import-from pcl slot-definition-name class-slots)
  #+mcl (:import-from ccl slot-definition-name class-instance-slots)
  (:export #:ordered-set #:set-order-fn #:set-length #:set-insert #:set-clear
	   #:set-copy #:set-union #:set-intersect #:set-minus #:set-map
	   #:set-member-p #:set-empty-p
	   
	   #:relation #:relation-get #:relation-map #:relation-empty-p
	   
	   #:fsa #:nfa #:dfa
	   #:*epsilon* #:fsa-start-state #:fsa-delta #:fsa-final-states
	   
	   #:symbol-order-fn #:state-order-fn
	   #:make-state #:make-nfa #:make-dfa #:make-ordered-set #:make-relation
	   #:make-state-map #:copy-delta #:make-state-relation
	   #:symbols-get #:delta-get #:states-get
	   
	   #:in-language-p #:determinize #:invert #:minimize )
  (:export #:fsa-concat #:fsa-closure #:fsa-plus #:fsa-union #:fsa-optional
           #:fsa-minus #:fsa-symbol

           #:regexp-to-fsa #:* #:+ #:? #:- #:/ #:or #:! #:sequence))


(cl:defpackage :sv-resource
  (:use :common-lisp)
  (:export #:alloc-sv #:%vector #:sv-copy #:free-sv))


(cl:defpackage :tdb
  (:use :common-lisp)
  (:use :tagger.internal)
  (:use :cl-extensions)
  (:use :ssb)
  (:use :fsa)
  (:use :string-resource)
  (:use :sv-resource)
  (:export #:tdb #:*tdb-root* #:tdb-pathname #:tdb-dirpath
	   #:def-tdb-system #:tdb-compile
	   #:*normally-optimized* #:*highly-optimized*)
  (:export #:corpus #:initialize-corpus #:open-corpus #:update-corpus
           #:close-corpus
           #:open-doc #:close-doc #:byte28
           #:doc-title #:display-doc
           #:match-doc-titles #:do-titles
           #:map-docs #:do-docs #:corpus-id-limit)
  (:export #:fsa-concat #:fsa-closure #:fsa-plus #:fsa-union #:fsa-optional
           #:fsa-minus #:fsa-symbol

           #:regexp-to-fsa #:* #:+ #:? #:- #:/ #:or #:! #:sequence)
  (:export #:analysis #:*analysis-directory*

           #:ts #:char-ts #:char-stream #:ts-char-stream #:ts-doc-id
           #:make-ts

           #:token-filter
           #:next-token #:token-start-pos #:token-end-pos
           #:sentence-start-pos #:paragraph-start-pos
           #:get-text-string #:ts-types

           #:do-tokens

           #:clean-text-string #:print-paragraph)
  (:export #:lexicon
           #:lexicon-tags #:lexicon-classes #:lexicon-open-class
           #:lexicon-lookup #:class-order
           #:lexicon-filter #:ts-lexicon
           #:*the-keyword-package*
           #:*tokenizer-class* #:re-read-lexicon))


(cl:defpackage :standard-states
  (:use :common-lisp :fsa)
  (:export #:fixnum-states #:last-state))


(cl:defpackage :standard-symbols
  (:use :common-lisp :fsa)
  (:export #:char-symbols #:symbol-symbols #:fixnum-symbols #:standard-symbols)
  (:export #:transducer-symbols
	   #:make-transducer-pair #:transducer-pair-p #:tp-upper #:tp-lower))


(cl:defpackage :list-sets
  (:use :common-lisp :fsa)
  (:export #:list-sets #:list-set))


(cl:defpackage :skip-list
  (:use :cl)
  ;; high-level interface
  (:export #:skip-list #:make-skip-list #:free-skip-list
           #:skip-list-length #:skip-list-empty-p
           #:skip-list-get #:skip-list-remove
           #:skip-list-top #:skip-list-pop
           #:skip-list-merge #:skip-list-copy
           #:skip-list-union #:skip-list-intersection #:skip-list-difference
           #:do-skip-list #:map-skip-list #:clear-skip-list))


(cl:defpackage :skip-list-relations
  (:use :common-lisp :fsa :skip-list)
  (:export #:skip-list-relations #:skip-list-relation))


(cl:defpackage :fsa-standard
  (:use :common-lisp :fsa :standard-states :standard-symbols
	:list-sets :skip-list-relations)
  (:export #:fsa-standard #:nfa-standard #:dfa-standard
	   #:fsa-hash #:nfa-hash #:dfa-hash))


(cl:defpackage :fsa-tokenizer
  (:use :common-lisp :cl-extensions :ssb :string-resource :tdb
	:fsa :fsa-standard :standard-states)
  (:nicknames :fsa-t)
  (:export #:* #:+ #:? #:- #:/ #:or #:! #:sequence)
  (:export #:fsa-tokenizer #:tokenizer-fsa #:make-tokenizer-fsa 
           #:def-tokenizer-fsa)
  (:export #:print-token-stream)
  (:shadow #:space))


(cl:defpackage :svb
  (:use :common-lisp :cl-extensions :string-resource)
  (:export #:svb #:make-svb #:copy-svb #:svb-buffer #:svb-pointer #:svb-push-extend
	   #:copy-svb-to-string #:copy-string-to-svb
	   #:svbref #:svb-size #:extend-svb))

(cl:defpackage :trie
  (:use :common-lisp :cl-extensions :svb)
  (:export #:trie #:define-trie
	   #:make-string-trie #:st-get #:map-st
	   #:trie-node-key #:trie-node-value #:walk-string
	   #:make-simple-vector-trie #:svt-get #:map-svt
	   #:sort-trie))


(cl:defpackage :tag-basics
  (:use :common-lisp :cl-extensions :trie :tdb :fsa-tokenizer)
  (:export #:basic-tag-ts
	   #:tagger-pathname #:tagger-symbol-biases #:tagger-transition-biases
	   #:tag->number #:number->tag #:tag-count
	   #:class->number #:number->class #:class-count
	   #:write-tag-state #:check-tag-state))


(cl:defpackage :cons-resource
  (:use :common-lisp :cl-extensions)
  (:export #:%cons #:%list #:%make-list #:%copy-tree #:%copy-list
	   #:%push #:%pop #:%with-collection #:%collect #:%delete
	   #:free-cell #:free-list #:free-tree))


(cl:defpackage :binary-io
  (:use :common-lisp :cl-extensions :string-resource :cons-resource)
  (:use :tagger.internal)
  (:export #:open-byte8-stream #:*default-byte8-stream-buffer-size*
	   #:close-byte8-stream #:with-open-byte8-stream #:flush-byte8-stream
	   #:get-byte8-stream-position #:set-byte8-stream-position
	   #:byte8-stream-length #:pad-byte8-stream)
  (:export #:io-fns #:io-fns-p #:io-fns-name #:ordered-io-fns #:ordered-io-fns-p
	   #:find-io-fns #:make-io-fns #:make-ordered-io-fns
	   #:io-size-fn #:io-read-fn #:io-write-fn #:io-free-fn #:io-copy-fn 
           #:io-order-fn
	   #:define-io-fns #:define-io-struct
	   #:debug-io-fns #:undebug-io-fns
	   #:io-fns-arg)
  (:export #:byte8 #:byte8-write #:byte8-read
	   #:byte16 #:byte16-read #:byte16-write #:byte16-order
	   #:byte32 #:read-byte32 #:write-byte32
	   #:swapped-byte32 #:byte32-read #:byte32-write)
  (:export #:null)
  (:export #:integer #:integer-write #:integer-read #:integer-size
           #:integer-order)
  (:export #:int29 #:int29-write #:int29-read #:int29-size #:int29-order)
  (:export #:string #:stringp #:simple-string 
	   #:string-write #:string-read #:string-size
	   #:string-order #:stringp-order #:simple-string-order
           #:simple-stringp-order #:simple-string-size)
  (:export #:single-float #:single-float-write #:single-float-read
	   #:single-float-size #:single-float-order))


(cl:defpackage :variable-storage
  (:use :common-lisp :cons-resource)
  (:use :tagger.internal)
  (:export #:make-variable-storage #:clear-variable-storage
	   #:count-variable-storage #:print-variable-storage
	   #:alloc-item #:free-item
	   #:with-storage-balance))


(cl:defpackage :vector-resource
  (:use :common-lisp :cl-extensions :variable-storage
        :tagger.internal) 
  ;;may want to add binary-io  later
  (:export #:resource-type #:fixvref #:adjust-lfixv #:fill-sv
           #:fixnum-vector #:fill-sfv #:let-sfv #:clear-byte32v-storage
           #:sfv-length #:sfv #:sv #:alloc-lsv #:adjust-byte32v #:make-byte32v
           #:sfv-add #:fill-lsfv #:fixv #:adjust-lsfv #:alloc-lsfv #:adjust-lsv
           #:fill-fixv #:clear-fixv-storage #:byte32vref #:fill-lfixv
           #:adjust-fixv #:alloc-sfv #:let-byte32v #:let-sv #:sfv-max
           #:fill-lsv #:alloc-fixv #:fill-lbyte32v #:copy-byte32v
           #:free-byte32v #:lert* #:free-fixv #:lert #:sfv-div #:copy-sv
           #:byte32-vector #:alloc-lfixv #:sfvref #:make-sv #:copy-sfv
           #:let-fixv #:copy-fixv #:adjust-lbyte32v #:fixv-length #:free-sfv
           #:adjust-sv #:adjust-sfv #:byte32v-length #:fill-byte32v
           #:clear-sfv-storage #:clear-sv-storage #:make-fixv #:free-fn
           #:alloc-lbyte32v #:alloc-byte32v #:free-sv #:make-sfv #:byte32v
           #:alloc-sv #:sv-length #:single-float-vector)
  (:export #:lert #:lert* #:free-fn #:resource-type
           #:let-sfv #:copy-sfv))


(cl:defpackage :float-vector
  (:use :common-lisp :cons-resource :binary-io :variable-storage
	:vector-resource) 
  (:export #:single-float-vector #:make-sfv #:sfvref #:make-sfv #:alloc-sfv 
	   #:adjust-sfv #:free-sfv #:copy-sfv #:let-sfv
	   #:fill-sfv #:sfv-add #:sfv-div #:sfv-max
	   #:sfv-read #:sfv-write #:sfv-size
	   #:clear-sfv-storage))


(cl:defpackage :hmm
  (:use :common-lisp :cl-extensions :cons-resource :float-vector :binary-io)
  (:shadow #:pi)
  (:export #:hmm #:make-hmm #:hmm-n #:hmm-m #:hmm-pi #:hmm-a #:hmm-b
	   #:hmm-est #:make-hmm-est #:hmm-maximal-path
	   #:hmm-train #:hmm-train-multiple
	   #:hmm-read-file #:hmm-write-file #:hmm-read #:hmm-write
	   #:make-sfm #:sfmref))


(cl:defpackage :tag-analysis
  (:use :common-lisp :cl-extensions :cons-resource :string-resource :svb :ssb
	:sv-resource :hmm :tdb :fsa-tokenizer :tag-basics)
  (:export #:tag-analysis #:tagging-ts #:token-class
	   #:tag-file #:tag-string #:tokenize-file))


(cl:defpackage :class-guesser
  (:use :common-lisp :cl-extensions :cons-resource :string-resource
	:sv-resource :trie
	:tdb :tag-basics)
  (:export #:write-string-trie #:read-string-trie
	   #:train-guesser-on-files #:suffix-lexicon #:suffix-pathname))


(cl:defpackage :vector-lexicon
  (:use :common-lisp :cl-extensions :string-resource :cons-resource
	:sv-resource :svb :ssb :skip-list :binary-io :trie :tdb)
  (:export #:vector-lexicon #:read-vector-lexicon #:write-vector-lexicon)
  (:shadow #:index))


(cl:defpackage :tag-brown
  (:use :common-lisp :cl-extensions :string-resource :ssb :trie :tdb
	:vector-lexicon :tag-basics :fsa-tokenizer :tag-analysis
	:class-guesser)
  (:export #:brown-tokenizer #:evaluate-brown))


(cl:defpackage :tag-english
  (:use :common-lisp :cl-extensions :tdb
	:tag-basics :fsa-tokenizer :vector-lexicon :class-guesser
	:tag-analysis :tag-brown)
  (:export #:english-tokenizer #:*english-tokenizer-fsa*))


(cl:defpackage :tag-trainer
  (:use :common-lisp :cl-extensions :cons-resource :string-resource
	:sv-resource :hmm :float-vector
	:tdb :tag-basics)
  (:shadow #:pi)
  (:export #:train-on-docs #:train-on-files #:train-combination))


;;; *EOF*
