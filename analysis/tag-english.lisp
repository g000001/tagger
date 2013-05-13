;;;-*- Package: TAG-ENGLISH; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992, 1993 by Xerox Corporation.  All rights reserved.

#|(cl:defpackage :tag-english
  (:use :common-lisp :cl-extensions :tdb
	:tag-basics :fsa-tokenizer :vector-lexicon :class-guesser
	:tag-analysis :tag-brown)
  (:export english-tokenizer *english-tokenizer-fsa*))|#

(cl:in-package :tag-english)

(defvar *english-tokenizer-fsa*)

(defclass english-tokenizer (brown-tokenizer) ()
  (:default-initargs :tokenizer-fsa *english-tokenizer-fsa*))

(setq *tokenizer-class* (find-class 'english-tokenizer))

(def-tokenizer-fsa *english-tokenizer-fsa*
  (let* ((space `(/ #\space #\tab #\page))
	 (eol #\newline)
	 (white `(/ ,space ,eol))
	 (digit `(/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	 (small `(/ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n
		    #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
	 (cap `(/ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N
		  #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
	 (alpha `(/ ,small ,cap))
	 (break `(/ #\- #\/))
	 (punct `(/ #\. #\, #\- #\/ #\; #\: #\( #\) #\` #\' #\" #\* #\=))
	 (numeric `(/ ,digit #\$ #\% #\^ #\# #\* #\+ #\@ #\, #\. #\: #\;))
	 (alphanumeric `(/ ,alpha ,numeric)))
    `(;; alpha with at most one, non-initial apostrophe
      (:word (! (+ ,alpha) (? (! #\' (* ,alpha)))))
      ;; abbreviations, e.g. U.S.A.
      (:word (/ (+ (! ,alpha #\.))
		(/ "Dr." "Mr." "Mrs." "Ms."
		   "dr." "mr." "mrs." "ms.")))
      ;; acronyms with ampersands, e.g. AT&T.
      (:word (! (+ ,alpha) "&" (+ ,alpha)))

      (:cd				; numbers, model, serial, etc.
       (- (! (* ,alphanumeric)
	     (/ ,alpha ,digit)
	     (* ,alphanumeric)
	     (* (! (? (! ,break (+ ,alphanumeric)))
		   ,break (- (+ ,alphanumeric) (+ ,alpha)))))
	  ;; can't start or end w/ punct, or contain multiple adjacent dots
	  (/ (! ,punct (+ (/ ,alphanumeric ,break)))
	     (! (+ (/ ,alphanumeric ,break)) ,punct)
	     (! (+ (/ ,alphanumeric ,break)) ".."
		(+ (/ ,alphanumeric ,break))))))

      (:cd				; phone numbers
       (! (? (! (/ (! "(" ,digit ,digit ,digit ")")
		   (! ,digit ,digit ,digit))
		(? (/ #\- ,white))))
	  ,digit ,digit ,digit
	  (? (/ #\- ,white))
	  ,digit ,digit ,digit ,digit))

      ;; sentences
      (:sent (! (/ #\. #\? #\!)
		;; whitespace with at most one eol
		(/ (+ ,space)
		   (! (* ,space) ,eol (* ,space)))
		,cap)
	     1)
      ;; paragraphs
      (:para ;; whitespace with at least two eols
       (! ,eol (+ (! (* ,space) ,eol))))

      ;; various character classes
      (:cm (/ #\, #\;))
      (:- (/  #\( #\) "--" #\: #\[ #\])))))
