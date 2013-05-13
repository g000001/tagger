;;;-*- Package: FSA-STANDARD; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
 
;;; Copyright (c) 1992 by Xerox Corporation

;;;; various implementations of FSA data representation protocols

#|(cl:defpackage :fsa-standard
  (:use :common-lisp :fsa :standard-states :standard-symbols
	:list-sets :skip-list-relations)
  (:export :fsa-standard :nfa-standard :dfa-standard
	   :fsa-hash :nfa-hash :dfa-hash))|#

(cl:in-package :fsa-standard)
			      
;;;; FSA-STANDARD: states are integers and symbols are characters

(defclass fsa-standard
    (fixnum-states standard-symbols skip-list-relations list-sets)
    ())

(defclass nfa-standard (fsa-standard nfa) ())
(defclass dfa-standard (fsa-standard dfa) ())

(defmethod make-nfa ((fsa fsa-standard))
  (make-instance 'nfa-standard))
(defmethod make-dfa ((fsa fsa-standard))
  (make-instance 'dfa-standard))
