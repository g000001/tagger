;;;-*- Package: STANDARD-SYMBOLS; Mode: Lisp; Base: 10 -*-
 
;;; Copyright (c) 1992, 1993 by Xerox Corporation.  All rights reserved.

(cl:defpackage :standard-symbols
  (:use :common-lisp :fsa)
  (:export :char-symbols :symbol-symbols :fixnum-symbols :standard-symbols)
  (:export :transducer-symbols
	   :make-transducer-pair :transducer-pair-p :tp-upper :tp-lower))

(cl:in-package :standard-symbols)

;;;; CHAR-SYMBOLS: mixin for FSAs whose symbols are characters
(defclass char-symbols () ())
(defmethod symbol-order-fn ((fsa char-symbols))
  #'(lambda (c1 c2)
      (declare (character c1 c2) #.tdb:*highly-optimized*)
      (if (char= c1 c2)
	  :equal
	#+excl (and (char< c1 c2) t) #-excl(char< c1 c2))))

(defclass symbol-symbols () ())
(defmethod symbol-order-fn ((fsa symbol-symbols))
  #'(lambda (s1 s2)
      (declare (symbol s1 s2) #.tdb:*highly-optimized*)
      (if (string= s1 s2)
	  :equal
	(and (string< s1 s2) t))))

(defclass fixnum-symbols () ())
(defmethod symbol-order-fn ((fsa fixnum-symbols))
  #'(lambda (x y)
      (declare (fixnum x y) #.tdb:*highly-optimized*)
      (if (= x y) :equal (< x y))))

;;;; STANDARD-SYMBOLS: mixin for FSAs whose states are fixnums, chars
;;;; or strings
(defclass standard-symbols () ())
(defmethod symbol-order-fn ((fsa standard-symbols))
  #'(lambda (s1 s2)
      (declare #.tdb:*highly-optimized*)
      ;; sort chars before fixnums before strings
      (etypecase s1
	(character (etypecase s2
		     (character
		      (if (char= (the character s1) (the character s2))
			  :equal
			#+excl (and (char< (the character s1)
					   (the character s2)) t)
			#-excl (char< (the character s1) (the character s2))))
		     ((or fixnum simple-string) t)))
	(fixnum (etypecase s2
		  (character nil)
		  (fixnum  (if (= (the fixnum s1) (the fixnum s2))
			       :equal
			     (< (the fixnum s1) (the fixnum s2))))
		  (simple-string t)))
	(simple-string (etypecase s2
			 ((or character fixnum) nil)
			 (simple-string
			  (if (string= s1 s2) :equal
			    (and (string< s1 s2) t))))))))

;;;; Transducer mixin.  To be mixed in before another symbol class

(defstruct (transducer-pair
	    (:conc-name tp-)
	    (:constructor make-transducer-pair (upper lower)))
  upper lower)

(defclass transducer-symbols () ())
(defmethod symbol-order-fn ((fsa transducer-symbols))
  (let ((sub-order-fn (call-next-method)))
    #'(lambda (s1 s2)
	(let ((upper1 s1)
	      (lower1 s1)
	      (upper2 s2)
	      (lower2 s2))
	  (if (transducer-pair-p s1)
	      (setq upper1 (tp-upper s1)
		    lower1 (tp-lower s1)))
	  (if (transducer-pair-p s2)
	      (setq upper2 (tp-upper s2)
		    lower2 (tp-lower s2)))
	  (let ((upper (funcall sub-order-fn upper1 upper2)))
	    (if (eq upper :equal)
		(funcall sub-order-fn lower1 lower2)
	      upper))))))
	       
		 
