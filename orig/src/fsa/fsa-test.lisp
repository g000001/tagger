;;;-*- Package: FSA-TEST; Syntax: Common-Lisp; Base: 10 -*-
;;; Copyright (c) 1992 by Xerox Corporation

(cl:defpackage :fsa-test
  (:use :common-lisp :fsa :fsa-standard))

(cl:in-package :fsa-test)

(defvar *standard-fsa* (make-instance 'fsa-standard))


(defmacro define-fsa-test (name &body body)
       `(eval-when (load)
               (format *error-output* "Starting test: ~s ..." ,name)
               (if ,@body
                   (format *error-output* "Succeeded~%~%")
                   (format *error-output* "Failed~%~%"))))

(define-fsa-test "Simple closure"
    (let ((n (regexp-to-fsa '(* (/ 0 1)) *standard-fsa*)))
      (and (in-language-p '#() n)
	   (in-language-p '#(0) n)
	   (in-language-p '#(1) n)
	   (in-language-p '#(0 0 1 0 1 0 1) n)
	   (in-language-p '#(1 1 0 0 0 1 0 1 0 1) n))))

(define-fsa-test "Simple plus"
    (let ((n (regexp-to-fsa '(+ (/ (! 0 0) (! 1 1)))
			    *standard-fsa*)))
      (and (not (in-language-p #() n))
	   (in-language-p '#(0 0) n)
	   (in-language-p '#(1 1) n)
	   (in-language-p '#(0 0 1 1 1 1 0 0 1 1) n)
	   (not (in-language-p '#(1 1 0 0 0 1 0 1 0 1) n)))))

(define-fsa-test "Simple optional"
    (let ((n (regexp-to-fsa '(+ (/ (! 0 (? 0)) (! 1 (? 1))))
			    *standard-fsa*)))
      (and (not (in-language-p '#() n))
	   (in-language-p '#(0) n)
	   (in-language-p '#(1 1 1) n)
	   (in-language-p '#(0 0 1 1 1 1 0 0 1 1) n)
	   (in-language-p '#(1 1 0 0 0 1 0 1 0 1) n))))


(define-fsa-test "Simple minus"
    (let ((n (regexp-to-fsa '(- (! (* 0) (+ 1)) (! (+ 1) (* 0)))
			    *standard-fsa*)))
      (and (not (in-language-p '#(0) n))
	   (not (in-language-p '#(1) n))
	   (in-language-p '#(0 1) n)
	   (in-language-p '#(0 0 1 1 1 1 1) n))))


(define-fsa-test "Simple or"
    (let ((n (regexp-to-fsa '(/ (! (* 0) (+ 1)) (! (+ 1) (* 0)))
			    *standard-fsa*)))
      (and (not (in-language-p '#(0) n))
	   (in-language-p '#(1) n)
	   (in-language-p '#(0 1) n)
	   (in-language-p '#(0 0 1 1) n)
	   (in-language-p '#(1 0) n)
	   (in-language-p '#(1 1 0 0 0) n)
	   (in-language-p '#(1 1 1 1 1) n)
	   (not (in-language-p '#(0 0 1 1 0) n)))))

(define-fsa-test "Simple word list"
    (let* ((words '("cat" "cattle" "candy" "camera" "nation"
		    "ration" "evolution" "revolution" 
		    "solution" "resolution"))
	   (fsa (regexp-to-fsa (cons '/ words) *standard-fsa*)))
      (dolist (w words t)
	(if (not (in-language-p w fsa))
	    (return nil)))))
