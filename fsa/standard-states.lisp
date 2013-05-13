;;;-*- Package: STANDARD-STATES; Mode: Lisp; Base: 10 -*-
 
;;; Copyright (c) 1992 by Xerox Corporation

#|(cl:defpackage :standard-states
  (:use :common-lisp :fsa)
  (:export fixnum-states last-state))|#

(cl:in-package :standard-states)

;;;; FIXNUM-STATES: mixin for FSAs whose states are fixnums
(defclass fixnum-states () ((last-state :accessor last-state :initform -1)))
(defmethod state-order-fn ((fsa fixnum-states))
  #'(lambda (i1 i2)
      (declare (fixnum i1 i2))
      (if (= i1 i2) :equal (< i1 i2))))
(defmethod make-state ((fsa fixnum-states)) (incf (last-state fsa)))
