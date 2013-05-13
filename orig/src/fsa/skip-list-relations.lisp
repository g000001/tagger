;;;-*- Package: SKIP-LIST-RELATIONS; Mode: Lisp; Base: 10 -*-
 
;;; Copyright (c) 1992 by Xerox Corporation

;;;; Skip list implementation of FSA relation protocol

(cl:defpackage :skip-list-relations
  (:use :common-lisp :fsa :skip-list)
  (:export :skip-list-relations :skip-list-relation))

(cl:in-package :skip-list-relations)

(defclass skip-list-relation (relation)
	  ((skip-list :initarg :skip-list :accessor relation-skip-list)))

(defmethod shared-initialize :after ((relation skip-list-relation) slots
				     &key order-fn &allow-other-keys)
  (declare (ignore slots))
  (setf (relation-skip-list relation) (make-skip-list order-fn)))

(defmethod print-object ((relation skip-list-relation) stream)
  (format stream "#<skip-list-relation: ~s>" (relation-skip-list relation)))

(defclass skip-list-relations () ())

(defmethod make-relation (order (relations skip-list-relations))
  (make-instance 'skip-list-relation :order-fn order))

(defmethod relation-get (key (relation skip-list-relation))
  (skip-list-get key (relation-skip-list relation)))

(defmethod (setf relation-get) (value key (relation skip-list-relation))
  (setf (skip-list-get key (relation-skip-list relation)) value))

(defmethod relation-map (fn (relation skip-list-relation))
  (do-skip-list (key value (relation-skip-list relation))
    (funcall fn key value)))

(defmethod relation-empty-p ((relation skip-list-relation))
  (skip-list-empty-p (relation-skip-list relation)))
