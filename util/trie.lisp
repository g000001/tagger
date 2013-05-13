;;;-*- Package: TRIE; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1988, 1989, 1990 by Xerox Corporation

#|(cl:defpackage :trie
  (:use :common-lisp :cl-extensions :svb)
  (:export #:trie #:define-trie
	   #:make-string-trie #:st-get #:map-st
	   #:trie-node-key #:trie-node-value #:walk-string
	   #:make-simple-vector-trie #:svt-get #:map-svt
	   #:sort-trie))|# 

(cl:in-package :trie)

(defstruct (trie (:print-function %print-trie))
  (root-value nil)
  (root-subnodes (make-svb 0) :type svb)
  (key-fn nil :type (or null function))
  (inverse-key-fn nil :type (or null function)))

(defun %print-trie (trie stream depth)
  (declare (ignore depth))
  (print-unreadable-object (trie stream :type t :identity t)))


(defstruct (trie-node (:constructor make-trie-node (key)))
  (key 0 :type fixnum)
  (value nil)
  (subnode nil)
  (next nil))

;;; Define a trie accessor for lookup of vector keys of a given element-type,
;;; with functions provided to map between vector elements and fixnums, and
;;; vice versa.

(defmacro define-trie (name
		       &key (constructor (format-name "MAKE-~A" name))
			    (accessor (format-name "~A-GET" name))
			    (mapper (format-name "MAP-~A" name))
			    (walker nil)
			    (element-type t)
			    (key-accessor 'aref)
                            (key-type `(simple-array ,element-type (*)))
			    (key-length 'length)
			    element-to-fixnum
			    fixnum-to-element
		       &aux (setter
			     #+lucid (format-name "~A-SET" name)
			     #-lucid `(setf ,accessor)))
  `(progn
     (defun ,constructor (,@(unless (and element-to-fixnum fixnum-to-element)
			      '(&key))
			  ,@(unless element-to-fixnum '(key-fn))
			  ,@(unless fixnum-to-element '(inverse-key-fn)))
       (make-trie ,@(unless element-to-fixnum '(:key-fn key-fn))
		  ,@(unless fixnum-to-element
		      '(:inverse-key-fn inverse-key-fn))))
     (defun ,accessor (key trie &optional default (limit (,key-length key)))
       (declare #|(type ,key-type key)|# (fixnum limit))
       (let (,@(unless element-to-fixnum '((key-fn (trie-key-fn trie))))
	     (offset 0))
	 (declare (fixnum offset)
		  ,@(unless element-to-fixnum '((function key-fn))))
	 (when (zerop limit)
	   (return-from ,accessor (or (trie-root-value trie) default)))
	 (let ((index (,@(if element-to-fixnum
			     `(,element-to-fixnum)
			   '(funcall key-fn))
			 (the ,element-type (,key-accessor key 0))))
	       (root-subnodes (trie-root-subnodes trie)))
	   (declare (fixnum index))
	   (if (>= index (svb-size root-subnodes))
	       default
	     (let ((node (svbref root-subnodes index)))
	       (loop
		 (unless node (return default))
		 (when (= (incf offset) limit)
		   (return (or (trie-node-value node) default)))
		 (setq index
		   (,@(if element-to-fixnum
			  `(,element-to-fixnum)
			'(funcall key-fn))
		      (the ,element-type (,key-accessor key offset))))
		 (do ((subnode (trie-node-subnode node)
			       (trie-node-next subnode)))
		     ((or (null subnode) (= index (trie-node-key subnode)))
		      (setq node subnode)))))))))
     (defun ,setter (value key trie
			      &optional default (limit (,key-length key)))
       (declare #|(type ,key-type key)|# (ignore default) (fixnum limit))
       (let (,@(unless element-to-fixnum '((key-fn (trie-key-fn trie))))
	     (offset 0))
	 (declare (fixnum offset)
		  ,@(unless element-to-fixnum '((function key-fn))))
	 (if (zerop limit)
	     (setf (trie-root-value trie) value)
	   (let ((index (,@(if element-to-fixnum
			       `(,element-to-fixnum)
			     '(funcall key-fn))
			   (the ,element-type (,key-accessor key 0))))
		 (root-subnodes (trie-root-subnodes trie)))
	     (declare (fixnum index))
	     (when (>= index (svb-size root-subnodes))
	       (extend-svb root-subnodes (the fixnum (1+ index)))
	       (setf (svb-pointer root-subnodes) (svb-size root-subnodes)))
	     (let ((node (svbref root-subnodes index)))
	       (unless node
		 (setq node (make-trie-node index))
		 (setf (svbref root-subnodes index) node))
	       (loop
		 (when (= (incf offset) limit)
		   (return (setf (trie-node-value node) value)))
		 (setq index
		   (,@(if element-to-fixnum
			  `(,element-to-fixnum)
			'(funcall key-fn))
		      (the ,element-type (,key-accessor key offset))))
		 (do ((subnode (trie-node-subnode node)
			       (trie-node-next subnode))
		      (prev nil subnode))
		     ((or (null subnode) (= index (trie-node-key subnode)))
		      (when (null subnode)
			(setq subnode (make-trie-node index))
			(if prev
			    (setf (trie-node-next prev) subnode)
			  (setf (trie-node-subnode node) subnode)))
		      (setq node subnode)))))))))
     #+lucid(defsetf ,accessor (&rest rest) (val) `(,',setter ,val ,@rest))
     ,(when walker
	`(defun ,walker (fn key trie &optional (limit (,key-length key)))
	   (declare #|(type ,key-type key)|# (fixnum limit) (function fn))
	   (let (,@(unless element-to-fixnum '((key-fn (trie-key-fn trie))))
		 (offset 0))
	     (declare (fixnum offset)
		      ,@(unless element-to-fixnum '((function key-fn))))
	     (unless (> limit 0) (return-from ,walker))
	     (let ((index (,@(if element-to-fixnum
				 `(,element-to-fixnum)
			       '(funcall key-fn))
			     (the ,element-type (,key-accessor key 0))))
		   (root-subnodes (trie-root-subnodes trie)))
	       (declare (fixnum index))
	       (when (>= index (svb-size root-subnodes))
		 (extend-svb root-subnodes (the fixnum (1+ index)))
		 (setf (svb-pointer root-subnodes) (svb-size root-subnodes)))
	       (let ((node (svbref root-subnodes index)))
		 (unless node
		   (setq node (make-trie-node index))
		   (setf (svbref root-subnodes index) node))
		 (loop
		   (funcall fn node offset)
		   (when (= (incf offset) limit) (return))
		   (setq index
		     (,@(if element-to-fixnum
			    `(,element-to-fixnum)
			  '(funcall key-fn))
			(the ,element-type (,key-accessor key offset))))
		   (do ((subnode (trie-node-subnode node)
				 (trie-node-next subnode))
			(prev nil subnode))
		       ((or (null subnode) (= index (trie-node-key subnode)))
			(when (null subnode)
			  (setq subnode (make-trie-node index))
			  (if prev
			      (setf (trie-node-next prev) subnode)
			    (setf (trie-node-subnode node) subnode)))
			(setq node subnode)))))))))
     (defun ,mapper (fn trie)
       (declare (function fn))
       (let (,@(unless fixnum-to-element
		 '((inverse-key-fn (trie-inverse-key-fn trie))))
	     (buffer (make-array 20 :element-type ',element-type :fill-pointer
				 0 :adjustable t)))
	 (declare (type (array ,element-type (*)) buffer)
		  ,@(unless fixnum-to-element '((function inverse-key-fn))))
	 (when (trie-root-value trie)
	   (funcall fn (copy-seq buffer) (trie-root-value trie)))
	 (labels ((recurse (node)
		    (when (trie-node-value node)
		      (funcall fn (copy-seq buffer) (trie-node-value node)))
		    (do ((subnode (trie-node-subnode node)
				  (trie-node-next subnode)))
			((null subnode))
		      (vector-push-extend
		       (,@(if fixnum-to-element
			      `(,fixnum-to-element)
			    '(funcall inverse-key-fn))
			  (trie-node-key subnode))
		       buffer)
		      (recurse subnode)
		      (vector-pop buffer))))
	   (let ((root-subnodes (trie-root-subnodes trie)))
	     (dotimes (i (svb-pointer root-subnodes))
	       (let ((subnode (svbref root-subnodes i)))
		 (when subnode
		   (vector-push-extend
		    (,@(if fixnum-to-element
			   `(,fixnum-to-element)
			 '(funcall inverse-key-fn))
		       (trie-node-key subnode))
		    buffer)
		   (recurse subnode)
		   (vector-pop buffer))))))))))


;;;; String tries

(define-trie string-trie
    :element-type character
    :element-to-fixnum char-code :fixnum-to-element code-char
    :accessor st-get :mapper map-st :walker walk-string)


;;;; Simple vector tries

(define-trie simple-vector-trie :accessor svt-get :mapper map-svt)

;;;; Trie sorting

(define-list-sorter sort-trie-node
    :key trie-node-key :next trie-node-next :order < :key-type fixnum)

(defun sort-trie (trie)
  (labels ((sort-subnodes (node)
	     (setf (trie-node-subnode node)
	       (sort-trie-node (trie-node-subnode node)))
	     (do ((subnode (trie-node-subnode node) (trie-node-next subnode)))
		 ((null subnode))
	       (sort-subnodes subnode))))
    (let ((root-subnodes (trie-root-subnodes trie)))
      (dotimes (i (svb-pointer root-subnodes))
	(let ((subnode (svbref root-subnodes i)))
	  (when subnode
	    (sort-subnodes subnode)))))))

;;;; Debugging
#|


(defun test-string-trie (&optional (n 100))
  (let ((strings '())
	(trie (make-string-trie)))
    (do-external-symbols (sym :lisp)
      (push (string-downcase sym) strings))
    (time (dotimes (j n)
	    (do ((tail strings (cdr tail))
		 (i 0 (1+ i)))
		((null tail))
	      (declare (fixnum i))
	      (setf (st-get (car tail) trie) i))
	    (do ((tail strings (cdr tail))
		 (i 0 (1+ i)))
		((null tail))
	      (declare (fixnum i))
	      (unless (= (st-get (car tail) trie) i)
		(error "Incorrect value: ~s at key: ~s"
		       (st-get (car tail) trie) (car tail))))))))


|#
	  
