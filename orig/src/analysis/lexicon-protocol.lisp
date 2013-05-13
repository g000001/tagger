;;;-*- Package: TDB; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1992 by Xerox Corporation.

(cl:in-package :tdb)

(eval-when (compile eval load)
  (use-package :sv-resource)
  (export '(lexicon
	    lexicon-tags lexicon-classes lexicon-open-class
	    lexicon-lookup class-order
	    lexicon-filter ts-lexicon
	    *the-keyword-package*
	    *tokenizer-class* re-read-lexicon)))


(defvar *the-keyword-package* (find-package :keyword))

(defclass lexicon () 
  ((lexicon-open-class :writer (setf lexicon-open-class))))

(defmethod initialize-instance :before ((lexicon lexicon)
					&key lexicon-open-class
					&allow-other-keys)
  (setf (lexicon-open-class lexicon)
    (sort (map 'simple-vector #'identity lexicon-open-class) #'string<)))

;;;; The Lexicon Protocol

;;; Returns (1) a vector of stems (strings) or a single string if all stems are
;;; the same ; and (2) a parallel sorted vector of tags (keywords).
;;; All vectors and strings are from resources.
(defgeneric lexicon-lookup (token lexicon))
(defmethod lexicon-lookup (token (lexicon lexicon))
  (values (simple-string-copy token) (sv-copy (lexicon-open-class lexicon))))

;;;; Returns a vector of classes.  Each class is a sorted vector of tags.
(defgeneric lexicon-classes (lexicon))
(defmethod lexicon-classes ((lexicon lexicon))
  (with-slots (lexicon-open-class) lexicon
    (vector lexicon-open-class)))

;;; Returns a vector of open class tags (from the vector resource).
(defgeneric lexicon-open-class (lexicon))
(defmethod lexicon-open-class ((lexicon lexicon))
  (with-slots (lexicon-open-class) lexicon
    (sv-copy lexicon-open-class)))



;;;; Returns a vector of tags.
(defun lexicon-tags (lexicon)
  (let* ((classes (lexicon-classes lexicon))
	 (table (make-hash-table :size (length classes))))
    (dotimes (i (length classes))
      (let ((class (svref classes i)))
	(dotimes (j (length class))
	  (setf (gethash (svref class j) table) t))))
    (let ((result (make-array (hash-table-count table)))
	  (index -1))
      (maphash #'(lambda (tag ignore)
		   (declare (ignore ignore))
		   (setf (svref result (incf index)) tag))
	       table)
      result)))

(defun class-order (class1 class2)
  (let ((l1 (length class1))
	(l2 (length class2)))
    (if (= l1 l2)
	(dotimes (i l1 :equal)
	  (when (string/= (svref class1 i) (svref class2 i))
	    (return (string< (svref class1 i) (svref class2 i)))))
      (< l1 l2))))
    


;;;; LEXICON-FILTER: splices a lexicon into a token stream.  The tokenizer
;;;; class :WORD is looked up in the lexicon, others are passed on.  Makes the
;;;; TS masquerade as a lexicon.  Most clients access the lexicon through this.

(defclass lexicon-filter (token-filter)
  ((lexicon :accessor ts-lexicon :initarg :lexicon)))

(defmethod lexicon-classes ((ts lexicon-filter))
  (remove-duplicates 
   (concatenate 'simple-vector
     (lexicon-classes (ts-lexicon ts))
     (map 'simple-vector #'vector
	  (remove :word (funcall (get-qua ts-types ts lexicon-filter) ts))))
   :test #'equalp))

(defmethod lexicon-lookup (token (ts lexicon-filter))
  (lexicon-lookup token (ts-lexicon ts)))
(defmethod lexicon-open-class ((ts lexicon-filter))
  (lexicon-open-class (ts-lexicon ts)))

(defmethod next-token ((ts lexicon-filter))
  ;; Returns three values (stems type surface).  Stems may be a string or a
  ;; vector of strings.  Type is a symbol and surface is a string.  The strings
  ;; are all reclaimable
  (multiple-value-bind (token type) (call-next-method)
    (when token
      (if (eq type :word)
	  (multiple-value-bind (stems type)
	      (lexicon-lookup token (ts-lexicon ts))
	    (values stems type token))
	(values token (%vector type) (simple-string-copy token))))))

;;;; *tokenizer-class* names a class suitable for embedding in 
;;;; tagger tokenerizer classes.  These typically mixin basic-tag-ts
;;;; and fsa-tokenizer with some number of intervening filters

(defvar *tokenizer-class*)

(defun re-read-lexicon ()
  (reinitialize-instance (ts-lexicon (make-instance *tokenizer-class*))))
