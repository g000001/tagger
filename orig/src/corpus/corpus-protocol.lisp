;;;-*- Package: TDB; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1989 -- 1994 by Xerox Corporation

(cl:in-package :tdb)

(eval-when (compile eval load)
  (use-package :cl-extensions))

(eval-when (compile eval load)
  (export '(corpus initialize-corpus open-corpus update-corpus close-corpus
	    open-doc close-doc byte28
	    doc-title display-doc
	    match-doc-titles do-titles
	    map-docs do-docs corpus-id-limit)))


;;;; corpus protocol

;;;; A corpus is a collection of documents identified by a unique integer id.

(defclass corpus () ())

;;; essential methods

;;; Function is called with arg id for every valid id in corpus
;;; The iteration can be bounded by specifying start and/or end ids.
(defgeneric map-docs (function corpus &key start end))


;;; Returns three values STREAM, START, and END.  STREAM is a character stream
;;; containing the contents of document ID between START and END.
;;; The stream returned from open-doc should be reclaimed by calling close-doc.
(defgeneric open-doc (id corpus))

;;; optional methods

;;; Must be called once before any calls to open-corpus
(defgeneric initialize-corpus (corpus))
(defmethod initialize-corpus ((corpus corpus)) corpus)

;;; Must be called before any other methods (aside from initialize-corpus) are
;;; evoked on CORPUS.
(defgeneric open-corpus (corpus))
(defmethod open-corpus ((corpus corpus)) corpus)

;;; Called to augment an existing corpus 
(defgeneric update-corpus (corpus))
(defmethod update-corpus ((corpus corpus)))

;;; Called to reclaim CORPUS.  No further operations may be performed on CORPUS
(defgeneric close-corpus (corpus))
(defmethod close-corpus ((corpus corpus)))

;;; An integer guaranteed to be one greater than the largest valid id.
;;; In general ids are not guaranteed to be dense in the range 0 to
;;; (corpus-id-limit CORPUS).   Use map-docs to iterate over valid ids.
(defgeneric corpus-id-limit (corpus))
(defmethod corpus-id-limit ((corpus corpus))
  (let ((limit 0))
    (map-docs #'(lambda (id) (setq limit id)) corpus)
    (1+ limit)))

;;; Called to reclaim a stream returned by open-doc
(defgeneric close-doc (stream id corpus))
(defmethod close-doc (stream id (corpus corpus))
  (declare (ignore stream id))
  nil)


;;; Displays the text of ID on DEVICE
(defgeneric display-doc (id corpus device))
(defmethod display-doc (id (corpus corpus) (device stream))
  (multiple-value-bind (doc-stream start-pos end-pos)
      (open-doc id corpus)
    (unwind-protect
	(progn (when start-pos
		 (file-position doc-stream start-pos))
	       (dotimes (i (- (or end-pos (file-length doc-stream))
			      (or start-pos (file-position doc-stream))))
		 (write-char (read-char doc-stream) device)))
      (close-doc doc-stream id corpus))))

;;; Returns a string which is the title for ID.  This string is from the string
;;; resource and may be reclaimed.
(defgeneric doc-title (id corpus))
(defmethod doc-title (id (corpus corpus))
  (format nil "~D" id))

;;; Calls FUNCTION on the IDs of docs in CORPUS which are titled TITLE.
(defgeneric match-doc-titles (function title corpus))

;;; The default implementation just does a linear search.
(defmethod match-doc-titles (function title (corpus corpus))
  (map-docs #'(lambda (id)
		(when (string= title (doc-title id corpus))
		  (funcall function id)))
	    corpus))




;;; some handy macros

;;; Binds ID to each valid id in CORPUS in sequence from START to END
(defmacro do-docs ((id corpus &key start end) &body body)
  `(block do-docs
     (map-docs #'(lambda (,id) (declare (type byte28 ,id)) ,@body)
	       ,corpus :start ,start :end ,end)))

;;; Binds ID to each valid id in CORPUS and TITLE to the corresponding title in
;;; sequence
(defmacro do-titles ((id title corpus) &body body)
  `(block do-titles
     (match-doc-titles #'(lambda (,id) (declare (type byte28 ,id)) ,@body)
		       ,title ,corpus)))
