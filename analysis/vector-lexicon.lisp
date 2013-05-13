;;;-*- Package: VECTOR-LEXICON; Syntax: Common-Lisp; Mode: Lisp -*-

;;;; Copyright (c) 1992 by Xerox Corporation.
;;;; All rights reserved.

#|(cl:defpackage :vector-lexicon
  (:use :common-lisp :cl-extensions :string-resource :cons-resource
	:sv-resource :svb :ssb :skip-list :binary-io :trie :tdb)
  (:export vector-lexicon read-vector-lexicon write-vector-lexicon)
  (:shadow index))|#

(cl:in-package :vector-lexicon)

;;;; The lexicon is represented by three parallel vectors.  This is equivalent
;;;; to a single vector of three element structs, but requires a bit less
;;;; storage.  One vector contains surface forms, one contains ambiguity class
;;;; numbers and the third contains stems.  Stems must always also be surface
;;;; forms, and can thus be referred to by index to share storage.

;;;; As a further storage optimization, when all the stems of a given surface
;;;; form are the same (the common case), the index of that stem is stored
;;;; directly in the stem vector.  Otherwise a vector with an entry for each
;;;; tag (in canonical order) is found in the stem vector.

;;;; A mappings between ambiguity classes (sets of tags) and their numbers is
;;;; also maintained.

(defclass vector-lexicon (lexicon)
  ((vl-pathname :accessor vl-pathname :initarg :lexicon-pathname)
   (vl-surface :accessor vl-surface :type svb :initarg :surface)
   (vl-classes :accessor vl-classes :type svb :initarg :classes)
   (vl-stems :accessor vl-stems :type svb :initarg :stems)
   (vl-class-tags :accessor vl-class-tags :type simple-vector :initform #())))

(defmacro vl-length (vl) `(svb-pointer (vl-surface ,vl)))



;;;; The lexicon reads itself when created.  It will read itself again when
;;;; reinitialized (with REINITIALIZE-INSTANCE).

(defmethod initialize-instance ((lexicon vector-lexicon) &rest rest
				&key (lexicon-size 0) &allow-other-keys)
  ;;; :LEXICON-SIZE initarg provides a hint for number of surface forms
  (let (#+excl(generations (sys:gsgc-parameter :generation-spread)))
    #+excl(setf (sys:gsgc-parameter :generation-spread) 1)
    (unwind-protect 
	(apply #'call-next-method lexicon
	       :surface (make-svb lexicon-size)
	       :classes (make-svb lexicon-size)
	       :stems (make-svb lexicon-size)
	       rest)
      #+excl(setf (sys:gsgc-parameter :generation-spread) generations))))

(defmethod reinitialize-instance :before ((lexicon vector-lexicon)
					  &key &allow-other-keys)
  ;;; reset lexicon to empty & reclaim storage from resources
  (let ((surface (vl-surface lexicon))
	(stems (vl-stems lexicon)))
    (dotimes (i (vl-length lexicon))
      (free-string (svbref surface i))
      (setf (svbref surface i) nil)
      (let ((stem (svbref stems i)))
	(unless (typep stem 'fixnum)
	  (free-sv stem)
	  (setf (svbref stems i) nil))))
    (setf (svb-pointer surface) 0)
    (setf (svb-pointer stems) 0)
    (setf (svb-pointer (vl-classes lexicon)) 0)
    (setf (vl-class-tags lexicon) #())))

(defmethod shared-initialize :after ((lexicon vector-lexicon) slots
				      &key &allow-other-keys)
  (declare (ignore slots))		; (re)read from file
  (with-open-file (stream (vl-pathname lexicon))
    (format *error-output* "~&;;; Reading lexicon from ~A ...~%"
	    (namestring stream))
    (read-vector-lexicon lexicon stream)
    (format *error-output* "~&;;; Done reading lexicon.~%")))




;;;; Implement lexicon protocol

(defmethod lexicon-classes ((lexicon vector-lexicon))
  (concatenate 'vector (vl-class-tags lexicon) (call-next-method)))

(defmethod lexicon-lookup (token (lexicon vector-lexicon))
  (let* ((surface (vl-surface lexicon))
	 (buffer (svb-buffer surface)) (pointer (svb-pointer surface))
	 (index (or (binary-search token buffer pointer)
		    ;; try exact case first, then lower
		    (let ((copy (nstring-downcase (simple-string-copy token))))
		      (prog1 (binary-search copy buffer pointer)
			(free-string copy))))))
    (if index
	(let* ((stems (svbref (vl-stems lexicon) index))
	       (class (svref (vl-class-tags lexicon)
			     (svbref (vl-classes lexicon) index)))
	       (length (length class)))
	  (declare (simple-vector class) (fixnum length))
	  (values
	   (if (typep stems 'fixnum)
	       (simple-string-copy (svbref surface stems))
	     (let ((stem-vector (alloc-sv length)))
	       (dotimes (i length stem-vector)
		 (setf (svref stem-vector i)
		   (simple-string-copy (svbref surface (svref stems i)))))))
	   (sv-copy class)))
      (call-next-method))))

(defun binary-search (key vector &optional (end (length vector)))
  (declare (simple-string key) (simple-vector vector) (fixnum end))
  (let ((low 0)
	(high (1- end)))
    (declare (fixnum low high) #.*highly-optimized*)
    (loop (when (< high low)
	    (return nil))
	  (let* ((mid (ash (the fixnum (+ low high)) -1))
		 (mid-key (aref vector mid))
		 (order (simple-string-order key mid-key)))
	    (declare (fixnum mid))
	    (cond ((null order)
		   (setq low (1+ mid)))
		  ((eq order :equal)
		   (return mid))
		  (t (setq high (1- mid))))))))


;;;; class numbering code

(defmacro make-keyword (string)
  (once-only (string)			; mustn't retain a pointer to STRING
    `(or (find-symbol ,string *the-keyword-package*)
	 (intern (copy-seq ,string) *the-keyword-package*))))

(defun make-class-numbers ()
  (let ((tag-numbers (make-string-trie))
	(tag-names nil))
    (make-simple-vector-trie
     :key-fn
     #'(lambda (tag)
	 (let ((name (symbol-name tag)))
	   (or (st-get name tag-numbers)
	       (setf (st-get name tag-numbers)
		 (incf (st-get "" tag-numbers -1))))))
     :inverse-key-fn
     #'(lambda (number)
	 (unless tag-names
	   ;; compute TAG-NAMES lazily
	   (setq tag-names (make-array (1+ (st-get "" tag-numbers))))
	   (map-st #'(lambda (name number)
		       (unless (zerop (length name))
			 (setf (svref tag-names number)
			   (intern name *the-keyword-package*))))
		   tag-numbers))
	 (svref tag-names number)))))

(defun get-class-number (tags class-numbers)
  ;; Tags is an svb of tag numbers
  (or (svt-get (svb-buffer tags) class-numbers nil (svb-pointer tags))
      (setf (svt-get (svb-buffer tags) class-numbers nil (svb-pointer tags))
	(incf (svt-get '#() class-numbers -1)))))

(defun invert-class-numbers (class-numbers)
  (let ((vector (make-array (1+ (svt-get '#() class-numbers)))))
    (map-svt #'(lambda (tags class-number)
		 (unless (zerop (length tags))
		   (setf (svref vector class-number) tags)))
	     class-numbers)
    vector))



(defun read-vector-lexicon (lexicon stream &optional (quiet t))
  (let* ((pending (make-skip-list #'simple-string-order))
	 (class-numbers (make-class-numbers)))

    (labels
	((insert-entry (surface tags stems)
	   ;; All the information about SURFACE has been gathered--TAGS &
	   ;; STEMS.  Updates PENDING and LEXICON.
	   (let ((class-number (get-class-number tags class-numbers))
		 (surface-number (svb-pointer (vl-surface lexicon))))
	     (svb-push-extend surface (vl-surface lexicon))
	     (svb-push-extend class-number (vl-classes lexicon))
	     (cond
	      ((all-stems-identical-p stems) ; common case
	       (svb-push-extend nil (vl-stems lexicon))
	       (handle-fixup surface (svbref stems 0) surface-number))
	      (t (svb-push-extend (alloc-sv (svb-pointer stems))
				  (vl-stems lexicon))
		 (dotimes (i (svb-pointer stems)) ; different stems per tag
		   (handle-fixup surface (svbref stems i)
				 (%cons surface-number i)))))))
	
	 ;; Fixups are references to stems.  A fixup is enqueued for every
	 ;; stem encountered.  Fixups are dequeued when the stem is encountered
	 ;; as a surface form, i.e., when its index is known.

	 (handle-fixup (surface stem fixup)
	   (if (simple-string-order stem surface)
	       (perform-fixups stem (%list fixup))
	     (enqueue-fixup stem fixup)))

	 (enqueue-fixup (stem fixup)
	   (let* ((fixups (skip-list-get stem pending))
		  (stem (if fixups stem (simple-string-copy stem))))
	     (setf (skip-list-get stem pending) (%cons fixup fixups))))

	 (dequeue-fixups (surface)
	   (loop (when (skip-list-empty-p pending) (return))
	     (if (simple-string-order (skip-list-top pending) surface)
		 (multiple-value-bind (stem fixups) (skip-list-pop pending)
		   (perform-fixups stem fixups)
		   (free-string stem))
	       (return))))
    
	 (perform-fixups (stem fixups)
	   (let* ((length (vl-length lexicon))
		  (last (1- length))
		  (surface (svb-buffer (vl-surface lexicon)))
		  (index
		   (if (eq (simple-string-order stem (svref surface last))
			   :equal)
		       last
		     (binary-search stem surface length))))
	     (unless index
	       (cerror "~*Continue for debugging.  Lexicon damaged."
		       "Stem ~S not also a surface form." stem)
	       (return-from perform-fixups))
	     (loop (unless fixups (return))
	       (let ((fixup (%pop fixups)))
		 (etypecase fixup
		   (fixnum (setf (svbref (vl-stems lexicon) fixup) index))
		   (cons (destructuring-bind (n . i) fixup
			   (setf (svref (svbref (vl-stems lexicon) n) i) index)
			   (free-cell fixup))))))))

	 )				; labels functions

      
      ;;; now we're ready to read the lexicon from the file

      (let ((ssb (make-ssb))
	    (tags (make-svb))
	    (stems (make-svb))
	    (last-surface ""))
	(flet ((collect-entry ()
		 (unless (zerop (svb-pointer tags)) ; note complete class
		   (insert-entry last-surface tags stems)
		   (dequeue-fixups last-surface))
		 (do ((limit (svb-pointer tags))
		      (i 0 (1+ i)))
		     ((= i limit)
		      (setf (svb-pointer tags) 0)
		      (setf (svb-pointer stems) 0))
		   (declare (fixnum i limit))
		   (free-string (svbref stems i)))))
	  (loop 
	    (let* ((surface (or (read-until-space stream ssb)
				(return)))
		   (tag-string (nstring-upcase (read-until-space stream ssb)))
		   (tag (make-keyword tag-string))
		   (stem (read-until-newline stream ssb))
		   (surface-order (simple-string-order last-surface surface)))
	      (free-string tag-string)
	      (cond
	       ((eq surface-order :equal) ; repeat surface form
		(let* ((tag-order	; examine tag ordering
			(if (> (svb-pointer tags) 0)
			    (simple-string-order
			     (symbol-name
			      (svbref tags (1- (svb-pointer tags))))
			     (symbol-name tag))
			  t)))
		  (cond
		   ((eq tag-order :equal) ; repeat of last tag seen
		    (unless quiet
		      (warn "Multiple stems for ~S with tag ~S.  Ignoring ~S."
			    surface tag stem))
		    (free-string stem))
		   (tag-order		; a new tag
		    (svb-push-extend tag tags) (svb-push-extend stem stems))
		   (t (error "Tag ~S out of order for ~S" tag surface))))
		(free-string surface))
	       (surface-order
		(collect-entry)		; new surface form
		(setq last-surface surface)
		(svb-push-extend tag tags) (svb-push-extend stem stems))
	       (t (error "Surface forms out of order: ~S and ~S"
			 last-surface surface)))))
	  (collect-entry)
	  (unless (skip-list-empty-p pending)
	    (error "Stem ~S has no surface form." (skip-list-pop pending)))
	  (setf (vl-class-tags lexicon) (invert-class-numbers class-numbers))))

      )))				; labels, let, defun


(defun read-until-space (stream ssb)
;;; Returns the next sequence of space or tab delimited characters from STREAM.
;;; An error is signalled if a newline is seen.
  (declare #.*highly-optimized*)
  (setf (ssb-pointer ssb) 0)
  (let ((non-space-seen-p nil))
    (loop (let ((char (fast-read-file-char stream)))
	    (when (eq char :eof) (return))
	    (case char
	      ((#\space #\tab)
	       (when non-space-seen-p
		 (return
		   (simple-string-copy (ssb-buffer ssb) (ssb-pointer ssb)))))
	      (#\newline (error "Unexpected newline at character ~D."
				(file-position stream)))
	      (t (ssb-push-extend char ssb)
		 (setq non-space-seen-p t)))))))

(defun read-until-newline (stream ssb)
;;; Returns the remainder of the line with spaces and tabs removed.
  (declare #.*highly-optimized*)
  (setf (ssb-pointer ssb) 0)
  (loop (let ((char (fast-read-file-char stream)))
	  (when (eq char :eof) (return))
	  (case char
	    ((#\space #\tab))
	    (#\newline
	     (return (simple-string-copy (ssb-buffer ssb) (ssb-pointer ssb))))
	    (t (ssb-push-extend char ssb))))))

(defun all-stems-identical-p (stems)
  (do ((first-stem (svbref stems 0))
       (limit (svb-pointer stems))
       (i 1 (1+ i)))
      ((= i limit) t)
    (declare (fixnum i limit))
    (unless (eq (simple-string-order first-stem (svbref stems i)) :equal)
      (return nil))))




(defmethod write-vector-lexicon ((lexicon vector-lexicon) pathname)
  (with-open-file (stream pathname :direction :output :if-exists :new-version)
    (dotimes (i (vl-length lexicon))
      (let ((class (svref (vl-class-tags lexicon)
			  (svbref (vl-classes lexicon) i)))
	    (stems (svbref (vl-stems lexicon) i))
	    (*print-case* :downcase))
	(dotimes (j (length class))
	  (format stream "~A ~A ~A~%"
		  (svbref (vl-surface lexicon) i)
		  (svref class j)
		  (svbref (vl-surface lexicon)
			  (if (typep stems 'fixnum)
			      stems
			    (svref stems j)))))))))
