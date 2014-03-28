;;;-*- Package: HMM; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1991-1994 by Xerox Corporation

#|(cl:defpackage :hmm
  (:use :common-lisp :cl-extensions :cons-resource :float-vector :binary-io)
  (:shadow common-lisp:pi)
  (:export hmm make-hmm hmm-n hmm-m hmm-pi hmm-a hmm-b
	   hmm-est make-hmm-est hmm-maximal-path
	   hmm-train hmm-train-multiple
	   hmm-read-file hmm-write-file hmm-read hmm-write
	   make-sfm sfmref))|#


(cl:in-package :hmm)

(defstruct (hmm
	    (:copier nil)
	    (:print-function
	     (lambda (hmm stream depth)
	       (declare (ignore depth))
	       #-cmu
	       (print-unreadable-object (hmm stream :type t :identity t))
	       #+cmu (format stream "#<hmm ~D,~D>" (hmm-m hmm) (hmm-n hmm)))))
  ;; n= num states
  (n 0 :type fixnum)
  ;; m= size sigma;  Sigma is assumed to be the integers s, 0<=s<m
  (m 0 :type fixnum)
  ;; Initial probabilities
  (pi (make-sfv 0) :type single-float-vector)
  ;; Transition probabilities; A single-float matrix
  (a '#() :type simple-vector)
  ;; Symbol probabilities; A single-float matrix
  (b '#() :type simple-vector))

(defun make-sfm (n m &optional initial-element)
  (declare (fixnum n m))
  (let ((array (make-array n)))
    (dotimes (i n array)
      (setf (svref array i) (make-sfv m initial-element)))))

(defmacro sfmref (array index-1 index-2)
  `(sfvref (svref ,array ,index-1) ,index-2))


(defun fill-sfm (matrix &optional (value 0.0f0))
  (declare (simple-vector matrix))
  (dotimes (i (length matrix))
    (fill-sfv (svref matrix i) value)))

(defun copy-sfm (m1 m2)
  (declare (simple-vector m1 m2))
  (let ((n (length m1)))
    (declare (fixnum n))
    (assert (= n (length m2)))
    (dotimes (i n m2)
      (declare (fixnum i))
      (copy-sfv (svref m1 i) (svref m2 i)))))

(defun sfm-add (m1 m2)
  (declare (simple-vector m1 m2))
  (let ((n (length m1)))
    (declare (fixnum n))
    (assert (= n (length m2)))
    (dotimes (i n)
      (declare (fixnum i))
      (setf (svref m1 i) (sfv-add (svref m1 i) (svref m2 i))))
    m1))

(defun sfm-div (m const)
  (declare (simple-vector m))
  (let ((n (length m)))
    (declare (fixnum n))
    (dotimes (i n m)
      (declare (fixnum i))
      (setf (svref m i) (sfv-div (svref m i) const)))))

(defun sfm-read (stream)
  (let* ((length (int29-read stream))
	 (sfm (make-array length)))
    (declare (fixnum length))
    (dotimes (i length)
      (setf (svref sfm i) (sfv-read stream)))
    sfm))

(defun sfm-write (sfm stream)
  (declare (simple-vector sfm))
  (let ((length (length sfm)))
    (declare (fixnum length))
    (int29-write length stream)
    (dotimes (i length)
      (sfv-write (svref sfm i) stream))
    sfm))


;;; copier

(defun copy-hmm (hmm &optional old-hmm)
  (if old-hmm
      (let ((n (hmm-n hmm))
	    (m (hmm-m hmm)))
	(assert (and (= n (hmm-n old-hmm)) (= m (hmm-m old-hmm))))
	(setf (hmm-pi old-hmm) (copy-sfv (hmm-pi hmm) (hmm-pi old-hmm)))
	(setf (hmm-a old-hmm) (copy-sfm (hmm-a hmm) (hmm-a old-hmm)))
	(setf (hmm-b old-hmm) (copy-sfm (hmm-b hmm) (hmm-b old-hmm)))
	old-hmm)
      (make-hmm :n (hmm-n hmm) :m (hmm-m hmm) :pi (copy-seq (hmm-pi hmm))
		:a (map 'vector #'copy-seq (hmm-a hmm))
		:b (map 'vector #'copy-seq (hmm-b hmm)))))

(defun hmm-add (hmm-1 hmm-2)
  (setf (hmm-pi hmm-1) (sfv-add (hmm-pi hmm-1) (hmm-pi hmm-2)))
  (setf (hmm-a hmm-1) (sfm-add (hmm-a hmm-1) (hmm-a hmm-2)))
  (setf (hmm-b hmm-1) (sfm-add (hmm-b hmm-1) (hmm-b hmm-2)))
  hmm-1)



(defun hmm-div (hmm const)
  (setf (hmm-pi hmm) (sfv-div (hmm-pi hmm) const))
  (setf (hmm-a hmm) (sfm-div (hmm-a hmm) const))
  (setf (hmm-b hmm) (sfm-div (hmm-b hmm) const))
  hmm)

;;; Hmm I/O

(defun hmm-write-file (hmm pathname &optional quiet)
  (unless quiet (format *error-output* "~&;;; Writing HMM to ~A~%" pathname))
  (with-open-byte8-stream (stream pathname
				  :direction :output :if-exists :new-version)
      (hmm-write hmm stream)))

(defun hmm-read-file (pathname &optional quiet)
  (unless quiet (format *error-output* "~&;;; Reading HMM from ~A~%" pathname))
  (with-open-byte8-stream (stream pathname)
      (hmm-read stream)))

(defun hmm-write (hmm stream)
  (int29-write (hmm-n hmm) stream)
  (int29-write (hmm-m hmm) stream)
  (sfv-write (hmm-pi hmm) stream)
  (sfm-write (hmm-a hmm) stream)
  (sfm-write (hmm-b hmm) stream)
  hmm)

(defun hmm-read (stream)
  (let* ((n (int29-read stream))
	 (m (int29-read stream))
	 (pi (sfv-read stream))
	 (a (sfm-read stream))
	 (b (sfm-read stream)))
    (make-hmm :n n :m m :pi pi :a a :b b)))

;;; Path of maximal probability

(defconstant +negative-infinity+
  #+allegro excl::*negative-infinity-single*
  #+lucid system:float-negative-infinity
  #+cmu extensions:single-float-negative-infinity
  #+mcl (ccl::make-float-from-fixnums 0 0 2047 -1)
  #+ccl -1E++0
  #+sbcl sb-ext:single-float-negative-infinity
  #-(or ccl allegro lucid cmu mcl sbcl)
  (error "Don't know how to represent -infinity."))

(defstruct (hmm-est
	    (:constructor %make-hmm-est)
	    (:print-function
	     (lambda (hmm-est stream depth)
	       (declare (ignore depth))
	       #-cmu
	       (print-unreadable-object (hmm-est stream :type t :identity t))
	       #+cmu (format stream "#<hmm-est ~D,~D>"
			     (hmm-est-m hmm-est) (hmm-est-n hmm-est)))))
  (n 0 :type fixnum)
  (m 0 :type fixnum)
  ;; Log initial probabilities
  (log-pi (make-sfv 0) :type single-float-vector)
  ;; Log transition probabilities; a single-float-matrix
  (log-a '#() :type simple-vector)
  ;; Log symbol probabilities; a single-float-matrix
  (log-b '#() :type simple-vector)
  (probs (make-sfv 0) :type single-float-vector)
  (maxes (make-sfv 0) :type single-float-vector)
  (resource nil))

(defun next-array (hmm-est)
  (let ((next (hmm-est-resource hmm-est)))
    (if next
	(setf (hmm-est-resource hmm-est) (svref next 0))
	(setq next (make-array (hmm-est-n hmm-est))))
    next))

(defun return-array (array hmm-est)
  (setf (svref array 0) (hmm-est-resource hmm-est))
  (setf (hmm-est-resource hmm-est) array))


(defun make-hmm-est (hmm)
  (let ((n (hmm-n hmm))
	(m (hmm-m hmm)))
    (%make-hmm-est :n n :m m
		   :log-pi (log-vector (hmm-pi hmm))
		   :log-b (log-matrix-transposed (hmm-b hmm))
		   :log-a (log-matrix-transposed (hmm-a hmm))
		   :probs (make-sfv n)
		   :maxes (make-sfv n))))


(defun log-vector (vector)
  (declare (type single-float-vector vector))
  (let* ((n (length vector))
	 (result (make-sfv n)))
    (declare (fixnum n))
    (dotimes (i n result)
      (declare (fixnum i))
      (let ((elt (sfvref vector i)))
	(declare (type (single-float 0.0f0 *) elt))
	(setf (sfvref result i)
	  (cond ((zerop elt) +negative-infinity+)
		((> elt 1.0f0) 0.0f0)	;Due to rounding error?
		(t (log elt))))))))

(defun log-matrix-transposed (matrix)
  (declare (simple-vector matrix))
  (let* ((m (length (the single-float-vector (svref matrix 0))))
	 (n (length matrix))
	 (result (make-sfm m n)))
    (declare (fixnum m n))
    (dotimes (i m result)
      (declare (fixnum i))
      (dotimes (j n)
	(declare (fixnum j))
	(let ((elt (sfmref matrix j i)))
	  (setf (sfmref result i j)
		(if (zerop elt)
		    +negative-infinity+
		    (log elt))))))))

(defun hmm-maximal-path (sequence hmm-est path
			 &optional (limit (length sequence)) maxp)
  (declare (simple-vector sequence path) (fixnum limit))
  (let ((probs (hmm-est-probs hmm-est))
	(maxes (hmm-est-maxes hmm-est))
	(trail ()))
    (declare (type single-float-vector probs maxes))
    (when (< limit 1)
      (return-from hmm-maximal-path))
    (initialize-path probs sequence hmm-est)
    (do ((time 1 (1+ time)))
	((= time limit))
      (declare (fixnum time))
      (let ((indices (next-array hmm-est)))
	(compute-maxes-at-sym
	 (svref sequence time) indices probs maxes hmm-est)
	;; recall max indices to later recover path
	(%push indices trail))
      ;; switch the roles of probs and maxes
      (rotatef probs maxes))
    (finalize-path probs trail path hmm-est limit maxp)))

(defun compute-maxes-at-sym (sym indices probs maxes hmm-est)
  (declare #.tdb:*highly-optimized*)
  (let ((log-b-at-sym (svref (hmm-est-log-b hmm-est) sym))
	(negative-infinity +negative-infinity+)) ; put in register
    (declare (single-float negative-infinity))
    (dotimes (j (hmm-est-n hmm-est))
      (declare (fixnum j))
      (let ((b-prob (sfvref log-b-at-sym j)))
	(declare (single-float b-prob))
	(if (= b-prob negative-infinity)
	    (setf (sfvref maxes j) negative-infinity)
	    (let ((log-a-at-j (svref (hmm-est-log-a hmm-est) j))
		  (max negative-infinity)
		  (max-index 0))
	      (declare (single-float max) (fixnum max-index))
	      (dotimes (i (hmm-est-n hmm-est))
		(declare (fixnum i))
		(let ((prob (+ (sfvref probs i) (sfvref log-a-at-j i))))
		  (declare (single-float prob))
		  (when (> prob max)
		    (setq max prob max-index i))))
	      (setf (sfvref maxes j) (+ max b-prob))
	      (setf (svref indices j) max-index)))))))

(defun initialize-path (probs sequence hmm-est)
  (declare (type single-float-vector probs))
  (let ((log-b-at-first (svref (hmm-est-log-b hmm-est) (svref sequence 0)))
	(log-pi (hmm-est-log-pi hmm-est)))
    (dotimes (i (hmm-est-n hmm-est))
      (declare (fixnum i))
      (setf (sfvref probs i)
	    (+ (sfvref log-pi i) (sfvref log-b-at-first i))))))

(defun finalize-path (probs trail path hmm-est limit maxp)
  (declare (type single-float-vector probs) (fixnum limit))
  (let ((max +negative-infinity+)
	max-index)
    (declare (single-float max))
    (dotimes (i (the fixnum (length probs)))
      (declare (fixnum i))
      (let ((prob (sfvref probs i)))
	(declare (single-float prob))
	(when (> prob max)
	  (setq max prob max-index i))))
    (when max-index
      (setf (svref path (1- limit)) max-index)
      (do* ((i (- limit 2) (1- i))
	    (tail trail (cdr tail))
	    (indices (car tail) (car tail))
	    (last max-index))
	  ((< i 0))
	(declare (fixnum i))
	(let ((next (svref indices last)))
	  (declare (fixnum next))
	  (setf (svref path i) next)
	  (setq last next))))
    (loop (return-array (or (%pop trail) (return)) hmm-est))
    (when max-index
      (values path (and maxp max)))))


;;; Training

(defun hmm-train (sequence hmm &key (iterations 5) (debug nil))
  (let* ((n (hmm-n hmm))
	 (limit (length sequence))
	 (alpha (make-sfm limit n))
	 (beta (make-sfm limit n))
	 (c (make-sfv limit))
	 (a (hmm-a hmm))
	 (b (hmm-b hmm))
	 (a-prime (make-sfm n n))
	 (b-prime (make-sfm n (hmm-m hmm)))
	 (norms (make-sfv n)))
    (when (> limit 0)
      (dotimes (i iterations)
	(when debug
	  (format *error-output* "~&Interation: ~2d~%" i))
	(hmm-train-step sequence hmm alpha beta c a-prime b-prime norms)
	(when debug
	  (format *error-output* "~&Log likelihood: ~5f~%"
		  (compute-log-likelihood c)))
	(rotatef a a-prime)
	(rotatef b b-prime)
	(setf (hmm-a hmm) a)
	(setf (hmm-b hmm) b)))
    hmm))


(defun hmm-train-step (sequence hmm alpha beta c a-prime b-prime norms)
  (compute-alpha sequence alpha c hmm)
  (compute-beta sequence beta c hmm)
  (compute-a-prime sequence a-prime alpha beta c norms hmm)
  (compute-b-prime sequence b-prime alpha beta norms hmm)
  (compute-pi-prime sequence alpha beta hmm)
  hmm)

(defun compute-alpha (sequence alpha c hmm)
  (declare (simple-vector sequence) (type single-float-vector c))
  (let* ((limit (length sequence))
	 (n (hmm-n hmm))
	 (a (hmm-a hmm))
	 (b (hmm-b hmm)))
    (declare (fixnum limit n))
    (let ((pi (hmm-pi hmm))
	  (first (svref sequence 0)))
      (dotimes (i n)
	(declare (fixnum i))
	(let* ((pi-prob (sfvref pi i))
	       (b-prob (sfmref b i first)))
	  (declare (single-float pi-prob b-prob))
	  (setf (sfmref alpha 0 i) (* pi-prob b-prob)))))
    (compute-c c alpha n 0)
    (normalize-alpha alpha n 0 c)
    (do* ((prev 0 time)
	  (time 1 (1+ time)))
	 ((= time limit))
      (declare (fixnum time prev))
      (let ((sym (svref sequence time))
	    (alpha-at-prev (svref alpha prev))
	    (alpha-at-time (svref alpha time)))
	(dotimes (j n)
	  (declare (fixnum j))
	  (let ((b-prob (sfmref b j sym)))
	    (declare (single-float b-prob))
	    (if (zerop b-prob)
		(setf (sfvref alpha-at-time j) 0.0f0)
		(let ((sum 0.0f0))
		  (declare (single-float sum))
		  (dotimes (i n)
		    (declare (fixnum i))
		    (incf sum (* (sfmref a i j) (sfvref alpha-at-prev i))))
		  (setf (sfvref alpha-at-time j) (* b-prob sum)))))))
      (compute-c c alpha n time)
      (normalize-alpha alpha n time c))))

(defun compute-c (c alpha n time)
  (declare (fixnum n time) (type single-float-vector c))
  (let ((sum 0.0f0))
    (declare (single-float sum))
    (dotimes (i n)
      (declare (fixnum i))
      (incf sum (sfmref alpha time i)))
    (setf (sfvref c time) (/ 1.0f0 sum))
    c))

(defun compute-log-likelihood (c)
  (declare (type single-float-vector c))
  (let ((n (length c))
	(sum 0.0f0))
    (dotimes (i n)
      (declare (fixnum i))
      (incf sum (log (the (single-float 0.0f0 1.0f0) (sfvref c i)))))
    (- sum)))

(defun normalize-alpha (alpha n time c)
  (declare (fixnum n time) (type single-float-vector c))
  (let ((factor (sfvref c time)))
    (declare (single-float factor))
    (dotimes (i n)
      (declare (fixnum i))
      (setf (sfmref alpha time i)
	    (* factor (sfmref alpha time i))))))


(defun compute-beta (sequence beta c hmm)
  (declare (simple-vector sequence) (type single-float-vector c))
  (let ((last-index (1- (length sequence))))
    (declare (fixnum last-index))
    (fill-sfm beta)
    (fill-sfv (svref beta last-index) 1.0f0)
    (do ((a (hmm-a hmm))
	 (b (hmm-b hmm))
	 (prev last-index time)
	 (time (1- last-index) (1- time)))
	((< time 0))
      (declare (fixnum time prev))
      (let ((beta-at-time (svref beta time))
	    (beta-at-prev (svref beta prev))
	    (c-at-prev (sfvref c prev)))
	(declare (single-float c-at-prev))
	(dotimes (j (hmm-n hmm))
	  (declare (fixnum j))
	  (let ((prod (* (sfmref b j (svref sequence prev))
			 (sfvref beta-at-prev j))))
	    (declare (single-float prod))
	    (unless (zerop prod)
	      (dotimes (i (hmm-n hmm))
		(declare (fixnum i))
		(incf (sfvref beta-at-time i)
		      (* prod (sfmref a i j) c-at-prev))))))))))

(defun compute-a-prime (sequence a-prime alpha beta c norms hmm)
  (declare (simple-vector sequence))
  (fill-sfv norms)
  (fill-sfm a-prime)
  (do ((limit (length sequence))
       (a (hmm-a hmm))
       (b (hmm-b hmm))
       (time 0 next)
       (next 1 (1+ next)))
      ((= next limit))
    (declare (fixnum time limit next))
    (let ((alpha-at-time (svref alpha time))
	  (beta-at-time (svref beta time))
	  (beta-at-next (svref beta next)))
      (dotimes (j (hmm-n hmm))
	(declare (fixnum j))
	(incf (sfvref norms j)
	      (* (sfvref alpha-at-time j) (sfvref beta-at-time j)))
	(let ((prod (* (sfvref beta-at-next j)
		       (sfmref b j (svref sequence next)))))
	  (declare (single-float prod))
	  (unless (zerop prod)
	    (let ((c-at-next (sfvref c next)))
	      (declare (single-float c-at-next) #.tdb:*highly-optimized*)
	      (dotimes (i (hmm-n hmm))
		(declare (fixnum i))
		(incf (sfmref a-prime i j)
		      (* prod (sfvref alpha-at-time i)
			 (sfmref a i j) c-at-next)))))))))
  (normalize-a-prime a-prime norms hmm))

(defun normalize-a-prime (a-prime norms hmm)
  (dotimes (i (hmm-n hmm))
    (declare (fixnum i))
    (let ((norm (sfvref norms i))
	  (a-prime-at-i (svref a-prime i)))
      (declare (single-float norm))
      (if (zerop norm)
	  (let ((n-inverse (/ 1.0f0 (hmm-n hmm))))
	    (declare (single-float n-inverse))
	    (dotimes (j (hmm-n hmm))
	      (declare (fixnum j))
	      (setf (sfvref a-prime-at-i j) n-inverse)))
	(dotimes (j (hmm-n hmm))
	  (declare (fixnum j))
	  (setf (sfvref a-prime-at-i j)
	    (/ (sfvref a-prime-at-i j) norm)))))))
	
(defun compute-b-prime (sequence b-prime alpha beta norms hmm)
  (declare (simple-vector sequence))
  (fill-sfv norms)
  (fill-sfm b-prime)
  (dotimes (time (length sequence))
    (declare (fixnum time))
    (dotimes (j (hmm-n hmm))
      (declare (fixnum j))
      (let ((prod (* (sfmref alpha time j) (sfmref beta time j))))
	(declare (single-float prod))
	(unless (zerop prod)
	  (incf (sfvref norms j) prod)
	  (incf (sfmref b-prime j (svref sequence time)) prod)))))
  (dotimes (j (hmm-n hmm))
    (declare (fixnum j))
    (let ((norm (sfvref norms j))
	  (b-prime-at-j (svref b-prime j)))
      (declare (single-float norm))
      (if (zerop norm)
	  (let ((b-at-j (svref (hmm-b hmm) j)))
	    (dotimes (k (hmm-m hmm))
	      (declare (fixnum k))
	      (setf (sfvref b-prime-at-j k) (sfvref b-at-j k))))
	(dotimes (k (hmm-m hmm))
	  (declare (fixnum k))
	  (setf (sfvref b-prime-at-j k)
	    (/ (sfvref b-prime-at-j k) norm)))))))


(defun compute-pi-prime (sequence alpha beta hmm)
  (declare (simple-vector sequence))
  (let* ((n (hmm-n hmm))
	 (pi (hmm-pi hmm))
	 (last-index (1- (length sequence)))
	 (p 0.0f0))
    (declare (fixnum n last-index) (single-float p))
    (dotimes (i n)
      (declare (fixnum i))
      (incf p (sfmref alpha last-index i)))
    (dotimes (i n)
      (declare (fixnum i))
      (setf (sfvref pi i)
	    (/ (* (sfmref alpha 0 i) (sfmref beta 0 i)) p)))))


;;; Train multiple

(defun hmm-train-multiple (sequence-fn hmm &key (iterations 5) (debug nil)
						(debug-fn nil))
  ;; Note this assumes that the states of HMM are distinguished (so multiple
  ;; instances can be averaged.)
  (let* ((orig-hmm (copy-hmm hmm))
	 (hmm-sum nil)
	 (cnt 0))
    (loop (let ((sequence (or (funcall sequence-fn) (return))))
	    (when debug (format *error-output* "~&Sequence has ~D tokens~%"
				(length sequence)))
	    (hmm-train sequence hmm :iterations iterations :debug debug)
	    (when (and debug debug-fn)
	      (funcall debug-fn hmm sequence cnt))
	    (if hmm-sum
		(hmm-add hmm-sum hmm)
	      (setq hmm-sum (copy-hmm hmm)))
	    (copy-hmm orig-hmm hmm)
	    (incf cnt)))
    (if hmm-sum
	(progn
	  (hmm-div hmm-sum (float cnt))
	  (constrain-hmm hmm-sum orig-hmm))
      hmm)))

(defun constrain-hmm (hmm original-hmm)
  (constrain-vector (hmm-pi hmm) (hmm-pi original-hmm))
  (constrain-matrix (hmm-a hmm) (hmm-a original-hmm))
  (constrain-matrix (hmm-b hmm) (hmm-b original-hmm))
  hmm)

(defun constrain-matrix (matrix original-matrix)
  (declare (simple-vector matrix original-matrix))
  (dotimes (i (length matrix))
    (constrain-vector (svref matrix i) (svref original-matrix i))))

(defun constrain-vector (vector original-vector)
  (declare (type single-float-vector vector original-vector))
  (let ((n (length vector))
	(value least-positive-normalized-single-float))
    (declare (fixnum n) (single-float value))
    (dotimes (i n)
      (declare (fixnum i))
      (when (and (not (zerop (sfvref original-vector i)))
		 (< (sfvref vector i) value))
	(setf (sfvref vector i) value)))
    vector))
