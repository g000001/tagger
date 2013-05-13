;;;-*- Package: HMM; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1991 by Xerox Corporation

(cl:in-package :hmm)

;;; HMM printing

(defun pretty-print-hmm (hmm &optional (stream *standard-output*))
  (format stream "n=~d, m=~d~%" (hmm-n hmm) (hmm-m hmm))
  (format stream "Pi:~%")
  (pretty-print-vector (hmm-pi hmm) stream)
  (format stream "A:~%")
  (pretty-print-matrix (hmm-a hmm) stream)
  (format stream "B:~%")
  (pretty-print-matrix (hmm-b hmm) stream))

(defun pretty-print-matrix (matrix stream)
  (dotimes (i (length matrix))
    (format stream "~d:~%" i)
    (pretty-print-vector (svref matrix i) stream)))

(defun pretty-print-vector (vector stream)
  (tdb:print-paragraph
   (format nil "~{~6f ~}" (map 'list #'identity vector))
   0 stream))
    

;;; Making sparse vectors and matrices

(defun make-sparse-sfm (n m initial-contents)
  (let ((sfm (make-sfm n m 0.0f0)))
    (dolist (row initial-contents sfm)
      (let ((i (car row)))
	(dolist (pair (cdr row))
	  (setf (sfmref sfm i (first pair)) (second pair)))))))      

(defun make-sparse-sfv (n initial-contents)
  (declare (fixnum n))
  (let ((array (make-sfv n 0.0f0)))
    (dolist (pair initial-contents array)
      (setf (sfvref array (first pair)) (second pair)))))

;;; Making random vector and matrices

(defun make-random-hmm (n m)
  (let ((a (make-random-matrix n n))
	(b (make-random-matrix n m))
	(pi (make-random-vector n)))
    (make-hmm :n n :m m :pi pi :b b :a a)))

(defun make-random-matrix (n m)
  (let ((result (make-array n)))
    (dotimes (i n result)
      (setf (svref result i) (make-random-vector m)))))

(defun make-random-vector (m)
  (let ((vector (make-sfv m))
	(sum 0.0f0))
    (declare (single-float sum))
    (dotimes (i m)
      (let ((number (random 1.0)))
	(declare (single-float number))
	(setf (sfvref vector i) number)
	(incf sum number)))
    (dotimes (i m)
      (declare (fixnum i))
      (setf (sfvref vector i)
	    (/ (sfvref vector i) sum)))
    vector))

(defun make-uniform-hmm (hmm)
  (let ((a (make-uniform-matrix (hmm-a hmm)))
	(b (make-uniform-matrix (hmm-b hmm)))
	(pi (make-uniform-vector (hmm-pi hmm))))
    (make-hmm :n (hmm-n hmm) :m (hmm-m hmm) :pi pi :b b :a a)))

(defun make-uniform-matrix (matrix)
  (let* ((n (length matrix))
	 (result (make-array n)))
    (dotimes (i n result)
      (setf (svref result i) (make-uniform-vector (svref matrix i))))))

(defun make-uniform-vector (vector)
  (let* ((n (length vector))
	 (result (make-sfv n 0.0f0))
	 (sum 0.0f0))
    (declare (single-float sum))
    (dotimes (i n)
      (unless (zerop (sfvref vector i))
	(incf sum 1.0)))
    (let ((rsum (/ 1.0 sum)))
      (declare (single-float rsum))
      (dotimes (i n)
	(declare (fixnum i))
	(unless (zerop (sfvref vector i))
	  (setf (sfvref result i) rsum))))
    result))


;;; Random sequence generation

(defun hmm-generate (hmm k)
  (declare (fixnum k))
  (let ((symbols (make-array k))
	(states (make-array k))
	(state (generate-start-state hmm)))
    (setf (svref symbols 0) (generate-symbol state hmm))
    (setf (svref states 0) state)
    (do ((i 1 (1+ i)))
	((= i k))
      (declare (fixnum i))
      (setq state (generate-transition state hmm))
      (setf (svref symbols i) (generate-symbol state hmm))
      (setf (svref states i) state))
     (values symbols states)))

(defun random-vector-index (vector)
  (declare (type single-float-vector vector))
  (let ((rand (random 1.0f0))
	(cum 0.0f0))
    (declare (single-float rand cum))
    (dotimes (index (length vector))
      (declare (fixnum index))
      (incf cum (sfvref vector index))
      (when (< rand cum)
	(return index)))))

(defun generate-start-state (hmm)
  (random-vector-index (hmm-pi hmm)))

(defun generate-symbol (state hmm)
  (random-vector-index (svref (hmm-b hmm) state)))

(defun generate-transition (state hmm)
  (random-vector-index (svref (hmm-a hmm) state)))


;;; Test HMM's

(defparameter *hmm-trivial-1*
  (let ((a (vector (make-sparse-sfv 1 '((0 1.0f0)))))
	(b (vector (make-sparse-sfv 2 '((0 0.5f0) (1 0.5f0)))))
	(pi (make-sparse-sfv 1 '((0 1.0f0)))))
    (make-hmm :n 1 :m 2 :pi pi :b b :a a)))


(defparameter *hmm-trivial-2*
  (let ((a (vector (make-sparse-sfv 2 '((0 0.0f0) (1 1.0f0)))
		   (make-sparse-sfv 2 '((0 1.0f0) (1 0.0f0)))))
	(b (vector (make-sparse-sfv 2 '((0 1.0f0) (1 0.0f0)))
		   (make-sparse-sfv 2 '((0 0.0f0) (1 1.0f0)))))
	(pi (make-sparse-sfv 2 '((0 0.5f0) (1 0.5f0)))))
    (make-hmm :n 2 :m 2 :pi pi :b b :a a)))

(defparameter *hmm-trivial-3*
  (let ((a (vector (make-sparse-sfv 2 '((0 0.5f0) (1 0.5f0)))
		   (make-sparse-sfv 2 '((0 0.5f0) (1 0.5f0)))))
	(b (vector (make-sparse-sfv 2 '((0 0.75f0) (1 0.25f0)))
		   (make-sparse-sfv 2 '((0 0.25f0) (1 0.75f0)))))
	(pi (make-sparse-sfv 2 '((0 0.5f0) (1 0.5f0)))))
    (make-hmm :n 2 :m 2 :pi pi :b b :a a)))

(defparameter *hmm-src44*
  (let ((a (vector
	    (make-sparse-sfv 4 '((2 0.5f0) (3 0.5f0)))
	    (make-sparse-sfv 4 '((0 0.5f0) (3 0.5f0)))
	    (make-sparse-sfv 4 '((0 0.5f0) (1 0.5f0)))
	    (make-sparse-sfv 4 '((1 0.5f0) (2 0.5f0)))))
	(b (vector
	    (make-sparse-sfv 4 '((0 0.5f0) (1 0.5f0)))
	    (make-sparse-sfv 4 '((1 0.5f0) (2 0.5f0)))
	    (make-sparse-sfv 4 '((2 0.5f0) (3 0.5f0)))
	    (make-sparse-sfv 4 '((0 0.5f0) (3 0.5f0)))))
	(pi (make-sparse-sfv 4 '((0 0.25f0) (1 0.25f0) (2 0.25f0) (3 0.25f0)))))
    (make-hmm :n 4 :m 4 :pi pi :b b :a a)))


(defparameter *hmm-src45*
  (let ((a (vector
	    (make-sparse-sfv 4 '((3 0.75f0) (1 0.25f0)))
	    (make-sparse-sfv 4 '((0 0.75f0) (2 0.25f0)))
	    (make-sparse-sfv 4 '((1 0.75f0) (3 0.25f0)))
	    (make-sparse-sfv 4 '((2 0.75f0) (0 0.25f0)))))
	(b (vector
	    (make-sparse-sfv 4 '((0 0.25f0) (1 0.75f0)))
	    (make-sparse-sfv 4 '((1 0.25f0) (2 0.75f0)))
	    (make-sparse-sfv 4 '((2 0.25f0) (3 0.75f0)))
	    (make-sparse-sfv 4 '((3 0.25f0) (0 0.75f0)))))
	(pi (make-sparse-sfv 4 '((0 0.25f0) (1 0.25f0) (2 0.25f0) (3 0.25f0)))))
    (make-hmm :n 4 :m 4 :pi pi :b b :a a)))


;;; Test Hmm-maximal-path

(defun hmm-log-sequence-prob (sequence states hmm)
  (do* ((pi (hmm-pi hmm))
	(a (hmm-a hmm))
	(b (hmm-b hmm))
	(state (svref states 0))
	(prob (+ (log (sfvref pi state))
		 (log (sfmref b state (svref sequence 0)))))
	(i 1 (1+ i)))
       ((= i (length sequence)) prob)
    (let* ((next (svref states i))
	   (sym (svref sequence i)))
      (incf prob (+ (log (sfmref a state next))
		    (log (sfmref b next sym))))
      (setq state next))))

(defun valid-sequence-p (sequence states hmm &key no-error-p)
  (do* ((pi (hmm-pi hmm))
	(a (hmm-a hmm))
	(b (hmm-b hmm))
	(state (svref states 0))
	(valid-p (not (zerop (* (sfvref pi state)
				(sfmref b state (svref sequence 0))))))
	(i 1 (1+ i)))
       ((= i (length sequence)) valid-p)
    (unless (or valid-p no-error-p)
      (error "Invalid sequence at position ~d" (1- i)))
    (let* ((next (svref states i))
	   (sym (svref sequence i)))
      (setq valid-p (and (not (zerop (* (sfmref a state next)
					(sfmref b next sym))))
			 valid-p))
      (setq state next))))


(defun test-hmm-maximal-path (hmm trials &key (path-length 50))
  (let ((hmm-est (make-hmm-est hmm)))
    (dotimes (i trials)
      (multiple-value-bind (sequence states)
	  (hmm-generate hmm path-length)
	(multiple-value-bind (path prob)
	    (hmm-maximal-path
	     sequence hmm-est (make-array path-length) path-length t)
	  (unless (valid-sequence-p sequence path hmm :no-error-p t)
	    (error "Invalid sequence: ~s states: ~s orig-sequence: ~s"
		   path states sequence))
	  (format t "~&~%Trial ~d:~%" i)
	  (format t "Log sequence prob: ~f~%"
		  (hmm-log-sequence-prob sequence states hmm))
	  (format t "Log maximal prob: ~f~%" prob)
	  (format t "True log maximal prob: ~f~%"
		  (hmm-log-sequence-prob sequence path hmm)))))))


;;; Markov test

(defun markov-test (file &key
			 (hmm (make-random-hmm 3 3))
			 (iterations 5) (debug t)) 
  (let ((sequence (file-to-vector file)))
    (hmm-train sequence hmm :iterations iterations :debug debug)))

(defun file-to-vector (file)
  (with-open-file (stream file)
    (let* ((n (file-length stream))
	   (vector (make-array n)))
      (dotimes (i n vector)
	(let ((char (char-downcase (read-char stream))))
	  (setf (svref vector i)
		(if (and (char>= char #\a) (char<= char #\z))
		    (case char
		      ((#\a #\e #\i #\o #\u) 0)
		      (t 1))
		    2)))))))

;;; test train multiple

(defparameter *hmm-simple-tag*
  (let ((a (vector
	    ;; adj
	    (make-sparse-sfv 4 '((0 0.2f0) (1 0.1f0)
				 (2 0.6f0) (3 0.1f0)))
	    ;; det
	    (make-sparse-sfv 4 '((0 0.3f0) (1 0.05f0)
				 (2 0.6f0) (3 0.05f0)))
	    ;; n
	    (make-sparse-sfv 4 '((0 0.1f0) (1 0.1f0)
				 (2 0.3f0) (3 0.5f0)))
	    ;; v
	    (make-sparse-sfv 4 '((0 0.2f0) (1 0.45f0)
				 (2 0.3f0) (3 0.05f0)))))
	(b (vector
	    ;; (adj),(det),(n),(v),(n v)
	    (make-sparse-sfv 5 '((0 1.0f0)))
	    (make-sparse-sfv 5 '((1 1.0f0)))
	    (make-sparse-sfv 5 '((2 0.5f0) (4 0.5f0)))
	    (make-sparse-sfv 5 '((3 0.5f0) (4 0.5f0)))))
	(pi (make-sparse-sfv 4 '((0 0.25f0) (1 0.35f0) (2 0.3f0) (3 0.1f0)))))
    (make-hmm :n 4 :m 5 :pi pi :b b :a a)))


(defun test-hmm-train-multiple (hmm trials &key
				    (sequence-length 50)
				    (iterations 5)
				    (debug nil))
  (let ((cnt 0))
    (hmm-train-multiple
     #'(lambda ()
	 (when (<= (incf cnt) trials)
	   (hmm-generate hmm sequence-length)))
     (make-uniform-hmm hmm)
     :iterations iterations
     :debug debug)))


;;; Checks

(defun stochastic-vector-p (vector)
  (let ((sum 0.0)
	(tolerance 0.000001))
    (declare (single-float sum))
    (dotimes (i (length vector))
      (incf sum (sfvref vector i)))
    (if (<= (abs (- sum 1.0)) tolerance)
	t
      (progn (warn "Sum not unity: ~f~%" sum) nil))))

(defun stochastic-matrix-p (matrix)
  (let ((result t))
    (dotimes (i (length matrix) result)
      (let ((new-result (stochastic-vector-p (svref matrix i))))
	(unless new-result (format t "at ~d~%" i))
	(setq result (and new-result result))))))


(defun check-hmm (hmm)
  (and (progn (format t "Testing pi~%") (stochastic-vector-p (hmm-pi hmm)))
       (progn (format t "Testing a~%") (stochastic-matrix-p (hmm-a hmm)))
       (progn (format t "Testing b~%") (stochastic-matrix-p (hmm-b hmm)))))
