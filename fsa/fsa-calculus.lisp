;;;-*- Package: FSA; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
 
;;; Copyright (c) 1992 by Xerox Corporation

;;; Development follows "Introduction to Automata Theory, Languages, and
;;; Computation" by J. Hopcroft and J. Ullman (Addison and Wesley 1979) 


(cl:in-package :fsa)

#|(eval-when (eval compile load)
  (export '(fsa-concat fsa-closure fsa-plus fsa-union fsa-optional
	    fsa-minus fsa-symbol

	    regexp-to-fsa * + ? - / or ! sequence)))|#

;;;; The FSA calculus

(defun copy-to-nfa (fsa)
  (let* ((nfa (make-nfa fsa))
	 (state-map (make-state-map fsa)))
    (copy-delta nfa fsa state-map)
    (copy-states nfa (fsa-final-states nfa) (fsa-final-states fsa) state-map)
    (setf (fsa-start-state nfa) (map-state (fsa-start-state fsa) state-map nfa))
    nfa))

(defun fsa-concat (fsa &rest other-fsas)
  (let* ((result (copy-to-nfa fsa))
	 (result-final-states (fsa-final-states result)))
    (dolist (fsa other-fsas)
      (let* ((state-map (make-state-map fsa))
	     (bridge (map-state (fsa-start-state fsa) state-map result)))
	(set-map
	 #'(lambda (state)
	     (set-insert bridge (states-get *epsilon* state result)))
	 result-final-states)
	(copy-delta result fsa state-map)
	(set-clear result-final-states) 
	(copy-states result result-final-states
		     (fsa-final-states fsa) state-map)))
    result))


(defun fsa-closure (fsa)
  (let* ((result (make-nfa fsa))
	 (result-final (make-state result))
	 (state-map (make-state-map fsa))
	 (destination-states (make-state-set result)))
    ;; copy transition function
    (copy-delta result fsa state-map)
    ;; add epsilon transitions
    (set-insert
     (map-state (fsa-start-state fsa) state-map result) destination-states)
    (set-insert result-final destination-states)
    ;; from start
    (setf (relation-get *epsilon*
			(symbols-get (fsa-start-state result) result))
      destination-states)
    ;; and from final states
    (set-map
     #'(lambda (state)
	 (let* ((new-state (map-state state state-map result))
		(new-relation (symbols-get new-state result)))
	   (setf (relation-get *epsilon* new-relation)
	     (set-union (states-get *epsilon* new-state result)
			(set-copy destination-states)))))
     (fsa-final-states fsa))
    ;; make a new final state
    (set-insert result-final (fsa-final-states result))
    result))

(defun fsa-plus (fsa)
  (fsa-concat fsa (fsa-closure fsa)))

(defun fsa-union (fsa &rest other-fsas)
  (let* ((result (make-nfa fsa))
	 (result-start (fsa-start-state result))
	 (result-final (make-state result))
	 (epsilon-dests (make-state-set result)))
    (dolist (fsa (cons fsa other-fsas))
      (let ((state-map (make-state-map fsa)))
	;; copy transition function
	(copy-delta result fsa state-map)
	;; add epsilon transition from start
	(set-insert (map-state (fsa-start-state fsa) state-map result)
		    epsilon-dests)
	;; and from final states
	(set-map
	 #'(lambda (state)
	     (set-insert result-final
			 (states-get *epsilon*
				     (map-state state state-map result)
				     result)))
	 (fsa-final-states fsa))))
    (setf (relation-get *epsilon* (symbols-get result-start result))
      epsilon-dests)
    (set-insert result-final (fsa-final-states result))
    result))

(defun fsa-optional (fsa)
  (let* ((result (copy-to-nfa fsa))
	 (new-start (make-state result)))
    (set-insert
     (fsa-start-state result)
     (states-get *epsilon* new-start result))
    (setf (fsa-start-state result) new-start)
    (set-insert new-start (fsa-final-states result))
    result))

(defun fsa-symbol (symbol fsa)
  (let* ((result (make-dfa fsa))
	 (final (make-state result)))
    (setf (relation-get symbol (symbols-get (fsa-start-state result) result))
      final)
    (set-insert final (fsa-final-states result))
    result))

(defun fsa-minus (fsa1 fsa2)
  (let* ((fsa1 (determinize fsa1))
	 (delta1 (fsa-delta fsa1))
	 (final1 (fsa-final-states fsa1))
	 (fsa2 (determinize fsa2))
	 (dead (make-state fsa2))
	 (delta2 (fsa-delta fsa2))
	 (final2 (fsa-final-states fsa2))
	 (result (make-dfa fsa1))
	 (result-final-states (fsa-final-states result))
	 (temp-start-state (cons (fsa-start-state fsa1) (fsa-start-state fsa2)))
	 (state-map (make-relation (make-pair-order-fn fsa1 fsa2) fsa1)))
    (labels ((intersect-state (pair)
	       (let ((symbols1 (relation-get (car pair) delta1))
		     (symbols2 (relation-get (cdr pair) delta2))
		     (reference (relation-get pair state-map)))
		 (when (and (set-member-p (car pair) final1)
			    (not (set-member-p (cdr pair) final2)))
		   (set-insert reference result-final-states))
		 (when symbols1
		   (relation-map
		    #'(lambda (symbol dest)
			(let* ((other
				(or (and symbols2
					 (relation-get symbol symbols2))
				    dead))
			       (other-pair (cons dest other))
			       (ref (relation-get other-pair state-map))
			       (relation (symbols-get reference result)))
			  (if ref
			      (setf (relation-get symbol relation) ref)
			    (let ((new (make-state result)))
			      (setf (relation-get other-pair state-map) new)
			      (setf (relation-get symbol relation) new)
			      (intersect-state other-pair)))))
		    symbols1)))))
      (setf (relation-get temp-start-state state-map) (fsa-start-state result))
      (intersect-state temp-start-state)
      result)))

(defun make-pair-order-fn (fsa1 fsa2)
  (let ((fn1 (state-order-fn fsa1))
	(fn2 (state-order-fn fsa2)))
    #'(lambda (x y)
	(let ((first (funcall fn1 (car x) (car y))))
	  (if (eq :equal first) (funcall fn2 (cdr x) (cdr y)) first)))))
	
    
(defun regexp-to-fsa (expr fsa)
  (let ((cache (make-hash-table :test 'equal)))
    (labels ((walk (x)
	       (or (gethash x cache)
		   (setf (gethash x cache)
		     (typecase x
		       (vector (walk `(sequence ,@(coerce x 'list))))
		       (cons
			(minimize
			 (ecase (first x)
			   (* (assert (null (cddr x)))
			      (fsa-closure (walk (second x))))
			   (+ (assert (null (cddr x)))
			      (fsa-plus (walk (second x))))
			   (? (assert (null (cddr x)))
			      (fsa-optional (walk (second x))))
			   (- (apply #'fsa-minus (mapcar #'walk (cdr x))))
			   ((/ or) (apply #'fsa-union (mapcar #'walk (cdr x))))
			   ((! sequence)
			    (apply #'fsa-concat (mapcar #'walk (cdr x)))))))
		       (otherwise (fsa-symbol x fsa)))))))
      (walk expr))))
