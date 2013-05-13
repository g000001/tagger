;;;-*- Package: FSA; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
 
;;; Copyright (c) 1992--1994 by Xerox Corporation

;;; Development follows "Introduction to Automata Theory, Languages, and
;;; Computation" by J. Hopcroft and J. Ullman (Addison and Wesley 1979) 


(cl:defpackage :finite-state-automata
  (:nicknames :fsa)
  (:use :common-lisp)
  #+clos(:import-from clos slot-definition-name class-slots)
  #+pcl (:import-from pcl slot-definition-name class-slots)
  #+mcl (:import-from ccl slot-definition-name class-instance-slots)
  (:export :ordered-set :set-order-fn :set-length :set-insert :set-clear
	   :set-copy :set-union :set-intersect :set-minus :set-map
	   :set-member-p :set-empty-p
	   
	   :relation :relation-get :relation-map :relation-empty-p
	   
	   :fsa :nfa :dfa
	   :*epsilon* :fsa-start-state :fsa-delta :fsa-final-states
	   
	   :symbol-order-fn :state-order-fn
	   :make-state :make-nfa :make-dfa :make-ordered-set :make-relation
	   :make-state-map :copy-delta :make-state-relation
	   :symbols-get :delta-get :states-get
	   
	   :in-language-p :determinize :invert :minimize ))

(cl:in-package :fsa)

;;;; Set abstraction
(defclass ordered-set () ())

;; order function on sets (whose elements are ordered)
(defgeneric set-order-fn (set))
(defgeneric set-length (set))

(defgeneric set-insert (element set))
(defgeneric set-clear (set))
(defgeneric set-copy (set))

;; set-union side-effects both args, returns the first
(defgeneric set-union (set1 set2))
;; set-intersect side-effects first arg and returns it.
(defgeneric set-intersect (set1 set2))
;; set-minus side-effects first arg and returns it.
(defgeneric set-minus (set1 set2))
(defgeneric set-map (fn set))
(defgeneric set-member-p (element set))
(defgeneric set-empty-p (set))

;;;; univariate relation
(defclass relation () ())
(defgeneric relation-get (x relation))
(defgeneric (setf relation-get) (nv x relation))
(defgeneric relation-map (fn relation))
(defgeneric relation-empty-p (relation))

#-cltl2(eval-when (compile) (defgeneric (setf relation-get) (nv x relation)))

;;;; A finite-state automaton is a quintuple, Q,S,d,q,F where Q is
;;;; the state set (here an integer range), S is the alphabet, d is a state
;;;; transition function and F is the set of final states.


;;; Epsilon is a distinguished symbol

(defvar *epsilon* "epsilon")

(defclass fsa ()
   (;; A function from QxS into either Q or 2^Q implemented as a relation of
    ;; states whose values are relations of symbols
    (fsa-delta :initarg :delta :initform nil :accessor fsa-delta)
    (fsa-start-state :initarg :start-state :initform nil
		     :accessor fsa-start-state)
    ;; A set of states
    (fsa-final-states :initarg :final-states :initform nil
		      :accessor fsa-final-states)))

(defmethod initialize-instance :after ((fsa fsa) &key &allow-other-keys)
  (unless (fsa-delta fsa)
    (setf (fsa-delta fsa) (make-delta fsa)))
  (unless (fsa-start-state fsa)
    (setf (fsa-start-state fsa) (make-state fsa)))
  (unless (fsa-final-states fsa)
    (setf (fsa-final-states fsa) (make-state-set fsa))))

;;; Required predicates (return t, nil, or :equal)
(defgeneric symbol-order-fn (fsa))
(defgeneric state-order-fn (fsa))

;;; Constructors
(defgeneric make-state (fsa))
(defgeneric make-nfa (fsa))
(defgeneric make-dfa (fsa))

;;; Sets and relations
(defgeneric make-ordered-set (predicate fsa))
(defun make-symbol-set (fsa)
  (make-ordered-set (symbol-order-fn fsa) fsa))
(defun make-state-set (fsa)
  (make-ordered-set (state-order-fn fsa) fsa))
(defun make-set-set (element fsa)
  (make-ordered-set (set-order-fn element) fsa))
(defgeneric make-relation (order-fn fsa))
(defun make-state-relation (fsa)
  (make-relation (state-order-fn fsa) fsa))
(defun make-symbol-relation (fsa)
  (make-relation (symbol-order-fn fsa) fsa))

;;; Transition functions
(defun make-delta (fsa)
  (make-state-relation fsa))

(defun delta-get (symbol state delta)
  (let ((relation (relation-get state delta)))
    (when relation (relation-get symbol relation))))

(defun symbols-get (state fsa)
  (let ((delta (fsa-delta fsa)))
    (or (relation-get state delta)
	(setf (relation-get state delta) (make-symbol-relation fsa)))))

(defun states-get (symbol state fsa)
  (let ((relation (symbols-get state fsa)))
    (or (relation-get symbol relation)
	(setf (relation-get symbol relation)
	  (make-state-set fsa)))))

(defun add-dests (states symbol relation)
  (let ((old (relation-get symbol relation)))
    (if old
	(set-union old states)
      (setf (relation-get symbol relation) states))))

;;; State remapping, copying

(defun make-state-map (old)
  (make-state-relation old))

(defun map-state (old-state relation new-fsa)
  (or (relation-get old-state relation)
      (setf (relation-get old-state relation) (make-state new-fsa))))

(defgeneric copy-delta (new old state-map))

(defun copy-states (new-fsa new-states old-states state-map)
  (set-map
   #'(lambda (state)
       (set-insert (map-state state state-map new-fsa) new-states))
   old-states)
  new-states)


;;; Returns T or Nil.  Vector is a vector of symbols
(defgeneric in-language-p (vector fsa))

;;; Returns a dfa
(defgeneric determinize (fsa))

;;; Used to carry over information from merged states in determinize
;;; Called for effect not value.
(defgeneric merge-states (old-state-set new-state new-fsa old-fsa))

;;; Returns a machine that accepts the reverse language
(defgeneric invert (fsa))

;;; Returns a dfa
(defgeneric minimize (fsa))

(defmethod minimize ((fsa fsa))
  (determinize (invert (determinize (invert fsa)))))



;;;; A non-deterministic finite-state automaton has a transition function 
;;;; that maps from Qx{S U e} into 2^Q (the powers set of Q).  S is augmented
;;;; by an epsilon symbol, e.  Note, here we are assuming that all
;;;; non-deterministic automata may have epsilon transitions.

(defclass nfa (fsa) ())

(defmethod symbol-order-fn :around ((fsa nfa))
  (let ((fn (call-next-method)))
    #'(lambda (x y)
	(cond ((eq x *epsilon*)
	       (if (eq y *epsilon*) :equal))
	      ((eq y *epsilon*) t)
	      (t (funcall fn x y))))))

(defmethod copy-delta ((new nfa) (old nfa) state-map)
  (relation-map
   #'(lambda (state relation)
       (let ((new-relation
	      (symbols-get (map-state state state-map new) new)))
	 (relation-map
	  #'(lambda (symbol dest)
	      (setf (relation-get symbol new-relation)
		(copy-states new (make-state-set new) dest state-map)))
	  relation)))
   (fsa-delta old)))

(defmethod epsilon-closure (state (fsa nfa))
  ;; All states reachable from state following only epsilon edges.
  (let ((delta (fsa-delta fsa))
	(result (make-state-set fsa)))
    (labels ((state-epsilon (state)
	       (set-insert state result)
	       (let ((states (delta-get *epsilon* state delta)))
		 (when states
		   (set-map
		    #'(lambda (state)
			(unless (set-member-p state result)
			  (state-epsilon state)))
		    states)))))
      (state-epsilon state))
    result))

(defmethod epsilon-closure ((states ordered-set) (fsa nfa))
  (let ((result (make-state-set fsa)))
    (set-map
     #'(lambda (state)
	 (set-union result (epsilon-closure state fsa)))
     states)
    result))

(defmethod extended-delta (symbol state (fsa nfa))
  (let ((new-state (delta-get symbol state (fsa-delta fsa))))
    (if new-state
	(epsilon-closure new-state fsa)
      (make-state-set fsa))))

(defmethod extended-delta (symbol (states ordered-set) (fsa nfa))
  (let ((result (make-state-set fsa)))
    (set-map
     #'(lambda (state)
	 (set-union result (extended-delta symbol state fsa)))
     states)
    result))

(defmethod in-language-p (vector (fsa nfa))
  (do ((final-states (fsa-final-states fsa))
       (states (epsilon-closure (fsa-start-state fsa) fsa)
	       (extended-delta (aref vector i) states fsa))
       (i 0 (1+ i)))
      ((or (= i (length vector)) (set-empty-p states))
       (not (set-empty-p (set-intersect states final-states))))))

(defmethod determinize ((fsa nfa))
  (let* ((delta (fsa-delta fsa))
         (final-states (fsa-final-states fsa))
         (result (make-dfa fsa))
         (result-delta (fsa-delta result))
         (result-final-states (fsa-final-states result))
         (state-map (make-relation (set-order-fn final-states) fsa))
         (temp-start-state (epsilon-closure (fsa-start-state fsa) fsa)))
    (labels ((determinize-state (temp-state)
               (let ((symbols (make-symbol-set fsa))
                     (reference (relation-get temp-state state-map))
                     (final-p nil))
                 (set-map
                  #'(lambda (state)
                      (when (set-member-p state final-states)
                        (setq final-p t))
                      (let ((relation (relation-get state delta)))
                        (when relation
                          (relation-map
                           #'(lambda (symbol ignore)
                               (declare (ignore ignore))
                               (unless (eq symbol *epsilon*)
                                 (set-insert symbol symbols)))
                           relation))))
                  temp-state)
                 (when final-p
                   (set-insert reference result-final-states))
                 (unless (set-empty-p symbols)
                   (let ((relation (make-symbol-relation result)))
                     (setf (relation-get reference result-delta) relation)
                     (set-map
                      #'(lambda (symbol)
                          (let* ((dest (extended-delta symbol temp-state fsa))
                                 (ref (relation-get dest state-map)))
                            (if ref
                                (setf (relation-get symbol relation) ref)
                              (let ((new (make-state result)))
                                (setf (relation-get dest state-map) new)
				(merge-states dest new result fsa)
                                (setf (relation-get symbol relation) new)
                                (determinize-state dest)))))
                      symbols))))))
      (setf (relation-get temp-start-state state-map) (fsa-start-state result))
      (merge-states temp-start-state (fsa-start-state result) result fsa)
      (determinize-state temp-start-state)
      result)))

(defmethod merge-states (old-state-set new-state new (old nfa))
  (declare (ignore old-state-set new-state new)))

(defmethod invert ((fsa nfa))
  ;; Produce a machine that accepts the reverse language of FSA
  (let* ((result (make-nfa fsa))
	 (state-map (make-state-map fsa)))
    ;; Invert Delta
    (relation-map
     #'(lambda (state relation)
	 (relation-map
	  #'(lambda (symbol states)
	      (set-map
	       #'(lambda (dest)
		   (set-insert
		    (map-state state state-map result)
		    (states-get symbol (map-state dest state-map result)
				result)))
	       states))
	  relation))
     (fsa-delta fsa))
    ;; make a new start state
    (let ((new-start (make-state result))
	  (start-dest (make-state-set result)))
      (set-map
       #'(lambda (state)
	   (set-insert (map-state state state-map result) start-dest))
       (fsa-final-states fsa))
      (setf (relation-get *epsilon* (symbols-get new-start result))
	start-dest)
      (setf (fsa-start-state result) new-start))
    ;; make old start final
    (set-insert (map-state (fsa-start-state fsa) state-map result)
		(fsa-final-states result))
    result))


;;;; A deterministic finite-state automaton has a transition function 
;;;; that maps from QxS into Q.

(defclass dfa (fsa) ())

(defmethod copy-delta ((new nfa) (old dfa) state-map)
  (relation-map
   #'(lambda (state relation)
       (let ((new-relation
	      (symbols-get (map-state state state-map new) new)))
	 (relation-map
	  #'(lambda (symbol dest)
	      (setf (relation-get symbol new-relation)
		(let ((set (make-state-set new)))
		  (set-insert (map-state dest state-map new) set)
		  set)))
	  relation)))
   (fsa-delta old)))

(defmethod in-language-p (vector (fsa dfa))
  (do ((delta (fsa-delta fsa))
       (state (fsa-start-state fsa) (delta-get (aref vector i) state delta))
       (i 0 (1+ i)))
      ((or (= i (length vector)) (null state))
       (and state (set-member-p state (fsa-final-states fsa))))))

(defmethod determinize ((fsa dfa))
  fsa)

(defmethod invert ((fsa dfa))
  ;; Produce a machine that accepts the reverse language of FSA
  (let* ((result (make-nfa fsa))
	 (state-map (make-state-map fsa)))
    ;; Invert Delta
    (relation-map
     #'(lambda (state relation)
	 (relation-map
	  #'(lambda (symbol dest)
	      (set-insert (map-state state state-map result)
			  (states-get symbol (map-state dest state-map result)
				      result)))
	  relation))
     (fsa-delta fsa))
    ;; make a new start state
    (let ((new-start (make-state result))
	  (start-dest (make-state-set result)))
      (set-map
       #'(lambda (state)
	   (set-insert (map-state state state-map result) start-dest))
       (fsa-final-states fsa))
      (setf (relation-get *epsilon* (symbols-get new-start result))
	start-dest)
      (setf (fsa-start-state result) new-start))
    ;; make old start final
    (set-insert (map-state (fsa-start-state fsa) state-map result)
		(fsa-final-states result))
    result))
		    


;;;; I/O

(defmethod fsa-print ((fsa fsa) &optional (stream *standard-output*))
  (format stream "start: ~s~%final: ~s~%" (fsa-start-state fsa)
	  (fsa-final-states fsa))
  (relation-map
   #'(lambda (state relation)
       (format stream "~s: " state)
       (relation-map
	#'(lambda (symbol dest)
	    (format stream "~a:~a " symbol dest))
	relation)
       (terpri stream))
   (fsa-delta fsa)))


;;; MAKE-LOAD-FORM implementation, so that FSAs can be saved in compiled files

#+mcl(defmethod class-slots (class) (class-instance-slots class))

(defmethod make-load-form ((fsa fsa) #+(or sgi cmu17) &optional
				     #+(or sgi cmu17) env)
  #+cmu17 (declare (ignore env))
  (multiple-value-bind (form fixups)
      #+cmu17
      (values
       `(make-instance ',(class-name (class-of fsa)))
       `(progn
	  ,@(mapcar
	     #'(lambda (name)
		 `(setf (slot-value ,fsa ',name) ',(slot-value fsa name)))
	     (set-difference
	      (mapcar #'slot-definition-name (class-slots (class-of fsa)))
	      '(fsa-delta fsa-final-states)))))
      #-cmu17
      (make-load-form-saving-slots
       fsa
       ;; save all of the slots but FSA-FINAL-STATES and FSA-DELTA
       #+sgi
       :slots
       (set-difference
	(mapcar #'slot-definition-name (class-slots (class-of fsa)))
	'(fsa-delta fsa-final-states))
       #+sgi :environment #+sgi env)
    (values
     form
     `(progn
        #+(or mcl (and sgi allegro-v4.2))(initialize-instance ,fsa)
	,fixups
	,(make-fsa-delta-load-form fsa)
	,(make-fsa-final-states-load-form fsa)))))

(defmethod make-fsa-delta-load-form ((dfa dfa))
  `(let ((delta (fsa-delta ',dfa)))
     ,@(let ((forms '()))
	 (relation-map
	  #'(lambda (state rel)
	      (let ((arcs '()))
		(relation-map
		 #'(lambda (symbol dest) (push (cons symbol dest) arcs))
		 rel)
		(push `(add-arcs ',arcs
				 (setf (relation-get ',state delta)
				   (make-symbol-relation ',dfa)))
		      forms)))
	  (fsa-delta dfa))
	 (nreverse forms))))

(defun add-arcs (arcs relation)
  (dolist (arc arcs)
    (setf (relation-get (car arc) relation) (cdr arc))))

(defmethod make-fsa-final-states-load-form ((fsa fsa))
  `(let ((final-states (fsa-final-states ',fsa)))
     ,@(let ((forms '()))
	 (set-map #'(lambda (state)
		      (push `(set-insert ',state final-states) forms))
		  (fsa-final-states fsa))
	 forms)))

;; A hack to make MAKE-LOAD-FORM usable in CMU CL, which doesn't support it.
#+cmu
(defun make-load-sx (object)
  (multiple-value-bind (form fixups) (make-load-form object)
    (let ((var (gensym)))
      `(let ((,var ,form))
	 ,(sublis `((,object . ,var) (',object . ,var)) fixups :test #'equal)
	 ,var))))
