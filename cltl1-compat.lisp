(in-package :tagger.internal)

(defmacro cltl1-eval-when ((&rest args) &body body)
  `(eval-when (,@(mapcar (lambda (k)
                           (case k
                             (compile :compile-toplevel)
                             (load :load-toplevel)
                             (eval :execute)))
                         args))
     ,@body))


(defmacro defun-compile-time (name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name ,args ,@body)))
