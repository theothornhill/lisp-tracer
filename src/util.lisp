(in-package #:lisp-tracer)

(defmacro transform-object (&rest transforms)
  `(reduce #'mult (list ,@(reverse transforms))))

(defun reflect (in normal)
  (declare (tuple in) (tuple normal))
  (sub in (mult normal (mult 2.0 (dot in normal)))))

(defmacro cond-let ((&rest predicates) &body body)
  `(let ,predicates
     (cond
       ,@body)))
