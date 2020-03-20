(in-package #:lisp-tracer)

(defmacro transform-object (&rest transforms)
  (declare (optimize (speed 3) (safety 0)))
  `(reduce #'mult (list ,@(reverse transforms))))

(defun reflect (in normal)
  (declare (tuple in) (tuple normal)
           (optimize (speed 3) (safety 0)))
  (sub in (mult normal (mult 2.0 (dot in normal)))))
