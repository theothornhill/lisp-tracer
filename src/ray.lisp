(in-package #:lisp-tracer)

(defun pos (ray time)
  (declare (ray ray) (float time))
  (add (ray-origin ray) (mult (ray-direction ray) time)))

(defun transform (ray matrix)
  (declare (type ray ray) (type matrix matrix))
  (make-ray :origin (mult matrix (ray-origin ray))
            :direction (mult matrix (ray-direction ray))))
