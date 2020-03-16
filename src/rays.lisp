(in-package #:lisp-tracer)

(defstruct ray
  origin
  direction)

(defun pos (ray time)
  (declare (ray ray) (number time))
  (add (ray-origin ray) (mult (ray-direction ray) time)))

(defun transform (ray matrix)
  (declare (type ray ray) (type matrix matrix)
           (optimize (speed 3)))
  (make-ray :origin (mult matrix (ray-origin ray))
            :direction (mult matrix (ray-direction ray))))
