(in-package #:lisp-tracer-rays)

(defclass ray ()
  ((origin
    :documentation "Origin of a ray"
    :initarg :origin
    :accessor origin)
   (direction
    :documentation "Direction of a ray"
    :initarg :direction
    :accessor direction)))

(defun make-ray (origin direction)
  (declare (tuple origin) (tuple direction))
  (make-instance 'ray
                 :origin origin
                 :direction direction))

(defun pos (ray time)
  (declare (ray ray) (number time))
  (add (origin ray) (mult (direction ray) time)))
