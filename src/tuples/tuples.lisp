(in-package #:lisp-tracer-tuples)

(defclass tuple ()
  ((x
    :initarg :x
    :accessor x)
   (y
    :initarg :y
    :accessor y)
   (z
    :initarg :z
    :accessor z)
   (w
    :initarg :w
    :accessor w)))

(defun tuple! (x y z w)
  (make-instance 'tuple :x x :y y :z z :w w))

(defun point! (x y z)
  (make-instance 'tuple :x x :y y :z z :w 1.0))

(defun vec! (x y z)
  (make-instance 'tuple :x x :y y :z z :w 0.0))

(defun zerovec ()
  (vec! 0 0 0))

(defun point? (tuple)
  (= (w tuple) 1))

(defun vec? (tuple)
  (= (w tuple) 0))

(defmethod print-object ((obj tuple) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x x)
                     (y y)
                     (z z)
                     (w w))
        obj
      (format stream "(~a, ~a, ~a, ~a)" x y z w))))
