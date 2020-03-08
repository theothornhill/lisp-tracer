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

(defun make-tuple (x y z w)
  (make-instance 'tuple :x x :y y :z z :w w))

(defun make-point (x y z)
  (make-instance 'tuple :x x :y y :z z :w 1.0))

(defun make-vec (x y z)
  (make-instance 'tuple :x x :y y :z z :w 0.0))

(defun zerovec ()
  (make-vec 0 0 0))

(defun point? (tuple)
  (= (w tuple) 1))

(defun vec? (tuple)
  (= (w tuple) 0))

(defun reflect (in normal)
  (declare (tuple in) (tuple normal))
  (sub in (reduce #'mult (list normal 2 (dot in normal)))))

(defun to-pixel (tuple)
  (make-tuple (round (x tuple))
              (round (y tuple))
              (z tuple)
              (w tuple)))

(defmethod print-object ((obj tuple) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x x)
                     (y y)
                     (z z)
                     (w w))
        obj
      (format stream "(~a, ~a, ~a, ~a)" x y z w))))
