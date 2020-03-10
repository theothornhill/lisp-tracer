(in-package #:lisp-tracer)

(defclass tuple ()
  ((x
    :initarg :x
    :accessor x
    :documentation "X component of a tuple of X Y Z W.")
   (y
    :initarg :y
    :accessor y
    :documentation "Y component of a tuple of X Y Z W.")
   (z
    :initarg :z
    :accessor z
    :documentation "Z component of a tuple of X Y Z W.")
   (w
    :initarg :w
    :accessor w
    :documentation "W component of a tuple of X Y Z W."))
  (:documentation "A tuple of length 4 that contains numbers."))

(defun make-tuple (x y z w)
  "Create a TUPLE with provided X Y Z W."
  (make-instance 'tuple :x x :y y :z z :w w))

(defun make-point (x y z)
  "A point is a TUPLE with provided X Y Z and W set to 1.0."
  (make-instance 'tuple :x x :y y :z z :w 1.0))

(defun make-vec (x y z)
  "A vector is a TUPLE with provided X Y Z and W set to 0.0."
  (make-instance 'tuple :x x :y y :z z :w 0.0))

(defun zerovec ()
  "A vector with all X Y Z W set to 0."
  (make-vec 0 0 0))

(defun point? (tuple)
  "T if W component of TUPLE is 1."
  (= (w tuple) 1))

(defun vec? (tuple)
  "T if W component of TUPLE is 0."
  (= (w tuple) 0))

(defun to-pixel (tuple)
  "Create a TUPLE with X Y components modified."
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
