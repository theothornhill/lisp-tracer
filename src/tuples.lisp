(in-package #:lisp-tracer)

(defstruct tuple
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (z 0f0 :type single-float)
  (w 0f0 :type single-float))

(declaim (inline make-point))
(declaim (inline make-tuple))
(declaim (inline make-vec))
(defun make-point (x y z)
  "A point is a TUPLE with provided X Y Z and W set to 1.0."
  (declare (optimize (speed 3) (safety 0)))
  (make-tuple :x x :y y :z z :w 1f0))

(defun make-vec (x y z)
  "A vector is a TUPLE with provided X Y Z and W set to 0.0."
  (declare (optimize (speed 3) (safety 0)))
  (make-tuple :x x :y y :z z :w 0f0))

(defun zerovec ()
  "A vector with all X Y Z W set to 0."
  (declare (optimize (speed 3) (safety 1)))
  (make-vec 0f0 0f0 0f0))

(defun point? (tuple)
  "T if W component of TUPLE is 1."
  (= (tuple-w tuple) 1f0))

(defun vec? (tuple)
  "T if W component of TUPLE is 0."
  (= (tuple-w tuple) 0f0))
