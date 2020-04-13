(in-package #:lisp-tracer)

(defun make-point (&key x y z)
  "A point is a TUPLE with provided X Y Z and W set to 1.0."
  (make-tuple :x x :y y :z z :w 1.0))

(defun make-vec (&key x y z)
  "A vector is a TUPLE with provided X Y Z and W set to 0.0."
  (make-tuple :x x :y y :z z :w 0.0))

(defun point? (tuple)
  "T if W component of TUPLE is 1."
  (= (tuple-w tuple) 1.0))

(defun vec? (tuple)
  "T if W component of TUPLE is 0."
  (= (tuple-w tuple) 0.0))
