(in-package #:lisp-tracer)

(defstruct tuple
  x y z w)

;; (defun create-tuple (x y z w)
;;   "Create a TUPLE with provided X Y Z W."
;;   (declare (optimize (speed 3) (safety 0)))
;;   (make-tuple :x x :y y :z z :w w))

(defun make-point (x y z)
  "A point is a TUPLE with provided X Y Z and W set to 1.0."
  (declare (optimize (speed 3) (safety 0)))
  (make-tuple :x x :y y :z z :w 1.0))

(defun make-vec (x y z)
  "A vector is a TUPLE with provided X Y Z and W set to 0.0."
  (declare (optimize (speed 3) (safety 0)))
  (make-tuple :x x :y y :z z :w 0.0))

(defun zerovec ()
  "A vector with all X Y Z W set to 0."
  (declare (optimize (speed 3) (safety 1)))
  (make-vec 0 0 0))

(defun point? (tuple)
  "T if W component of TUPLE is 1."
  (= (tuple-w tuple) 1))

(defun vec? (tuple)
  "T if W component of TUPLE is 0."
  (= (tuple-w tuple) 0))

(defun to-pixel (tuple)
  "Create a TUPLE with X Y components modified."
  (make-tuple :x (round (tuple-x tuple))
              :y (round (tuple-y tuple))
              :z (tuple-z tuple)
              :w (tuple-w tuple)))

(defmethod print-object ((obj tuple) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((tuple-x tuple-x)
                     (tuple-y tuple-y)
                     (tuple-z tuple-z)
                     (tuple-w tuple-w))
        obj
      (format stream "(~a, ~a, ~a, ~a)"
              tuple-x
              tuple-y
              tuple-z
              tuple-w))))
