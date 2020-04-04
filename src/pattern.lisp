(in-package #:lisp-tracer)

(defstruct pattern
  (a nil :type (or color null))
  (b nil :type (or color null))
  (transform (identity-matrix) :type matrix))

(defun stripe-pattern (a b)
  (declare (type color a b))
  (make-pattern :a a :b b))

(defun stripe-at (pattern point)
  (declare (type pattern pattern) (type tuple point))
  (if (= (mod (floor (tuple-x point)) 2) 0)
      (pattern-a pattern)
      (pattern-b pattern)))

(defun stripe-at-object (pattern object point)
  (let* ((object-point
           (mult (inverse (shape-transform object)) point))
         (pattern-point
           (mult (inverse (pattern-transform pattern)) object-point)))
    (stripe-at pattern pattern-point)))
