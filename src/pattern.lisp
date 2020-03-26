(in-package #:lisp-tracer)

(defstruct pattern
  (a nil :type color)
  (b nil :type color))

(defun stripe-pattern (a b)
  (declare (type color a b))
  (make-pattern :a a :b b))

(defun stripe-at (pattern point)
  (declare (type pattern pattern) (type tuple point))
  (if (= (mod (floor (tuple-x point)) 2) 0)
      (pattern-a pattern)
      (pattern-b pattern)))
