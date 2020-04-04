(in-package #:lisp-tracer)

(defstruct pattern
  (a nil :type (or color null))
  (b nil :type (or color null))
  (transform (identity-matrix) :type matrix))

(defstruct (stripes (:include pattern)))
(defstruct (gradient (:include pattern)))

(defun stripe-pattern (a b)
  (declare (type color a b))
  (make-stripes :a a :b b))

(defun gradient-pattern (a b)
  (declare (type color a b))
  (make-gradient :a a :b b))

(defgeneric pattern-at (pattern point)
  (:documentation "Return pattern at a given point."))

(defmethod pattern-at ((stripes stripes) point)
  (if (= (mod (floor (tuple-x point)) 2) 0)
      (pattern-a stripes)
      (pattern-b stripes)))

(defmethod pattern-at ((gradient gradient) point)
  (let ((distance (sub (pattern-b gradient) (pattern-a gradient)))
        (fraction (float (- (tuple-x point)
                            (floor (tuple-x point))))))
    (add (pattern-a gradient) (mult distance fraction))))

(defun pattern-at-object (pattern object point)
  (let* ((object-point
           (mult (inverse (shape-transform object)) point))
         (pattern-point
           (mult (inverse (pattern-transform pattern)) object-point)))
    (pattern-at pattern pattern-point)))
