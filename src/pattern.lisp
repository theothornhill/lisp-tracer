(in-package #:lisp-tracer)

(defstruct pattern
  (a nil :type (or color null))
  (b nil :type (or color null))
  (transform (identity-matrix) :type matrix))

(defstruct (stripes (:include pattern)))
(defstruct (gradient (:include pattern)))
(defstruct (rings (:include pattern)))
(defstruct (checkers (:include pattern)))

(defun stripe-pattern (a b)
  (declare (type color a b))
  (make-stripes :a a :b b))

(defun gradient-pattern (a b)
  (declare (type color a b))
  (make-gradient :a a :b b))

(defun ring-pattern (a b)
  (declare (type color a b))
  (make-rings :a a :b b))

(defun checkers-pattern (a b)
  (declare (type color a b))
  (make-checkers :a a :b b))

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

(defmethod pattern-at ((rings rings) point)
  "Calculates floor(sqrt(p_x^2 + p_z^2) mod 2 = 0"
  (if (=
       (mod (floor (sqrt (add (expt (tuple-x point) 2)
                              (expt (tuple-z point) 2))))
            2)
       0)
      (pattern-a rings)
      (pattern-b rings)))

(defmethod pattern-at ((checkers checkers) point)
  "Calculates p_x + p_y + p_z mod 2 = 0"
  (if (=
       (mod (+ (floor (tuple-x point))
               (floor (tuple-y point))
               (floor (tuple-z point)))
            2)
       0)
      (pattern-a checkers)
      (pattern-b checkers)))

(defun pattern-at-object (pattern object point)
  (let* ((object-point
           (mult (inverse (shape-transform object)) point))
         (pattern-point
           (mult (inverse (pattern-transform pattern)) object-point)))
    (pattern-at pattern pattern-point)))
