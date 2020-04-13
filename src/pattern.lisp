(in-package #:lisp-tracer)

(defstruct pattern
  (a nil :type (or color null))
  (b nil :type (or color null))
  (transform (identity-matrix) :type matrix))

(defstruct (stripes (:include pattern)))
(defstruct (gradient (:include pattern)))
(defstruct (rings (:include pattern)))
(defstruct (checkers (:include pattern)))
(defstruct (test (:include pattern)))

(defun test-pattern (&key (transform (identity-matrix)))
  (declare (type matrix transform))
  (make-test :a (make-color :red 1.0 :green 1.0 :blue 1.0)
             :b (make-color :red 0.0 :green 0.0 :blue 0.0)
             :transform transform))

(defun stripe-pattern (&key
                         (a (make-color :red 1.0 :green 1.0 :blue 1.0))
                         (b (make-color :red 0.0 :green 0.0 :blue 0.0))
                         (transform (identity-matrix)))
  (declare (type color a b) (type matrix transform))
  (make-stripes :a a :b b :transform transform))

(defun gradient-pattern (&key
                           (a (make-color :red 1.0 :green 1.0 :blue 1.0))
                           (b (make-color :red 0.0 :green 0.0 :blue 0.0))
                           (transform (identity-matrix)))
  (declare (type color a b) (type matrix transform))
  (make-gradient :a a :b b :transform transform))

(defun ring-pattern (&key
                       (a (make-color :red 1.0 :green 1.0 :blue 1.0))
                       (b (make-color :red 0.0 :green 0.0 :blue 0.0))
                       (transform (identity-matrix)))
  (declare (type color a b) (type matrix transform))
  (make-rings :a a :b b :transform transform))

(defun checkers-pattern (&key
                           (transform (identity-matrix))
                           (a (make-color :red 1.0 :green 1.0 :blue 1.0))
                           (b (make-color :red 0.0 :green 0.0 :blue 0.0)))
  (declare (type color a b) (type matrix transform))
  (make-checkers :a a :b b :transform transform))

(defgeneric pattern-at (pattern point)
  (:documentation "Return pattern at a given point."))

(defmethod pattern-at ((test test) point)
  (make-color :red (tuple-x point)
              :green (tuple-y point)
              :blue (tuple-z point)))

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
