(in-package #:lisp-tracer)

(defstruct (sphere (:include shape)))

(defun glass-sphere ()
  (make-sphere
   :material (make-material
              :transparency 1.0
              :refractive-index 1.5)))

(defmethod local-intersect ((sphere sphere) (ray ray))
  (with-slots (origin direction) ray
    (let* ((sphere-to-ray (sub origin (make-point)))
           (a (dot direction direction))
           (b (mult 2.0 (dot direction sphere-to-ray)))
           (c (sub (dot sphere-to-ray sphere-to-ray) 1.0))
           (discriminant (sub (mult b b)
                              (mult 4.0 (mult a c)))))
      (declare (type double-float discriminant a b c))
      (unless (< discriminant 0.0)
        (intersections (make-intersection :tt (/ (- (- b) (sqrt discriminant))
                                                 (* 2.0 a))
                                          :object sphere)
                       (make-intersection :tt (/ (+ (- b) (sqrt discriminant))
                                                 (* 2.0 a))
                                          :object sphere))))))

(defmethod local-normal-at ((sphere sphere) (point tuple))
  (sub point (make-point)))
