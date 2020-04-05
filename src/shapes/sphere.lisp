(in-package #:lisp-tracer)

(defstruct (sphere (:include shape))
  (id 1))

(defun glass-sphere ()
  (make-sphere
   :material (make-material
              :transparency 1.0
              :refractive-index 1.5)))

(defmethod local-intersect ((sphere sphere) (ray ray))
  (let* ((sphere-to-ray (sub (ray-origin ray) (make-point 0.0 0.0 0.0)))
         (ray-direction (ray-direction ray))
         (a (dot ray-direction ray-direction))
         (b (mult 2.0 (dot ray-direction sphere-to-ray)))
         (c (sub (dot sphere-to-ray sphere-to-ray) 1.0))
         (discriminant (sub (mult b b)
                            (mult 4.0 (mult a c)))))
    (declare (type double-float discriminant a b c))
    (unless (< discriminant 0.0)
      (intersections (make-intersection (/ (- (- b) (sqrt discriminant))
                                           (* 2.0 a))
                                        sphere)
                     (make-intersection (/ (+ (- b) (sqrt discriminant))
                                           (* 2.0 a))
                                        sphere)))))

(defmethod local-normal-at ((sphere sphere) (point tuple))
  (sub point (make-point 0.0 0.0 0.0)))
