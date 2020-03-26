(in-package #:lisp-tracer)

(defstruct (plane (:include shape)))

(defmethod local-normal-at ((plane plane) (point tuple))
  (make-vec 0.0 1.0 0.0))

(defmethod local-intersect ((plane plane) (ray ray))
  (unless (< (abs (tuple-y (ray-direction ray))) epsilon)
    (intersections (make-intersection
                    (/ (neg (tuple-y (ray-origin ray)))
                       (tuple-y (ray-direction ray)))
                    plane))))
