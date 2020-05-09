(in-package :lisp-tracer)

(defstruct (plane (:include shape)))

(defmethod local-normal-at ((plane plane) (point tuple))
  (make-vec :x 0.0 :y 1.0 :z 0.0))

(defmethod local-intersect ((plane plane) (ray ray))
  (with-ray-slots ray (((ori-y y))
                       ((dir-y y)))
    (unless (< (abs dir-y) epsilon)
      (intersections (make-intersection
                      :tt (/ (neg ori-y) dir-y)
                      :object plane)))))
