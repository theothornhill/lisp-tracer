(in-package #:lisp-tracer)

(defparameter ray-origin (make-point 0.0 0.0 -50.0))
(defparameter wall-z 100.0)
(defparameter wall-size 70.0)
(defparameter canvas-pixels 1000)
(defparameter pixel-size (float (/ wall-size canvas-pixels)))
(defparameter half (float (/ wall-size 2)))


(sb-sprof:with-profiling
    (:mode :alloc :show-progress t :max-samples 1000)
  (let* ((canv (create-canvas canvas-pixels canvas-pixels))
         (shape (make-sphere))
         (light-position (make-point -100.0 -100.0 -100.0))
         (light-color (make-color :red 1.0 :green 1.0 :blue 1.0))
         (light (point-light light-position light-color)))
    (setf (material-color (shape-material shape)) (make-color :red 1.0 :green 0.2 :blue 1.0))
    (setf (shape-transform shape) (scaling 10.0 10.0 10.0))
    (iter (for y from 0 below canvas-pixels)
      (let ((world-y (float (- (* pixel-size y) half))))
        (iter (for x from 0 below canvas-pixels)
          (let* ((world-x (float (+ (- half) (* pixel-size x))))
                 (pos (make-point world-x world-y wall-z))
                 (r (make-ray :origin ray-origin
                              :direction (normalize (sub pos ray-origin))))
                 (xs (intersect shape r)))
            (when (hit xs)
              (let* ((hit (hit xs))
                     (hit-object (rt-intersection-object hit))
                     (point (pos r (rt-intersection-tt hit)))
                     (normal (normal-at hit-object point))
                     (eyev (neg (ray-direction r)))
                     (color (lighting (shape-material hit-object)
                                      light
                                      point
                                      eyev
                                      normal
                                      nil)))
                (write-pixel canv x y color)))))))
    (canvas-to-ppm canv)))
