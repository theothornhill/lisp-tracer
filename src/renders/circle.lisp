(in-package #:lisp-tracer)

(defparameter ray-origin (make-point 0 0 -50))
(defparameter wall-z 100)
(defparameter wall-size 70.0)
(defparameter canvas-pixels 1000)
(defparameter pixel-size (/ wall-size canvas-pixels))
(defparameter half (/ wall-size 2))

(require :sb-sprof)
(sb-sprof:with-profiling
    (:mode :alloc :show-progress t :max-samples 1000)
  (let* ((canv (make-canvas canvas-pixels canvas-pixels))
         (shape (make-sphere))
         (light-position (make-point -100 -100 -100))
         (light-color (make-color 1 1 1))
         (light (point-light light-position light-color)))
    (setf (material-color (sphere-material shape)) (make-color 1 0.2 1))
    (setf (transform-matrix shape) (scaling 10 10 10))
    (iter (for y from 0 below canvas-pixels)
      (let ((world-y (- (* pixel-size y) half)))
        (iter (for x from 0 below canvas-pixels)
          (let* ((world-x (+ (- half) (* pixel-size x)))
                 (pos (make-point world-x world-y wall-z))
                 (r (make-ray ray-origin (normalize (sub pos ray-origin))))
                 (xs (intersect shape r)))
            (when (hit xs)
              (let* ((hit (hit xs))
                     (point (pos r (tt hit)))
                     (normal (normal-at (object hit) point))
                     (eyev (neg (direction r)))
                     (color (lighting (sphere-material (object hit))
                                      light
                                      point
                                      eyev
                                      normal)))
                (write-pixel canv x y color)))))))
    (canvas-to-ppm canv)))
