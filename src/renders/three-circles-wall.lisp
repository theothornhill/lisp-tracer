(in-package #:lisp-tracer-renders)

(defparameter *backdrop*
  (make-plane
   :transform (mult (rotation-x (/ pi 2))
                    (translation 0.0 1.5 0.0))
   :material (make-material
              :pattern (checkers-pattern (mult (scaling 0.5 0.5 0.5)
                                               (translation-y 0.1))))))

(defparameter *middle*
  (make-sphere
   :transform (translation -0.5 1.0 0.5)
   :material (make-material
              :specular 0.1
              :diffuse 0.0
              :transparency 0.3
              :reflective 0.6
              :refractive-index glass)))

(defparameter *world*
  (make-world
   :objects (list *backdrop* *middle*)
   :light (point-light
           (make-point -250.0 250.0 -250.0)
           (make-color :red 1.0 :green 1.0 :blue 1.0))))

(defparameter *camera*
  (create-camera
   1000
   500
   (/ pi 3)
   (view-transform (make-point 0.0 1.5 -5.0)
                   (make-point 0.0 1.0 0.0)
                   (make-vec 0.0 1.0 0.0))))

(defparameter *canvas* (render *camera* *world*))
(canvas-to-ppm *canvas*)
