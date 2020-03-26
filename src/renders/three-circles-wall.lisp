(in-package #:lisp-tracer-renders)

(defparameter *backdrop*
  (make-plane
   :transform (mult (rotation-x (/ pi 2))
                    (translation 0.0 1.5 0.0))
   :material (make-material
              :color (make-color :red 1.0 :green 0.9 :blue 0.9)
              :specular 0.0)))

(defparameter *middle*
  (make-sphere
   :transform (translation -0.5 1.0 0.5)
   :material (make-material
              :color (make-color :red 0.1 :green 1.0 :blue 0.5)
              :specular 0.3
              :diffuse 0.7)))

(defparameter *right*
  (make-sphere
   :transform (mult (scaling 0.5 0.5 0.5)
                    (translation 2.5 1.0 -0.5))
   :material (make-material
              :color (make-color :red 0.5 :green 1.0 :blue 0.1)
              :specular 0.3
              :diffuse 0.7)))

(defparameter *left*
  (make-sphere
   :transform (mult (translation -1.5 0.33 -0.75)
                    (scaling 0.33 0.33 0.33))
   :material (make-material
              :color (make-color :red 1.0 :green 0.8 :blue 0.1)
              :specular 0.3
              :diffuse 0.7)))

(defparameter *world*
  (make-world
   :objects (list *backdrop*
                  *middle*
                  *right*
                  *left*)
   :light (point-light
           (make-point -200.0 250.0 -300.0)
           (make-color :red 1.0
                       :green 1.0
                       :blue 1.0))))

(defparameter *camera*
  (create-camera
   1000
   500
   (/ pi 1.8)
   (view-transform (make-point 0.0 1.5 -5.0)
                   (make-point 0.0 1.0 0.0)
                   (make-vec 0.0 1.0 0.0))))

(defparameter *canvas* (render *camera* *world*))
(canvas-to-ppm *canvas*)
