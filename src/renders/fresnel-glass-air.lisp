(in-package #:lisp-tracer-renders)

(defparameter *backdrop*
  (make-plane
   :transform (mult (rotation-x (/ pi 2))
                    (translation 0.0 5.0 0.0))
   :material (make-material
              :pattern (checkers-pattern (mult (scaling 0.5 0.5 0.5)
                                               (translation-y 0.1))))))

(defparameter *glass-sphere*
  (make-sphere
   :transform (translation -0.5 1.0 0.0)
   :material (make-material
              :specular 0.5
              :diffuse 0.0
              :transparency 1.0
              :ambient 0.0
              :reflective 0.9
              :refractive-index glass)))

(defparameter *air-bubble*
  (make-sphere
   :transform (mult (scaling 0.5 0.5 0.5)
                    (translation -1.0 2.0 0.0))
   :material (make-material
              :specular 0.5
              :diffuse 0.0
              :ambient 0.0
              :transparency 1.0
              :reflective 0.9
              :refractive-index air)))

(defparameter *world*
  (make-world
   :objects (list *backdrop* *glass-sphere* *air-bubble*)
   :light (point-light
           (make-point -100.0 600.0 -250.0)
           (make-color :red 1.0 :green 1.0 :blue 1.0))))

(defparameter *camera*
  (create-camera
   1000
   1000
   (/ pi 4)
   (view-transform (make-point 0.0 1.0 -5.0)
                   (make-point 0.0 1.0 0.0)
                   (make-vec 0.0 1.0 0.0))))

(defparameter *canvas* (render *camera* *world*))
(canvas-to-ppm *canvas*)
