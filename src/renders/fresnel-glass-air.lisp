(in-package #:lisp-tracer-renders)

(defparameter *backdrop*
  (make-plane
   :transform (mult (rotation-x (/ pi 2))
                    (translation 0.0 5.0 0.0))
   :material (make-material
              :pattern (checkers-pattern :transform
                                         (mult (scaling 0.5 0.5 0.5)
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
           :position (make-point :x -100.0 :y 600.0 :z -250.0)
           :intensity (make-color :red 1.0 :green 1.0 :blue 1.0))))

(defparameter *camera*
  (create-camera
   :transform (view-transform
               :from (make-point :x 0.0 :y 1.0 :z -5.0)
               :to (make-point :x 0.0 :y 1.0 :z 0.0)
               :up (make-vec :x 0.0 :y 1.0 :z 0.0))))

(defparameter *canvas* (render *camera* *world*))
(canvas-to-ppm *canvas*)
