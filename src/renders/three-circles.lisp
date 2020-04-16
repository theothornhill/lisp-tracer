(in-package #:lisp-tracer-renders)

(defparameter *scene-floor*
  (make-sphere
   :transform (scaling 10.0 0.01 10.0)
   :material (make-material
              :color (make-color :red 1.0 :green 0.9 :blue 0.9)
              :specular 0.0
              :reflective 0.5
              :pattern (gradient-pattern :a (make-color :red 0.4 :green 0.9 :blue 0.9)
                                         :b (make-color :red 0.6 :green 0.5 :blue 0.2)
                                         :transform (mult (translation -1.0 1.0 1.0)
                                                          (scaling 2.0 1.0 1.0))))))

(defparameter *scene-left-wall*
  (make-sphere
   :material (make-material
              :color (make-color :red 0.3 :green 0.3 :blue 0.3)
              :reflective 1.0
              :refractive-index diamond
              :specular 0.0)
   :transform (transform-object
               (scaling 10.0 0.01 10.0)
               (rotation-x (/ pi 2.0))
               (rotation-y (- (/ pi 4.0)))
               (translation 0.0 0.0 5.0))))

(defparameter *scene-right-wall*
  (make-sphere
   :material (make-material
              :color (make-color :red 1.0 :green 0.9 :blue 0.9)
              :specular 0.0)
   :transform (transform-object
               (scaling 10.0 0.01 10.0)
               (rotation-x (/ pi 2.0))
               (rotation-y (/ pi 4.0))
               (translation 0.0 0.0 5.0))))

(defparameter *middle*
  (make-sphere
   :transform (translation -0.5 1.0 0.5)
   :material (make-material
              :color (make-color :red 0.1 :green 0.4 :blue 0.5)
              :specular 0.3
              :reflective 0.4
              :transparency 0.6
              :refractive-index 1.333
              :diffuse 0.7)))

(defparameter *right*
  (make-cylinder
   :minimum 1.0
   :maximum 2.0
   :closed t
   :transform (mult (scaling 0.5 0.5 0.5)
                    (mult (translation 2.5 2.5 -0.5)
                          (rotation-x (/ pi 1.2))))
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
   :objects (list *scene-floor*
                  *scene-left-wall*
                  *scene-right-wall*
                  *middle*
                  *right*
                  *left*)
   :light (point-light
           :position (make-point :x -300.0 :y 300.0 :z -500.0)
           :intensity (make-color :red 1.0
                                  :green 1.0
                                  :blue 1.0))))

(defparameter *camera*
  (create-camera
   :hsize 1000
   :vsize 500
   :field-of-view (/ pi 3)
   :transform (view-transform
               :from (make-point :x 0.0 :y 1.5 :z -5.0)
               :to (make-point :x 0.0 :y 1.0 :z 0.0)
               :up (make-vec :x 0.0 :y 1.0 :z 0.0))))

(defparameter *canvas* (render *camera* *world*))
(canvas-to-ppm *canvas*)
