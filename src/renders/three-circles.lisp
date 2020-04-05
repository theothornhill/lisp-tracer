(in-package #:lisp-tracer-renders)

(defparameter *scene-floor*
  (make-sphere
   :transform (scaling 10.0 0.01 10.0)
   :material (make-material
              :color (make-color :red 1.0 :green 0.9 :blue 0.9)
              :specular 0.0
              :reflective 0.5
              :pattern (gradient-pattern (make-color :red 0.4 :green 0.9 :blue 0.9)
                                         (make-color :red 0.6 :green 0.5 :blue 0.2)
                                         (mult (translation -1.0 1.0 1.0)
                                               (scaling 2.0 1.0 1.0))))))

(defparameter *scene-left-wall*
  (make-sphere
   :material (make-material
              :color (make-color :red 1.0 :green 0.9 :blue 0.9)
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
              :color (make-color :red 0.1 :green 1.0 :blue 0.5)
              :specular 0.3
              :reflective 0.8
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
   :objects (list *scene-floor*
                  *scene-left-wall*
                  *scene-right-wall*
                  *middle*
                  *right*
                  *left*)
   :light (point-light
           (make-point -300.0 300.0 -500.0)
           (make-color :red 1.0
                       :green 1.0
                       :blue 1.0))))

(defparameter *camera*
  (create-camera
   1000
   500
   (/ pi 1.5)
   (view-transform (make-point 0.0 1.5 -5.0)
                   (make-point 0.0 1.0 0.0)
                   (make-vec 0.0 1.0 0.0))))

(defparameter *canvas* (render *camera* *world*))
(canvas-to-ppm *canvas*)
