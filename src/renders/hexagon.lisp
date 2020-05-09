(in-package :lisp-tracer-renders)

(defun hexagon-corner ()
  (make-sphere
   :transform (mult (translation-z -1.0)
                    (scaling 0.25 0.25 0.25))))

(defun hexagon-edge ()
  (make-cylinder
   :minimum 0.0
   :maximum 1.0
   :transform (reduce #'mult (list (translation-z -1.0)
                                   (rotation-y (- (/ pi 6.0)))
                                   (rotation-z (- (/ pi 2.0)))
                                   (scaling 0.25 1.0 0.25)))))

(defun hexagon-side (&key (transform (identity-matrix)))
  (let ((side (make-group :transform transform)))
    (add-child side (hexagon-corner))
    (add-child side (hexagon-edge))
    side))

(defun hexagon ()
  (let ((hex (make-group)))
    (loop for i from 0 to 5 do
      (add-child
       hex
       (hexagon-side :transform (rotation-y (* i (/ pi 3.0))))))
    hex))

(defparameter *world*
  (make-world
   :objects (list (hexagon))
   :light (point-light
           :position (make-point :x -100.0 :y 600.0 :z -250.0)
           :intensity (make-color :red 1.0 :green 1.0 :blue 1.0))))

(defparameter *camera*
  (create-camera
   :transform (view-transform
               :from (make-point :x 2.0 :y 3.0 :z -5.0)
               :to (make-point :x 0.0 :y 1.0 :z 0.0)
               :up (make-vec :x 0.0 :y 1.0 :z 0.0))))

(defparameter *canvas* (render *camera* *world*))

(canvas-to-ppm *canvas*)
