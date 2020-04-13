(in-package #:lisp-tracer)

(defun point-light (&key
                      (position (make-point :x 0.0 :y 0.0 :z 0.0))
                      (intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
  "Create a POINT-LIGHT with POSITION and INTENSITY"
  (declare (tuple position) (color intensity))
  (make-light
   :position position
   :intensity intensity))
