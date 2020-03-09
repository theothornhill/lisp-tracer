(in-package #:lisp-tracer-lights)

(defclass light ()
  ((posit
    :initarg :posit
    :accessor posit
    :documentation "Position of light source")
   (intensity
    :initarg :intensity
    :accessor intensity
    :documentation "Intensity of the light source")))

(defun point-light (posit intensity)
  (make-instance 'light
                 :posit posit
                 :intensity intensity))
