(in-package #:lisp-tracer)

(defclass light ()
  ((posit
    :initarg :posit
    :accessor posit
    :documentation "Position of light source")
   (intensity
    :initarg :intensity
    :accessor intensity
    :documentation "Intensity of the light source"))
  (:documentation "A light has a position and an intensity"))

(defun point-light (posit intensity)
  "Create a POINT-LIGHT with POSIT and INTENSITY"
  (declare (tuple posit) (color intensity))
  (make-instance 'light
                 :posit posit
                 :intensity intensity))
