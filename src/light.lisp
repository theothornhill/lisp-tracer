(in-package #:lisp-tracer)

(defun point-light (position intensity)
  "Create a POINT-LIGHT with POSIT and INTENSITY"
  (declare (tuple position) (color intensity))
  (make-light
   :position position
   :intensity intensity))
