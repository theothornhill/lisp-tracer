(in-package #:lisp-tracer-canvas)

(defclass canvas ()
  ((width
    :initarg :width
    :accessor width)
   (height
    :initarg :height
    :accessor height)
   (grid
    :initarg :grid
    :accessor grid)))

(defun make-grid (width height)
  (loop for i from 0 to height collect
       (loop for j from 0 to width collect (color! 0 0 0))))


(defun canvas! (width height)
  (make-instance 'canvas
                 :width width
                 :height height
                 :grid (make-grid width height)))
