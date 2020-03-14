(in-package #:lisp-tracer)

(defparameter *canvas* (make-canvas 200 100))

(defun create-pixel (x)
  (let* ((white (make-color 1 1 1))
         (radians (mult x (div pi 6)))
         (r (transform-object (make-point 0f0 0f0 0f0)
                              (translation 0 30 0)
                              (rotation-z radians)
                              (translation 100 50 0))))
    (let* ((pixel (to-pixel r))
           (i (tuple-x pixel))
           (j (tuple-y pixel)))
      (write-pixel *canvas* i j white))))

(defun clock ()
  (map 'list #'create-pixel
       (iter (for i from 1 to 12)
         (collect i))))

;(clock)


(canvas-to-ppm *canvas*)
