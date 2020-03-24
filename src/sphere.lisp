(in-package #:lisp-tracer)

(defun normal-at (s world-point)
  (declare (sphere s) (tuple world-point))
  (let* ((inverse-matrix (inverse (sphere-transform s)))
         (object-point (mult inverse-matrix world-point))
         (world-normal
           (mult (transpose inverse-matrix)
                 (sub object-point (make-point 0.0 0.0 0.0)))))
    (setf (tuple-w world-normal) 0.0)
    (normalize world-normal)))
