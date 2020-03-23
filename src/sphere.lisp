(in-package #:lisp-tracer)

(defstruct sphere
  (id 1)
  (transform (identity-matrix))
  (material (make-material)))

(defun set-transform (sphere transform)
  (declare (sphere sphere) (matrix transform))
  (setf (sphere-transform sphere) transform))

(defun normal-at (s world-point)
  (declare (sphere s) (tuple world-point)
           (optimize (speed 3) (safety 0)))
  (let* ((inverse-matrix (inverse (sphere-transform s)))
         (object-point (mult inverse-matrix world-point))
         (world-normal
           (mult (transpose inverse-matrix)
                 (sub object-point (make-point 0f0 0f0 0f0)))))
    (setf (tuple-w world-normal) 0f0)
    (normalize world-normal)))
