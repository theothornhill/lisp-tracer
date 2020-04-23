(in-package #:lisp-tracer)

(defstruct shape
  (parent nil :type (or shape null))
  (transform (identity-matrix) :type matrix)
  (material (make-material) :type (or material null)))

(defgeneric local-intersect (shape ray)
  (:documentation "Generic intersections for different shapes."))

(defgeneric local-normal-at (shape point)
  (:documentation "Generic normals for different shapes"))

(defun normal-at (shape point)
  (declare (shape shape) (tuple point))
  (let* ((inverse-matrix (inverse (shape-transform shape)))
         (local-point (mult inverse-matrix point))
         (local-normal (local-normal-at shape local-point))
         (world-normal (mult (transpose inverse-matrix) local-normal)))
    (setf (tuple-w world-normal) 0.0)
    (normalize world-normal)))
