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
  (let* ((local-point (world-to-object shape point))
         (local-normal (local-normal-at shape local-point)))
    (normal-to-world shape local-normal)))

