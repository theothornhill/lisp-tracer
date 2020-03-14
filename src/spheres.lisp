(in-package #:lisp-tracer)

(defclass sphere ()
  ((id
    :initarg :id
    :initform (gensym)
    :accessor id)
   (transform-matrix
    :initarg :transform-matrix
    :initform (identity-matrix)
    :accessor transform-matrix)
   (sphere-material
    :initarg :sphere-material
    :initform (make-material)
    :accessor sphere-material)))

(defun make-sphere ()
  (make-instance 'sphere))

(defun set-transform (sphere transform)
  (declare (sphere sphere) (matrix transform))
  (setf (transform-matrix sphere) transform))

(defun normal-at (s world-point)
  (declare (sphere s) (tuple world-point)
           (optimize (speed 3) (safety 0)))
  (let* ((inverse-matrix (inverse (transform-matrix s)))
         (object-point (mult inverse-matrix world-point))
         (world-normal
           (mult (transpose inverse-matrix)
                 (sub object-point (make-point 0f0 0f0 0f0)))))
    (setf (tuple-w world-normal) 0f0)
    (normalize world-normal)))
