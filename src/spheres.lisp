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
  (declare (sphere s) (tuple world-point))
  (let* ((inverse-matrix (inverse (transform-matrix s)))
         (object-point (mult inverse-matrix world-point))
         (world-normal
           (-> object-point
               (sub (make-point 0 0 0))
               (transform-object (transpose inverse-matrix)))))
    (setf (w world-normal) 0)
    (normalize world-normal)))
