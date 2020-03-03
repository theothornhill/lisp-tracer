(in-package #:lisp-tracer-spheres)

(defclass sphere ()
  ((id
    :initarg :id
    :initform (gensym)
    :accessor id)
   (transform-matrix
    :initarg :transform-matrix
    :initform (identity-matrix)
    :accessor transform-matrix)))

(defun make-sphere ()
  (make-instance 'sphere))

(defun set-transform (sphere transform)
  (declare (sphere sphere) (matrix transform))
  (setf (transform-matrix sphere) transform))
