(in-package #:lisp-tracer-spheres)

(defclass sphere ()
  ((id
    :initarg :id
    :initform (gensym)
    :accessor id)))

(defun sphere! ()
  (make-instance 'sphere))

(defun intersect (sphere ray)
  (declare (sphere sphere) (ray ray)))
