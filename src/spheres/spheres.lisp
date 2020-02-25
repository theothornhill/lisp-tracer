(in-package #:lisp-tracer-spheres)

(defclass sphere ()
  ((id
    :initarg :id
    :initform (gensym)
    :accessor id)))

(defun make-sphere ()
  (make-instance 'sphere))
