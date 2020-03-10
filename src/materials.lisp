(in-package #:lisp-tracer)

(defclass material ()
  ((material-color
    :initarg :material-color
    :initform (make-color 1 1 1)
    :accessor material-color)
   (ambient
    :initarg :ambient
    :initform 0.1
    :accessor ambient)
   (diffuse
    :initarg :diffuse
    :initform 0.9
    :accessor diffuse)
   (specular
    :initarg :specular
    :initform 0.9
    :accessor specular)
   (shininess
    :initarg :shininess
    :initform 200
    :accessor shininess)))

(defun make-material ()
  (make-instance 'material))
