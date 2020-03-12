(in-package #:lisp-tracer)

(defclass material ()
  ((material-color
    :initarg :material-color
    :initform (make-color 1 1 1)
    :accessor material-color
    :documentation "The MATERIAL has a COLOR.")
   (ambient
    :initarg :ambient
    :initform 0.1
    :accessor ambient
    :documentation "Ambience component of a MATERIAL.")
   (diffuse
    :initarg :diffuse
    :initform 0.9
    :accessor diffuse
    :documentation "Diffuse component of a MATERIAL.")
   (specular
    :initarg :specular
    :initform 0.9
    :accessor specular
    :documentation "Specular component of a MATERIAL.")
   (shininess
    :initarg :shininess
    :initform 200
    :accessor shininess
    :documentation "Specular component of a MATERIAL."))
  (:documentation "A MATERIAL with COLOR and AMBIENT, DIFFUSE, SPECULAR and SHININESS."))

(defun make-material ()
  "Create a MATERIAL with defaults set."
  (make-instance 'material))

(defun lighting (material light point eyev normalv)
  (let* ((effective-color (mult (material-color material) (intensity light)))
         (lightv (normalize (sub (posit light) point)))
         (ambient (mult effective-color (ambient material)))
         (light-dot-normal (dot lightv normalv)))
    (let (diffuse specular)
      (if (< light-dot-normal 0)
          (progn
            (setf diffuse (black))
            (setf specular (black)))
          (progn
            (setf diffuse (reduce #'mult (list effective-color
                                               (diffuse material)
                                               light-dot-normal)))
            (let ((reflect-dot-eye (dot (reflect (neg lightv) normalv) eyev)))
              (if (<= reflect-dot-eye 0)
                  (setf specular (black))
                  (setf specular (reduce #'mult (list (intensity light)
                                                      (specular material)
                                                      (expt reflect-dot-eye
                                                            (shininess material)))))))))
      (reduce #'add (list ambient diffuse specular)))))
