(in-package :lisp-tracer)

(defconstant vacuum 1.0)
(defconstant air 1.00029)
(defconstant water 1.333)
(defconstant glass 1.52)
(defconstant diamond 2.417)

(defstruct material
  (color (make-color :red 1.0 :green 1.0 :blue 1.0))
  (pattern nil :type (or pattern null))
  (ambient 0.1 :type double-float)
  (diffuse 0.9 :type double-float)
  (specular 0.9 :type double-float)
  (shininess 200.0 :type double-float)
  (transparency 0.0 :type double-float)
  (refractive-index 1.0 :type double-float)
  (reflective 0.0 :type double-float)
  (shadowed? nil :type t))

(defun lighting (material object light point eyev normalv shadowed?)
  (with-slots (pattern color shininess ambient diffuse specular) material
    (with-slots (position intensity) light
      (let* ((effective-color
               (mult (if pattern
                         (pattern-at-object pattern object point)
                         color)
                     intensity))
             (lightv (normalize (sub position point)))
             (ambient (mult effective-color ambient))
             (light-dot-normal (dot lightv normalv)))
        (declare (type double-float light-dot-normal))
        (if (or (< light-dot-normal 0.0) shadowed?)
            ambient
            (let ((diffuse (mult effective-color
                                 (mult diffuse light-dot-normal)))
                  (reflect-dot-eye (dot (reflect (neg lightv) normalv) eyev)))
              (declare (type double-float reflect-dot-eye))
              (if (<= reflect-dot-eye 0)
                  (add ambient diffuse)
                  (let ((specular
                          (mult intensity
                                (mult
                                 specular
                                 (expt reflect-dot-eye shininess)))))
                    (add ambient (add diffuse specular))))))))))
