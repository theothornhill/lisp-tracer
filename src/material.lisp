(in-package #:lisp-tracer)

(defstruct material
  (color (make-color :red 1.0 :green 1.0 :blue 1.0))
  (pattern nil :type (or pattern null))
  (ambient 0.1 :type double-float)
  (diffuse 0.9 :type double-float)
  (specular 0.9 :type double-float)
  (shininess 200.0 :type double-float)
  (shadowed? nil :type t))

(defun lighting (material object light point eyev normalv shadowed?)
  (let* ((material-pattern (material-pattern material))
         (effective-color
           (mult (if material-pattern
                     (stripe-at-object material-pattern object point)
                     (material-color material))
                 (light-intensity light)))
         (lightv (normalize (sub (light-position light) point)))
         (ambient (mult effective-color (material-ambient material)))
         (light-dot-normal (dot lightv normalv)))
    (declare (type double-float light-dot-normal))
    (if (or (< light-dot-normal 0.0) shadowed?)
        ambient
        (let ((diffuse (mult effective-color
                             (mult (material-diffuse material)
                                   light-dot-normal)))
              (reflect-dot-eye (dot (reflect (neg lightv) normalv) eyev)))
          (declare (type double-float reflect-dot-eye))
          (if (<= reflect-dot-eye 0)
              (add ambient diffuse)
              (let ((specular
                      (mult (light-intensity light)
                            (mult
                             (material-specular material)
                             (expt reflect-dot-eye
                                   (material-shininess material))))))
                (add ambient (add diffuse specular))))))))
