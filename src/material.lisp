(in-package #:lisp-tracer)

(defun lighting (material light point eyev normalv shadowed?)
  (let* ((effective-color (mult (material-color material) (light-intensity light)))
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
