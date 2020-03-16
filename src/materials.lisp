(in-package #:lisp-tracer)

(defstruct material
  (col (make-color :red 1.0 :green 1.0 :blue 1.0))
  (ambient 0.1)
  (diffuse 0.9)
  (specular 0.9)
  (shininess 200.0))

(defun lighting (material light point eyev normalv)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((effective-color (mult (material-col material) (intensity light)))
         (lightv (normalize (sub (posit light) point)))
         (material-ambient (mult effective-color (material-ambient material)))
         (light-dot-normal (dot lightv normalv))
         (black (make-color)))
    (let (material-diffuse material-specular)
      (declare (type single-float light-dot-normal))
      (if (< light-dot-normal 0.0)
          (progn
            (setf material-diffuse black)
            (setf material-specular black))
          (progn
            (setf material-diffuse (mult effective-color
                                (mult (material-diffuse material) light-dot-normal)))
            (let ((reflect-dot-eye (dot (reflect (neg lightv) normalv) eyev)))
              (declare (type single-float reflect-dot-eye))
              (if (<= reflect-dot-eye 0.0)
                  (setf material-specular black)
                  (setf material-specular (mult (intensity light)
                                       (mult
                                        (material-specular material)
                                        (expt reflect-dot-eye
                                              (material-shininess material)))))))))
      (add material-ambient (add material-diffuse material-specular)))))
