(in-package :lisp-tracer)

(defun default-world ()
  (let* ((l (point-light
             :position (make-point :x -10.0 :y 10.0 :z -10.0)
             :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
         (s1 (make-sphere))
         (s2 (make-sphere)))
    (with-slots (material) s1
      (with-slots (color diffuse specular) material
        (setf color (make-color :red 0.8 :green 1.0 :blue 0.6))
        (setf diffuse 0.7)
        (setf specular 0.2)))
    (with-slots (transform) s2
      (setf transform (scaling 0.5 0.5 0.5)))
    (make-world :objects (list s1 s2) :light l)))

(defun shade-hit (w comps &optional (remaining 5))
  (declare (type world w) (type computations comps))
  (with-slots (over-point object point eyev normalv) comps
    (with-slots (light) w
      (with-slots (material) object
        (let* ((shadowed? (is-shadowed? w over-point))
               (surface (lighting material object light point eyev normalv shadowed?))
               (reflected (reflected-color w comps remaining))
               (refracted (refracted-color w comps remaining)))
          (with-slots (reflective transparency) material
            (if (and (> reflective 0) (> transparency 0))
                (let ((reflectance (schlick comps)))
                  (add surface
                       (add (mult reflected reflectance)
                            (mult refracted (- 1 reflectance)))))
                (add (add surface reflected) refracted))))))))

(defun color-at (w r &optional (remaining 5))
  (declare (type world w) (type ray r))
  (let* ((hits (intersect-world w r))
         (hit (hit hits)))
    (if hit
        (shade-hit w (prepare-computations hit r hits) remaining)
        (make-color :red 0.0 :green 0.0 :blue 0.0))))

(defun is-shadowed? (world point)
  (declare (world world) (tuple point))
  (with-slots (light) world
    (with-slots (position) light
      (let* ((v (sub position point))
             (distance (magnitude v))
             (direction (normalize v))
             (ray (make-ray :origin point :direction direction))
             (xs (intersect-world world ray))
             (hit (hit xs)))
        (with-slots (tt) hit
          (and hit (< tt distance)))))))

(defun reflected-color (world comps &optional (remaining 5))
  (with-slots (object over-point reflectv) comps
    (with-slots (material) object
      (with-slots (reflective) material
        (if (or (equal? reflective 0.0) (<= remaining 0))
            (make-color :red 0.0 :green 0.0 :blue 0.0)
            (let* ((reflect-ray (make-ray :origin over-point :direction reflectv))
                   (color (color-at world reflect-ray (1- remaining))))
              (mult color reflective)))))))

(defun refract-computations (world comps remaining)
  "Helper function to decide whether we have total internal reflection."
  (with-slots (object n1 n2 eyev normalv under-point) comps
    (with-slots (material) object
      (with-slots (transparency) material
        (let* ((n-ratio (/ n1 n2))
               (cos-i (dot eyev normalv))
               (sin2-t (* (expt n-ratio 2) (- 1.0 (expt cos-i 2))))
               (snells-law (> sin2-t 1.0))
               (cos-t (sqrt (- 1.0 sin2-t)))
               (direction (sub (mult normalv (- (* n-ratio cos-i) (realpart cos-t)))
                               (mult eyev n-ratio)))
               (refract-ray (make-ray :origin under-point :direction direction))
               (color (mult (color-at world refract-ray (1- remaining)) transparency)))
          (values snells-law color))))))

(defun refracted-color (world comps &optional (remaining 5))
  (with-slots (object) comps
    (with-slots (material) object
      (with-slots (transparency) material
        (let ((black (make-color :red 0.0 :green 0.0 :blue 0.0))
              (transparency (equal? transparency 0.0)))
          (if (or transparency (<= remaining 0))
              black
              (multiple-value-bind (snells-law color)
                  (refract-computations world comps remaining)
                (if snells-law black color))))))))
