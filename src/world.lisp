(in-package #:lisp-tracer)

(defun default-world ()
  (let* ((l (point-light (make-point -10.0 10.0 -10.0)
                         (make-color :red 1.0 :green 1.0 :blue 1.0)))
         (s1 (make-sphere))
         (s2 (make-sphere)))
    (setf (material-color (shape-material s1)) (make-color :red 0.8 :green 1.0 :blue 0.6))
    (setf (material-diffuse (shape-material s1)) 0.7)
    (setf (material-specular (shape-material s1)) 0.2)
    (setf (shape-transform s2) (scaling 0.5 0.5 0.5))
    (make-world
     :objects (list s1 s2)
     :light l)))

(defun shade-hit (w comps &optional (remaining 5))
  (declare (type world w) (type computations comps))
  (let* ((shadowed? (is-shadowed? w (computations-over-point comps)))
         (material (shape-material (computations-object comps)))
         (surface (lighting material
                            (computations-object comps)
                            (world-light w)
                            (computations-point comps)
                            (computations-eyev comps)
                            (computations-normalv comps)
                            shadowed?))
         (reflected (reflected-color w comps remaining)))
    (add surface reflected)))

(defun color-at (w r &optional (remaining 5))
  (declare (type world w) (type ray r))
  (let ((hit (hit (intersect-world w r))))
    (if hit
        (shade-hit w (prepare-computations hit r) remaining)
        (make-color :red 0.0 :green 0.0 :blue 0.0))))

(defun is-shadowed? (world point)
  (declare (world world) (tuple point))
  (let* ((v (sub (light-position (world-light world)) point))
         (distance (magnitude v))
         (direction (normalize v))
         (ray (make-ray :origin point :direction direction))
         (xs (intersect-world world ray))
         (hit (hit xs)))
    (and hit (< (rt-intersection-tt hit) distance))))

(defun reflected-color (world comps &optional (remaining 5))
  (let ((reflective (material-reflective
                     (shape-material
                      (computations-object comps)))))
    (if (or (equal? reflective 0.0) (<= remaining 0))
        (make-color :red 0.0 :green 0.0 :blue 0.0)
        (let* ((reflect-ray
                 (make-ray :origin (computations-over-point comps)
                           :direction (computations-reflectv comps)))
               (color (color-at world reflect-ray (1- remaining))))
          (mult color reflective)))))
