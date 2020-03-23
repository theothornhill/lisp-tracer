(in-package #:lisp-tracer)

(defstruct world
  (objects nil)
  (light nil))

(defun default-world ()
  (let* ((l (point-light (make-point -10.0 10.0 -10.0)
                         (make-color :red 1.0 :green 1.0 :blue 1.0)))
         (s1 (make-sphere))
         (s2 (make-sphere)))
    (setf (material-col (sphere-material s1)) (make-color :red 0.8 :green 1.0 :blue 0.6))
    (setf (material-diffuse (sphere-material s1)) 0.7)
    (setf (material-specular (sphere-material s1)) 0.2)
    (setf (sphere-transform s2) (scaling 0.5 0.5 0.5))
    (make-world
     :objects (list s1 s2)
     :light l)))

(defun shade-hit (w comps)
  (declare (type world w) (type computations comps))
  (lighting (sphere-material
             (computations-object comps))
            (world-light w)
            (computations-point comps)
            (computations-eyev comps)
            (computations-normalv comps)))

(defun color-at (w r)
  (declare (type world w) (type ray r)
           (optimize (speed 3) (safety 0)))
  (let ((hit (hit (intersect-world w r))))
    (if hit
        (shade-hit w (prepare-computations hit r))
        (make-color :red 0.0 :green 0.0 :blue 0.0))))
