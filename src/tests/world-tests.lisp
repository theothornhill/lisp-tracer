(in-package #:lisp-tracer-tests)

(deftest create-world
  (testing "Creating a world"
    (let ((w (make-world)))
      (ok (null (world-objects w)))
      (ok (null (world-light w)))))
  (testing "The default world"
    (let* ((l (point-light (make-point -10.0 -10.0 -10.0)
                           (make-color :red 1.0 :green 1.0 :blue 1.0)))
           (s1 (make-sphere))
           (s2 (make-sphere))
           (w (default-world)))
      (setf (material-col (sphere-material s1)) (make-color :red 0.8 :green 1.0 :blue 0.6))
      (setf (material-diffuse (sphere-material s1)) 0.7)
      (setf (material-specular (sphere-material s1)) 0.2)
      (setf (sphere-matrix s2) (scaling 0.5 0.5 0.5))
      (ok (equal? (light-intensity (world-light w)) (light-intensity l)))
      (ok (equalp (car (world-objects w)) s1))
      (ok (equalp (cadr (world-objects w)) s2)))))
