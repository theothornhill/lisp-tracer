(in-package #:lisp-tracer-tests)

(deftest material-test
  (testing "The default material"
    (let ((m (make-material)))
      (ok (equal? (material-color m) (make-color 1 1 1)))
      (ok (equal? (material-color m) (make-color 1 1 1)))
      (ok (equal? (ambient m) 0.1))
      (ok (equal? (diffuse m) 0.9))
      (ok (equal? (specular m) 0.9))
      (ok (equal? (shininess m) 200)))))

(deftest light-testing
  (let ((m (make-material))
        (p (make-point 0 0 0)))
    (testing "Lighting with the eye between the light and the surface"
      (let* ((eyev (make-vec 0 0 -1))
             (normalv (make-vec 0 0 -1))
             (light (point-light (make-point 0 0 -10)
                                 (make-color 1 1 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color 1.9 1.9 1.9)))))
    (testing "Lighting with the eye between light and surface, eye offset 45°"
      (let* ((sqrt2/2 (/ (sqrt 2) 2))
             (eyev (make-vec 0 sqrt2/2 (- sqrt2/2)))
             (normalv (make-vec 0 0 -1))
             (light (point-light (make-point 0 0 -10)
                                 (make-color 1 1 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color 1.0 1.0 1.0)))))
    (testing "Lighting with eye opposite surface, light offset 45°"
      (let* ((eyev (make-vec 0 0 -1))
             (normalv (make-vec 0 0 -1))
             (light (point-light (make-point 0 10 -10)
                                 (make-color 1 1 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color 0.7364 0.7364 0.7364)))))
    (testing "Lighting with eye in the path of the reflection vector"
      (let* ((sqrt2/2 (/ (sqrt 2) 2))
             (eyev (make-vec 0 (- sqrt2/2) (- sqrt2/2)))
             (normalv (make-vec 0 0 -1))
             (light (point-light (make-point 0 10 -10)
                                 (make-color 1 1 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color 1.63638 1.63638 1.63638)))))
    (testing "Lighting with the light behind the surface"
      (let* ((eyev (make-vec 0 0 -1))
             (normalv (make-vec 0 0 -1))
             (light (point-light (make-point 0 0 10)
                                 (make-color 1 1 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color 0.1 0.1 0.1)))))))
