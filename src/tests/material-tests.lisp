(in-package #:lisp-tracer-tests)

(deftest material-test
  (testing "The default material"
    (let ((m (make-material)))
      (ok (equal? (material-color m)
                  (make-color :red 1.0 :green 1.0 :blue 1.0)))
      (ok (equal? (material-color m)
                  (make-color :red 1.0 :green 1.0 :blue 1.0)))
      (ok (equal? (material-ambient m) 0.1))
      (ok (equal? (material-diffuse m) 0.9))
      (ok (equal? (material-specular m) 0.9))
      (ok (equal? (material-shininess m) 200)))))

(deftest light-testing
  (let ((m (make-material))
        (p (make-point 0.0 0.0 0.0)))
    (testing "Lighting with the eye between the light and the surface"
      (let* ((eyev (make-vec 0.0 0.0 -1.0))
             (normalv (make-vec 0.0 0.0 -1.0))
             (light (point-light (make-point 0.0 0.0 -10.0)
                                 (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m light p eyev normalv nil)))
        (ok (equal? result (make-color :red 1.9 :green 1.9 :blue 1.9)))))
    (testing "Lighting with the eye between light and surface, eye offset 45°"
      (let* ((sqrt2/2 (/ (sqrt 2) 2.0))
             (eyev (make-vec 0.0 sqrt2/2 (- sqrt2/2)))
             (normalv (make-vec 0.0 0.0 -1.0))
             (light (point-light (make-point 0.0 0.0 -10.0)
                                 (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m light p eyev normalv nil)))
        (ok (equal? result (make-color :red 1.0 :green 1.0 :blue 1.0)))))
    (testing "Lighting with eye opposite surface, light offset 45°"
      (let* ((eyev (make-vec 0.0 0.0 -1.0))
             (normalv (make-vec 0.0 0.0 -1.0))
             (light (point-light (make-point 0.0 10.0 -10.0)
                                 (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m light p eyev normalv nil)))
        (ok (equal? result (make-color :red 0.7364 :green 0.7364 :blue 0.7364)))))
    (testing "Lighting with eye in the path of the reflection vector"
      (let* ((sqrt2/2 (/ (sqrt 2) 2.0))
             (eyev (make-vec 0.0 (- sqrt2/2) (- sqrt2/2)))
             (normalv (make-vec 0.0 0.0 -1.0))
             (light (point-light (make-point 0.0 10.0 -10.0)
                                 (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m light p eyev normalv nil)))
        (ok (equal? result (make-color :red 1.6363930 :green 1.6363930 :blue 1.6363930)))))
    (testing "Lighting with the light behind the surface"
      (let* ((eyev (make-vec 0.0 0.0 -1.0))
             (normalv (make-vec 0.0 0.0 -1.0))
             (light (point-light (make-point 0.0 0.0 10.0)
                                 (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m light p eyev normalv nil)))
        (ok (equal? result (make-color :red 0.1 :green 0.1 :blue 0.1)))))
    (testing "Lighting with the surface in shadow"
      (let* ((eyev (make-vec 0.0 0.0 -1.0))
             (normalv (make-vec 0.0 0.0 -1.0))
             (light (point-light (make-point 0.0 0.0 -10.0)
                                 (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (shadowed? t)
             (result (lighting m light p eyev normalv shadowed?)))
        (ok (equal? result (make-color :red 0.1 :green 0.1 :blue 0.1)))))
    (testing "Lighting with a pattern applied"
      (let* ((m (make-material
                 :ambient 1.0
                 :diffuse 0.0
                 :specular 0.0
                 :pattern (stripe-pattern (make-color :red 1.0 :green 1.0 :blue 1.0)
                                          (make-color :red 0.0 :green 0.0 :blue 0.0))))
             (eyev (make-vec 0.0 0.0 -1.0))
             (normalv (make-vec 0.0 0.0 -1.0))
             (light (point-light (make-point 0.0 0.0 -10.0)
                                 (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (c1 (lighting m light
                           (make-point 0.9 0.0 0.0)
                           eyev normalv nil))
             (c2 (lighting m light
                           (make-point 1.1 0.0 0.0)
                           eyev normalv nil)))
        (ok (equal? c1 (make-color :red 1.0 :green 1.0 :blue 1.0)))
        (ok (equal? c2 (make-color :red 0.0 :green 0.0 :blue 0.0)))))))
