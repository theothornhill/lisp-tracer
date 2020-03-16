(in-package #:lisp-tracer-tests)

(deftest material-test
  (testing "The default material"
    (let ((m (make-material)))
      (ok (equal? (material-col m) (make-color :red 1 :green 1 :blue 1)))
      (ok (equal? (material-col m) (make-color :red 1 :green 1 :blue 1)))
      (ok (equal? (material-ambient m) 0.1))
      (ok (equal? (material-diffuse m) 0.9))
      (ok (equal? (material-specular m) 0.9))
      (ok (equal? (material-shininess m) 200)))))

(deftest light-testing
  (let ((m (make-material))
        (p (make-point 0f0 0f0 0f0)))
    (testing "Lighting with the eye between the light and the surface"
      (let* ((eyev (make-vec 0f0 0f0 -1f0))
             (normalv (make-vec 0f0 0f0 -1f0))
             (light (point-light (make-point 0f0 0f0 -10f0)
                                 (make-color :red 1 :green 1 :blue 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color :red 1.9 :green 1.9 :blue 1.9)))))
    (testing "Lighting with the eye between light and surface, eye offset 45°"
      (let* ((sqrt2/2 (float (/ (sqrt 2) 2)))
             (eyev (make-vec 0f0 sqrt2/2 (- sqrt2/2)))
             (normalv (make-vec 0f0 0f0 -1f0))
             (light (point-light (make-point 0f0 0f0 -10f0)
                                 (make-color :red 1 :green 1 :blue 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color :red 1.0 :green 1.0 :blue 1.0)))))
    (testing "Lighting with eye opposite surface, light offset 45°"
      (let* ((eyev (make-vec 0f0 0f0 -1f0))
             (normalv (make-vec 0f0 0f0 -1f0))
             (light (point-light (make-point 0f0 10f0 -10f0)
                                 (make-color :red 1 :green 1 :blue 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color :red 0.7364 :green 0.7364 :blue 0.7364)))))
    (testing "Lighting with eye in the path of the reflection vector"
      (let* ((sqrt2/2 (float (/ (sqrt 2) 2)))
             (eyev (make-vec 0f0 (- sqrt2/2) (- sqrt2/2)))
             (normalv (make-vec 0f0 0f0 -1f0))
             (light (point-light (make-point 0f0 10f0 -10f0)
                                 (make-color :red 1 :green 1 :blue 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color :red 1.63638 :green 1.63638 :blue 1.63638)))))
    (testing "Lighting with the light behind the surface"
      (let* ((eyev (make-vec 0f0 0f0 -1f0))
             (normalv (make-vec 0f0 0f0 -1f0))
             (light (point-light (make-point 0f0 0f0 10f0)
                                 (make-color :red 1 :green 1 :blue 1)))
             (result (lighting m light p eyev normalv)))
        (ok (equal? result (make-color :red 0.1 :green 0.1 :blue 0.1)))))))
