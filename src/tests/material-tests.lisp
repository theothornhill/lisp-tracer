(in-package :lisp-tracer-tests)

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

(defparameter *mock-sphere* (make-sphere))
(deftest light-testing
  (let ((m (make-material))
        (p (make-point)))
    (testing "Lighting with the eye between the light and the surface"
      (let* ((eyev (make-vec :x 0.0 :y 0.0 :z -1.0))
             (normalv (make-vec :x 0.0 :y 0.0 :z -1.0))
             (light (point-light :position (make-point :x 0.0 :y 0.0 :z -10.0)
                                 :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m *mock-sphere* light p eyev normalv nil)))
        (ok (equal? result (make-color :red 1.9 :green 1.9 :blue 1.9)))))
    (testing "Lighting with the eye between light and surface, eye offset 45°"
      (let* ((sqrt2/2 (/ (sqrt 2) 2.0))
             (eyev (make-vec :x 0.0 :y sqrt2/2 :z (- sqrt2/2)))
             (normalv (make-vec :x 0.0 :y 0.0 :z -1.0))
             (light (point-light :position (make-point :x 0.0 :y 0.0 :z -10.0)
                                 :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m *mock-sphere* light p eyev normalv nil)))
        (ok (equal? result (make-color :red 1.0 :green 1.0 :blue 1.0)))))
    (testing "Lighting with eye opposite surface, light offset 45°"
      (let* ((eyev (make-vec :x 0.0 :y 0.0 :z -1.0))
             (normalv (make-vec :x 0.0 :y 0.0 :z -1.0))
             (light (point-light :position (make-point :x 0.0 :y 10.0 :z -10.0)
                                 :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m *mock-sphere* light p eyev normalv nil)))
        (ok (equal? result (make-color :red 0.7364 :green 0.7364 :blue 0.7364)))))
    (testing "Lighting with eye in the path of the reflection vector"
      (let* ((sqrt2/2 (/ (sqrt 2) 2.0))
             (eyev (make-vec :x 0.0 :y (- sqrt2/2) :z (- sqrt2/2)))
             (normalv (make-vec :x 0.0 :y 0.0 :z -1.0))
             (light (point-light :position (make-point :x 0.0 :y 10.0 :z -10.0)
                                 :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m *mock-sphere* light p eyev normalv nil)))
        (ok (equal? result (make-color :red 1.6363930 :green 1.6363930 :blue 1.6363930)))))
    (testing "Lighting with the light behind the surface"
      (let* ((eyev (make-vec :x 0.0 :y 0.0 :z -1.0))
             (normalv (make-vec :x 0.0 :y 0.0 :z -1.0))
             (light (point-light :position (make-point :x 0.0 :y 0.0 :z 10.0)
                                 :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (result (lighting m *mock-sphere* light p eyev normalv nil)))
        (ok (equal? result (make-color :red 0.1 :green 0.1 :blue 0.1)))))
    (testing "Lighting with the surface in shadow"
      (let* ((eyev (make-vec :x 0.0 :y 0.0 :z -1.0))
             (normalv (make-vec :x 0.0 :y 0.0 :z -1.0))
             (light (point-light :position (make-point :x 0.0 :y 0.0 :z -10.0)
                                 :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (shadowed? t)
             (result (lighting m *mock-sphere* light p eyev normalv shadowed?)))
        (ok (equal? result (make-color :red 0.1 :green 0.1 :blue 0.1)))))
    (testing "Lighting with a pattern applied"
      (let* ((m (make-material
                 :ambient 1.0
                 :diffuse 0.0
                 :specular 0.0
                 :pattern (stripe-pattern)))
             (eyev (make-vec :x 0.0 :y 0.0 :z -1.0))
             (normalv (make-vec :x 0.0 :y 0.0 :z -1.0))
             (light (point-light :position (make-point :x 0.0 :y 0.0 :z -10.0)
                                 :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
             (c1 (lighting m *mock-sphere* light
                           (make-point :x 0.9 :y 0.0 :z 0.0)
                           eyev normalv nil))
             (c2 (lighting m *mock-sphere* light
                           (make-point :x 1.1 :y 0.0 :z 0.0)
                           eyev normalv nil)))
        (ok (equal? c1 (make-color :red 1.0 :green 1.0 :blue 1.0)))
        (ok (equal? c2 (make-color :red 0.0 :green 0.0 :blue 0.0)))))))

(deftest reflecting
  (testing "Reflectivity for the default material"
    (let ((material (make-material)))
      (ok (equal? (material-reflective material) 0.0))))
  (testing "Precomputing the reflection vector"
    (let* ((shape (make-plane))
           (r (make-ray :origin (make-point :x 0.0 :y 1.0 :z -1.0)
                        :direction (make-vec :x 0.0 :y
                                             (- (div (sqrt 2.0) 2.0)) :z
                                             (div (sqrt 2.0) 2.0))))
           (i (make-intersection :tt (sqrt 2.0) :object shape))
           (comps (prepare-computations i r)))
      (ok (equal? (computations-reflectv comps)
                  (make-vec :x 0.0 :y
                            (div (sqrt 2.0) 2.0) :z
                            (div (sqrt 2.0) 2.0))))))
  (testing "The reflected color for a nonreflective material"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (shape (cadr (world-objects w))))
      (setf (material-ambient (shape-material shape)) 1.0)
      (let* ((i (make-intersection :tt 1.0 :object shape))
             (comps (prepare-computations i r))
             (color (reflected-color w comps)))
        (ok (equal? color (make-color :red 0.0 :green 0.0 :blue 0.0))))))
  (testing "The reflected color for a reflective material"
    (let ((w (default-world))
          (shape (make-plane
                  :material (make-material :reflective 0.5)
                  :transform (translation 0.0 -1.0 0.0))))
      (setf (world-objects w) (append (world-objects w) (list shape)))
      (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -3.0)
                          :direction (make-vec :x 0.0 :y
                                               (- (div (sqrt 2.0) 2.0)) :z
                                               (div (sqrt 2.0) 2.0))))
             (i (make-intersection :tt (sqrt 2.0) :object shape))
             (comps (prepare-computations i r))
             (color (reflected-color w comps)))
        (ok (equal? color (make-color :red 0.1903323203
                                      :green 0.23791540
                                      :blue 0.142749240))))))
  (testing "shade-hit with a reflective material"
    (let ((w (default-world))
          (shape (make-plane
                  :material (make-material :reflective 0.5)
                  :transform (translation 0.0 -1.0 0.0))))
      (setf (world-objects w) (append (world-objects w) (list shape)))
      (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -3.0)
                          :direction (make-vec :x 0.0 :y
                                               (- (div (sqrt 2.0) 2.0)) :z
                                               (div (sqrt 2.0) 2.0))))
             (i (make-intersection :tt (sqrt 2.0) :object shape))
             (comps (prepare-computations i r))
             (color (shade-hit w comps)))
        (ok (equal? color (make-color :red 0.8767577
                                      :green 0.92434
                                      :blue 0.829174))))))
  (testing "color-at with mutually reflective surfaces"
    (let* ((light (point-light :position (make-point)
                               :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
           (lower (make-plane
                   :material (make-material
                              :reflective 1.0)
                   :transform (translation 0.0 -1.0 0.0)))
           (upper (make-plane
                   :material (make-material
                              :reflective 1.0)
                   :transform (translation 0.0 1.0 0.0)))
           (w (make-world
               :objects (list lower upper)
               :light light))
           (r (make-ray :origin (make-point)
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0))))
      (ok (color-at w r))))
  (testing "shade-hit with a reflective material"
    (let ((w (default-world))
          (shape (make-plane
                  :material (make-material :reflective 0.5)
                  :transform (translation 0.0 -1.0 0.0))))
      (setf (world-objects w) (append (world-objects w) (list shape)))
      (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -3.0)
                          :direction (make-vec :x 0.0 :y
                                               (- (div (sqrt 2.0) 2.0)) :z
                                               (div (sqrt 2.0) 2.0))))
             (i (make-intersection :tt (sqrt 2.0) :object shape))
             (comps (prepare-computations i r))
             (color (reflected-color w comps 0)))
        (ok (equal? color (make-color :red 0.0 :green 0.0 :blue 0.0)))))))

(deftest refracting-transparency
  (testing "Transparency and Refractive Index for the default material"
    (let ((m (make-material)))
      (ok (equal? (material-transparency m) 0.0))
      (ok (equal? (material-refractive-index m) 1.0)))))
