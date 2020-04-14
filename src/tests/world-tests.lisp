(in-package #:lisp-tracer-tests)

(deftest create-world
  (testing "Creating a world"
    (let ((w (make-world)))
      (ok (null (world-objects w)))
      (ok (null (world-light w)))))
  (testing "The default world"
    (let* ((l (point-light :position (make-point :x -10.0 :y -10.0 :z -10.0)
                           :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
           (s1 (make-sphere))
           (s2 (make-sphere))
           (w (default-world)))
      (setf (material-color (shape-material s1)) (make-color :red 0.8 :green 1.0 :blue 0.6))
      (setf (material-diffuse (shape-material s1)) 0.7)
      (setf (material-specular (shape-material s1)) 0.2)
      (setf (shape-transform s2) (scaling 0.5 0.5 0.5))
      (ok (equal? (light-intensity (world-light w)) (light-intensity l)))
      (ok (equalp (car (world-objects w)) s1))
      (ok (equalp (cadr (world-objects w)) s2)))))

(deftest ray-world-intersection
  (testing "Intersect a world with a ray"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (xs (intersect-world w r)))
      (ok (equal? (length xs) 4))
      (ok (equal? (rt-intersection-tt (car xs)) 4.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) 4.5))
      (ok (equal? (rt-intersection-tt (caddr xs)) 5.5))
      (ok (equal? (rt-intersection-tt (cadddr xs)) 6)))))

(deftest shading-intersection
  (testing "Shading an intersection"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (shape (car (world-objects w)))
           (i (make-intersection :tt 4.0 :object shape))
           (comps (prepare-computations i r))
           (c (shade-hit w comps)))
      (ok (equal? c (make-color :red 0.38066
                                :green 0.47583
                                :blue 0.2855)))))
  (testing "Shading an intersection"
    (let ((w (default-world)))
      (setf (world-light w)
            (point-light :position (make-point :x 0.0 :y -0.25 :z 0.0)
                         :intensity (make-color :red 1.0 :green 1.0 :blue 1.0)))
      (let* ((r (make-ray :origin (make-point)
                          :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
             (shape (cadr (world-objects w)))
             (i (make-intersection :tt 0.5 :object shape))
             (comps (prepare-computations i r))
             (c (shade-hit w comps)))
        (ok (equal? c (make-color :red 0.90498
                                  :green 0.90498
                                  :blue 0.90498)))))))

(deftest color-at-world
  (testing "The color when a ray misses"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0)))
           (c (color-at w r)))
      (ok (equal? c (make-color :red 0.0 :green 0.0 :blue 0.0)))))
  (testing "The color when a ray hits"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (c (color-at w r)))
      (ok (equal? c (make-color :red 0.38066 :green 0.47583 :blue 0.2855)))))
  (testing "The color with an intersection behind the ray"
    (let* ((w (default-world)))
      (setf (material-ambient
             (shape-material
              (car (world-objects w))))
            1.0)
      (setf (material-ambient
             (shape-material
              (cadr (world-objects w))))
            1.0)
      (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z 0.75)
                          :direction (make-vec :x 0.0 :y 0.0 :z -1.0)))
             (c (color-at w r)))
        (ok (equal? c (material-color
                       (shape-material
                        (cadr (world-objects w))))))))))

(deftest shadow-testing
  (testing "There is no shadow when nothing is collinear with point and light"
    (let ((w (default-world))
          (p (make-point :x 0.0 :y 10.0 :z 0.0)))
      (ng (is-shadowed? w p))))
  (testing "The shadow when an object is between the point and the light"
    (let ((w (default-world))
          (p (make-point :x 10.0 :y -10.0 :z 10.0)))
      (ok (is-shadowed? w p))))
  (testing "There is no shadow when an object is behind the light"
    (let ((w (default-world))
          (p (make-point :x -20.0 :y 20.0 :z -20.0)))
      (ng (is-shadowed? w p))))
  (testing "There is no shadow when an object is behind the point"
    (let ((w (default-world))
          (p (make-point :x -2.0 :y 2.0 :z -2.0)))
      (ng (is-shadowed? w p)))))

(deftest shade-hit-shadowed
  (testing "shade-hit is given an intersection in shadow"
    (let* ((s1 (make-sphere))
           (s2 (make-sphere
                :transform (translation 0.0 0.0 10.0)))
           (w (make-world
               :objects (list s1 s2)
               :light (point-light
                       :position (make-point :x 0.0 :y 0.0 :z -10.0)
                       :intensity (make-color :red 1.0 :green 1.0 :blue 1.0))))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z 5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (i (make-intersection :tt 4.0 :object s2))
           (comps (prepare-computations i r))
           (c (shade-hit w comps)))
      (ok (equal? (make-color :red 0.1 :green 0.1 :blue 0.1) c)))))

(deftest refracted-world
  (testing "The refracted color with an opaque surface"
    (let* ((w (default-world))
           (shape (car (world-objects w)))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (xs (intersections
                (make-intersection :tt 4.0 :object shape)
                (make-intersection :tt 6.0 :object shape)))
           (comps (prepare-computations (car xs) r xs))
           (c (refracted-color w comps 5)))
      (ok (equal? c (make-color :red 0.0 :green 0.0 :blue 0.0)))))
  (testing "The refracted color at the maximum recursive depth"
    (let* ((w (default-world))
           (shape (car (world-objects w)))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0))))
      (setf (material-transparency (shape-material shape)) 1.0)
      (setf (material-refractive-index (shape-material shape)) 1.5)
      (let* ((xs (intersections
                  (make-intersection :tt 4.0 :object shape)
                  (make-intersection :tt 6.0 :object shape)))
             (comps (prepare-computations (car xs) r xs))
             (c (refracted-color w comps 0)))
        (ok (equal? c (make-color :red 0.0 :green 0.0 :blue 0.0))))))
  (testing "The refracted color under total internal reflection"
    (let* ((w (default-world))
           (shape (car (world-objects w)))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z (/ (sqrt 2) 2.0))
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0))))
      (setf (material-transparency (shape-material shape)) 1.0)
      (setf (material-refractive-index (shape-material shape)) 1.5)
      (let* ((xs (intersections
                  (make-intersection :tt (- (/ (sqrt 2) 2.0)) :object shape)
                  (make-intersection :tt (/ (sqrt 2) 2.0) :object shape)))
             (comps (prepare-computations (cadr xs) r xs))
             (c (refracted-color w comps 5)))
        (ok (equal? c (make-color :red 0.0 :green 0.0 :blue 0.0))))))
  (testing "The refracted color with a refracted ray"
    (let* ((w (default-world))
           (a (car (world-objects w)))
           (b (cadr (world-objects w)))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z 0.1)
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0))))
      (setf (material-ambient (shape-material a)) 1.0)
      (setf (material-pattern (shape-material a)) (test-pattern))
      (setf (material-transparency (shape-material b)) 1.0)
      (setf (material-refractive-index (shape-material b)) 1.5)
      (let* ((xs (intersections
                  (make-intersection :tt -0.9899 :object a)
                  (make-intersection :tt -0.4899 :object b)
                  (make-intersection :tt 0.4899 :object b)
                  (make-intersection :tt 0.9899 :object a)))
             (comps (prepare-computations (caddr xs) r xs))
             (c (refracted-color w comps 5)))
        (ok (equal? c (make-color :red 0.0 :green 0.998884539 :blue 0.04721945253)))))))

(deftest shading-refracting
  (testing "shade-hit with a transparent material"
    (let ((floor (make-plane
                  :transform (translation 0.0 -1.0 0.0)
                  :material (make-material
                             :transparency 0.5
                             :refractive-index 1.5)))
          (ball (make-sphere
                 :material (make-material
                            :color (make-color :red 1.0 :green 0.0 :blue 0.0)
                            :ambient 0.5)
                 :transform (translation 0.0 -3.5 -0.5)))
          (w (default-world)))
      (setf (world-objects w) (append (world-objects w) (list floor ball)))
      (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -3.0)
                          :direction (make-vec :x 0.0 :y
                                               (- (/ (sqrt 2) 2.0)) :z
                                               (/ (sqrt 2) 2.0))))
             (xs (intersections (make-intersection :tt (sqrt 2.0) :object floor)))
             (comps (prepare-computations (car xs) r xs))
             (color (shade-hit w comps 5)))
        (ok (equal? color (make-color :red 0.93642 :green 0.68642 :blue 0.68642))))))
  (testing "shade-hit with a transparent material"
    (let ((floor (make-plane
                  :transform (translation 0.0 -1.0 0.0)
                  :material (make-material
                             :transparency 0.5
                             :reflective 0.5
                             :refractive-index 1.5)))
          (ball (make-sphere
                 :material (make-material
                            :color (make-color :red 1.0 :green 0.0 :blue 0.0)
                            :ambient 0.5)
                 :transform (translation 0.0 -3.5 -0.5)))
          (w (default-world)))
      (setf (world-objects w) (append (world-objects w) (list floor ball)))
      (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -3.0)
                          :direction (make-vec :x 0.0 :y
                                               (- (/ (sqrt 2) 2.0)) :z
                                               (/ (sqrt 2) 2.0))))
             (xs (intersections (make-intersection :tt (sqrt 2.0) :object floor)))
             (comps (prepare-computations (car xs) r xs))
             (color (shade-hit w comps 5)))
        (ok (equal? color (make-color :red 0.93391
                                      :green 0.69643
                                      :blue 0.69243)))))))
