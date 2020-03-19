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

(deftest ray-world-intersection
  (testing "Intersect a world with a ray"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (xs (intersect-world w r)))
      (ok (equal? (length xs) 4))
      (ok (equal? (rt-intersection-tt (car xs)) 4.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) 4.5))
      (ok (equal? (rt-intersection-tt (caddr xs)) 5.5))
      (ok (equal? (rt-intersection-tt (cadddr xs)) 6)))))

(deftest shading-intersection
  (testing "Shading an intersection"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (shape (car (world-objects w)))
           (i (make-intersection 4.0 shape))
           (comps (prepare-computations i r))
           (c (shade-hit w comps)))
      (ok (equal? c (make-color :red 0.38066
                                :green 0.47583
                                :blue 0.2855)))))
  (testing "Shading an intersection"
    (let ((w (default-world)))
      (setf (world-light w)
            (point-light (make-point 0.0 -0.25 0.0)
                         (make-color :red 1.0 :green 1.0 :blue 1.0)))
      (let* ((r (make-ray :origin (make-point 0.0 0.0 0.0)
                          :direction (make-vec 0.0 0.0 1.0)))
             (shape (cadr (world-objects w)))
             (i (make-intersection 0.5 shape))
             (comps (prepare-computations i r))
             (c (shade-hit w comps)))
        (ok (equal? c (make-color :red 0.90498
                                  :green 0.90498
                                  :blue 0.90498)))))))

(deftest color-at-world
  (testing "The color when a ray misses"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 1.0 0.0)))
           (c (color-at w r)))
      (ok (equal? c (make-color :red 0.0 :green 0.0 :blue 0.0)))))
  (testing "The color when a ray hits"
    (let* ((w (default-world))
           (r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (c (color-at w r)))
      (ok (equal? c (make-color :red 0.38066 :green 0.47583 :blue 0.2855)))))
  (testing "The color with an intersection behind the ray"
    (let* ((w (default-world)))
      (setf (material-ambient
             (sphere-material
              (car (world-objects w))))
            1.0)
      (setf (material-ambient
             (sphere-material
              (cadr (world-objects w))))
            1.0)
      (let* ((r (make-ray :origin (make-point 0.0 0.0 0.75)
                          :direction (make-vec 0.0 0.0 -1.0)))
             (c (color-at w r)))
        (ok (equal? c (material-col
                       (sphere-material
                        (cadr (world-objects w))))))))))
