(in-package #:lisp-tracer-tests)

(deftest ray-sphere-intersect
  (testing "A ray intersects a sphere at two points"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (car xs)) 4.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) 6.0)))))

(deftest ray-intersect-tangent
  (testing "A ray intersect a sphere at a tangent"
    (let* ((r (make-ray :origin (make-point 0.0 1.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (car xs)) 5.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) 5.0)))))

(deftest ray-intersect-miss
  (testing "A ray misses a sphere"
    (let* ((r (make-ray :origin (make-point 0.0 2.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 0)))))

(deftest ray-intersect-inside
  (testing "A ray originates inside a sphere"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 0.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (car xs)) -1.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) 1.0)))))

(deftest ray-intersect-outside
  (testing "A sphere is behind a ray"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (car xs)) -6.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) -4.0)))))

(deftest intersect-set-object
  (testing "Intersect sets the object on the intersection"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (eq (rt-intersection-object (first xs)) s))
      (ok (eq (rt-intersection-object (second xs)) s)))))

(deftest sphere-transformations
  (testing "The default transformation"
    (let ((s (make-shape)))
      (ok (equal? (shape-transform s) (identity-matrix)))))
  (testing "Assigning a transformation"
    (let ((s (make-shape))
          (trans (translation 2.0 3.0 4.0)))
      (setf (shape-transform s) trans)
      (ok (equal? (shape-transform s) trans)))))

(deftest intersecting-spheres
  (testing "Intersecting a scaled sphere with a ray"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (s (make-sphere)))
      (setf (shape-transform s) (scaling 2.0 2.0 2.0))
      (let ((xs (intersect s r)))
        (ok (= (length xs) 2))
        (ok (= (rt-intersection-tt (car xs)) 3.0))
        (ok (= (rt-intersection-tt (cadr xs)) 7.0)))))
  (testing "Intersecting a translated sphere with a ray"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (s (make-sphere)))
      (setf (shape-transform s) (translation 5.0 0.0 0.0))
      (let ((xs (intersect s r)))
        (ok (= (length xs) 0))))))

(deftest normal-testing
  (testing "The normal on a sphere at a point on the x axis"
    (let* ((s (make-sphere))
           (n (normal-at s (make-point 1.0 0.0 0.0))))
      (ok (equal? n (make-vec 1.0 0.0 0.0)))))
  (testing "The normal on a sphere at a point on the y axis"
    (let* ((s (make-sphere))
           (n (normal-at s (make-point 0.0 1.0 0.0))))
      (ok (equal? n (make-vec 0.0 1.0 0.0)))))
  (testing "The normal on a sphere at a point on the z axis"
    (let* ((s (make-sphere))
           (n (normal-at s (make-point 0.0 0.0 1.0))))
      (ok (equal? n (make-vec 0.0 0.0 1.0)))))
  (testing "The normal on a sphere at a nonaxial point"
    (let* ((s (make-sphere))
           (sqrt3/3 (/ (sqrt 3) 3.0))
           (n (normal-at s (make-point sqrt3/3 sqrt3/3 sqrt3/3))))
      (ok (equal? n (make-vec sqrt3/3 sqrt3/3 sqrt3/3)))))
  (testing "The normal is a normalized vector"
    (let* ((s (make-sphere))
           (sqrt3/3 (/ (sqrt 3) 3.0))
           (n (normal-at s (make-point sqrt3/3 sqrt3/3 sqrt3/3))))
      (ok (equal? n (normalize n))))))

(deftest normalizing-transformed-sphere
  (testing "Computing the normal on a translated sphere"
    (let* ((s (make-sphere)))
      (setf (shape-transform s) (translation 0.0 1.0 0.0))
      (let ((n (normal-at s (make-point 0.0 1.70711 -0.70711))))
        (ok (equal? n (make-vec 0.0 0.70711 -0.70711))))))
  (testing "Computing the normal on a transformed sphere"
    (let* ((s (make-sphere))
           (sqrt2/2 (/ (sqrt 2) 2.0))
           (m (transform-object
               (rotation-z (div (coerce pi 'double-float) 5.0))
               (scaling 1.0 0.5 1.0))))
      (setf (shape-transform s) m)
      (let ((n (normal-at s (make-point 0.0 sqrt2/2 (neg sqrt2/2)))))
        (ok (equal? n (make-vec 0.0 0.97014 -0.24254)))))))

(deftest sphere-materials
  (testing "The default material"
    (let ((s (make-shape)))
      (ok (equal? (shape-material s) (make-material)))))
  (testing "Assigning a material"
    (let ((s (make-shape))
          (m (make-material)))
      (setf (material-ambient m) 1.0)
      (setf (shape-material s) m)
      (ok (equal? (shape-material s) m)))))
