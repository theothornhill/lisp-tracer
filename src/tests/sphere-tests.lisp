(in-package #:lisp-tracer-tests)

(deftest ray-sphere-intersect
  (testing "A ray intersects a sphere at two points"
    (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (tt (car xs)) 4.0))
      (ok (equal? (tt (cadr xs)) 6.0)))))

(deftest ray-intersect-tangent
  (testing "A ray intersect a sphere at a tangent"
    (let* ((r (make-ray (make-point 0 1 -5) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (tt (car xs)) 5.0))
      (ok (equal? (tt (cadr xs)) 5.0)))))

(deftest ray-intersect-miss
  (testing "A ray misses a sphere"
    (let* ((r (make-ray (make-point 0 2 -5) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 0)))))

(deftest ray-intersect-inside
  (testing "A ray originates inside a sphere"
    (let* ((r (make-ray (make-point 0 0 0) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (tt (car xs)) -1.0))
      (ok (equal? (tt (cadr xs)) 1.0)))))

(deftest ray-intersect-outside
  (testing "A sphere is behind a ray"
    (let* ((r (make-ray (make-point 0 0 5) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (tt (car xs)) -6.0))
      (ok (equal? (tt (cadr xs)) -4.0)))))

(deftest intersect-set-object
  (testing "Intersect sets the object on the intersection"
    (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (eq (object (first xs)) s))
      (ok (eq (object (second xs)) s)))))

(deftest sphere-transformations
  (testing "A sphere's default transformation"
    (let ((s (make-sphere)))
      (ok (equal? (transform-matrix s) (identity-matrix)))))
  (testing "Changing a sphere's transformation"
    (let ((s (make-sphere))
          (trans (translation 2 3 4)))
      (set-transform s trans)
      (ok (equal? (transform-matrix s) trans)))))

(deftest intersecting-spheres
  (testing "Intersecting a scaled sphere with a ray"
    (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
           (s (make-sphere)))
      (set-transform s (scaling 2 2 2))
      (let ((xs (intersect s r)))
        (ok (= (length xs) 2))
        (ok (= (tt (car xs)) 3))
        (ok (= (tt (cadr xs)) 7)))))
  (testing "Intersecting a translated sphere with a ray"
    (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
           (s (make-sphere)))
      (set-transform s (translation 5 0 0))
      (let ((xs (intersect s r)))
        (ok (= (length xs) 0))))))

