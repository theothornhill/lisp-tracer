(in-package #:lisp-tracer-tests)

(deftest ray-sphere-intersect
  (testing "A ray intersects a sphere at two points"
    (let* ((r (make-ray (make-point 0 0 -5) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (car xs) 4.0))
      (ok (equal? (cadr xs) 6.0)))))

(deftest ray-intersect-tangent
  (testing "A ray intersect a sphere at a tangent"
    (let* ((r (make-ray (make-point 0 1 -5) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (car xs) 5.0))
      (ok (equal? (cadr xs) 5.0)))))

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
      (ok (equal? (car xs) -1.0))
      (ok (equal? (cadr xs) 1.0)))))

(deftest ray-intersect-outside
  (testing "A sphere is behind a ray"
    (let* ((r (make-ray (make-point 0 0 5) (make-vec 0 0 1)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (car xs) -6.0))
      (ok (equal? (cadr xs) -4.0)))))
