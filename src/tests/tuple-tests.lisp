(in-package #:lisp-tracer-tests)

(deftest point-testing
  (testing "A tuple with w=1.0 is a point"
    (let ((a (tuple! 4.3 -4.2 3.1 1.0)))
      (ok (= (x a) 4.3))
      (ok (= (y a) -4.2))
      (ok (= (z a) 3.1))
      (ok (= (w a) 1.0))
      (ok (point? a))
      (ng (vec? a))))
  (testing "point creates tuples with w=1"
    (let ((p (point! 4 -4 3)))
      (ok (= (w p) 1))
      (ok (equal? p (tuple! 4 -4 3 1))))))

(deftest vec-testing
  (testing "A tuple with w=1.0 is a point"
    (let ((a (tuple! 4.3 -4.2 3.1 0.0)))
      (ok (= (x a) 4.3))
      (ok (= (y a) -4.2))
      (ok (= (z a) 3.1))
      (ok (= (w a) 0.0))
      (ng (point? a))
      (ok (vec? a))))
  (testing "point creates tuples with w=1"
    (let ((v (vec! 4 -4 3)))
      (ok (= (w v) 0))
      (ok (equal? v (tuple! 4 -4 3 0))))))


(deftest tuple-math
  (testing "Adding two tuples"
    (let ((a1 (tuple! 3 -2 5 1))
          (a2 (tuple! -2 3 1 0)))
      (ok (equal? (add a1 a2) (tuple! 1 1 6 1)))))
  (testing "Subtracting two points"
    (let ((p1 (point! 3 2 1))
          (p2 (point! 5 6 7)))
      (ok (equal? (sub p1 p2) (vec! -2 -4 -6)))))
  (testing "Subtracting a vector from a point"
    (let ((p (point! 3 2 1))
          (v (vec! 5 6 7)))
      (ok (equal? (sub p v) (point! -2 -4 -6)))))
  (testing "Subtracting two vectors"
    (let ((v1 (vec! 3 2 1))
          (v2 (vec! 5 6 7)))
      (ok (equal? (sub v1 v2) (vec! -2 -4 -6)))))
  (testing "Subtracting a vector from zero vector"
    (let ((zero (zerovec))
          (v (vec! 1 -2 3)))
      (ok (equal? (sub zero v) (vec! -1 2 -3)))))
  (testing "Negating a tuple!"
    (let ((a (tuple! 1 -2 3 -4))
          (-a (tuple! -1 2 -3 4)))
      (ok (equal? (neg a) -a)))))


(deftest scalar-testing
  (testing "Mutltiplying a tuple by a scalar"
    (let ((a (tuple! 1 -2 3 -4)))
      (ok (equal? (mult a 3.5) (tuple! 3.5 -7 10.5 -14)))))
  (testing "Mutltiplying a tuple by a fraction"
    (let ((a (tuple! 1 -2 3 -4)))
      (ok (equal? (mult a 0.5) (tuple! 0.5 -1 1.5 -2)))))
  (testing "Dividing a tuple by a scalar"
    (let ((a (tuple! 1 -2 3 -4)))
      (ok (equal? (div a 2) (tuple! 0.5 -1 1.5 -2)))))
  (testing "Computing the magnitude of (vec 1 0 0)"
    (let ((v (vec! 1 0 0)))
      (ok (equal? (magnitude v) 1))))
  (testing "Computing the magnitude of (vec 0 1 0)"
    (let ((v (vec! 0 1 0)))
      (ok (equal? (magnitude v) 1))))
  (testing "Computing the magnitude of (vec 0 0 1)"
    (let ((v (vec! 0 0 1)))
      (ok (equal? (magnitude v) 1))))
  (testing "Computing the magnitude of (vec 1 2 3)"
    (let ((v (vec! 1 2 3)))
      (ok (equal? (magnitude v) (sqrt 14)))))
  (testing "Computing the magnitude of (vec -1 -2 -3)"
    (let ((v (vec! -1 -2 -3)))
      (ok (equal? (magnitude v) (sqrt 14))))))

(deftest normalizing
  (testing "Normalizing (vec 4 0 0) gives (1 0 0)"
    (let ((v (vec! 4 0 0)))
      (ok (equal? (normalize v) (vec! 1 0 0)))))
  (testing "Normalizing (vec 1 2 3)"
    (let ((v (vec! 1 2 3)))
      (ok (equal? (normalize v) (vec! 0.26726 0.53452 0.80178)))))
  (testing "The magnitude of a normalized vector"
    (let* ((v (vec! 1 2 3))
          (norm (normalize v)))
      (ok (equal? (magnitude norm) 1)))))

(deftest dot-product
  (testing "The dot product of two tuples"
    (let ((a (vec! 1 2 3))
          (b (vec! 2 3 4)))
      (ok (equal? (dot a b) 20)))))

(deftest cross-product
  (testing "The cross product of two vectors"
    (let ((a (vec! 1 2 3))
          (b (vec! 2 3 4)))
      (ok (equal? (cross a b) (vec! -1 2 -1)))
      (ok (equal? (cross b a) (vec! 1 -2 1))))))
