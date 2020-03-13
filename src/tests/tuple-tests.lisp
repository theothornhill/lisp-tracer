(in-package #:lisp-tracer-tests)

(deftest point-testing
  (testing "A tuple with w=1.0 is a point"
    (let ((a (create-tuple 4.3 -4.2 3.1 1.0)))
      (ok (= (tuple-x a) 4.3))
      (ok (= (tuple-y a) -4.2))
      (ok (= (tuple-z a) 3.1))
      (ok (= (tuple-w a) 1.0))
      (ok (point? a))
      (ng (vec? a))))
  (testing "point creates tuples with w=1"
    (let ((p (make-point 4 -4 3)))
      (ok (= (tuple-w p) 1))
      (ok (equal? p (create-tuple 4 -4 3 1))))))

(deftest vec-testing
  (testing "A tuple with w=1.0 is a point"
    (let ((a (create-tuple 4.3 -4.2 3.1 0.0)))
      (ok (= (tuple-x a) 4.3))
      (ok (= (tuple-y a) -4.2))
      (ok (= (tuple-z a) 3.1))
      (ok (= (tuple-w a) 0.0))
      (ng (point? a))
      (ok (vec? a))))
  (testing "point creates tuples with w=1"
    (let ((v (make-vec 4 -4 3)))
      (ok (= (tuple-w v) 0))
      (ok (equal? v (create-tuple 4 -4 3 0))))))


(deftest tuple-math
  (testing "Adding two tuples"
    (let ((a1 (create-tuple 3 -2 5 1))
          (a2 (create-tuple -2 3 1 0)))
      (ok (equal? (add a1 a2) (create-tuple 1 1 6 1)))))
  (testing "Subtracting two points"
    (let ((p1 (make-point 3 2 1))
          (p2 (make-point 5 6 7)))
      (ok (equal? (sub p1 p2) (make-vec -2 -4 -6)))))
  (testing "Subtracting a vector from a point"
    (let ((p (make-point 3 2 1))
          (v (make-vec 5 6 7)))
      (ok (equal? (sub p v) (make-point -2 -4 -6)))))
  (testing "Subtracting two vectors"
    (let ((v1 (make-vec 3 2 1))
          (v2 (make-vec 5 6 7)))
      (ok (equal? (sub v1 v2) (make-vec -2 -4 -6)))))
  (testing "Subtracting a vector from zero vector"
    (let ((zero (zerovec))
          (v (make-vec 1 -2 3)))
      (ok (equal? (sub zero v) (make-vec -1 2 -3)))))
  (testing "Negating a create-tuple"
    (let ((a (create-tuple 1 -2 3 -4))
          (-a (create-tuple -1 2 -3 4)))
      (ok (equal? (neg a) -a)))))


(deftest scalar-testing
  (testing "Mutltiplying a tuple by a scalar"
    (let ((a (create-tuple 1 -2 3 -4)))
      (ok (equal? (mult a 3.5) (create-tuple 3.5 -7 10.5 -14)))))
  (testing "Mutltiplying a tuple by a fraction"
    (let ((a (create-tuple 1 -2 3 -4)))
      (ok (equal? (mult a 0.5) (create-tuple 0.5 -1 1.5 -2)))))
  (testing "Dividing a tuple by a scalar"
    (let ((a (create-tuple 1 -2 3 -4)))
      (ok (equal? (div a 2) (create-tuple 0.5 -1 1.5 -2)))))
  (testing "Computing the magnitude of (vec 1 0 0)"
    (let ((v (make-vec 1 0 0)))
      (ok (equal? (magnitude v) 1))))
  (testing "Computing the magnitude of (vec 0 1 0)"
    (let ((v (make-vec 0 1 0)))
      (ok (equal? (magnitude v) 1))))
  (testing "Computing the magnitude of (vec 0 0 1)"
    (let ((v (make-vec 0 0 1)))
      (ok (equal? (magnitude v) 1))))
  (testing "Computing the magnitude of (vec 1 2 3)"
    (let ((v (make-vec 1 2 3)))
      (ok (equal? (magnitude v) (sqrt 14)))))
  (testing "Computing the magnitude of (vec -1 -2 -3)"
    (let ((v (make-vec -1 -2 -3)))
      (ok (equal? (magnitude v) (sqrt 14))))))

(deftest normalizing
  (testing "Normalizing (vec 4 0 0) gives (1 0 0)"
    (let ((v (make-vec 4 0 0)))
      (ok (equal? (normalize v) (make-vec 1 0 0)))))
  (testing "Normalizing (vec 1 2 3)"
    (let ((v (make-vec 1 2 3)))
      (ok (equal? (normalize v) (make-vec 0.26726 0.53452 0.80178)))))
  (testing "The magnitude of a normalized vector"
    (let* ((v (make-vec 1 2 3))
          (norm (normalize v)))
      (ok (equal? (magnitude norm) 1)))))

(deftest dot-product
  (testing "The dot product of two tuples"
    (let ((a (make-vec 1 2 3))
          (b (make-vec 2 3 4)))
      (ok (equal? (dot a b) 20)))))

(deftest cross-product
  (testing "The cross product of two vectors"
    (let ((a (make-vec 1 2 3))
          (b (make-vec 2 3 4)))
      (ok (equal? (cross a b) (make-vec -1 2 -1)))
      (ok (equal? (cross b a) (make-vec 1 -2 1))))))

(deftest reflecting
  (testing "Reflecting a vector approaching at 45 degrees"
    (let* ((v (make-vec 1 -1 0))
           (n (make-vec 0 1 0))
           (r (reflect v n)))
      (ok (equal? r (make-vec 1 1 0)))))
  (testing "Reflecting a vector off a slanted surface"
    (let* ((v (make-vec 0 -1 0))
           (n (make-vec (div (sqrt 2) 2)
                        (div (sqrt 2) 2)
                        0))
           (r (reflect v n)))
      (ok (equal? r (make-vec 1 0 0))))))
