(in-package :lisp-tracer-tests)

(deftest point-testing
  (testing "A tuple with w=1.0 is a point"
    (let ((a (make-tuple :x 4.3 :y -4.2 :z 3.1 :w 1.0)))
      (ok (= (tuple-x a) 4.3))
      (ok (= (tuple-y a) -4.2))
      (ok (= (tuple-z a) 3.1))
      (ok (= (tuple-w a) 1.0))
      (ok (point? a))
      (ng (vec? a))))
  (testing "point creates tuples with w=1"
    (let ((p (make-point :x 4.0 :y -4.0 :z 3.0)))
      (ok (= (tuple-w p) 1))
      (ok (equal? p (make-tuple :x 4.0 :y -4.0 :z 3.0 :w 1.0)))))
  (testing "make-point creates tuples with 0.0 0.0 0.0 1.0"
    (let ((p (make-point)))
      (ok (equal? p (make-tuple :x 0.0 :y 0.0 :z 0.0 :w 1.0))))))

(deftest vec-testing
  (testing "A tuple with w=1.0 is a point"
    (let ((a (make-tuple :x 4.3 :y -4.2 :z 3.1 :w 0.0)))
      (ok (= (tuple-x a) 4.3))
      (ok (= (tuple-y a) -4.2))
      (ok (= (tuple-z a) 3.1))
      (ok (= (tuple-w a) 0.0))
      (ng (point? a))
      (ok (vec? a))))
  (testing "point creates tuples with w=1"
    (let ((v (make-vec :x 4.0 :y -4.0 :z 3.0)))
      (ok (= (tuple-w v) 0))
      (ok (equal? v (make-tuple :x 4.0 :y -4.0 :z 3.0 :w 0.0)))))
  (testing "make-vec creates tuples with 0.0 0.0 0.0 0.0"
    (let ((p (make-vec)))
      (ok (equal? p (make-tuple :x 0.0 :y 0.0 :z 0.0 :w 0.0))))))


(deftest tuple-math
  (testing "Adding two tuples"
    (let ((a1 (make-tuple :x 3.0 :y -2.0 :z 5.0 :w 1.0))
          (a2 (make-tuple :x -2.0 :y 3.0 :z 1.0 :w 0.0)))
      (ok (equal? (add a1 a2) (make-tuple :x 1.0 :y 1.0 :z 6.0 :w 1.0)))))
  (testing "Subtracting two points"
    (let ((p1 (make-point :x 3.0 :y 2.0 :z 1.0))
          (p2 (make-point :x 5.0 :y 6.0 :z 7.0)))
      (ok (equal? (sub p1 p2) (make-vec :x -2.0 :y -4.0 :z -6.0)))))
  (testing "Subtracting a vector from a point"
    (let ((p (make-point :x 3.0 :y 2.0 :z 1.0))
          (v (make-vec :x 5.0 :y 6.0 :z 7.0)))
      (ok (equal? (sub p v) (make-point :x -2.0 :y -4.0 :z -6.0)))))
  (testing "Subtracting two vectors"
    (let ((v1 (make-vec :x 3.0 :y 2.0 :z 1.0))
          (v2 (make-vec :x 5.0 :y 6.0 :z 7.0)))
      (ok (equal? (sub v1 v2) (make-vec :x -2.0 :y -4.0 :z -6.0)))))
  (testing "Negating a make-tuple :x"
    (let ((a (make-tuple :x 1.0 :y -2.0 :z 3.0 :w -4.0))
          (-a (make-tuple :x -1.0 :y 2.0 :z -3.0 :w 4.0)))
      (ok (equal? (neg a) -a)))))


(deftest scalar-testing
  (testing "Mutltiplying a tuple by a scalar"
    (let ((a (make-tuple :x 1.0 :y -2.0 :z 3.0 :w -4.0)))
      (ok (equal? (mult a 3.5) (make-tuple :x 3.5 :y -7.0 :z 10.5 :w -14.0)))))
  (testing "Mutltiplying a tuple by a fraction"
    (let ((a (make-tuple :x 1.0 :y -2.0 :z 3.0 :w -4.0)))
      (ok (equal? (mult a 0.5) (make-tuple :x 0.5 :y -1.0 :z 1.5 :w -2.0)))))
  (testing "Dividing a tuple by a scalar"
    (let ((a (make-tuple :x 1.0 :y -2.0 :z 3.0 :w -4.0)))
      (ok (equal? (div a 2.0) (make-tuple :x 0.5 :y -1.0 :z 1.5 :w -2.0)))))
  (testing "Computing the magnitude of (vec 1 0 0)"
    (let ((v (make-vec :x 1.0 :y 0.0 :z 0.0)))
      (ok (equal? (magnitude v) 1.0))))
  (testing "Computing the magnitude of (vec 0 1 0)"
    (let ((v (make-vec :x 0.0 :y 1.0 :z 0.0)))
      (ok (equal? (magnitude v) 1.0))))
  (testing "Computing the magnitude of (vec 0 0 1)"
    (let ((v (make-vec :x 0.0 :y 0.0 :z 1.0)))
      (ok (equal? (magnitude v) 1.0))))
  (testing "Computing the magnitude of (vec 1 2 3)"
    (let ((v (make-vec :x 1.0 :y 2.0 :z 3.0)))
      (ok (equal? (magnitude v) (sqrt 14)))))
  (testing "Computing the magnitude of (vec -1 -2 -3)"
    (let ((v (make-vec :x -1.0 :y -2.0 :z -3.0)))
      (ok (equal? (magnitude v) (sqrt 14))))))

(deftest normalizing
  (testing "Normalizing (vec 4 0 0) gives (1 0 0)"
    (let ((v (make-vec :x 4.0 :y 0.0 :z 0.0)))
      (ok (equal? (normalize v) (make-vec :x 1.0 :y 0.0 :z 0.0)))))
  (testing "Normalizing (vec 1 2 3)"
    (let ((v (make-vec :x 1.0 :y 2.0 :z 3.0)))
      (ok (equal? (normalize v) (make-vec :x 0.26726 :y 0.53452 :z 0.80178)))))
  (testing "The magnitude of a normalized vector"
    (let* ((v (make-vec :x 1.0 :y 2.0 :z 3.0))
          (norm (normalize v)))
      (ok (equal? (magnitude norm) 1)))))

(deftest dot-product
  (testing "The dot product of two tuples"
    (let ((a (make-vec :x 1.0 :y 2.0 :z 3.0))
          (b (make-vec :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (dot a b) 20)))))

(deftest cross-product
  (testing "The cross product of two vectors"
    (let ((a (make-vec :x 1.0 :y 2.0 :z 3.0))
          (b (make-vec :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (cross a b) (make-vec :x -1.0 :y 2.0 :z -1.0)))
      (ok (equal? (cross b a) (make-vec :x 1.0 :y -2.0 :z 1.0))))))

(deftest reflecting
  (testing "Reflecting a vector approaching at 45 degrees"
    (let* ((v (make-vec :x 1.0 :y -1.0 :z 0.0))
           (n (make-vec :x 0.0 :y 1.0 :z 0.0))
           (r (reflect v n)))
      (ok (equal? r (make-vec :x 1.0 :y 1.0 :z 0.0)))))
  (testing "Reflecting a vector off a slanted surface"
    (let* ((v (make-vec :x 0.0 :y -1.0 :z 0.0))
           (n (make-vec :x (/ (sqrt 2) 2.0) :y
                        (/ (sqrt 2) 2.0) :z
                        0.0))
           (r (reflect v n)))
      (ok (equal? r (make-vec :x 1.0 :y 0.0 :z 0.0))))))
