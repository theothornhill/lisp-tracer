(in-package #:lisp-tracer-tests)

(deftest intersection-test
  (testing "An intersection encapsulates time and object"
    (let* ((s (make-sphere))
           (i (make-intersection 3.5 s)))
      (ok (equal? (rt-intersection-tt i) 3.5))
      (ok (eq (rt-intersection-object i) s)))))

(deftest aggregating-intersections
  (testing "Aggregating intersections"
    (let* ((s (make-sphere))
           (i1 (make-intersection 1.0 s))
           (i2 (make-intersection 2.0 s))
           (xs (intersections i1 i2)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (first xs)) 1))
      (ok (equal? (rt-intersection-tt (second xs)) 2)))))

(deftest hit-testing
  (testing "The hit, when all intersections have positive t"
    (let* ((s (make-sphere))
           (i1 (make-intersection 1.0 s))
           (i2 (make-intersection 2.0 s))
           (xs (intersections i2 i1))
           (i (hit xs)))
      (ok (eq i i1))))
  (testing "The hit, when some intersections have negative t"
    (let* ((s (make-sphere))
           (i1 (make-intersection -1.0 s))
           (i2 (make-intersection 2.0 s))
           (xs (intersections i2 i1))
           (i (hit xs)))
      (ok (eq i i2))))
  (testing "The hit, when all intersections have negative t"
    (let* ((s (make-sphere))
           (i1 (make-intersection -2.0 s))
           (i2 (make-intersection -1.0 s))
           (xs (intersections i2 i1))
           (i (hit xs)))
      (ok (null i))))
  (testing "The hit is always the lowest nonnegative intersection"
    (let* ((s (make-sphere))
           (i1 (make-intersection 5.0 s))
           (i2 (make-intersection 7.0 s))
           (i3 (make-intersection -3.0 s))
           (i4 (make-intersection 2.0 s))
           (xs (intersections i1 i2 i3 i4))
           (i (hit xs)))
      (ok (eq i i4)))))

(deftest precomputing
  (testing "Precomputing the state of an intersection"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (shape (make-sphere))
           (i (make-intersection 4.0 shape))
           (computations (prepare-computations i r)))
      (ok (equal? (computations-tt computations) (rt-intersection-tt i)))
      (ok (equalp (computations-object computations) (rt-intersection-object i)))
      (ok (equal? (computations-point computations) (make-point 0.0 0.0 -1.0)))
      (ok (equal? (computations-eyev computations) (make-vec 0.0 0.0 -1.0)))
      (ok (equal? (computations-normalv computations) (make-vec 0.0 0.0 -1.0))))))

(deftest hit-intersection-inside-outside
  (testing "The hit, when an intersection occurs on the outside"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 -5.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (shape (make-sphere))
           (i (make-intersection 4.0 shape))
           (comps (prepare-computations i r)))
      (ng (computations-inside comps))))
  (testing "The hit, when an intersection occurs on the inside"
    (let* ((r (make-ray :origin (make-point 0.0 0.0 0.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (shape (make-sphere))
           (i (make-intersection 1.0 shape))
           (comps (prepare-computations i r)))
      (ok (equal? (computations-point comps) (make-point 0.0 0.0 1.0)))
      (ok (equal? (computations-eyev comps) (make-vec 0.0 0.0 -1.0)))
      (ok (computations-inside comps))
      (ok (equal? (computations-normalv comps) (make-vec 0.0 0.0 -1.0))))))
