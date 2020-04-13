(in-package #:lisp-tracer-tests)

(deftest intersection-test
  (testing "An intersection encapsulates time and object"
    (let* ((s (make-sphere))
           (i (make-intersection :tt 3.5 :object s)))
      (ok (equal? (rt-intersection-tt i) 3.5))
      (ok (eq (rt-intersection-object i) s)))))

(deftest aggregating-intersections
  (testing "Aggregating intersections"
    (let* ((s (make-sphere))
           (i1 (make-intersection :tt 1.0 :object s))
           (i2 (make-intersection :tt 2.0 :object s))
           (xs (intersections i1 i2)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (first xs)) 1))
      (ok (equal? (rt-intersection-tt (second xs)) 2)))))

(deftest hit-testing
  (testing "The hit, when all intersections have positive t"
    (let* ((s (make-sphere))
           (i1 (make-intersection :tt 1.0 :object s))
           (i2 (make-intersection :tt 2.0 :object s))
           (xs (intersections i2 i1))
           (i (hit xs)))
      (ok (eq i i1))))
  (testing "The hit, when some intersections have negative t"
    (let* ((s (make-sphere))
           (i1 (make-intersection :tt -1.0 :object s))
           (i2 (make-intersection :tt 2.0 :object s))
           (xs (intersections i2 i1))
           (i (hit xs)))
      (ok (eq i i2))))
  (testing "The hit, when all intersections have negative t"
    (let* ((s (make-sphere))
           (i1 (make-intersection :tt -2.0 :object s))
           (i2 (make-intersection :tt -1.0 :object s))
           (xs (intersections i2 i1))
           (i (hit xs)))
      (ok (null i))))
  (testing "The hit is always the lowest nonnegative intersection"
    (let* ((s (make-sphere))
           (i1 (make-intersection :tt 5.0 :object  s))
           (i2 (make-intersection :tt 7.0 :object  s))
           (i3 (make-intersection :tt -3.0 :object  s))
           (i4 (make-intersection :tt 2.0 :object  s))
           (xs (intersections i1 i2 i3 i4))
           (i (hit xs)))
      (ok (eq i i4)))))

(deftest precomputing
  (testing "Precomputing the state of an intersection"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (shape (make-sphere))
           (i (make-intersection :tt 4.0 :object  shape))
           (computations (prepare-computations i r)))
      (ok (equal? (computations-tt computations) (rt-intersection-tt i)))
      (ok (equalp (computations-object computations) (rt-intersection-object i)))
      (ok (equal? (computations-point computations) (make-point :x 0.0 :y 0.0 :z -1.0)))
      (ok (equal? (computations-eyev computations) (make-vec :x 0.0 :y 0.0 :z -1.0)))
      (ok (equal? (computations-normalv computations) (make-vec :x 0.0 :y 0.0 :z -1.0))))))

(deftest hit-intersection-inside-outside
  (testing "The hit, when an intersection occurs on the outside"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (shape (make-sphere))
           (i (make-intersection :tt 4.0 :object  shape))
           (comps (prepare-computations i r)))
      (ng (computations-inside? comps))))
  (testing "The hit, when an intersection occurs on the inside"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z 0.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (shape (make-sphere))
           (i (make-intersection :tt 1.0 :object  shape))
           (comps (prepare-computations i r)))
      (ok (equal? (computations-point comps) (make-point :x 0.0 :y 0.0 :z 1.0)))
      (ok (equal? (computations-eyev comps) (make-vec :x 0.0 :y 0.0 :z -1.0)))
      (ok (computations-inside? comps))
      (ok (equal? (computations-normalv comps) (make-vec :x 0.0 :y 0.0 :z -1.0))))))

(deftest hit-offset-over-point
  (testing "The hit should offset the point"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (shape (make-sphere :transform (translation 0.0 0.0 1.0)))
           (i (make-intersection :tt 5.0 :object shape))
           (comps (prepare-computations i r)))
      (ok (< (tuple-z (computations-over-point comps))
             (float (/ (neg epsilon) 2))))
      (ok (> (tuple-z (computations-point comps))
             (tuple-z (computations-over-point comps)))))))

(deftest n1-n2-computations
  (testing "Finding n1 and n2 at various intersections"
    (let ((a (glass-sphere))
          (b (glass-sphere))
          (c (glass-sphere))
          (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -4.0)
                       :direction (make-vec :x 0.0 :y 0.0 :z 1.0))))
      (setf (shape-transform a) (scaling 2.0 2.0 2.0))
      (setf (material-refractive-index (shape-material a)) 1.5)
      (setf (shape-transform b) (scaling-z -0.25))
      (setf (material-refractive-index (shape-material b)) 2.0)
      (setf (shape-transform c) (scaling-z 0.25))
      (setf (material-refractive-index (shape-material c)) 2.5)
      (let* ((xs (intersections
                  (make-intersection :tt 2.0 :object a)
                  (make-intersection :tt 2.75 :object b)
                  (make-intersection :tt 3.25 :object c)
                  (make-intersection :tt 4.75 :object b)
                  (make-intersection :tt 5.25 :object c)
                  (make-intersection :tt 6.0 :object a)))
             (comps1 (prepare-computations (first xs) r xs))
             (comps2 (prepare-computations (second xs) r xs))
             (comps3 (prepare-computations (third xs) r xs))
             (comps4 (prepare-computations (fourth xs) r xs))
             (comps5 (prepare-computations (fifth xs) r xs))
             (comps6 (prepare-computations (sixth xs) r xs)))
        (ok (equal? (computations-n1 comps1) 1.0))
        (ok (equal? (computations-n2 comps1) 1.5))
        (ok (equal? (computations-n1 comps2) 1.5))
        (ok (equal? (computations-n2 comps2) 2.0))
        (ok (equal? (computations-n1 comps3) 2.0))
        (ok (equal? (computations-n2 comps3) 2.5))
        (ok (equal? (computations-n1 comps4) 2.5))
        (ok (equal? (computations-n2 comps4) 2.5))
        (ok (equal? (computations-n1 comps5) 2.5))
        (ok (equal? (computations-n2 comps5) 1.5))
        (ok (equal? (computations-n1 comps6) 1.5))
        (ok (equal? (computations-n2 comps6) 1.0))))))

(deftest under-point-tests
  (testing "The under point is offset below the surface"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (shape (glass-sphere)))
      (setf (shape-transform shape) (translation-y 1.0))
      (let* ((i (make-intersection :tt 5.0 :object shape))
             (comps (prepare-computations i r)))
        (ok (> (tuple-z (computations-under-point comps))
               (float (/ (neg epsilon) 2))))
        (ok (< (tuple-z (computations-point comps))
               (tuple-z (computations-under-point comps))))))))

(deftest schlick-tests
  (testing "The schlick approx. under total internal reflection"
    (let* ((shape (glass-sphere))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z (/ (sqrt 2) 2.0))
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0)))
           (xs (intersections
                (make-intersection :tt (- (/ (sqrt 2) 2.0)) :object shape)
                (make-intersection :tt (/ (sqrt 2) 2.0) :object shape)))
           (comps (prepare-computations (cadr xs) r xs))
           (reflectance (schlick comps)))
      (ok (equal? reflectance 1.0))))
  (testing "The schlick approx with a perpendicular viewing angle"
    (let* ((shape (glass-sphere))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z 0.0)
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0)))
           (xs (intersections
                (make-intersection :tt -1.0 :object shape)
                (make-intersection :tt 1.0 :object shape)))
           (comps (prepare-computations (cadr xs) r xs))
           (reflectance (schlick comps)))
      (ok (equal? reflectance 0.04))))
  (testing "The schlick approx with small angle and n2 > n1"
    (let* ((shape (glass-sphere))
           (r (make-ray :origin (make-point :x 0.0 :y 0.99 :z -2.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (xs (intersections
                (make-intersection :tt 1.8589 :object shape)))
           (comps (prepare-computations (car xs) r xs))
           (reflectance (schlick comps)))
      (ok (equal? reflectance 0.48873)))))
