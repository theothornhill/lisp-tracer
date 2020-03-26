(in-package #:lisp-tracer-tests)

(deftest normal-plane
  (testing "The normal of a plane is constant everywhere"
    (let* ((p (make-plane))
           (n1 (local-normal-at p (make-point 0.0 0.0 0.0)))
           (n2 (local-normal-at p (make-point 10.0 0.0 -10.0)))
           (n3 (local-normal-at p (make-point -5.0 0.0 150.0))))
      (ok (equal? n1 (make-vec 0.0 1.0 0.0)))
      (ok (equal? n2 (make-vec 0.0 1.0 0.0)))
      (ok (equal? n3 (make-vec 0.0 1.0 0.0))))))

(deftest plane-intersect
  (testing "Intersect with a ray parallel to the plane"
    (let* ((p (make-plane))
           (r (make-ray :origin (make-point 0.0 10.0 0.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (xs (local-intersect p r)))
      (ok (null xs))))
  (testing "Intersect with a coplanar ray"
    (let* ((p (make-plane))
           (r (make-ray :origin (make-point 0.0 10.0 0.0)
                        :direction (make-vec 0.0 0.0 1.0)))
           (xs (local-intersect p r)))
      (ok (null xs))))
  (testing "A ray intersecting a plane from above"
    (let* ((p (make-plane))
           (r (make-ray :origin (make-point 0.0 1.0 0.0)
                        :direction (make-vec 0.0 -1.0 0.0)))
           (xs (local-intersect p r)))
      (ok (equal? (length xs) 1))
      (ok (equal? (rt-intersection-tt (car xs)) 1.0))
      (ok (equalp (rt-intersection-object (car xs)) p))))
  (testing "A ray intersecting a plane from below"
    (let* ((p (make-plane))
           (r (make-ray :origin (make-point 0.0 -1.0 0.0)
                        :direction (make-vec 0.0 1.0 0.0)))
           (xs (local-intersect p r)))
      (ok (equal? (length xs) 1))
      (ok (equal? (rt-intersection-tt (car xs)) 1.0))
      (ok (equalp (rt-intersection-object (car xs)) p)))))
