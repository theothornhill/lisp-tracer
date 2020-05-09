(in-package :lisp-tracer-tests)

(deftest normal-plane
  (testing "The normal of a plane is constant everywhere"
    (let* ((p (make-plane))
           (n1 (local-normal-at p (make-point)))
           (n2 (local-normal-at p (make-point :x 10.0 :y 0.0 :z -10.0)))
           (n3 (local-normal-at p (make-point :x -5.0 :y 0.0 :z 150.0))))
      (ok (equal? n1 (make-vec :x 0.0 :y 1.0 :z 0.0)))
      (ok (equal? n2 (make-vec :x 0.0 :y 1.0 :z 0.0)))
      (ok (equal? n3 (make-vec :x 0.0 :y 1.0 :z 0.0))))))

(deftest plane-intersect
  (testing "Intersect with a ray parallel to the plane"
    (let* ((p (make-plane))
           (r (make-ray :origin (make-point :x 0.0 :y 10.0 :z 0.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (xs (local-intersect p r)))
      (ok (null xs))))
  (testing "Intersect with a coplanar ray"
    (let* ((p (make-plane))
           (r (make-ray :origin (make-point :x 0.0 :y 10.0 :z 0.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (xs (local-intersect p r)))
      (ok (null xs))))
  (testing "A ray intersecting a plane from above"
    (let* ((p (make-plane))
           (r (make-ray :origin (make-point :x 0.0 :y 1.0 :z 0.0)
                        :direction (make-vec :x 0.0 :y -1.0 :z 0.0)))
           (xs (local-intersect p r)))
      (ok (equal? (length xs) 1))
      (ok (equal? (rt-intersection-tt (car xs)) 1.0))
      (ok (equalp (rt-intersection-object (car xs)) p))))
  (testing "A ray intersecting a plane from below"
    (let* ((p (make-plane))
           (r (make-ray :origin (make-point :x 0.0 :y -1.0 :z 0.0)
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0)))
           (xs (local-intersect p r)))
      (ok (equal? (length xs) 1))
      (ok (equal? (rt-intersection-tt (car xs)) 1.0))
      (ok (equalp (rt-intersection-object (car xs)) p)))))
