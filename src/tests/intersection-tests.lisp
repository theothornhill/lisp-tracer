(in-package #:lisp-tracer-tests)

(deftest intersection-test
  (testing "An intersection encapsulates time and object"
    (let* ((s (make-sphere))
           (i (make-intersection 3.5 s)))
      (ok (equal? (tt i) 3.5))
      (ok (eq (object i) s)))))

(deftest aggregating-intersections
  (testing "Aggregating intersections"
    (let* ((s (make-sphere))
           (i1 (make-intersection 1 s))
           (i2 (make-intersection 2 s))
           (xs (intersections i1 i2)))
      (ok (equal? (length xs) 2))
      (ok (equal? (tt (first xs)) 1))
      (ok (equal? (tt (second xs)) 2)))))
