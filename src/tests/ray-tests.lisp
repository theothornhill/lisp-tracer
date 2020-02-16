(in-package #:lisp-tracer-tests)

(deftest creating-ray
  (testing "Creating and querying a ray"
    (let* ((origin (make-point 1 2 3))
           (direction (make-vec 4 5 6))
           (r (make-ray origin direction)))
      (ok (equal? (origin r) origin))
      (ok (equal? (direction r) direction)))))

(deftest distance-testing
  (testing "Computing a point from a distance"
    (let ((r (make-ray (make-point 2 3 4) (make-vec 1 0 0))))
      (ok (equal? (pos r 0) (make-point 2 3 4)))
      (ok (equal? (pos r 1) (make-point 3 3 4)))
      (ok (equal? (pos r -1) (make-point 1 3 4)))
      (ok (equal? (pos r 2.5) (make-point 4.5 3 4))))))
