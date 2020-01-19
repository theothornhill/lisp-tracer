(in-package #:lisp-tracer-tests)

(deftest creating-ray
  (testing "Creating and querying a ray"
    (let* ((origin (point! 1 2 3))
           (direction (vec! 4 5 6))
           (r (ray! origin direction)))
      (ok (equal? (origin r) origin))
      (ok (equal? (direction r) direction)))))

(deftest distance-testing
  (testing "Computing a point from a distance"
    (let ((r (ray! (point! 2 3 4) (vec! 1 0 0))))
      (ok (equal? (pos r 0) (point! 2 3 4)))
      (ok (equal? (pos r 1) (point! 3 3 4)))
      (ok (equal? (pos r -1) (point! 1 3 4)))
      (ok (equal? (pos r 2.5) (point! 4.5 3 4))))))
