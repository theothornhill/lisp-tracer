(in-package #:lisp-tracer-tests)

(deftest creating-ray
  (testing "Creating and querying a ray"
    (let* ((origin (make-point 1.0 2.0 3.0))
           (direction (make-vec 4.0 5.0 6.0))
           (r (make-ray :origin origin
                        :direction direction)))
      (ok (equal? (ray-origin r) origin))
      (ok (equal? (ray-direction r) direction)))))

(deftest distance-testing
  (testing "Computing a point from a distance"
    (let ((r (make-ray :origin (make-point 2.0 3.0 4.0)
                       :direction (make-vec 1.0 0.0 0.0))))
      (ok (equal? (pos r 0.0) (make-point 2.0 3.0 4.0)))
      (ok (equal? (pos r 1.0) (make-point 3.0 3.0 4.0)))
      (ok (equal? (pos r -1.0) (make-point 1.0 3.0 4.0)))
      (ok (equal? (pos r 2.5) (make-point 4.5 3.0 4.0))))))

(deftest translating-a-ray
  (testing "Ray translation"
    (let* ((r (make-ray :origin (make-point 1.0 2.0 3.0)
                        :direction (make-vec 0.0 1.0 0.0)))
           (m (translation 3.0 4.0 5.0))
           (r2 (transform r m)))
      (ok (equal? (ray-origin r2) (make-point 4.0 6.0 8.0)))
      (ok (equal? (ray-direction r2) (make-vec 0.0 1.0 0.0))))))

(deftest scaling-a-ray
  (testing "Ray translation"
    (let* ((r (make-ray :origin (make-point 1.0 2.0 3.0)
                        :direction (make-vec 0.0 1.0 0.0)))
           (m (scaling 2.0 3.0 4.0))
           (r2 (transform r m)))
      (ok (equal? (ray-origin r2) (make-point 2.0 6.0 12.0)))
      (ok (equal? (ray-direction r2) (make-vec 0.0 3.0 0.0))))))
