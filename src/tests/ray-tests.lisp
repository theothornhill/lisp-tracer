(in-package #:lisp-tracer-tests)

(deftest creating-ray
  (testing "Creating and querying a ray"
    (let* ((origin (make-point 1f0 2f0 3f0))
           (direction (make-vec 4f0 5f0 6f0))
           (r (make-ray :origin origin
                        :direction direction)))
      (ok (equal? (ray-origin r) origin))
      (ok (equal? (ray-direction r) direction)))))

(deftest distance-testing
  (testing "Computing a point from a distance"
    (let ((r (make-ray :origin (make-point 2f0 3f0 4f0)
                       :direction (make-vec 1f0 0f0 0f0))))
      (ok (equal? (pos r 0.0) (make-point 2f0 3f0 4f0)))
      (ok (equal? (pos r 1.0) (make-point 3f0 3f0 4f0)))
      (ok (equal? (pos r -1.0) (make-point 1f0 3f0 4f0)))
      (ok (equal? (pos r 2.5) (make-point 4.5 3.0 4.0))))))

(deftest translating-a-ray
  (testing "Ray translation"
    (let* ((r (make-ray :origin (make-point 1f0 2f0 3f0)
                        :direction (make-vec 0f0 1f0 0f0)))
           (m (translation 3.0 4.0 5.0))
           (r2 (transform r m)))
      (ok (equal? (ray-origin r2) (make-point 4f0 6f0 8f0)))
      (ok (equal? (ray-direction r2) (make-vec 0f0 1f0 0f0))))))

(deftest scaling-a-ray
  (testing "Ray translation"
    (let* ((r (make-ray :origin (make-point 1f0 2f0 3f0)
                        :direction (make-vec 0f0 1f0 0f0)))
           (m (scaling 2.0 3.0 4.0))
           (r2 (transform r m)))
      (ok (equal? (ray-origin r2) (make-point 2f0 6f0 12f0)))
      (ok (equal? (ray-direction r2) (make-vec 0f0 3f0 0f0))))))
