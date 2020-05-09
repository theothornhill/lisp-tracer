(in-package :lisp-tracer-tests)

(deftest creating-ray
  (testing "Creating and querying a ray"
    (let* ((origin (make-point :x 1.0 :y 2.0 :z 3.0))
           (direction (make-vec :x 4.0 :y 5.0 :z 6.0))
           (r (make-ray :origin origin
                        :direction direction)))
      (ok (equal? (ray-origin r) origin))
      (ok (equal? (ray-direction r) direction)))))

(deftest distance-testing
  (testing "Computing a point from a distance"
    (let ((r (make-ray :origin (make-point :x 2.0 :y 3.0 :z 4.0)
                       :direction (make-vec :x 1.0 :y 0.0 :z 0.0))))
      (ok (equal? (pos r 0.0) (make-point :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (pos r 1.0) (make-point :x 3.0 :y 3.0 :z 4.0)))
      (ok (equal? (pos r -1.0) (make-point :x 1.0 :y 3.0 :z 4.0)))
      (ok (equal? (pos r 2.5) (make-point :x 4.5 :y 3.0 :z 4.0))))))

(deftest translating-a-ray
  (testing "Ray translation"
    (let* ((r (make-ray :origin (make-point :x 1.0 :y 2.0 :z 3.0)
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0)))
           (m (translation 3.0 4.0 5.0))
           (r2 (transform r m)))
      (ok (equal? (ray-origin r2) (make-point :x 4.0 :y 6.0 :z 8.0)))
      (ok (equal? (ray-direction r2) (make-vec :x 0.0 :y 1.0 :z 0.0))))))

(deftest scaling-a-ray
  (testing "Ray translation"
    (let* ((r (make-ray :origin (make-point :x 1.0 :y 2.0 :z 3.0)
                        :direction (make-vec :x 0.0 :y 1.0 :z 0.0)))
           (m (scaling 2.0 3.0 4.0))
           (r2 (transform r m)))
      (ok (equal? (ray-origin r2) (make-point :x 2.0 :y 6.0 :z 12.0)))
      (ok (equal? (ray-direction r2) (make-vec :x 0.0 :y 3.0 :z 0.0))))))
