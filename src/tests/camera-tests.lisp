(in-package #:lisp-tracer-tests)

(deftest camera-construction
  (testing "Constructing a camera"
    (let ((c (make-camera
              :hsize 160.0
              :vsize 120.0
              :field-of-view (div (coerce pi 'single-float) 2.0))))
      (ok (equal? (camera-hsize c) 160))
      (ok (equal? (camera-vsize c) 120))
      (ok (equal? (camera-field-of-view c) (div (coerce pi 'single-float) 2.0)))
      (ok (equal? (camera-transform c) (identity-matrix))))))

(deftest camera-construction
  (testing "The pixel size for a horizontal canvas"
    (let ((c (create-camera 200.0 125.0 (div (coerce pi 'single-float) 2.0))))
      (ok (equal? (camera-pixel-size c) 0.01))))
  (testing "The pixel size for a horizontal canvas"
    (let ((c (create-camera 125.0 200.0 (div (coerce pi 'single-float) 2.0))))
      (ok (equal? (camera-pixel-size c) 0.01)))))

(deftest ray-through-canvas
  (testing "Constructing a ray through the center of the canvas"
    (let* ((c (create-camera 201.0 101.0 (div (coerce pi 'single-float) 2.0)))
           (r (ray-for-pixel c 100.0 50.0)))
      (ok (equal? (ray-origin r) (make-point 0.0 0.0 0.0)))
      (ok (equal? (ray-direction r) (make-vec 0.0 0.0 -1.0)))))
  (testing "Constructing a ray through a corner of the canvas"
    (let* ((c (create-camera 201.0 101.0 (div (coerce pi 'single-float) 2.0)))
           (r (ray-for-pixel c 0.0 0.0)))
      (ok (equal? (ray-origin r) (make-point 0.0 0.0 0.0)))
      (ok (equal? (ray-direction r) (make-vec 0.66519 0.33259 -0.66851)))))
  (testing "Constructing a ray when the camera is transformed"
    (let* ((c (create-camera 201.0 101.0 (div (coerce pi 'single-float) 2.0)))
           (sqrt2/2 (div (sqrt 2.0) 2.0)))
      (setf (camera-transform c)
            (mult (rotation-y (div (coerce pi 'single-float) 4.0))
                  (translation 0.0 -2.0 5.0)))
      (let ((r (ray-for-pixel c 100.0 50.0)))
        (ok (equal? (ray-origin r) (make-point 0.0 2.0 -5.0)))
        (ok (equal? (ray-direction r) (make-vec sqrt2/2 0.0 (neg sqrt2/2))))))))