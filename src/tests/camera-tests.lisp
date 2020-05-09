(in-package :lisp-tracer-tests)

(deftest camera-construction
  (testing "Constructing a camera"
    (let ((c (make-camera
              :hsize 160
              :vsize 120
              :field-of-view (/ pi 2.0))))
      (ok (equal? (camera-hsize c) 160))
      (ok (equal? (camera-vsize c) 120))
      (ok (equal? (camera-field-of-view c) (/ pi 2.0)))
      (ok (equal? (camera-transform c) (identity-matrix))))))

(deftest camera-construction
  (testing "The pixel size for a horizontal canvas"
    (let ((c (create-camera :hsize 200
                            :vsize 125
                            :field-of-view (/ pi 2.0))))
      (ok (equal? (camera-pixel-size c) 0.01))))
  (testing "The pixel size for a horizontal canvas"
    (let ((c (create-camera :hsize 125
                            :vsize 200
                            :field-of-view (/ pi 2.0))))
      (ok (equal? (camera-pixel-size c) 0.01)))))

(deftest ray-through-canvas
  (testing "Constructing a ray through the center of the canvas"
    (let* ((c (create-camera :hsize 201
                             :vsize 101
                             :field-of-view (/ pi 2.0)))
           (r (ray-for-pixel c 100 50)))
      (ok (equal? (ray-origin r) (make-point)))
      (ok (equal? (ray-direction r) (make-vec :x 0.0 :y 0.0 :z -1.0)))))
  (testing "Constructing a ray through a corner of the canvas"
    (let* ((c (create-camera :hsize 201
                             :vsize 101
                             :field-of-view (/ pi 2.0)))
           (r (ray-for-pixel c 0 0)))
      (ok (equal? (ray-origin r) (make-point)))
      (ok (equal? (ray-direction r) (make-vec :x 0.66519 :y 0.33259 :z -0.66851)))))
  (testing "Constructing a ray when the camera is transformed"
    (let* ((c (create-camera :hsize 201
                             :vsize 101
                             :field-of-view (/ pi 2.0)))
           (sqrt2/2 (/ (sqrt 2.0) 2.0)))
      (setf (camera-transform c)
            (mult (rotation-y (/ pi 4.0))
                  (translation 0.0 -2.0 5.0)))
      (let ((r (ray-for-pixel c 100 50)))
        (ok (equal? (ray-origin r) (make-point :x 0.0 :y 2.0 :z -5.0)))
        (ok (equal? (ray-direction r) (make-vec :x sqrt2/2 :y 0.0 :z (neg sqrt2/2))))))))

(deftest render-camera-world
  (testing "Rendering a world with a camera"
    (let* ((w (default-world))
           (from (make-point :x 0.0 :y 0.0 :z -5.0))
           (to (make-vec))
           (up (make-point :x 0.0 :y 1.0 :z 0.0))
           (c (create-camera
               :hsize 11
               :vsize 11
               :field-of-view (/ pi 2.0)
               :transform (view-transform :from from :to to :up up))))
      (let ((image (render c w)))
        (ok (equal? (pixel-at image 5 5)
                    (make-color :red 0.38066
                                :green 0.47583
                                :blue 0.2855)))))))
