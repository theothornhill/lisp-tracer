(in-package :lisp-tracer-tests)

(deftest matrix-testing
  (testing "Constructing and inspecting a 4x4 matrix"
    (let ((matrix (create-matrix 1.0 2.0 3.0 4.0
                                 5.5 6.5 7.5 8.5
                                 9.0 10.0 11.0 12.0
                                 13.5 14.5 15.5 16.6)))
      (ok (= (matrix-m00 matrix) 1.0))
      (ok (= (matrix-m03 matrix) 4.0))
      (ok (= (matrix-m10 matrix) 5.5))
      (ok (= (matrix-m12 matrix) 7.5))
      (ok (= (matrix-m22 matrix) 11.0))
      (ok (= (matrix-m30 matrix) 13.5))
      (ok (= (matrix-m32 matrix) 15.5)))))

(deftest matrix-equality
  (testing "Matrix equality with identical matrices"
    (let ((a (create-matrix 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0))
          (b (create-matrix 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0)))
      (ok (equal? a b))))
  (testing "Matrix equality with different matrices"
    (let ((a (create-matrix 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0))
          (b (create-matrix 1.0 2.0 3.0 4.0 5.0 9.0 9.0 8.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0)))
      (ng (equal? a b)))))

(deftest matrix-math
  (testing "Multiplying two matrices"
    (let ((a (create-matrix 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0))
          (b (create-matrix -2.0 1.0 2.0 3.0 3.0 2.0 1.0 -1.0 4.0 3.0 6.0 5.0 1.0 2.0 7.0 8.0))
          (a*b (create-matrix 20.0 22.0 50.0 48.0 44.0 54.0 114.0 108.0 40.0 58.0 110.0 102.0 16.0 26.0 46.0 42.0)))
      (ok (equal? (mult a b) a*b))))
  (testing "Multiplying matrix by a tuple"
    (let ((a (create-matrix 1.0 2.0 3.0 4.0 2.0 4.0 4.0 2.0 8.0 6.0 4.0 1.0 0.0 0.0 0.0 1.0))
           (b (make-tuple :x 1.0 :y 2.0 :z 3.0 :w 1.0)))
      (ok (equal? (mult a b) (make-tuple :x 18.0 :y 24.0 :z 33.0 :w 1.0)))))
  (testing "Multiplying matrix by identity matrix"
    (let ((a (create-matrix 0.0 1.0 2.0 4.0 1.0 2.0 4.0 8.0 2.0 4.0 8.0 16.0 4.0 8.0 16.0 32.0))
          (id-matrix (identity-matrix)))
      (ok (equal? (mult a id-matrix) a)))))

(deftest transposing-matrix
  (testing "Transposing a matrix"
    (let ((a (create-matrix 0.0 9.0 3.0 0.0 9.0 8.0 0.0 8.0 1.0 8.0 5.0 3.0 0.0 0.0 5.0 8.0))
          (b (create-matrix 0.0 9.0 1.0 0.0 9.0 8.0 8.0 0.0 3.0 0.0 5.0 5.0 0.0 8.0 3.0 8.0)))
      (ok (equal? (transpose a) b))))
  (testing "Transposing the identity matrix"
    (let ((a (transpose (identity-matrix))))
      (ok (equal? a (identity-matrix))))))

(deftest invert-matrix
  (testing "Calculating the inverse o. a matrix"
    (let* ((a (create-matrix -5.0 2.0 6.0 -8.0 1.0 -5.0 1.0 8.0 7.0 7.0 -6.0 -7.0 1.0 -3.0 7.0 4.0))
           (b (inverse a)))
      (ok (equal? (matrix-m32 b) (div -160.0 532.0)))
      (ok (equal? (matrix-m23 b) (div 105.0 532.0)))
      (ok (equal? b (create-matrix 0.21805 0.45113 0.24060 -0.04511
                                   -0.80827 -1.45677 -0.44361 0.52068
                                   -0.07895 -0.22368 -0.05263 0.19737
                                   -0.52256 -0.81391 -0.30075 0.30639))))))

(deftest calculating-iverses
  (testing "Calculating the inverse of another matrix"
    (let* ((a (create-matrix 8.0 -5.0 9.0 2.0 7.0 5.0 6.0 1.0 -6.0 0.0 9.0 6.0 -3.0 0.0 -9.0 -4.0))
           (inverted (inverse a))
           (result-matrix (create-matrix -0.15385 -0.15385 -0.28205 -0.53846
                                         -0.07692 0.12308 0.02564 0.03077
                                         0.35897 0.35897 0.43590 0.92308
                                         -0.69231 -0.69231 -0.76923 -1.92308)))
      (ok (equal? inverted result-matrix)))))

(deftest calculating-iverses-2
  (testing "Calculating the inverse of a third matrix"
    (let* ((a (create-matrix 9.0 3.0 0.0 9.0 -5.0 -2.0 -6.0 -3.0 -4.0 9.0 6.0 4.0 -7.0 6.0 6.0 2.0))
           (inverted (inverse a))
           (result-matrix (create-matrix -0.04074 -0.07778 0.14444 -0.22222
                                         -0.07778 0.03333 0.36667 -0.33333
                                         -0.02901 -0.14630 -0.10926 0.12963
                                         0.17778 0.06667 -0.26667 0.33333)))
      (ok (equal? inverted result-matrix)))))

(deftest multiplying-matrices-inversion
  (testing "Multiplying a product by its inverse"
    (let* ((a (create-matrix 3.0 -9.0 7.0 3.0 3.0 -8.0 2.0 -9.0 -4.0 4.0 4.0 1.0 -6.0 5.0 -1.0 1.0))
           (b (create-matrix 8.0 2.0 2.0 2.0 3.0 -1.0 7.0 0.0 7.0 0.0 5.0 4.0 6.0 -2.0 0.0 5.0))
           (c (mult a b)))
      (ok (equal? a (mult c (inverse b)))))))

(deftest translate-matrix
  (testing "Multiplying by a translation matrix"
    (let ((transform (translation 5.0 -3.0 2.0))
          (p (make-point :x -3.0 :y 4.0 :z 5.0)))
      (ok (equal? (mult transform p) (make-point :x 2.0 :y 1.0 :z 7.0)))))
  (testing "Multiplying by the inverse of a translation matrix"
    (let* ((transform (translation 5.0 -3.0 2.0))
           (inv (inverse transform))
           (p (make-point :x -3.0 :y 4.0 :z 5.0)))
      (ok (equal? (mult inv p) (make-point :x -8.0 :y 7.0 :z 3.0)))))
  (testing "Translation does not affect vectors"
    (let ((transform (translation 5.0 -3.0 2.0))
          (v (make-vec :x -3.0 :y 4.0 :z 5.0)))
      (ok (equal? (mult transform v) v)))))

(deftest scaling-matrix
  (testing "A scaling matrix applied to a point"
    (let ((transform (scaling 2.0 3.0 4.0))
          (p (make-point :x -4.0 :y 6.0 :z 8.0)))
      (ok (equal? (mult transform p) (make-point :x -8.0 :y 18.0 :z 32.0)))))
  (testing "A scaling matrix applied to a vector"
    (let ((transform (scaling 2.0 3.0 4.0))
          (v (make-vec :x -4.0 :y 6.0 :z 8.0)))
      (ok (equal? (mult transform v) (make-vec :x -8.0 :y 18.0 :z 32.0)))))
  (testing "Multiplying by the inverse of a scaling matrix"
    (let* ((transform (scaling 2.0 3.0 4.0))
           (inv (inverse transform))
           (v (make-vec :x -4.0 :y 6.0 :z 8.0)))
      (ok (equal? (mult inv v) (make-vec :x -2.0 :y 2.0 :z 2.0)))))
  (testing "Reflection is scaling by a negative value"
    (let ((transform (scaling -1.0 1.0 1.0))
          (p (make-point :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (mult transform p) (make-point :x -2.0 :y 3.0 :z 4.0))))))

(deftest rotation-x-axis
  (testing "Rotating a point around the x axis"
    (let ((p (make-point :x 0.0 :y 1.0 :z 0.0))
          (half-quarter (rotation-x (div (coerce pi 'double-float) 4.0)))
          (full-quarter (rotation-x (div (coerce pi 'double-float) 2.0))))
      (ok (equal? (mult half-quarter p) (make-point :x 0.0 :y
                                                    (/ (sqrt 2) 2.0) :z
                                                    (/ (sqrt 2) 2.0))))
      (ok (equal? (mult full-quarter p) (make-point :x 0.0 :y 0.0 :z 1.0)))))
  (testing "The inverse of an x-rotation rotates in the opposite direction"
    (let* ((p (make-point :x 0.0 :y 1.0 :z 0.0))
           (half-quarter (rotation-x (float (div (coerce pi 'double-float) 4.0))))
           (inv (inverse half-quarter)))
      (ok (equal? (mult inv p) (make-point :x 0.0 :y
                                           (/ (sqrt 2) 2.0) :z
                                           (- (/ (sqrt 2) 2.0))))))))

(deftest rotation-y-axis
  (testing "Rotating a point around the y axis"
    (let ((p (make-point :x 0.0 :y 0.0 :z 1.0))
          (half-quarter (rotation-y (/ (coerce pi 'double-float) 4)))
          (full-quarter (rotation-y (/ (coerce pi 'double-float) 2))))
      (ok (equal? (mult half-quarter p) (make-point :x (/ (sqrt 2) 2.0) :y
                                                    0.0 :z
                                                    (/ (sqrt 2) 2.0))))
      (ok (equal? (mult full-quarter p) (make-point :x 1.0 :y 0.0 :z 0.0))))))

(deftest rotation-z-axis
  (testing "Rotating a point around the z axis"
    (let ((p (make-point :x 0.0 :y 1.0 :z 0.0))
          (half-quarter (rotation-z (div (coerce pi 'double-float) 4.0)))
          (full-quarter (rotation-z (div (coerce pi 'double-float) 2.0))))
      (ok (equal? (mult half-quarter p) (make-point :x (- (/ (sqrt 2) 2.0)) :y
                                                    (/ (sqrt 2) 2.0) :z
                                                    0.0)))
      (ok (equal? (mult full-quarter p) (make-point :x -1.0 :y 0.0 :z 0.0))))))

(deftest shearing
  (testing "A shearing transformation moves x in proportion to y"
    (let ((transform (shearing 1.0 0.0 0.0 0.0 0.0 0.0))
          (p (make-point :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (mult transform p) (make-point :x 5.0 :y 3.0 :z 4.0)))))
  (testing "A shearing transformation moves x in proportion to z"
    (let ((transform (shearing 0.0 1.0 0.0 0.0 0.0 0.0))
          (p (make-point :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (mult transform p) (make-point :x 6.0 :y 3.0 :z 4.0)))))
  (testing "A shearing transformation moves y in proportion to x"
    (let ((transform (shearing 0.0 0.0 1.0 0.0 0.0 0.0))
          (p (make-point :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (mult transform p) (make-point :x 2.0 :y 5.0 :z 4.0)))))
  (testing "A shearing transformation moves y in proportion to z"
    (let ((transform (shearing 0.0 0.0 0.0 1.0 0.0 0.0))
          (p (make-point :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (mult transform p) (make-point :x 2.0 :y 7.0 :z 4.0)))))
  (testing "A shearing transformation moves z in proportion to x"
    (let ((transform (shearing 0.0 0.0 0.0 0.0 1.0 0.0))
          (p (make-point :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (mult transform p) (make-point :x 2.0 :y 3.0 :z 6.0)))))
  (testing "A shearing transformation moves z in proportion to y"
    (let ((transform (shearing 0.0 0.0 0.0 0.0 0.0 1.0))
          (p (make-point :x 2.0 :y 3.0 :z 4.0)))
      (ok (equal? (mult transform p) (make-point :x 2.0 :y 3.0 :z 7.0))))))

(deftest matrix-composing
  (testing "Individual transformations are applied in sequence"
    (let* ((p (make-point :x 1.0 :y 0.0 :z 1.0))
           (a (rotation-x (div (coerce pi 'double-float) 2.0)))
           (b (scaling 5.0 5.0 5.0))
           (c (translation 10.0 5.0 7.0))
           (p2 (mult a p))
           (p3 (mult b p2))
           (p4 (mult c p3)))
      (ok (equal? p4 (make-point :x 15.0 :y 0.0 :z 7.0)))))
  (testing "Chained transformations must be applied in reverse order"
    (let* ((p (make-point :x 1.0 :y 0.0 :z 1.0))
           (a (rotation-x (div (coerce pi 'double-float) 2.0)))
           (b (scaling 5.0 5.0 5.0))
           (c (translation 10.0 5.0 7.0))
           (transformed (reduce #'mult (list c b a p))))
      (ok (equal? transformed (make-point :x 15.0 :y 0.0 :z 7.0))))))

(deftest translation-testing
  (testing "Multiplying by a translation matrix"
    (let* ((transform (translation 5.0 -3.0 2.0))
           (p (make-point :x -3.0 :y 4.0 :z 5.0))
           (inv (inverse transform))
           (v (make-vec :x -3.0 :y 4.0 :z 5.0)))
      (ok (equal? (mult transform p) (make-point :x 2.0 :y 1.0 :z 7.0)))
      (ok (equal? (mult inv p) (make-point :x -8.0 :y 7.0 :z 3.0)))
      (ok (equal? (mult transform v) v)))))

(deftest scaling-testing
  (testing "A scaling matrix applied to a point"
    (let* ((transform (scaling 2.0 3.0 4.0))
           (p (make-point :x -4.0 :y 6.0 :z 8.0))
           (v (make-vec :x -4.0 :y 6.0 :z 8.0))
           (inv (inverse transform)))
      (ok (equal? (mult transform p) (make-point :x -8.0 :y 18.0 :z 32.0)))
      (ok (equal? (mult transform v) (make-vec :x -8.0 :y 18.0 :z 32.0)))
      (ok (equal? (mult inv v) (make-vec :x -2.0 :y 2.0 :z 2.0))))))

(deftest transform-matrix-testing
  (testing "The transformation matrix for the default orientation"
    (let* ((from (make-point))
           (to (make-point :x 0.0 :y 0.0 :z -1.0))
           (up (make-vec :x 0.0 :y 1.0 :z 0.0))
           (trans (view-transform :from from :to to :up up)))
      (equal? trans (identity-matrix))))
  (testing "A view transformation matrix looking in positive z direction"
    (let* ((from (make-point))
           (to (make-point :x 0.0 :y 0.0 :z 1.0))
           (up (make-vec :x 0.0 :y 1.0 :z 0.0))
           (trans (view-transform :from from :to to :up up)))
          (equal? trans (scaling -1.0 1.0 -1.0))))
  (testing "The view transformation moves the world"
    (let* ((from (make-point :x 0.0 :y 0.0 :z 8.0))
           (to (make-point))
           (up (make-vec :x 0.0 :y 1.0 :z 0.0))
           (trans (view-transform :from from :to to :up up)))
      (equal? trans (translation 0.0 0.0 -8.0))))
  (testing "An arbitrary view transformation"
    (let* ((from (make-point :x 1.0 :y 3.0 :z 2.0))
           (to (make-point :x 4.0 :y -2.0 :z 8.0))
           (up (make-vec :x 1.0 :y 1.0 :z 0.0))
           (trans (view-transform :from from :to to :up up)))
      (equal? trans (create-matrix
                     -0.50709 0.50709 0.67612 -2.36643
                     0.76772 0.60609 0.12122 -2.82843
                     -0.35857 0.59761 -0.71714 0.00000
                     0.00000 0.00000 0.00000 1.00000)))))
