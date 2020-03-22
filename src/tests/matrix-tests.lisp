(in-package #:lisp-tracer-tests)

(deftest matrix-testing
  (testing "Constructing and inspecting a 4x4 matrix"
    (let ((matrix (create-matrix 1f0 2f0 3f0 4f0
                                 5f5 6f5 7f5 8f5
                                 9f0 10f0 11f0 12f0
                                 13f5 14f5 15f5 16f6)))
      (ok (= (matrix-m00 matrix) 1f0))
      (ok (= (matrix-m03 matrix) 4f0))
      (ok (= (matrix-m10 matrix) 5f5))
      (ok (= (matrix-m12 matrix) 7f5))
      (ok (= (matrix-m22 matrix) 11f0))
      (ok (= (matrix-m30 matrix) 13f5))
      (ok (= (matrix-m32 matrix) 15f5)))))

(deftest matrix-equality
  (testing "Matrix equality with identical matrices"
    (let ((a (create-matrix 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 9f0 8f0 7f0 6f0 5f0 4f0 3f0 2f0))
          (b (create-matrix 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 9f0 8f0 7f0 6f0 5f0 4f0 3f0 2f0)))
      (ok (equal? a b))))
  (testing "Matrix equality with different matrices"
    (let ((a (create-matrix 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 9f0 8f0 7f0 6f0 5f0 4f0 3f0 2f0))
          (b (create-matrix 1f0 2f0 3f0 4f0 5f0 9f0 9f0 8f0 9f0 8f0 7f0 6f0 5f0 4f0 3f0 2f0)))
      (ng (equal? a b)))))

(deftest matrix-math
  (testing "Multiplying two matrices"
    (let ((a (create-matrix 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 9f0 8f0 7f0 6f0 5f0 4f0 3f0 2f0))
          (b (create-matrix -2f0 1f0 2f0 3f0 3f0 2f0 1f0 -1f0 4f0 3f0 6f0 5f0 1f0 2f0 7f0 8f0))
          (a*b (create-matrix 20f0 22f0 50f0 48f0 44f0 54f0 114f0 108f0 40f0 58f0 110f0 102f0 16f0 26f0 46f0 42f0)))
      (ok (equal? (mult a b) a*b))))
  (testing "Multiplying matrix by a tuple"
    (let ((a (create-matrix 1f0 2f0 3f0 4f0 2f0 4f0 4f0 2f0 8f0 6f0 4f0 1f0 0f0 0f0 0f0 1f0))
           (b (make-tuple :x 1f0 :y 2f0 :z 3f0 :w 1f0)))
      (ok (equal? (mult a b) (make-tuple :x 18f0 :y 24f0 :z 33f0 :w 1f0)))))
  (testing "Multiplying matrix by identity matrix"
    (let ((a (create-matrix 0f0 1f0 2f0 4f0 1f0 2f0 4f0 8f0 2f0 4f0 8f0 16f0 4f0 8f0 16f0 32f0))
          (id-matrix (identity-matrix)))
      (ok (equal? (mult a id-matrix) a)))))

(deftest transposing-matrix
  (testing "Transposing a matrix"
    (let ((a (create-matrix 0f0 9f0 3f0 0f0 9f0 8f0 0f0 8f0 1f0 8f0 5f0 3f0 0f0 0f0 5f0 8f0))
          (b (create-matrix 0f0 9f0 1f0 0f0 9f0 8f0 8f0 0f0 3f0 0f0 5f0 5f0 0f0 8f0 3f0 8f0)))
      (ok (equal? (transpose a) b))))
  (testing "Transposing the identity matrix"
    (let ((a (transpose (identity-matrix))))
      (ok (equal? a (identity-matrix))))))

(deftest invert-matrix
  (testing "Calculating the inverse of a matrix"
    (let* ((a (create-matrix -5f0 2f0 6f0 -8f0 1f0 -5f0 1f0 8f0 7f0 7f0 -6f0 -7f0 1f0 -3f0 7f0 4f0))
           (b (inverse a)))
      (ok (equal? (matrix-m32 b) (div -160.0 532.0)))
      (ok (equal? (matrix-m23 b) (div 105.0 532.0)))
      (ok (equal? b (create-matrix 0.21805 0.45113 0.24060 -0.04511
                                   -0.80827 -1.45677 -0.44361 0.52068
                                   -0.07895 -0.22368 -0.05263 0.19737
                                   -0.52256 -0.81391 -0.30075 0.30639))))))

(deftest calculating-iverses
  (testing "Calculating the inverse of another matrix"
    (let* ((a (create-matrix 8f0 -5f0 9f0 2f0 7f0 5f0 6f0 1f0 -6f0 0f0 9f0 6f0 -3f0 0f0 -9f0 -4f0))
           (inverted (inverse a))
           (result-matrix (create-matrix -0.15385 -0.15385 -0.28205 -0.53846
                                         -0.07692 0.12308 0.02564 0.03077
                                         0.35897 0.35897 0.43590 0.92308
                                         -0.69231 -0.69231 -0.76923 -1.92308)))
      (ok (equal? inverted result-matrix)))))

(deftest calculating-iverses-2
  (testing "Calculating the inverse of a third matrix"
    (let* ((a (create-matrix 9f0 3f0 0f0 9f0 -5f0 -2f0 -6f0 -3f0 -4f0 9f0 6f0 4f0 -7f0 6f0 6f0 2f0))
           (inverted (inverse a))
           (result-matrix (create-matrix -0.04074 -0.07778 0.14444 -0.22222
                                         -0.07778 0.03333 0.36667 -0.33333
                                         -0.02901 -0.14630 -0.10926 0.12963
                                         0.17778 0.06667 -0.26667 0.33333)))
      (ok (equal? inverted result-matrix)))))

(deftest multiplying-matrices-inversion
  (testing "Multiplying a product by its inverse"
    (let* ((a (create-matrix 3f0 -9f0 7f0 3f0 3f0 -8f0 2f0 -9f0 -4f0 4f0 4f0 1f0 -6f0 5f0 -1f0 1f0))
           (b (create-matrix 8f0 2f0 2f0 2f0 3f0 -1f0 7f0 0f0 7f0 0f0 5f0 4f0 6f0 -2f0 0f0 5f0))
           (c (mult a b)))
      (ok (equal? a (mult c (inverse b)))))))

(deftest translate-matrix
  (testing "Multiplying by a translation matrix"
    (let ((transform (translation 5.0 -3.0 2.0))
          (p (make-point -3f0 4f0 5f0)))
      (ok (equal? (mult transform p) (make-point 2f0 1f0 7f0)))))
  (testing "Multiplying by the inverse of a translation matrix"
    (let* ((transform (translation 5.0 -3.0 2.0))
           (inv (inverse transform))
           (p (make-point -3f0 4f0 5f0)))
      (ok (equal? (mult inv p) (make-point -8f0 7f0 3f0)))))
  (testing "Translation does not affect vectors"
    (let ((transform (translation 5.0 -3.0 2.0))
          (v (make-vec -3.0 4.0 5.0)))
      (ok (equal? (mult transform v) v)))))

(deftest scaling-matrix
  (testing "A scaling matrix applied to a point"
    (let ((transform (scaling 2.0 3.0 4.0))
          (p (make-point -4f0 6f0 8f0)))
      (ok (equal? (mult transform p) (make-point -8f0 18f0 32f0)))))
  (testing "A scaling matrix applied to a vector"
    (let ((transform (scaling 2.0 3.0 4.0))
          (v (make-vec -4.0 6.0 8.0)))
      (ok (equal? (mult transform v) (make-vec -8.0 18.0 32.0)))))
  (testing "Multiplying by the inverse of a scaling matrix"
    (let* ((transform (scaling 2.0 3.0 4.0))
           (inv (inverse transform))
           (v (make-vec -4.0 6.0 8.0)))
      (ok (equal? (mult inv v) (make-vec -2.0 2.0 2.0)))))
  (testing "Reflection is scaling by a negative value"
    (let ((transform (scaling -1.0 1.0 1.0))
          (p (make-point 2f0 3f0 4f0)))
      (ok (equal? (mult transform p) (make-point -2f0 3f0 4f0))))))

(deftest rotation-x-axis
  (testing "Rotating a point around the x axis"
    (let ((p (make-point 0f0 1f0 0f0))
          (half-quarter (rotation-x (div (coerce pi 'single-float) 4.0)))
          (full-quarter (rotation-x (div (coerce pi 'single-float) 2.0))))
      (ok (equal? (mult half-quarter p) (make-point 0.0
                                                    (div (sqrt 2) 2.0)
                                                    (div (sqrt 2) 2.0))))
      (ok (equal? (mult full-quarter p) (make-point 0f0 0f0 1f0)))))
  (testing "The inverse of an x-rotation rotates in the opposite direction"
    (let* ((p (make-point 0f0 1f0 0f0))
           (half-quarter (rotation-x (float (div (coerce pi 'single-float) 4.0))))
           (inv (inverse half-quarter)))
      (ok (equal? (mult inv p) (make-point 0f0
                                           (div (sqrt 2) 2.0)
                                           (- (div (sqrt 2) 2.0))))))))

(deftest rotation-y-axis
  (testing "Rotating a point around the y axis"
    (let ((p (make-point 0f0 0f0 1f0))
          (half-quarter (rotation-y (/ (coerce pi 'single-float) 4)))
          (full-quarter (rotation-y (/ (coerce pi 'single-float) 2))))
      (ok (equal? (mult half-quarter p) (make-point (float (/ (sqrt 2) 2))
                                                    0.0
                                                    (float (/ (sqrt 2) 2)))))
      (ok (equal? (mult full-quarter p) (make-point 1f0 0f0 0f0))))))

(deftest rotation-z-axis
  (testing "Rotating a point around the z axis"
    (let ((p (make-point 0f0 1f0 0f0))
          (half-quarter (rotation-z (div (coerce pi 'single-float) 4.0)))
          (full-quarter (rotation-z (div (coerce pi 'single-float) 2.0))))
      (ok (equal? (mult half-quarter p) (make-point (float (- (div (sqrt 2) 2.0)))
                                                    (float (div (sqrt 2) 2.0))
                                                    0f0)))
      (ok (equal? (mult full-quarter p) (make-point -1f0 0f0 0f0))))))

(deftest shearing
  (testing "A shearing transformation moves x in proportion to y"
    (let ((transform (shearing 1.0 0.0 0.0 0.0 0.0 0.0))
          (p (make-point 2f0 3f0 4f0)))
      (ok (equal? (mult transform p) (make-point 5f0 3f0 4f0)))))
  (testing "A shearing transformation moves x in proportion to z"
    (let ((transform (shearing 0.0 1.0 0.0 0.0 0.0 0.0))
          (p (make-point 2f0 3f0 4f0)))
      (ok (equal? (mult transform p) (make-point 6f0 3f0 4f0)))))
  (testing "A shearing transformation moves y in proportion to x"
    (let ((transform (shearing 0.0 0.0 1.0 0.0 0.0 0.0))
          (p (make-point 2f0 3f0 4f0)))
      (ok (equal? (mult transform p) (make-point 2f0 5f0 4f0)))))
  (testing "A shearing transformation moves y in proportion to z"
    (let ((transform (shearing 0.0 0.0 0.0 1.0 0.0 0.0))
          (p (make-point 2f0 3f0 4f0)))
      (ok (equal? (mult transform p) (make-point 2f0 7f0 4f0)))))
  (testing "A shearing transformation moves z in proportion to x"
    (let ((transform (shearing 0.0 0.0 0.0 0.0 1.0 0.0))
          (p (make-point 2f0 3f0 4f0)))
      (ok (equal? (mult transform p) (make-point 2f0 3f0 6f0)))))
  (testing "A shearing transformation moves z in proportion to y"
    (let ((transform (shearing 0.0 0.0 0.0 0.0 0.0 1.0))
          (p (make-point 2f0 3f0 4f0)))
      (ok (equal? (mult transform p) (make-point 2f0 3f0 7f0))))))

(deftest matrix-composing
  (testing "Individual transformations are applied in sequence"
    (let* ((p (make-point 1f0 0f0 1f0))
           (a (rotation-x (div (coerce pi 'single-float) 2.0)))
           (b (scaling 5.0 5.0 5.0))
           (c (translation 10.0 5.0 7.0))
           (p2 (mult a p))
           (p3 (mult b p2))
           (p4 (mult c p3)))
      (ok (equal? p4 (make-point 15f0 0f0 7f0)))))
  (testing "Chained transformations must be applied in reverse order"
    (let* ((p (make-point 1f0 0f0 1f0))
           (a (rotation-x (div (coerce pi 'single-float) 2.0)))
           (b (scaling 5.0 5.0 5.0))
           (c (translation 10.0 5.0 7.0))
           (transformed (reduce #'mult (list c b a p))))
      (ok (equal? transformed (make-point 15f0 0f0 7f0))))))

(deftest translation-testing
  (testing "Multiplying by a translation matrix"
    (let* ((transform (translation 5.0 -3.0 2.0))
           (p (make-point -3.0 4.0 5.0))
           (inv (inverse transform))
           (v (make-vec -3.0 4.0 5.0)))
      (ok (equal? (mult transform p) (make-point 2.0 1.0 7.0)))
      (ok (equal? (mult inv p) (make-point -8.0 7.0 3.0)))
      (ok (equal? (mult transform v) v)))))

(deftest scaling-testing
  (testing "A scaling matrix applied to a point"
    (let* ((transform (scaling 2.0 3.0 4.0))
           (p (make-point -4.0 6.0 8.0))
           (v (make-vec -4.0 6.0 8.0))
           (inv (inverse transform)))
      (ok (equal? (mult transform p) (make-point -8.0 18.0 32.0)))
      (ok (equal? (mult transform v) (make-vec -8.0 18.0 32.0)))
      (ok (equal? (mult inv v) (make-vec -2.0 2.0 2.0))))))

(deftest transform-matrix-testing
  (testing "The transformation matrix for the default orientation"
    (let* ((from (make-point 0.0 0.0 0.0))
           (to (make-point 0.0 0.0 -1.0))
           (up (make-vec 0.0 1.0 0.0))
           (trans (view-transform from to up)))
      (equal? trans (identity-matrix))))
  (testing "A view transformation matrix looking in positive z direction"
    (let* ((from (make-point 0.0 0.0 0.0))
           (to (make-point 0.0 0.0 1.0))
           (up (make-vec 0.0 1.0 0.0))
           (trans (view-transform from to up)))
          (equal? trans (scaling -1.0 1.0 -1.0))))
  (testing "The view transformation moves the world"
    (let* ((from (make-point 0.0 0.0 8.0))
           (to (make-point 0.0 0.0 0.0))
           (up (make-vec 0.0 1.0 0.0))
           (trans (view-transform from to up)))
      (equal? trans (translation 0.0 0.0 -8.0))))
  (testing "An arbitrary view transformation"
    (let* ((from (make-point 1.0 3.0 2.0))
           (to (make-point 4.0 -2.0 8.0))
           (up (make-vec 1.0 1.0 0.0))
           (trans (view-transform from to up)))
      (equal? trans (create-matrix
                     -0.50709 0.50709 0.67612 -2.36643
                     0.76772 0.60609 0.12122 -2.82843
                     -0.35857 0.59761 -0.71714 0.00000
                     0.00000 0.00000 0.00000 1.00000)))))
