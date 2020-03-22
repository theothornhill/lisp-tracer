(in-package #:lisp-tracer-tests)

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
