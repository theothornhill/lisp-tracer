(in-package #:lisp-tracer-tests)

(deftest translation-testing
  (testing "Multiplying by a translation matrix"
    (let* ((transform (translation 5 -3 2))
           (p (make-point -3 4 5))
           (inv (inverse transform))
           (v (make-vec -3 4 5)))
      (ok (equal? (mult transform p) (make-point 2 1 7)))
      (ok (equal? (mult inv p) (make-point -8 7 3)))
      (ok (equal? (mult transform v) v)))))

(deftest scaling-testing
  (testing "A scaling matrix applied to a point"
    (let* ((transform (scaling 2 3 4))
           (p (make-point -4 6 8))
           (v (make-vec -4 6 8))
           (inv (inverse transform)))
      (ok (equal? (mult transform p) (make-point -8 18 32)))
      (ok (equal? (mult transform v) (make-vec -8 18 32)))
      (ok (equal? (mult inv v) (make-vec -2 2 2))))))
