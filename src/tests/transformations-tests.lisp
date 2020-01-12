(in-package #:lisp-tracer-tests)

(deftest translation-testing
  (testing "Multiplying by a translation matrix"
    (let* ((transform (translation 5 -3 2))
           (p (point! -3 4 5))
           (inv (inverse transform))
           (v (vec! -3 4 5)))
      (ok (equal? (mult transform p) (point! 2 1 7)))
      (ok (equal? (mult inv p) (point! -8 7 3)))
      (ok (equal? (mult transform v) v)))))

(deftest scaling-testing
  (testing "A scaling matrix applied to a point"
    (let* ((transform (scaling 2 3 4))
           (p (point! -4 6 8))
           (v (vec! -4 6 8))
           (inv (inverse transform)))
      (ok (equal? (mult transform p) (point! -8 18 32)))
      (ok (equal? (mult transform v) (vec! -8 18 32)))
      (ok (equal? (mult inv v) (vec! -2 2 2))))))
