(in-package #:lisp-tracer-tests)

(deftest color-test
  (testing "Colors are (red, green, blue) tuples"
    (let ((c (make-color -0.5 0.4 1.7)))
      (ok (equal? (red c) -0.5))
      (ok (equal? (green c) 0.4))
      (ok (equal? (blue c) 1.7)))))

(deftest color-maths
  (testing "Adding colors"
    (let ((c1 (make-color 0.9 0.6 0.75))
          (c2 (make-color 0.7 0.1 0.25)))
      (ok (equal? (add c1 c2) (make-color 1.6 0.7 1.0)))))
  (testing "Subtracting colors"
    (let ((c1 (make-color 0.9 0.6 0.75))
          (c2 (make-color 0.7 0.1 0.25)))
      (ok (equal? (sub c1 c2) (make-color 0.2 0.5 0.5)))))
  (testing "Multiplying a color by a scalar"
    (let ((c (make-color 0.2 0.3 0.4)))
      (ok (equal? (mult c 2.0) (make-color 0.4 0.6 0.8)))))
  (testing "Multiplying colors"
    (let ((c1 (make-color 1 0.2 0.4))
          (c2 (make-color 0.9 1 0.1)))
      (ok (equal? (mult c1 c2) (make-color 0.9 0.2 0.04))))))
