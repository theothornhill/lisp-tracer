(in-package :lisp-tracer-tests)

(deftest color-test
  (testing "Colors are (red, green, blue) tuples"
    (let ((c (make-color :red -0.5 :green 0.4 :blue 1.7)))
      (ok (equal? (color-red c) -0.5))
      (ok (equal? (color-green c) 0.4))
      (ok (equal? (color-blue c) 1.7)))))

(deftest color-maths
  (testing "Adding colors"
    (let ((c1 (make-color :red 0.9 :green 0.6 :blue 0.75))
          (c2 (make-color :red 0.7 :green 0.1 :blue 0.25)))
      (ok (equal? (add c1 c2) (make-color :red 1.6 :green 0.7 :blue 1.0)))))
  (testing "Subtracting colors"
    (let ((c1 (make-color :red 0.9 :green 0.6 :blue 0.75))
          (c2 (make-color :red 0.7 :green 0.1 :blue 0.25)))
      (ok (equal? (sub c1 c2) (make-color :red 0.2 :green 0.5 :blue 0.5)))))
  (testing "Multiplying a color by a scalar"
    (let ((c (make-color :red 0.2 :green 0.3 :blue 0.4)))
      (ok (equal? (mult c 2.0) (make-color :red 0.4 :green 0.6 :blue 0.8)))))
  (testing "Multiplying colors"
    (let ((c1 (make-color :red 1.0 :green 0.2 :blue 0.4))
          (c2 (make-color :red 0.9 :green 1.0 :blue 0.1)))
      (ok (equal? (mult c1 c2) (make-color :red 0.9 :green 0.2 :blue 0.04))))))
