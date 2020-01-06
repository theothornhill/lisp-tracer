(in-package #:lisp-tracer-tests)

(deftest matrix-testing
  (testing "Constructing and inspecting a 4x4 matrix"
    (let ((matrix (matrix! 4 '(1 2 3 4
                               5.5 6.5 7.5 8.5
                               9 10 11 12
                               13.5 14.5 15.5 16.6))))
      (ok (= (m matrix 0 0) 1))
      (ok (= (m matrix 0 3) 4))
      (ok (= (m matrix 1 0) 5.5))
      (ok (= (m matrix 1 2) 7.5))
      (ok (= (m matrix 2 2) 11))
      (ok (= (m matrix 3 0) 13.5))
      (ok (= (m matrix 3 2) 15.5)))))

(deftest 2x2
  (testing "A 2x2 matrix ought to be representable"
    (let ((matrix (matrix! 2 '(-3 5 1 -2))))
      (ok (= (m matrix 0 0) -3))
      (ok (= (m matrix 0 1) 5))
      (ok (= (m matrix 1 0) 1))
      (ok (= (m matrix 1 1) -2)))))

(deftest 3x3
  (testing "A 3x3 matrix ought to be representable"
    (let ((matrix (matrix! 3 '(-3 5 0 1 -2 -7 0 1 1))))
      (ok (= (m matrix 0 0) -3))
      (ok (= (m matrix 1 1) -2))
      (ok (= (m matrix 2 2) 1)))))


(deftest matrix-equality
  (testing "Matrix equality with identical matrices"
    (let ((a (matrix! 4 '(1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2)))
          (b (matrix! 4 '(1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2))))
      (ok (equal? a b))))
  (testing "Matrix equality with different matrices"
    (let ((a (matrix! 4 '(1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2)))
          (b (matrix! 4 '(1 2 3 4 5 9 9 8 9 8 7 6 5 4 3 2))))
      (ng (equal? a b)))))
