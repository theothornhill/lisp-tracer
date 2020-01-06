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

(deftest matrix-math
  (testing "Multiplying two matrices"
    (let ((a (matrix! 4 '(1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2)))
          (b (matrix! 4 '(-2 1 2 3 3 2 1 -1 4 3 6 5 1 2 7 8)))
          (a*b (matrix! 4 '(20 22 50 48 44 54 114 108 40 58 110 102 16 26 46 42))))
      (ok (equal? (mult a b) a*b))))
  (testing "Multiplying matrix by a tuple"
    (let ((a (matrix! 4 '(1 2 3 4 2 4 4 2 8 6 4 1 0 0 0 1)))
          (b (tuple! 1 2 3 1)))
      (ok (equal? (mult a b) (tuple! 18 24 33 1)))))
  (testing "Multiplying matrix by identity matrix"
    (let ((a (matrix! 4 '(0 1 2 4 1 2 4 8 2 4 8 16 4 8 16 32)))
          (id-matrix (identity-matrix)))
      (ok (equal? (mult a id-matrix) a)))))

(deftest transposing-matrix
  (testing "Transposing a matrix"
    (let ((a (matrix! 4 '(0 9 3 0 9 8 0 8 1 8 5 3 0 0 5 8)))
          (b (matrix! 4 '(0 9 1 0 9 8 8 0 3 0 5 5 0 8 3 8))))
      (ok (equal? (transpose a) b))))
  (testing "Transposing the identity matrix"
    (let ((a (transpose (identity-matrix))))
      (ok (equal? a (identity-matrix))))))

(deftest determinant-determination
  (testing "Calculating the determinant of a 2x2 matrix"
    (let ((a (matrix! 2 '(1 5 -3 2))))
      (ok (equal? (determinant a) 17)))))

(deftest submatrices
  (testing "A submatrix of a 3x3 matrix is a 2x2 matrix"
    (let ((a (matrix! 3 '(1 5 0 -3 2 7 0 6 -3)))
          (b (matrix! 2 '(-3 2 0 6))))
      (ok (equal? (submatrix a 0 2) b))))
  (testing "A submatrix of a 4x4 matrix is a 3x3 matrix"
    (let ((a (matrix! 4 '(-6 1 1 6 -8 5 8 6 -1 0 8 2 -7 1 -1 1)))
          (b (matrix! 3 '(-6 1 6 -8 8 6 -7 -1 1))))
      (ok (equal? (submatrix a 2 1) b)))))
