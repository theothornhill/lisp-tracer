(in-package #:lisp-tracer-tests)

(defparameter black (make-color :red 0.0 :green 0.0 :blue 0.0))
(defparameter white (make-color :red 1.0 :green 1.0 :blue 1.0))

(deftest stripe-pattern
  (testing "Creating a stripe pattern"
    (let ((pattern (stripe-pattern white black)))
      (ok (equal? (pattern-a pattern) white))
      (ok (equal? (pattern-b pattern) black)))))

(deftest pattern-constant-alternates
  (testing "A stripe pattern is constant in y"
    (let ((pattern (stripe-pattern white black)))
      (ok (equal? (stripe-at pattern (make-point 0.0 0.0 0.0)) white))
      (ok (equal? (stripe-at pattern (make-point 0.0 1.0 0.0)) white))
      (ok (equal? (stripe-at pattern (make-point 0.0 2.0 0.0)) white))))
  (testing "A stripe pattern is constant in z"
    (let ((pattern (stripe-pattern white black)))
      (ok (equal? (stripe-at pattern (make-point 0.0 0.0 0.0)) white))
      (ok (equal? (stripe-at pattern (make-point 0.0 0.0 1.0)) white))
      (ok (equal? (stripe-at pattern (make-point 0.0 0.0 2.0)) white))))
  (testing "A stripe pattern alternates in x"
    (let ((pattern (stripe-pattern white black)))
      (ok (equal? (stripe-at pattern (make-point 0.0 0.0 0.0)) white))
      (ok (equal? (stripe-at pattern (make-point 0.9 0.0 0.0)) white))
      (ok (equal? (stripe-at pattern (make-point 1.0 0.0 0.0)) black))
      (ok (equal? (stripe-at pattern (make-point -0.1 0.0 0.0)) black))
      (ok (equal? (stripe-at pattern (make-point -1.0 0.0 0.0)) black))
      (ok (equal? (stripe-at pattern (make-point -1.1 0.0 0.0)) white)))))
