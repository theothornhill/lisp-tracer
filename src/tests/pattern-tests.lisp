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
      (ok (equal? (pattern-at pattern (make-point 0.0 0.0 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point 0.0 1.0 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point 0.0 2.0 0.0)) white))))
  (testing "A stripe pattern is constant in z"
    (let ((pattern (stripe-pattern white black)))
      (ok (equal? (pattern-at pattern (make-point 0.0 0.0 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point 0.0 0.0 1.0)) white))
      (ok (equal? (pattern-at pattern (make-point 0.0 0.0 2.0)) white))))
  (testing "A stripe pattern alternates in x"
    (let ((pattern (stripe-pattern white black)))
      (ok (equal? (pattern-at pattern (make-point 0.0 0.0 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point 0.9 0.0 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point 1.0 0.0 0.0)) black))
      (ok (equal? (pattern-at pattern (make-point -0.1 0.0 0.0)) black))
      (ok (equal? (pattern-at pattern (make-point -1.0 0.0 0.0)) black))
      (ok (equal? (pattern-at pattern (make-point -1.1 0.0 0.0)) white)))))

(deftest stripe-testing-object
  (testing "Stripes with an object tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern white black)))
      (setf (shape-transform object) (scaling 2.0 2.0 2.0))
      (ok (equal? (pattern-at-object pattern object (make-point 1.5 0.0 0.0))
                  white))))
  (testing "Stripes with a pattern tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern white black)))
      (setf (pattern-transform pattern) (scaling 2.0 2.0 2.0))
      (ok (equal? (pattern-at-object pattern object (make-point 1.5 0.0 0.0))
                  white))))
  (testing "Stripes with both an object and a pattern tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern white black)))
      (setf (shape-transform object) (scaling 2.0 2.0 2.0))
      (setf (pattern-transform pattern) (translation 0.5 0.0 0.0))
      (ok (equal? (pattern-at-object pattern object (make-point 2.5 0.0 0.0))
                  white)))))

(deftest gradient-pattern-tests
  (testing "A gradient linearly interpolates between colors"
    (let ((pattern (gradient-pattern white black)))
      (equal? (pattern-at pattern (make-point 0.0 0.0 0.0)) white)
      (equal? (pattern-at pattern (make-point 0.25 0.0 0.0))
              (make-color :red 0.75 :green  0.75 :blue  0.75))
      (equal? (pattern-at pattern (make-point 0.5 0.0 0.0))
              (make-color :red 0.5 :green  0.5 :blue  0.5))
      (equal? (pattern-at pattern (make-point 0.75 0.0 0.0))
              (make-color :red 0.25 :green  0.25 :blue  0.25)))))

(deftest ring-pattern-tests
  (testing "A ring should extend in both x and z"
    (let ((pattern (ring-pattern white black)))
      (equal? (pattern-at pattern (make-point 0.0 0.0 0.0)) white)
      (equal? (pattern-at pattern (make-point 1.0 0.0 0.0)) black)
      (equal? (pattern-at pattern (make-point 0.0 0.0 1.0)) black)
      (equal? (pattern-at pattern (make-point 0.708 0.0 0.708)) black))))

(deftest checkers-pattern-tests
  (let ((pattern (checkers-pattern white black)))
    (testing "Checkers should repeat in x"
      (equal? (pattern-at pattern (make-point 0.0 0.0 0.0)) white)
      (equal? (pattern-at pattern (make-point 0.99 0.0 0.0)) white)
      (equal? (pattern-at pattern (make-point 1.01 0.0 0.0)) black))
    (testing "Checkers should repeat in y"
      (equal? (pattern-at pattern (make-point 0.0 0.0 0.0)) white)
      (equal? (pattern-at pattern (make-point 0.0 0.99 0.0)) white)
      (equal? (pattern-at pattern (make-point 0.0 1.01 0.0)) black))
    (testing "Checkers should repeat in z"
      (equal? (pattern-at pattern (make-point 0.0 0.0 0.0)) white)
      (equal? (pattern-at pattern (make-point 0.0 0.0 0.99)) white)
      (equal? (pattern-at pattern (make-point 0.0 0.0 1.01)) black))))
