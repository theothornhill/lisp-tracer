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

(deftest stripe-testing-object
  (testing "Stripes with an object tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern white black)))
      (setf (shape-transform object) (scaling 2.0 2.0 2.0))
      (ok (equal? (stripe-at-object pattern object (make-point 1.5 0.0 0.0))
                  white))))
  (testing "Stripes with a pattern tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern white black)))
      (setf (pattern-transform pattern) (scaling 2.0 2.0 2.0))
      (ok (equal? (stripe-at-object pattern object (make-point 1.5 0.0 0.0))
                  white))))
  (testing "Stripes with both an object and a pattern tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern white black)))
      (setf (shape-transform object) (scaling 2.0 2.0 2.0))
      (setf (pattern-transform pattern) (translation 0.5 0.0 0.0))
      (ok (equal? (stripe-at-object pattern object (make-point 2.5 0.0 0.0))
                  white)))))
