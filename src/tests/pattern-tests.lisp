(in-package #:lisp-tracer-tests)

(defparameter black (make-color :red 0.0 :green 0.0 :blue 0.0))
(defparameter white (make-color :red 1.0 :green 1.0 :blue 1.0))

(deftest stripe-pattern
  (testing "Creating a stripe pattern"
    (let ((pattern (stripe-pattern)))
      (ok (equal? (pattern-a pattern) white))
      (ok (equal? (pattern-b pattern) black)))))

(deftest pattern-constant-alternates
  (testing "A stripe pattern is constant in y"
    (let ((pattern (stripe-pattern)))
      (ok (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point :x 0.0 :y 1.0 :z 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point :x 0.0 :y 2.0 :z 0.0)) white))))
  (testing "A stripe pattern is constant in z"
    (let ((pattern (stripe-pattern)))
      (ok (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 1.0)) white))
      (ok (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 2.0)) white))))
  (testing "A stripe pattern alternates in x"
    (let ((pattern (stripe-pattern)))
      (ok (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point :x 0.9 :y 0.0 :z 0.0)) white))
      (ok (equal? (pattern-at pattern (make-point :x 1.0 :y 0.0 :z 0.0)) black))
      (ok (equal? (pattern-at pattern (make-point :x -0.1 :y 0.0 :z 0.0)) black))
      (ok (equal? (pattern-at pattern (make-point :x -1.0 :y 0.0 :z 0.0)) black))
      (ok (equal? (pattern-at pattern (make-point :x -1.1 :y 0.0 :z 0.0)) white)))))

(deftest stripe-testing-object
  (testing "Stripes with an object tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern)))
      (setf (shape-transform object) (scaling 2.0 2.0 2.0))
      (ok (equal? (pattern-at-object pattern
                                     object
                                     (make-point :x 1.5 :y 0.0 :z 0.0))
                  white))))
  (testing "Stripes with a pattern tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern)))
      (setf (pattern-transform pattern) (scaling 2.0 2.0 2.0))
      (ok (equal? (pattern-at-object pattern
                                     object
                                     (make-point :x 1.5 :y 0.0 :z 0.0))
                  white))))
  (testing "Stripes with both an object and a pattern tranformation"
    (let ((object (make-sphere))
          (pattern (stripe-pattern)))
      (setf (shape-transform object) (scaling 2.0 2.0 2.0))
      (setf (pattern-transform pattern) (translation 0.5 0.0 0.0))
      (ok (equal? (pattern-at-object pattern
                                     object
                                     (make-point :x 2.5 :y 0.0 :z 0.0))
                  white)))))

(deftest gradient-pattern-tests
  (testing "A gradient linearly interpolates between colors"
    (let ((pattern (gradient-pattern)))
      (equal? (pattern-at pattern
                          (make-point :x 0.0 :y 0.0 :z 0.0)) white)
      
      (equal? (pattern-at pattern (make-point :x 0.25 :y 0.0 :z 0.0))
              (make-color :red 0.75 :green  0.75 :blue  0.75))
      (equal? (pattern-at pattern (make-point :x 0.5 :y 0.0 :z 0.0))
              (make-color :red 0.5 :green  0.5 :blue  0.5))
      (equal? (pattern-at pattern (make-point :x 0.75 :y 0.0 :z 0.0))
              (make-color :red 0.25 :green  0.25 :blue  0.25)))))

(deftest ring-pattern-tests
  (testing "A ring should extend in both x and z"
    (let ((pattern (ring-pattern)))
      (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 0.0)) white)
      (equal? (pattern-at pattern (make-point :x 1.0 :y 0.0 :z 0.0)) black)
      (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 1.0)) black)
      (equal? (pattern-at pattern (make-point :x 0.708 :y 0.0 :z 0.708)) black))))

(deftest checkers-pattern-tests
  (let ((pattern (checkers-pattern :transform (identity-matrix))))
    (testing "Checkers should repeat in x"
      (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 0.0)) white)
      (equal? (pattern-at pattern (make-point :x 0.99 :y 0.0 :z 0.0)) white)
      (equal? (pattern-at pattern (make-point :x 1.01 :y 0.0 :z 0.0)) black))
    (testing "Checkers should repeat in y"
      (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 0.0)) white)
      (equal? (pattern-at pattern (make-point :x 0.0 :y 0.99 :z 0.0)) white)
      (equal? (pattern-at pattern (make-point :x 0.0 :y 1.01 :z 0.0)) black))
    (testing "Checkers should repeat in z"
      (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 0.0)) white)
      (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 0.99)) white)
      (equal? (pattern-at pattern (make-point :x 0.0 :y 0.0 :z 1.01)) black))))
