(in-package #:lisp-tracer-tests)

(deftest canvas-test
  (testing "Creating a canvas"
    (let ((c (canvas! 10 20)))
      (ok (equal? (width c) 10))
      (ok (equal? (height c) 20)))))

(run-suite *package*)
