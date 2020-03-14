(in-package #:lisp-tracer-tests)

(deftest point-light-testing
  (testing "A point light has a position and intensity"
    (let* ((intensity (make-color 1 1 1))
           (position (make-point 0f0 0f0 0f0))
           (light (point-light position intensity)))
      (ok (equal? (posit light) position))
      (ok (equal? (intensity light) intensity)))))
