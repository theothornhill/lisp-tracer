(in-package #:lisp-tracer-tests)

(deftest point-light-testing
  (testing "A point light has a position and intensity"
    (let* ((intensity (make-color :red 1 :green 1 :blue 1))
           (position (make-point 0f0 0f0 0f0))
           (light (point-light position intensity)))
      (ok (equal? (light-position light) position))
      (ok (equal? (light-intensity light) intensity)))))
