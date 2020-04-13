(in-package #:lisp-tracer-tests)

(deftest point-light-testing
  (testing "A point light has a position and intensity"
    (let* ((intensity (make-color :red 1.0 :green 1.0 :blue 1.0))
           (position (make-point :x 0.0 :y 0.0 :z 0.0))
           (light (point-light :position position :position intensity)))
      (ok (equal? (light-position light) position))
      (ok (equal? (light-intensity light) intensity)))))
