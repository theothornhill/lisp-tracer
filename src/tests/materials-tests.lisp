(in-package #:lisp-tracer-tests)

(deftest material-test
  (testing "The default material"
    (let ((m (make-material)))
      (ok (equal? (material-color m) (make-color 1 1 1)))
      (ok (equal? (material-color m) (make-color 1 1 1)))
      (ok (equal? (ambient m) 0.1))
      (ok (equal? (diffuse m) 0.9))
      (ok (equal? (specular m) 0.9))
      (ok (equal? (shininess m) 200)))))
