(in-package #:lisp-tracer)

(defun view-transform (from to up)
  (declare (type tuple from to up))
  (let* ((forward (normalize (sub to from)))
         (left (cross forward (normalize up)))
         (true-up (cross left forward))
         (negated-forward (normalize (sub to from)))
         (negated-from (neg from))
         (orientation (create-matrix
                       (tuple-x left)
                       (tuple-y left)
                       (tuple-z left)
                       0.0
                       (tuple-x true-up)
                       (tuple-y true-up)
                       (tuple-z true-up)
                       0.0
                       (tuple-x negated-forward)
                       (tuple-y negated-forward)
                       (tuple-z negated-forward)
                       0.0
                       0.0
                       0.0
                       0.0
                       1.0)))
    (mult orientation (translation (tuple-x negated-from)
                                   (tuple-y negated-from)
                                   (tuple-z negated-from)))))
