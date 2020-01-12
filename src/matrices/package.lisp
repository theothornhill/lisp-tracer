(defpackage #:lisp-tracer-matrices
  (:use #:cl)
  (:export #:matrix!
           #:matrix
           #:m
           #:dimensions
           #:grid
           #:identity-matrix
           #:transpose
           #:determinant
           #:submatrix
           #:minor
           #:cofactor
           #:invertible?
           #:inverse
           #:translation
           #:scaling
           #:rotation-x
           #:rotation-y
           #:rotation-z
           #:shearing))
