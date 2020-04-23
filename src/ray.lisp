(in-package #:lisp-tracer)

;;; TODO: Below macros are for readablilty of code. Find out
;;; how to make these nested accessors more general
(defmacro ray-direction-x (ray)
  `(tuple-x (ray-direction ,ray)))

(defmacro ray-direction-y (ray)
  `(tuple-y (ray-direction ,ray)))

(defmacro ray-direction-z (ray)
  `(tuple-z (ray-direction ,ray)))

(defmacro ray-origin-x (ray)
  `(tuple-x (ray-origin ,ray)))

(defmacro ray-origin-y (ray)
  `(tuple-y (ray-origin ,ray)))

(defmacro ray-origin-z (ray)
  `(tuple-z (ray-origin ,ray)))

(defun pos (ray time)
  (declare (ray ray) (float time))
  (add (ray-origin ray) (mult (ray-direction ray) time)))

(defun transform (ray matrix)
  (declare (type ray ray) (type matrix matrix))
  (make-ray :origin (mult matrix (ray-origin ray))
            :direction (mult matrix (ray-direction ray))))
