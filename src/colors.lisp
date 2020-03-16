(in-package #:lisp-tracer)

(defstruct color
  (red 0.0)
  (green 0.0)
  (blue 0.0))

;; (defmethod print-object ((obj color) stream)
;;   (print-unreadable-object (obj stream :type nil)
;;     (with-accessors ((color-red color-red)
;;                      (color-green color-green)
;;                      (color-blue color-blue))
;;         obj
;;       (format stream "~a ~a ~a " color-red color-green color-blue))))
