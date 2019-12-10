(in-package #:lisp-tracer-colors)

(defclass color ()
  ((red
    :initarg :red
    :accessor red)
   (green
    :initarg :green
    :accessor green)
   (blue
    :initarg :blue
    :accessor blue)))

(defun color! (red green blue)
  (make-instance 'color
                 :red red
                 :green green
                 :blue blue))


(defun black ()
  (color! 0 0 0))

(defmethod print-object ((obj color) stream)
      (print-unreadable-object (obj stream :type nil)
        (with-accessors ((red red)
                         (green green)
                         (blue blue))
            obj
          (format stream "~a ~a ~a " red green blue))))
