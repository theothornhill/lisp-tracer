(in-package #:lisp-tracer)

(defclass color ()
  ((red
    :initarg :red
    :accessor red
    :documentation "The red value of a color.")
   (green
    :initarg :green
    :accessor green
    :documentation "The green value of a color.")
   (blue
    :initarg :blue
    :accessor blue
    :documentation "The blue value of a color."))
  (:documentation "A COLOR is an object with slots RED GREEN BLUE."))

(defun make-color (red green blue)
  "Create an instance of COLOR with values RED GREEN BLUE."
  (make-instance 'color
                 :red red
                 :green green
                 :blue blue))


(defun black ()
  "Create a color with R G B set to 0 0 0."
  (make-color 0 0 0))

(defmethod print-object ((obj color) stream)
  (print-unreadable-object (obj stream :type nil)
    (with-accessors ((red red)
                     (green green)
                     (blue blue))
        obj
      (format stream "~a ~a ~a " red green blue))))
