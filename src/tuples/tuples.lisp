(in-package #:lisp-tracer-tuples)

(defun tuple (x y z w)
  (list x y z w))

(defun point (x y z)
  (tuple x y z 1))

(defun vec (x y z)
  (tuple x y z 0))

(defun zerovec ()
  (vec 0 0 0))

(defun x (tuple)
  (first tuple))

(defun y (tuple)
  (second tuple))

(defun z (tuple)
  (third tuple))

(defun w (tuple)
  (fourth tuple))

(defun point? (tuple)
  (not (zerop (w tuple))))

(defun vec? (tuple)
  (zerop (w tuple)))
