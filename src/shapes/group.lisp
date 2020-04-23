(in-package #:lisp-tracer)

(defstruct (group (:include shape))
  (items nil :type (or list null)))

(defun add-child (group shape)
  (setf (group-items group) (append (group-items group) (list shape)))
  (setf (shape-parent shape) group))

(defmethod local-intersect ((group group) (ray ray))
  (sort (mapcan (lambda (shape) (intersect shape ray))
                (group-items group))
        #'tt<))

(defun world-to-object (shape point)
  (when (shape-parent shape)
    (setf point (world-to-object (shape-parent shape) point)))
  (mult (inverse (shape-transform shape)) point))
