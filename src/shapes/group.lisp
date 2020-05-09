(in-package :lisp-tracer)

(defstruct (group (:include shape))
  (items nil :type (or list null)))

(defun add-child (group shape)
  (with-slots (items) group
    (with-slots (parent) shape
      (setf items (append items (list shape)))
      (setf parent group))))

(defmethod local-intersect ((group group) (ray ray))
  (with-slots (items) group
    (sort (mapcan (lambda (shape) (intersect shape ray)) items) #'tt<)))

(defun world-to-object (shape point)
  (with-slots (parent transform) shape 
    (when parent
      (setf point (world-to-object parent point)))
    (mult (inverse transform) point)))

(defun normal-to-world (shape normal)
  (with-slots (parent transform) shape
    (setf normal (mult (transpose (inverse transform)) normal))
    (setf (tuple-w normal) 0.0)
    (setf normal (normalize normal))
    (when parent
      (setf normal (normal-to-world parent normal)))
    normal))
