(in-package #:lisp-tracer)

(defconstant epsilon 0.00001)

(defstruct camera
  (hsize 0 :type integer)
  (vsize 0 :type integer)
  (half-width 0.0 :type double-float)
  (half-height 0.0 :type double-float)
  (field-of-view 0.0 :type double-float)
  (transform (identity-matrix) :type matrix)
  (pixel-size 0.0 :type double-float))

(defstruct canvas
  width
  height
  grid)

(defstruct color
  (red 0.0 :type double-float)
  (green 0.0 :type double-float)
  (blue 0.0 :type double-float))

(defstruct light
  position
  intensity)

(defstruct matrix
  (m00 1.0 :type double-float)
  (m01 0.0 :type double-float)
  (m02 0.0 :type double-float)
  (m03 0.0 :type double-float)
  (m10 0.0 :type double-float)
  (m11 1.0 :type double-float)
  (m12 0.0 :type double-float)
  (m13 0.0 :type double-float)
  (m20 0.0 :type double-float)
  (m21 0.0 :type double-float)
  (m22 1.0 :type double-float)
  (m23 0.0 :type double-float)
  (m30 0.0 :type double-float)
  (m31 0.0 :type double-float)
  (m32 0.0 :type double-float)
  (m33 1.0 :type double-float))

(defstruct ray
  (origin nil :type tuple)
  (direction nil :type tuple))

(defstruct tuple
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (z 0.0 :type double-float)
  (w 0.0 :type double-float))

(defstruct world
  (objects nil)
  (light nil))

(defstruct rt-intersection
  (tt nil :type double-float)
  (object nil :type shape))

(defstruct computations
  (tt 0.0 :type double-float)
  (n1 0.0 :type double-float)
  (n2 0.0 :type double-float)
  (object nil :type shape)
  (inside? nil :type t)
  (point (make-point) :type tuple)
  (over-point (make-point) :type tuple)
  (under-point (make-point) :type tuple)
  (eyev (make-vec) :type tuple)
  (normalv (make-vec) :type tuple)
  (reflectv (make-vec) :type tuple))
