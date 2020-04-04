(defpackage #:lisp-tracer
  (:use #:cl
        #:iterate)
  (:export
   #:add
   #:camera
   #:camera-field-of-view
   #:camera-hsize
   #:camera-pixel-size
   #:camera-transform
   #:camera-vsize
   #:canvas
   #:canvas-height
   #:canvas-to-ppm
   #:canvas-width
   #:checkers-pattern
   #:color
   #:color-at
   #:color-blue
   #:color-green
   #:color-red
   #:computations-eyev
   #:computations-inside?
   #:computations-normalv
   #:computations-object
   #:computations-over-point
   #:computations-point
   #:computations-tt
   #:create-camera
   #:create-canvas
   #:create-matrix
   #:cross
   #:default-world
   #:div
   #:dot
   #:epsilon
   #:equal?
   #:gradient-pattern
   #:hit
   #:identity-matrix
   #:intersect
   #:intersect-world
   #:intersection
   #:intersections
   #:inverse
   #:is-shadowed?
   #:light-intensity
   #:light-position
   #:lighting
   #:local-intersect
   #:local-normal-at
   #:magnitude
   #:make-camera
   #:make-canvas
   #:make-color
   #:make-intersection
   #:make-material
   #:make-matrix
   #:make-pattern
   #:make-plane
   #:make-point
   #:make-ray
   #:make-shape
   #:make-sphere
   #:make-tuple
   #:make-vec
   #:make-world
   #:material
   #:material-ambient
   #:material-color
   #:material-diffuse
   #:material-pattern
   #:material-shadowed?
   #:material-shininess
   #:material-specular
   #:matrix
   #:matrix-m00
   #:matrix-m01
   #:matrix-m02
   #:matrix-m03
   #:matrix-m10
   #:matrix-m11
   #:matrix-m12
   #:matrix-m13
   #:matrix-m20
   #:matrix-m21
   #:matrix-m22
   #:matrix-m23
   #:matrix-m30
   #:matrix-m31
   #:matrix-m32
   #:matrix-m33
   #:mult
   #:neg
   #:normal-at
   #:normalize
   #:pattern
   #:pattern-a
   #:pattern-at
   #:pattern-at-object
   #:pattern-b
   #:pattern-transform
   #:pixel-at
   #:point
   #:point-light
   #:point?
   #:pos
   #:prepare-computations
   #:ray
   #:ray-direction
   #:ray-for-pixel
   #:ray-origin
   #:reflect
   #:render
   #:ring-pattern
   #:rotation-x
   #:rotation-y
   #:rotation-z
   #:rt-intersection-object
   #:rt-intersection-tt
   #:scaling
   #:shade-hit
   #:shape-material
   #:shape-transform
   #:shearing
   #:sphere
   #:stripe-pattern
   #:sub
   #:transform
   #:transform-object
   #:translation
   #:transpose
   #:tuple
   #:tuple-w
   #:tuple-x
   #:tuple-y
   #:tuple-z
   #:vec
   #:vec?
   #:view-transform
   #:world
   #:world-light
   #:world-objects
   #:write-pixel
   #:zerovec
   ))
