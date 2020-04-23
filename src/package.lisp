(defpackage #:lisp-tracer
  (:use #:cl
        #:iterate)
  (:export
   #:add
   #:air
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
   #:computations-n1
   #:computations-n2
   #:computations-normalv
   #:computations-object
   #:computations-over-point
   #:computations-point
   #:computations-reflectv
   #:computations-tt
   #:computations-under-point
   #:create-camera
   #:create-canvas
   #:create-matrix
   #:cross
   #:cylinder-closed
   #:cylinder-maximum
   #:cylinder-minimum
   #:default-world
   #:diamond
   #:div
   #:dot
   #:epsilon
   #:equal?
   #:glass
   #:glass-sphere
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
   #:make-cube
   #:make-cylinder
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
   #:material-reflective
   #:material-refractive-index
   #:material-shadowed?
   #:material-shininess
   #:material-specular
   #:material-transparency
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
   #:ray-direction-x
   #:ray-direction-y
   #:ray-direction-z
   #:ray-for-pixel
   #:ray-origin
   #:ray-origin-x
   #:ray-origin-y
   #:ray-origin-z
   #:reflect
   #:reflected-color
   #:refracted-color
   #:render
   #:ring-pattern
   #:rotation-x
   #:rotation-y
   #:rotation-z
   #:rt-intersection-object
   #:rt-intersection-tt
   #:scaling
   #:scaling-x
   #:scaling-y
   #:scaling-z
   #:schlick
   #:shade-hit
   #:shape-material
   #:shape-transform
   #:shearing
   #:sphere
   #:stripe-pattern
   #:sub
   #:test-pattern
   #:transform
   #:transform-object
   #:translation
   #:translation-x
   #:translation-y
   #:translation-z
   #:transpose
   #:tuple
   #:tuple-w
   #:tuple-x
   #:tuple-y
   #:tuple-z
   #:vacuum
   #:vec
   #:vec?
   #:view-transform
   #:water
   #:world
   #:world-light
   #:world-objects
   #:write-pixel
   ))
