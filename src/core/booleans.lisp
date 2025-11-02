;;;; src/core/booleans.lisp --- Functional core for boolean operations

(in-package :clad.core)

;;; ============================================================================
;;; Boolean Operations
;;; ============================================================================

(defun union-shapes (&rest shapes)
  "Compute boolean union of multiple shapes.

  Arguments:
    shapes - One or more shapes to union

  Returns: New shape representing the union

  Example:
    (union-shapes box1 box2 box3)
    (union-shapes (make-box 10 10 10) (translate (make-box 5 5 5) 5 0 0))"
  (when (null shapes)
    (error "UNION-SHAPES requires at least one shape"))

  (reduce (lambda (s1 s2)
            (ensure-shape s1)
            (ensure-shape s2)
            (let ((result-handle (clad.ffi:ffi-union
                                  (shape-handle s1)
                                  (shape-handle s2))))
              (make-shape result-handle)))
          shapes))

(defun cut-shapes (base-shape &rest tool-shapes)
  "Subtract tool shapes from base shape.

  Arguments:
    base-shape  - Shape to subtract from
    tool-shapes - One or more shapes to subtract

  Returns: New shape with material removed

  Example:
    (cut-shapes box cylinder)
    (cut-shapes housing bolt-hole1 bolt-hole2 shaft-hole)"
  (when (null tool-shapes)
    (error "CUT-SHAPES requires at least one tool shape"))

  (ensure-shape base-shape)

  (reduce (lambda (base tool)
            (ensure-shape tool)
            (let ((result-handle (clad.ffi:ffi-cut
                                  (shape-handle base)
                                  (shape-handle tool))))
              (make-shape result-handle)))
          tool-shapes
          :initial-value base-shape))

(defun intersect-shapes (&rest shapes)
  "Compute boolean intersection of multiple shapes.

  Arguments:
    shapes - One or more shapes to intersect

  Returns: New shape representing the intersection (common volume)

  Example:
    (intersect-shapes box1 box2)
    (intersect-shapes sphere1 sphere2 sphere3)"
  (when (null shapes)
    (error "INTERSECT-SHAPES requires at least one shape"))

  (reduce (lambda (s1 s2)
            (ensure-shape s1)
            (ensure-shape s2)
            (let ((result-handle (clad.ffi:ffi-intersect
                                  (shape-handle s1)
                                  (shape-handle s2))))
              (make-shape result-handle)))
          shapes))

;;; ============================================================================
;;; Convenience Aliases
;;; ============================================================================

(defun fuse-shapes (&rest shapes)
  "Alias for UNION-SHAPES"
  (apply #'union-shapes shapes))
