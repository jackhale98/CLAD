;;;; src/selectors/utilities.lisp --- Utility functions for selectors

(in-package :clad.selectors)

;;; ============================================================================
;;; Vector Utilities
;;; ============================================================================

(defun normalize-vector (x y z)
  "Normalize a 3D vector to unit length.

  Arguments:
    x, y, z - Vector components

  Returns: (values nx ny nz) - normalized components"
  (let ((length (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (< length 1e-10)
        (values 0.0d0 0.0d0 0.0d0)  ; Zero vector
        (values (/ x length)
                (/ y length)
                (/ z length)))))

(defun dot-product (x1 y1 z1 x2 y2 z2)
  "Compute dot product of two 3D vectors.

  Arguments:
    x1, y1, z1 - First vector components
    x2, y2, z2 - Second vector components

  Returns: Dot product (scalar)"
  (+ (* x1 x2) (* y1 y2) (* z1 z2)))

(defun vector-length (x y z)
  "Compute length of a 3D vector.

  Arguments:
    x, y, z - Vector components

  Returns: Length (scalar)"
  (sqrt (+ (* x x) (* y y) (* z z))))

(defun parallel-p (x1 y1 z1 x2 y2 z2 tolerance)
  "Check if two vectors are parallel within tolerance.

  Two normalized vectors are parallel if |dot(v1, v2)| ≈ 1

  Arguments:
    x1, y1, z1 - First vector components
    x2, y2, z2 - Second vector components
    tolerance  - Tolerance for comparison

  Returns: T if parallel, NIL otherwise"
  (multiple-value-bind (nx1 ny1 nz1)
      (normalize-vector x1 y1 z1)
    (multiple-value-bind (nx2 ny2 nz2)
        (normalize-vector x2 y2 z2)
      (let ((dot (abs (dot-product nx1 ny1 nz1 nx2 ny2 nz2))))
        (> dot (- 1.0 tolerance))))))

(defun perpendicular-p (x1 y1 z1 x2 y2 z2 tolerance)
  "Check if two vectors are perpendicular within tolerance.

  Two vectors are perpendicular if dot(v1, v2) ≈ 0

  Arguments:
    x1, y1, z1 - First vector components
    x2, y2, z2 - Second vector components
    tolerance  - Tolerance for comparison

  Returns: T if perpendicular, NIL otherwise"
  (multiple-value-bind (nx1 ny1 nz1)
      (normalize-vector x1 y1 z1)
    (multiple-value-bind (nx2 ny2 nz2)
        (normalize-vector x2 y2 z2)
      (let ((dot (abs (dot-product nx1 ny1 nz1 nx2 ny2 nz2))))
        (< dot tolerance)))))

;;; ============================================================================
;;; Axis Conversion
;;; ============================================================================

(defun axis-keyword-to-vector (axis-keyword)
  "Convert axis keyword to unit vector.

  Arguments:
    axis-keyword - One of :x, :y, :z, :+x, :+y, :+z, :-x, :-y, :-z

  Returns: (values x y z) - unit vector components"
  (ecase axis-keyword
    ((:x :+x)  (values 1.0d0 0.0d0 0.0d0))
    ((:y :+y)  (values 0.0d0 1.0d0 0.0d0))
    ((:z :+z)  (values 0.0d0 0.0d0 1.0d0))
    (:-x       (values -1.0d0 0.0d0 0.0d0))
    (:-y       (values 0.0d0 -1.0d0 0.0d0))
    (:-z       (values 0.0d0 0.0d0 -1.0d0))))

;;; ============================================================================
;;; Comparison Utilities
;;; ============================================================================

(defun approx-equal (a b tolerance)
  "Check if two numbers are approximately equal within tolerance.

  Arguments:
    a, b      - Numbers to compare
    tolerance - Tolerance for comparison

  Returns: T if approximately equal, NIL otherwise"
  (< (abs (- a b)) tolerance))
