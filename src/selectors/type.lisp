;;;; src/selectors/type.lisp --- Type-based selectors (Phase 8)

(in-package :clad.selectors)

;;; ============================================================================
;;; Type Selector (Phase 8 Enhancement)
;;; ============================================================================

(defclass type-selector (base-selector)
  ((shape-type :initarg :shape-type
               :accessor selector-shape-type
               :type keyword
               :documentation "Geometric type keyword: :line, :circle, :plane, :cylinder, etc."))
  (:documentation "Selector for filtering shapes by geometric type.

  This selector queries the underlying OCCT geometry to determine the
  exact type of edges and faces.

  Edge types:
    :line      - Straight line edges
    :circle    - Full circular edges
    :ellipse   - Elliptical edges
    :bspline   - B-spline curves
    :bezier    - Bezier curves
    :other     - Other edge types

  Face types:
    :plane     - Planar faces
    :cylinder  - Cylindrical faces
    :sphere    - Spherical faces
    :cone      - Conical faces
    :torus     - Toroidal faces
    :bspline   - B-spline surfaces
    :other     - Other face types

  Examples:
    (make-instance 'type-selector :shape-type :plane)
    (make-instance 'type-selector :shape-type :circle)"))

;;; ============================================================================
;;; Implementation
;;; ============================================================================

(defmethod apply-selector ((selector type-selector) shape-list)
  "Select shapes matching the specified geometric type.

  Algorithm:
    1. Get desired type from selector
    2. For each shape, query its geometric type using CLOS method
    3. Return shapes matching the type

  The geometric type is determined by calling (geom-type shape) which
  queries the OCCT geometry via FFI."

  (when (null shape-list)
    (return-from apply-selector nil))

  (let ((desired-type (selector-shape-type selector)))

    ;; Filter shapes by geometric type
    (remove-if-not
     (lambda (shape)
       (handler-case
           (let ((shape-geom-type (clad.shapes:geom-type shape)))
             (eq shape-geom-type desired-type))
         ;; If we can't determine type, exclude this shape
         (error () nil)))
     shape-list)))
