# CLAD Phase 8: Advanced Features - Implementation Plan

**Version:** 1.0
**Date:** 2025-11-01
**Status:** Design Specification
**Dependencies:** Phases 1-7 Complete

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Selector System Enhancements](#selector-system-enhancements)
3. [Feature 1: Fillets and Chamfers](#feature-1-fillets-and-chamfers)
4. [Feature 2: Splines and Complex Curves](#feature-2-splines-and-complex-curves)
5. [Feature 3: Sweeps](#feature-3-sweeps)
6. [Feature 4: Lofts](#feature-4-lofts)
7. [Feature 5: Shelling](#feature-5-shelling)
8. [Feature 6: Mirroring](#feature-6-mirroring)
9. [Feature 7: Draft Angles](#feature-7-draft-angles)
10. [DSL Integration](#dsl-integration)
11. [Testing Strategy](#testing-strategy)
12. [Implementation Timeline](#implementation-timeline)
13. [Examples Library](#examples-library)

---

## Executive Summary

### Goals

Phase 8 completes CLAD's production-ready feature set by implementing advanced CAD operations that enable building complex, real-world mechanical parts. This phase focuses on:

1. **Manufacturing readiness** - Fillets, chamfers, draft angles
2. **Organic shapes** - Splines, lofts, sweeps
3. **Design efficiency** - Shell, mirror, advanced patterns
4. **Professional quality** - Complete example library

### Key Principles

1. **Test-Driven Development** - Write tests before implementation
2. **Common Lisp Strengths** - Leverage macros, CLOS, conditions
3. **Layered Architecture** - Maintain FFI → Core → CLOS → Context → DSL
4. **Selector-Driven** - All operations integrate with selector system
5. **Composability** - Features combine naturally in DSL

### Deliverables

- 7 new FFI modules with OCCT bindings
- Enhanced selector system with 10+ new selector types
- Core API extensions for all advanced features
- Context API methods for sequential operations
- DSL forms for declarative usage
- 100+ comprehensive tests
- 20+ example parts demonstrating all features

---

## Selector System Enhancements

### Current State Analysis

The existing selector system supports:
- **Direction selectors**: `:direction :+z :extreme :max`
- **Parallel selectors**: `:parallel :z`
- **Perpendicular selectors**: `:perpendicular :z`
- **Custom predicates**: `(lambda (shape) ...)`
- **Combinators**: `:and`, `:or`, `:not`

### Limitations for Phase 8

1. **No geometry-type filtering** - Can't select only circular edges or planar faces
2. **No size-based selection** - Can't select edges by length or faces by area
3. **No radius selection** - Can't select edges by curvature radius
4. **No angle-based selection** - Can't select edges by angle between them
5. **No named selections** - Can't save and reuse selections

### Enhancement 1: Geometry Type Selectors

#### Edge Type Selectors

```lisp
;; New selector types for edges
:type :line           ; Straight edges
:type :circle         ; Circular edges
:type :arc            ; Arc edges (partial circles)
:type :ellipse        ; Elliptical edges
:type :bspline        ; B-spline edges
:type :bezier         ; Bezier curve edges
```

#### Face Type Selectors

```lisp
;; New selector types for faces
:type :plane          ; Planar faces
:type :cylinder       ; Cylindrical faces
:type :sphere         ; Spherical faces
:type :cone           ; Conical faces
:type :torus          ; Toroidal faces
:type :bspline        ; B-spline surfaces
```

#### Implementation (FFI Layer)

```lisp
;; src/ffi/queries.lisp additions

(defcfun ("occt_edge_geom_type" %occt-edge-geom-type) :int
  "Get geometric type of an edge"
  (edge-handle :pointer)
  (out-type :pointer)   ; Returns: 0=line, 1=circle, 2=ellipse, etc.
  (err-msg :pointer))

(defcfun ("occt_face_geom_type" %occt-face-geom-type) :int
  "Get geometric type of a face"
  (face-handle :pointer)
  (out-type :pointer)   ; Returns: 0=plane, 1=cylinder, 2=sphere, etc.
  (err-msg :pointer))
```

#### Implementation (Selector Layer)

```lisp
;; src/selectors/geometric.lisp additions

(defclass type-selector ()
  ((shape-type :initarg :shape-type
               :accessor selector-shape-type
               :type keyword
               :documentation "Shape type: :line, :circle, :plane, etc."))
  (:documentation "Selector for geometric type"))

(defmethod apply-selector ((sel type-selector) shape-list)
  "Select shapes by geometric type"
  (remove-if-not
   (lambda (shape)
     (eq (geom-type shape) (selector-shape-type sel)))
   shape-list))
```

#### Usage Examples

```lisp
;; Select all circular edges for filleting
(:on-edge :type :circle
  (fillet 2))

;; Select all planar faces for extrusion
(:on-face :type :plane
  (:add (extrude 10)))

;; Combine with other selectors
(:on-edge :type :line :parallel :z
  (chamfer 1))
```

### Enhancement 2: Size-Based Selectors

```lisp
;; New selector types
:length :> 10          ; Edges longer than 10mm
:length :< 50          ; Edges shorter than 50mm
:length :between 10 50 ; Edges between 10-50mm
:area :> 1000          ; Faces larger than 1000mm²
:radius := 5           ; Edges/faces with radius ≈5mm
```

#### Implementation

```lisp
;; src/selectors/size.lisp (new file)

(defclass size-selector ()
  ((property :initarg :property
             :accessor selector-property
             :type keyword)  ; :length, :area, :radius, :volume
   (comparator :initarg :comparator
               :accessor selector-comparator
               :type keyword)  ; :>, :<, :=, :between
   (value1 :initarg :value1
           :accessor selector-value1)
   (value2 :initarg :value2
           :initform nil
           :accessor selector-value2)
   (tolerance :initarg :tolerance
              :initform 1e-6
              :accessor selector-tolerance))
  (:documentation "Selector based on size/dimension"))

(defmethod apply-selector ((sel size-selector) shape-list)
  "Select shapes by size criteria"
  (let ((prop (selector-property sel))
        (comp (selector-comparator sel))
        (val1 (selector-value1 sel))
        (val2 (selector-value2 sel))
        (tol (selector-tolerance sel)))
    (remove-if-not
     (lambda (shape)
       (let ((actual (get-property shape prop)))
         (case comp
           (:> (> actual val1))
           (:< (< actual val1))
           (:>= (>= actual val1))
           (:<= (<= actual val1))
           (:= (< (abs (- actual val1)) tol))
           (:between (and (>= actual val1) (<= actual val2))))))
     shape-list)))

(defun get-property (shape property)
  "Get a property value from a shape"
  (case property
    (:length (if (edge-p shape) (edge-length shape) 0))
    (:area (if (face-p shape) (face-area shape) 0))
    (:radius (shape-radius shape))
    (:volume (if (solid-p shape) (solid-volume shape) 0))
    (t (error "Unknown property: ~A" property))))
```

#### Usage Examples

```lisp
;; Fillet only short edges (< 10mm)
(:on-edge :length :< 10
  (fillet 1))

;; Chamfer long edges (> 50mm)
(:on-edge :length :> 50
  (chamfer 2))

;; Select small holes (area < 100mm²)
(:on-face :type :cylinder :area :< 100
  (:cut (make-cylinder 8 20)))
```

### Enhancement 3: Radius-Based Selectors

```lisp
;; Specific for curved edges and faces
:radius := 5           ; Radius approximately 5mm
:radius :> 10          ; Radius greater than 10mm
:radius :< 3           ; Radius less than 3mm
```

#### FFI Support

```lisp
;; src/ffi/queries.lisp additions

(defcfun ("occt_edge_radius" %occt-edge-radius) :int
  "Get radius of circular/arc edge"
  (edge-handle :pointer)
  (out-radius :pointer)
  (err-msg :pointer))

(defcfun ("occt_face_radius" %occt-face-radius) :int
  "Get radius of cylindrical/spherical face"
  (face-handle :pointer)
  (out-radius :pointer)
  (err-msg :pointer))
```

### Enhancement 4: Angle-Based Selectors

```lisp
;; Select edges by angle with other edges
:angle-with-edge edge-obj :approximately 90  ; Perpendicular to edge
:angle-with-face face-obj :approximately 45  ; At 45° to face
```

#### Implementation

```lisp
;; src/selectors/angular.lisp (new file)

(defclass angle-selector ()
  ((reference-shape :initarg :reference
                    :accessor selector-reference)
   (target-angle :initarg :angle
                 :accessor selector-target-angle)
   (tolerance :initarg :tolerance
              :initform 1.0  ; degrees
              :accessor selector-tolerance))
  (:documentation "Select shapes by angle relative to reference"))

(defmethod apply-selector ((sel angle-selector) shape-list)
  (let ((ref (selector-reference sel))
        (target (selector-target-angle sel))
        (tol (selector-tolerance sel)))
    (remove-if-not
     (lambda (shape)
       (let ((actual-angle (compute-angle shape ref)))
         (< (abs (- actual-angle target)) tol)))
     shape-list)))
```

### Enhancement 5: Named Selection Sets

```lisp
;; Save selections for reuse
(save-selection 'top-face
  (select-faces :direction :+z :extreme :max))

;; Use named selection later
(:on-selection 'top-face
  (:cut (make-cylinder 10 20)))
```

#### Implementation

```lisp
;; src/selectors/named.lisp (new file)

(defvar *named-selections* (make-hash-table)
  "Global registry of named selections")

(defun save-selection (name selection &optional (ctx *context*))
  "Save a selection with a name for later reuse"
  (setf (gethash name *named-selections*) selection)
  selection)

(defun get-selection (name)
  "Retrieve a named selection"
  (or (gethash name *named-selections*)
      (error "No selection named ~A" name)))

(defun clear-selections ()
  "Clear all named selections"
  (clrhash *named-selections*))
```

### Enhanced Selector API

```lisp
;; src/selectors/api.lisp updates

(defun select (shape-list selector-spec &rest args)
  "Enhanced select function with new selector types"
  (cond
    ;; Existing selectors...
    ((functionp selector-spec) ...)
    ((eq selector-spec :direction) ...)
    ((eq selector-spec :parallel) ...)
    ((eq selector-spec :perpendicular) ...)

    ;; NEW: Type selector
    ((eq selector-spec :type)
     (let ((type-keyword (first args)))
       (let ((selector (make-instance 'type-selector
                                      :shape-type type-keyword)))
         (apply-selector selector shape-list))))

    ;; NEW: Size selector
    ((member selector-spec '(:length :area :radius :volume))
     (destructuring-bind (comparator &rest values) args
       (let ((selector (make-instance 'size-selector
                                      :property selector-spec
                                      :comparator comparator
                                      :value1 (first values)
                                      :value2 (second values))))
         (apply-selector selector shape-list))))

    ;; NEW: Named selection
    ((eq selector-spec :named)
     (get-selection (first args)))

    (t (error "Unknown selector type: ~A" selector-spec))))
```

---

## Feature 1: Fillets and Chamfers

### Overview

Fillets and chamfers are essential manufacturing features that:
- Eliminate sharp edges (stress concentrators)
- Improve aesthetics
- Enable proper molding/casting
- Facilitate assembly

### 1.1 Fillet Implementation

#### OCCT APIs

- `BRepFilletAPI_MakeFillet` - Create fillets on edges
- `BRepFilletAPI_MakeFillet2d` - Create 2D fillets
- Support for constant, variable, and evolved radius

#### FFI Layer

```lisp
;; src/ffi/fillets.lisp (new file)

(defcfun ("occt_make_fillet" %occt-make-fillet) :int
  "Create fillet on edges of a shape"
  (shape-handle :pointer)
  (edge-handles :pointer)    ; Array of edge handles
  (num-edges :int)
  (radius :double)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_variable_fillet" %occt-make-variable-fillet) :int
  "Create variable-radius fillet"
  (shape-handle :pointer)
  (edge-handle :pointer)
  (param-array :pointer)     ; Array of (parameter, radius) pairs
  (num-params :int)
  (out-shape :pointer)
  (err-msg :pointer))

(defun ffi-fillet (shape edges radius)
  "Create fillets on specified edges.

  Args:
    shape - Base shape (occt-handle)
    edges - List of edge occt-handles
    radius - Fillet radius in mm

  Returns: New shape with fillets applied

  Signals: occt-error on failure"
  (unless (and (plusp radius) (not (null edges)))
    (error 'occt-domain-error
           :message (format nil "Invalid fillet parameters: r=~A, edges=~A"
                            radius (length edges))))

  (if *occt-available-p*
      ;; Real OCCT implementation
      (with-foreign-object (edge-array :pointer (length edges))
        ;; Fill array with edge handles
        (loop for edge in edges
              for i from 0
              do (setf (mem-aref edge-array :pointer i)
                      (occt-handle-pointer edge)))

        (with-foreign-objects ((shape-ptr :pointer)
                               (err-ptr :pointer))
          (let ((result-code (%occt-make-fillet
                              (occt-handle-pointer shape)
                              edge-array
                              (length edges)
                              (coerce radius 'double-float)
                              shape-ptr
                              err-ptr)))
            (check-occt-result result-code "fillet")
            (make-occt-handle (mem-ref shape-ptr :pointer)
                             :type :solid
                             :inc-ref nil))))

      ;; Stub implementation
      (stub-fillet shape edges radius)))
```

#### Core API

```lisp
;; src/core/fillets.lisp (new file)

(defun fillet (shape edges radius)
  "Apply fillet to edges of a shape.

  Args:
    shape - Shape to fillet (from clad.core)
    edges - List of edges to fillet
    radius - Fillet radius (can be number or list of (edge radius) pairs)

  Returns: New filleted shape

  Examples:
    ;; Constant radius fillet
    (fillet box (edges box) 5)

    ;; Variable radius - different for each edge
    (fillet box
            (list (cons edge1 5) (cons edge2 10)))"

  (etypecase radius
    (number
     ;; Constant radius for all edges
     (clad.ffi:ffi-fillet shape edges radius))

    (list
     ;; Variable radius - list of (edge . radius) pairs
     (loop with result = shape
           for (edge . rad) in radius
           do (setf result (clad.ffi:ffi-fillet result (list edge) rad))
           finally (return result)))))

(defun fillet-chain (shape edge-chain radius)
  "Fillet a chain of connected edges.

  Args:
    shape - Shape to fillet
    edge-chain - List of connected edges
    radius - Fillet radius

  Returns: New filleted shape

  This is optimized for chains of edges (e.g., all edges of a face)"
  (clad.ffi:ffi-fillet shape edge-chain radius))
```

#### Context API

```lisp
;; src/context/context.lisp additions

(defun fillet-selected (radius &optional (ctx *context*))
  "Apply fillet to currently selected edges.

  Args:
    radius - Fillet radius in mm
    ctx - Context (defaults to *context*)

  Returns: The context

  Example:
    (with-context ()
      (add (make-box 100 100 100))
      (select-edges :parallel :z)
      (fillet-selected 5))"

  (let ((current (current-shape ctx))
        (selection (current-selection ctx)))

    (unless current
      (error "Cannot fillet without a current shape"))

    (unless selection
      (error "Cannot fillet without selected edges"))

    ;; Unwrap shapes, apply fillet, rewrap
    (let* ((unwrapped-current (clad.shapes:unwrap-shape current))
           (unwrapped-edges (mapcar #'clad.shapes:unwrap-shape selection))
           (filleted (fillet unwrapped-current unwrapped-edges radius)))
      (setf (current-shape ctx)
            (clad.shapes:wrap-shape filleted 'clad.shapes:cad-solid))))

  ;; Pop selection
  (pop-selection ctx)
  ctx)
```

#### DSL Integration

```lisp
;; src/dsl/defpart.lisp additions

;; Support :fillet in feature forms
(defun expand-feature-form-at-compile-time (feature-form)
  (case (first feature-form)
    ...
    (:fillet
     (let ((radius (second feature-form)))
       `(fillet-selected ,radius)))
    ...))
```

#### Usage Examples

```lisp
;; Example 1: Fillet all vertical edges
(defpart filleted-box ((size 100) (fillet-radius 5))
  (:body (make-box size size size))

  (:on-edge :parallel :z
    (:fillet fillet-radius)))

;; Example 2: Fillet only top edges
(defpart filleted-top ((size 100))
  (:body (make-box size size size))

  (:on-edge :direction :+z :extreme :max :type :line
    (:fillet 3)))

;; Example 3: Variable radius fillet
(defpart variable-fillet-box ((size 100))
  (:body (make-box size size size))

  (:on-edge :length :< 50
    (:fillet 2))

  (:on-edge :length :> 50
    (:fillet 5)))

;; Example 4: Fillet by edge type
(defpart fillet-curves ((size 100))
  (:body (complex-shape size))

  ;; Fillet only circular edges
  (:on-edge :type :circle
    (:fillet 3))

  ;; Different fillet for straight edges
  (:on-edge :type :line
    (:fillet 1)))
```

### 1.2 Chamfer Implementation

#### OCCT APIs

- `BRepFilletAPI_MakeChamfer` - Create chamfers
- Support for distance-distance and distance-angle chamfers

#### FFI Layer

```lisp
;; src/ffi/fillets.lisp additions

(defcfun ("occt_make_chamfer" %occt-make-chamfer) :int
  "Create chamfer on edges"
  (shape-handle :pointer)
  (edge-handles :pointer)
  (num-edges :int)
  (distance1 :double)       ; Distance on first face
  (distance2 :double)       ; Distance on second face (0 for symmetric)
  (out-shape :pointer)
  (err-msg :pointer))

(defun ffi-chamfer (shape edges distance1 &optional distance2)
  "Create chamfers on specified edges.

  Args:
    shape - Base shape
    edges - List of edges
    distance1 - Chamfer distance on first adjacent face
    distance2 - Chamfer distance on second face (nil for symmetric)

  Returns: New chamfered shape"

  (let ((d2 (or distance2 distance1)))  ; Default to symmetric
    (unless (and (plusp distance1) (plusp d2))
      (error 'occt-domain-error
             :message (format nil "Invalid chamfer distances: ~A, ~A"
                              distance1 d2)))

    (if *occt-available-p*
        (with-foreign-object (edge-array :pointer (length edges))
          (loop for edge in edges
                for i from 0
                do (setf (mem-aref edge-array :pointer i)
                        (occt-handle-pointer edge)))

          (with-foreign-objects ((shape-ptr :pointer)
                                 (err-ptr :pointer))
            (let ((result-code (%occt-make-chamfer
                                (occt-handle-pointer shape)
                                edge-array
                                (length edges)
                                (coerce distance1 'double-float)
                                (coerce d2 'double-float)
                                shape-ptr
                                err-ptr)))
              (check-occt-result result-code "chamfer")
              (make-occt-handle (mem-ref shape-ptr :pointer)
                               :type :solid
                               :inc-ref nil))))

        (stub-chamfer shape edges distance1 d2))))
```

#### Core API

```lisp
;; src/core/fillets.lisp additions

(defun chamfer (shape edges distance &optional distance2)
  "Apply chamfer to edges.

  Args:
    shape - Shape to chamfer
    edges - List of edges
    distance - Chamfer distance (or list of (edge distance) pairs)
    distance2 - Second distance for asymmetric chamfer

  Returns: New chamfered shape

  Examples:
    ;; Symmetric chamfer
    (chamfer box edges 2)

    ;; Asymmetric chamfer
    (chamfer box edges 2 3)

    ;; Variable chamfer
    (chamfer box (list (cons edge1 2) (cons edge2 4)))"

  (etypecase distance
    (number
     (clad.ffi:ffi-chamfer shape edges distance distance2))
    (list
     (loop with result = shape
           for (edge . dist) in distance
           do (setf result (clad.ffi:ffi-chamfer result (list edge) dist distance2))
           finally (return result)))))
```

#### DSL Integration

```lisp
;; Example usage in DSL
(defpart chamfered-bracket ((size 100))
  (:body (make-box size size 10))

  ;; Chamfer all edges
  (:on-edge :type :line
    (:chamfer 1))

  ;; Asymmetric chamfer on specific edges
  (:on-edge :direction :+x :extreme :max
    (:chamfer 2 3)))
```

### 1.3 Test Specifications

```lisp
;; tests/fillet-tests.lisp

(def-suite fillet-tests
  :description "Tests for fillet operations")

(in-suite fillet-tests)

(test fillet-simple-edge
  "Fillet a single edge of a box"
  (let* ((box (make-box 100 100 100))
         (edges (edges box))
         (top-edges (select edges :direction :+z :extreme :max :type :line))
         (filleted (fillet box top-edges 5)))
    (is (not (null filleted)))
    (is (shape-valid-p filleted))
    ;; Volume should be less than original (material removed)
    (is (< (volume filleted) (volume box)))))

(test fillet-all-edges
  "Fillet all edges of a box"
  (let* ((box (make-box 100 100 100))
         (all-edges (edges box))
         (filleted (fillet box all-edges 5)))
    (is (not (null filleted)))
    (is (shape-valid-p filleted))
    ;; All edges should now be curved
    (is (every (lambda (e) (member (geom-type e) '(:circle :bspline)))
               (edges filleted)))))

(test fillet-variable-radius
  "Apply variable radius fillet"
  (let* ((box (make-box 100 100 100))
         (edges (edges box))
         (edge-radii (mapcar (lambda (e) (cons e (+ 2 (random 5))))
                            edges))
         (filleted (fillet box edge-radii)))
    (is (not (null filleted)))
    (is (shape-valid-p filleted))))

(test chamfer-simple
  "Chamfer edges of a box"
  (let* ((box (make-box 100 100 100))
         (edges (select (edges box) :direction :+z :extreme :max))
         (chamfered (chamfer box edges 2)))
    (is (not (null chamfered)))
    (is (shape-valid-p chamfered))
    (is (< (volume chamfered) (volume box)))))

(test chamfer-asymmetric
  "Asymmetric chamfer"
  (let* ((box (make-box 100 100 100))
         (edges (select (edges box) :direction :+x :extreme :max))
         (chamfered (chamfer box edges 2 4)))
    (is (not (null chamfered)))
    (is (shape-valid-p chamfered))))

(test fillet-in-context
  "Fillet using context API"
  (let ((result
         (with-context ()
           (add (make-box 100 100 100))
           (select-edges :parallel :z)
           (fillet-selected 5)
           (get-result))))
    (is (not (null result)))
    (is (shape-valid-p result))))
```

---

## Feature 2: Splines and Complex Curves

### Overview

Splines enable creation of smooth, organic curves for:
- Aerodynamic profiles
- Ergonomic handles
- Artistic designs
- Cam profiles

### 2.1 B-Spline Curves

#### OCCT APIs

- `Geom_BSplineCurve` - B-spline curve definition
- `BRepBuilderAPI_MakeEdge` - Create edge from curve
- `GeomAPI_PointsToBSpline` - Fit spline through points

#### FFI Layer

```lisp
;; src/ffi/curves.lisp (new file)

(defcfun ("occt_make_bspline_from_points" %occt-make-bspline-from-points) :int
  "Create B-spline curve through points"
  (points-array :pointer)     ; Array of (x,y,z) points
  (num-points :int)
  (degree :int)               ; Spline degree (1=linear, 3=cubic, etc.)
  (closed :bool)              ; True for closed curve
  (out-edge :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_bezier_curve" %occt-make-bezier-curve) :int
  "Create Bezier curve from control points"
  (control-points :pointer)
  (num-points :int)
  (out-edge :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_interpolated_curve" %occt-make-interpolated-curve) :int
  "Create curve interpolating through points"
  (points-array :pointer)
  (num-points :int)
  (tangent-start :pointer)    ; Optional start tangent (x,y,z) or NULL
  (tangent-end :pointer)      ; Optional end tangent
  (out-edge :pointer)
  (err-msg :pointer))
```

#### Core API

```lisp
;; src/core/curves.lisp (new file)

(defun make-spline (points &key (degree 3) (closed nil) (method :interpolate))
  "Create a spline curve through points.

  Args:
    points - List of points: ((x1 y1 z1) (x2 y2 z2) ...)
    degree - Spline degree (1=linear, 2=quadratic, 3=cubic, etc.)
    closed - T for closed curve, NIL for open
    method - :interpolate (pass through points) or :approximate (fit near points)

  Returns: Edge representing the spline

  Examples:
    ;; Simple curved path
    (make-spline '((0 0 0) (10 5 0) (20 8 0) (30 5 0))
                 :degree 3)

    ;; Closed curve for profile
    (make-spline '((0 0 0) (10 0 0) (10 10 0) (0 10 0))
                 :degree 3
                 :closed t)"

  (unless (>= (length points) (1+ degree))
    (error "Need at least ~A points for degree ~A spline" (1+ degree) degree))

  ;; Convert points to foreign array
  (with-foreign-object (pts-array :double (* (length points) 3))
    (loop for (x y z) in points
          for i from 0 by 3
          do (setf (mem-aref pts-array :double i) (coerce x 'double-float))
             (setf (mem-aref pts-array :double (+ i 1)) (coerce y 'double-float))
             (setf (mem-aref pts-array :double (+ i 2)) (coerce z 'double-float)))

    (case method
      (:interpolate
       (clad.ffi:ffi-make-interpolated-curve pts-array (length points)
                                             nil nil closed))
      (:approximate
       (clad.ffi:ffi-make-bspline-from-points pts-array (length points)
                                              degree closed)))))

(defun make-bezier (control-points)
  "Create a Bezier curve from control points.

  Args:
    control-points - List of control points

  Returns: Edge representing the Bezier curve

  Example:
    (make-bezier '((0 0 0) (10 10 0) (20 10 0) (30 0 0)))"

  (clad.ffi:ffi-make-bezier-curve control-points))

(defun make-arc-3points (p1 p2 p3)
  "Create circular arc through three points.

  Args:
    p1, p2, p3 - Three points defining the arc

  Returns: Edge representing the arc

  Example:
    (make-arc-3points '(0 0 0) '(10 10 0) '(20 0 0))"

  (clad.ffi:ffi-make-arc-3points p1 p2 p3))

(defun make-arc-center-radius (center radius start-angle end-angle
                                &key (axis '(0 0 1)))
  "Create circular arc by center, radius, and angles.

  Args:
    center - Center point (x y z)
    radius - Arc radius
    start-angle - Start angle in degrees
    end-angle - End angle in degrees
    axis - Axis of rotation (default Z axis)

  Returns: Edge representing the arc

  Example:
    (make-arc-center-radius '(50 50 0) 20 0 90)"

  (clad.ffi:ffi-make-arc-center-radius center radius
                                       start-angle end-angle axis))
```

### 2.2 Wire Operations

```lisp
;; src/core/wires.lisp (new file)

(defun make-wire (edges)
  "Create a wire from a list of connected edges.

  Args:
    edges - List of edges (must be connected)

  Returns: Wire shape

  Example:
    (let* ((e1 (make-line '(0 0 0) '(10 0 0)))
           (e2 (make-arc-3points '(10 0 0) '(15 5 0) '(20 0 0)))
           (e3 (make-line '(20 0 0) '(20 10 0))))
      (make-wire (list e1 e2 e3)))"

  (clad.ffi:ffi-make-wire edges))

(defun close-wire (wire)
  "Close an open wire by adding a segment from end to start.

  Args:
    wire - Open wire

  Returns: Closed wire"

  (clad.ffi:ffi-close-wire wire))

(defun wire-closed-p (wire)
  "Check if wire is closed.

  Args:
    wire - Wire to check

  Returns: T if closed, NIL if open"

  (clad.ffi:ffi-wire-closed-p wire))
```

### 2.3 Usage Examples

```lisp
;; Example 1: Splined handle
(defpart ergonomic-handle ()
  (:body
    (let* ((profile-points '((0 0 0) (2 1 0) (4 0.5 0) (6 0 0)))
           (profile-curve (make-spline profile-points :degree 3))
           (profile-wire (make-wire (list profile-curve))))
      (sweep-wire-along-path profile-wire
                             (make-line '(0 0 0) '(0 0 100))))))

;; Example 2: Airfoil profile
(defpart airfoil ((chord 100) (thickness 12))
  (:body
    (let* ((points (generate-naca-profile chord thickness))
           (profile (make-spline points :degree 3 :closed t))
           (face (make-face-from-wire profile)))
      (extrude face 50))))

;; Example 3: Cam profile
(defpart cam-profile ((radius 30) (lift 5))
  (:body
    (let* ((angles (loop for a from 0 to 360 by 10 collect a))
           (points (loop for a in angles
                        collect (list (* (+ radius (calc-lift a lift))
                                       (cos (* a pi 1/180)))
                                     (* (+ radius (calc-lift a lift))
                                        (sin (* a pi 1/180)))
                                     0)))
           (profile (make-spline points :degree 3 :closed t)))
      (revolve profile '(0 0 0) '(0 0 1) 360))))
```

---

## Feature 3: Sweeps

### Overview

Sweep operations create 3D shapes by moving a 2D profile along a path. Essential for:
- Pipes and tubes
- Handrails
- Cable routing
- Complex extrusions

### 3.1 Basic Sweep

#### OCCT APIs

- `BRepOffsetAPI_MakePipe` - Basic pipe/sweep
- `BRepOffsetAPI_MakePipeShell` - Advanced sweep with multiple profiles

#### FFI Layer

```lisp
;; src/ffi/sweep.lisp (new file)

(defcfun ("occt_make_pipe" %occt-make-pipe) :int
  "Sweep profile along path"
  (profile-wire :pointer)     ; Profile to sweep (wire or face)
  (path-wire :pointer)        ; Path to follow (wire)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_pipe_with_twist" %occt-make-pipe-with-twist) :int
  "Sweep with twist"
  (profile-wire :pointer)
  (path-wire :pointer)
  (twist-angle :double)       ; Total twist in degrees
  (out-shape :pointer)
  (err-msg :pointer))

(defun ffi-sweep (profile path &key (twist 0))
  "Sweep a profile along a path.

  Args:
    profile - Profile shape (wire or face) to sweep
    path - Path to follow (wire)
    twist - Optional twist angle in degrees

  Returns: Swept solid or shell"

  (if (zerop twist)
      (clad.ffi:ffi-make-pipe profile path)
      (clad.ffi:ffi-make-pipe-with-twist profile path twist)))
```

#### Core API

```lisp
;; src/core/sweep.lisp (new file)

(defun sweep (profile path &key (twist 0) (scale-factor 1.0))
  "Sweep a 2D profile along a 3D path.

  Args:
    profile - 2D profile (wire or face) to sweep
    path - Path curve (wire) to follow
    twist - Twist angle in degrees (applied over full path)
    scale-factor - Scale factor at end (1.0 = no scaling)

  Returns: Swept solid

  Examples:
    ;; Simple pipe
    (sweep (make-circle 10)
           (make-spline '((0 0 0) (100 0 0) (100 100 0))))

    ;; Twisted extrusion
    (sweep (make-rectangle 20 20)
           (make-line '(0 0 0) '(0 0 100))
           :twist 90)

    ;; Tapered sweep
    (sweep (make-circle 20)
           (make-line '(0 0 0) '(0 0 100))
           :scale-factor 0.5)"

  (let ((result (clad.ffi:ffi-sweep profile path :twist twist)))
    (if (= scale-factor 1.0)
        result
        ;; Apply scaling transformation
        (apply-scale-along-path result profile path scale-factor))))

(defun sweep-multi-section (sections path)
  "Sweep with multiple cross-sections along path.

  Args:
    sections - List of (position profile) pairs
               position is parameter 0-1 along path
    path - Guide path

  Returns: Swept solid

  Example:
    (sweep-multi-section
      (list (list 0.0 (make-circle 10))
            (list 0.5 (make-rectangle 15 15))
            (list 1.0 (make-circle 5)))
      (make-spline ...))"

  (clad.ffi:ffi-sweep-multi-section sections path))
```

#### DSL Integration

```lisp
;; DSL form for sweep operations
(:sweep profile-expr path-expr &key twist scale)

;; Example
(defpart twisted-bar ((length 100))
  (:body
    (let ((profile (make-rectangle 10 10))
          (path (make-line '(0 0 0) (list 0 0 length))))
      (sweep profile path :twist 180))))
```

### 3.2 Usage Examples

```lisp
;; Example 1: Pipe along curved path
(defpart curved-pipe ((radius 25) (thickness 3))
  (:body
    (let* ((outer-profile (make-circle radius))
           (inner-profile (make-circle (- radius thickness)))
           (path (make-spline '((0 0 0) (100 50 0)
                               (200 50 50) (300 0 100))))
           (outer (sweep outer-profile path))
           (inner (sweep inner-profile path)))
      (cut outer inner))))

;; Example 2: Twisted blade
(defpart turbine-blade ((length 200) (chord 50) (twist 45))
  (:body
    (let* ((profile (make-airfoil-profile chord))
           (path (make-line '(0 0 0) (list 0 0 length))))
      (sweep profile path :twist twist))))

;; Example 3: Variable cross-section
(defpart transition-duct ()
  (:body
    (sweep-multi-section
      (list (list 0.0 (make-circle 50))
            (list 0.5 (make-rectangle 60 60))
            (list 1.0 (make-rectangle 40 80)))
      (make-line '(0 0 0) '(0 0 200)))))

;; Example 4: Handrail with end caps
(defpart handrail ((length 2000) (radius 15))
  (:body
    (let* ((profile (make-circle radius))
           (path (make-spline (generate-handrail-path length)))
           (rail (sweep profile path)))
      rail))

  ;; Add spherical end caps
  (:on-end :start
    (:add (make-sphere radius)))

  (:on-end :end
    (:add (translate (make-sphere radius) 0 0 length))))
```

---

## Feature 4: Lofts

### Overview

Loft creates smooth transitions between multiple profiles. Used for:
- Bottle shapes
- Fuselage sections
- Complex transitions
- Organic forms

### 4.1 Loft Implementation

#### OCCT APIs

- `BRepOffsetAPI_ThruSections` - Loft through sections
- Support for solid/shell, ruled/smooth

#### FFI Layer

```lisp
;; src/ffi/loft.lisp (new file)

(defcfun ("occt_make_loft" %occt-make-loft) :int
  "Create loft through sections"
  (sections :pointer)         ; Array of wire/face handles
  (num-sections :int)
  (solid :bool)               ; True for solid, false for shell
  (ruled :bool)               ; True for ruled surface, false for smooth
  (out-shape :pointer)
  (err-msg :pointer))

(defun ffi-loft (sections &key (solid t) (ruled nil))
  "Create lofted shape through sections.

  Args:
    sections - List of profile wires/faces
    solid - T to create solid, NIL for shell
    ruled - T for ruled surface, NIL for smooth

  Returns: Lofted shape"

  (unless (>= (length sections) 2)
    (error 'occt-domain-error
           :message "Loft requires at least 2 sections"))

  (with-foreign-object (sect-array :pointer (length sections))
    (loop for section in sections
          for i from 0
          do (setf (mem-aref sect-array :pointer i)
                  (occt-handle-pointer section)))

    (with-foreign-objects ((shape-ptr :pointer)
                           (err-ptr :pointer))
      (let ((result-code (%occt-make-loft
                          sect-array
                          (length sections)
                          solid
                          ruled
                          shape-ptr
                          err-ptr)))
        (check-occt-result result-code "loft")
        (make-occt-handle (mem-ref shape-ptr :pointer)
                         :type (if solid :solid :shell)
                         :inc-ref nil)))))
```

#### Core API

```lisp
;; src/core/loft.lisp (new file)

(defun loft (sections &key (solid t) (ruled nil) (spine nil))
  "Create lofted solid/shell through sections.

  Args:
    sections - List of profile wires (at least 2)
    solid - T to create solid, NIL for shell surface
    ruled - T for ruled (straight) surface, NIL for smooth
    spine - Optional guide curve for loft path

  Returns: Lofted shape

  Examples:
    ;; Simple loft between two profiles
    (loft (list (make-circle 50)
                (translate (make-rectangle 30 30) 0 0 100)))

    ;; Smooth transition (perfume bottle)
    (loft (list (make-circle 30)
                (translate (make-circle 40) 0 0 50)
                (translate (make-circle 20) 0 0 100)
                (translate (make-circle 10) 0 0 120))
          :solid t
          :ruled nil)

    ;; Ruled surface (no smoothing)
    (loft (list (make-rectangle 50 50)
                (translate (make-circle 30) 0 0 100))
          :ruled t)"

  (if spine
      (clad.ffi:ffi-loft-with-spine sections spine :solid solid :ruled ruled)
      (clad.ffi:ffi-loft sections :solid solid :ruled ruled)))

(defun loft-with-guides (sections guide-curves &key (solid t))
  "Create loft with guide curves constraining the surface.

  Args:
    sections - List of profile wires
    guide-curves - List of curves connecting corresponding points on profiles
    solid - T for solid, NIL for shell

  Returns: Guided loft shape

  Example:
    (loft-with-guides
      (list (make-circle 50) (translate (make-circle 30) 0 0 100))
      (list (make-spline '((50 0 0) (50 0 50) (30 0 100)))
            (make-spline '((-50 0 0) (-50 0 50) (-30 0 100)))))"

  (clad.ffi:ffi-loft-with-guides sections guide-curves solid))
```

#### DSL Integration

```lisp
;; DSL support for loft
(:loft section1 section2 ... &key solid ruled)

;; Example
(defpart bottle-shape ((height 150))
  (:loft
    (make-circle 40)                           ; Base
    (translate (make-circle 50) 0 0 50)       ; Widest point
    (translate (make-circle 30) 0 0 100)      ; Neck start
    (translate (make-circle 15) 0 0 height)   ; Top
    :solid t
    :ruled nil))
```

### 4.2 Usage Examples

```lisp
;; Example 1: Perfume bottle
(defpart perfume-bottle ()
  (:body
    (loft (list
            ;; Base
            (make-circle 30)
            ;; Body
            (translate (make-ellipse 35 30) 0 0 50)
            (translate (make-ellipse 32 28) 0 0 80)
            ;; Neck
            (translate (make-circle 15) 0 0 100)
            (translate (make-circle 12) 0 0 120)
            ;; Top
            (translate (make-circle 8) 0 0 125))
          :solid t
          :ruled nil))

  ;; Add threads for cap
  (:on-face :direction :+z :extreme :max :type :cylinder
    (thread-profile 8 1.5 15)))

;; Example 2: Wing airfoil loft
(defpart wing ((span 1000) (root-chord 200) (tip-chord 100))
  (:body
    (let* ((root-profile (make-airfoil root-chord 0.12))
           (mid-profile (translate (rotate
                                    (make-airfoil 150 0.12)
                                    :z 2)
                                  0 (/ span 2) 0))
           (tip-profile (translate (rotate
                                    (make-airfoil tip-chord 0.10)
                                    :z 5)
                                  0 span 0)))
      (loft (list root-profile mid-profile tip-profile)
            :solid t
            :ruled nil))))

;; Example 3: Morphing shape
(defpart morph-box-to-cylinder ((height 100))
  (:body
    (loft (list
            (make-rectangle 60 60)
            (translate (make-rectangle 50 50) 0 0 (* height 0.25))
            (translate (make-octagon 40) 0 0 (* height 0.5))
            (translate (make-circle 30) 0 0 (* height 0.75))
            (translate (make-circle 20) 0 0 height))
          :solid t)))

;; Example 4: Complex duct with guide curves
(defpart variable-duct ()
  (:body
    (let* ((section1 (make-rectangle 100 50))
           (section2 (translate (rotate (make-ellipse 60 40) :z 45)
                               100 0 100))
           (guide1 (make-spline '((50 25 0) (80 30 50) (90 20 100))))
           (guide2 (make-spline '((50 -25 0) (80 -30 50) (90 -20 100)))))
      (loft-with-guides (list section1 section2)
                        (list guide1 guide2)))))
```

---

## Feature 5: Shelling

### Overview

Shelling creates hollow parts with uniform wall thickness. Critical for:
- Injection molded parts
- Cast components
- Weight reduction
- Material savings

### 5.1 Shell Implementation

#### OCCT APIs

- `BRepOffsetAPI_MakeThickSolid` - Shell operation
- Support for different thicknesses on different faces

#### FFI Layer

```lisp
;; src/ffi/shell.lisp (new file)

(defcfun ("occt_make_shell" %occt-make-shell) :int
  "Create hollow shell from solid"
  (solid-handle :pointer)
  (faces-to-remove :pointer)  ; Array of face handles to remove (can be NULL)
  (num-faces :int)
  (thickness :double)         ; Wall thickness (negative for inward)
  (tolerance :double)
  (out-shape :pointer)
  (err-msg :pointer))

(defun ffi-shell (solid faces-to-remove thickness &optional (tolerance 1.0e-6))
  "Create hollow shell from solid by removing faces.

  Args:
    solid - Solid to shell
    faces-to-remove - List of faces to remove (creates openings)
    thickness - Wall thickness (positive=outward, negative=inward)
    tolerance - Geometric tolerance

  Returns: Shelled solid"

  (unless (plusp (abs thickness))
    (error 'occt-domain-error
           :message (format nil "Shell thickness must be non-zero: ~A" thickness)))

  (if (null faces-to-remove)
      ;; No openings - just offset all faces
      (with-foreign-objects ((shape-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-make-shell
                            (occt-handle-pointer solid)
                            (null-pointer)
                            0
                            (coerce thickness 'double-float)
                            (coerce tolerance 'double-float)
                            shape-ptr
                            err-ptr)))
          (check-occt-result result-code "shell")
          (make-occt-handle (mem-ref shape-ptr :pointer)
                           :type :solid
                           :inc-ref nil)))

      ;; With openings
      (with-foreign-object (face-array :pointer (length faces-to-remove))
        (loop for face in faces-to-remove
              for i from 0
              do (setf (mem-aref face-array :pointer i)
                      (occt-handle-pointer face)))

        (with-foreign-objects ((shape-ptr :pointer)
                               (err-ptr :pointer))
          (let ((result-code (%occt-make-shell
                              (occt-handle-pointer solid)
                              face-array
                              (length faces-to-remove)
                              (coerce thickness 'double-float)
                              (coerce tolerance 'double-float)
                              shape-ptr
                              err-ptr)))
            (check-occt-result result-code "shell")
            (make-occt-handle (mem-ref shape-ptr :pointer)
                             :type :solid
                             :inc-ref nil))))))
```

#### Core API

```lisp
;; src/core/shell.lisp (new file)

(defun shell (solid thickness &key faces-to-remove (tolerance 1.0e-6))
  "Create hollow shell from solid.

  Args:
    solid - Solid shape to shell
    thickness - Wall thickness
                - Positive: offset outward
                - Negative: offset inward
    faces-to-remove - Optional list of faces to remove (creates openings)
    tolerance - Geometric tolerance

  Returns: Shelled solid

  Examples:
    ;; Shell entire box (no openings)
    (shell (make-box 100 100 100) -3)

    ;; Shell box with top face removed
    (let* ((box (make-box 100 100 100))
           (top-face (car (select (faces box)
                                  :direction :+z :extreme :max))))
      (shell box -2 :faces-to-remove (list top-face)))

    ;; Thick walls on outside
    (shell (make-cylinder 50 100) 5)"

  (clad.ffi:ffi-shell solid faces-to-remove thickness tolerance))

(defun shell-variable-thickness (solid face-thickness-pairs)
  "Create shell with variable thickness on different faces.

  Args:
    solid - Solid to shell
    face-thickness-pairs - List of (face thickness) pairs

  Returns: Shelled solid with variable thickness

  Example:
    (shell-variable-thickness
      box
      (list (cons top-face 5)
            (cons bottom-face 3)
            (cons nil 2)))  ; nil means all other faces"

  (clad.ffi:ffi-shell-variable solid face-thickness-pairs))
```

#### Context API

```lisp
;; src/context/context.lisp additions

(defun shell-op (thickness &key remove-selected (ctx *context*))
  "Shell the current shape, optionally removing selected faces.

  Args:
    thickness - Wall thickness
    remove-selected - If T, remove currently selected faces
    ctx - Context

  Returns: The context

  Example:
    (with-context ()
      (add (make-box 100 100 100))
      (select-faces :direction :+z :extreme :max)
      (shell-op -2 :remove-selected t))"

  (let* ((current (current-shape ctx))
         (faces-to-remove (when remove-selected
                           (current-selection ctx))))

    (unless current
      (error "Cannot shell without current shape"))

    (let* ((unwrapped (clad.shapes:unwrap-shape current))
           (unwrapped-faces (when faces-to-remove
                             (mapcar #'clad.shapes:unwrap-shape
                                    faces-to-remove)))
           (shelled (shell unwrapped thickness
                          :faces-to-remove unwrapped-faces)))

      (setf (current-shape ctx)
            (clad.shapes:wrap-shape shelled 'clad.shapes:cad-solid))))

  (when remove-selected
    (pop-selection ctx))

  ctx)
```

#### DSL Integration

```lisp
;; DSL form for shelling
(:shell thickness &key remove-selected-faces)

;; Example
(defpart hollow-box ((size 100) (wall-thickness 3))
  (:body (make-box size size size))

  ;; Remove top face
  (:on-face :direction :+z :extreme :max
    (:shell (- wall-thickness) :remove-selected-faces t)))
```

### 5.2 Usage Examples

```lisp
;; Example 1: Simple hollow box
(defpart hollow-enclosure ((width 100) (height 100) (depth 50)
                           (wall 2.5))
  (:body (make-box width height depth))

  (:on-face :direction :+z :extreme :max
    (:save-selection 'top-face))

  (:shell (- wall) :faces-to-remove (get-selection 'top-face)))

;; Example 2: Bottle with opening at top
(defpart bottle ((height 150))
  (:body
    (loft (list
            (make-circle 40)
            (translate (make-circle 50) 0 0 50)
            (translate (make-circle 30) 0 0 100)
            (translate (make-circle 15) 0 0 height))))

  ;; Shell with top opening
  (:on-face :direction :+z :extreme :max
    (:shell -2.5 :remove-selected-faces t)))

;; Example 3: Electronic enclosure with mounting bosses
(defpart electronics-enclosure ((size 120))
  (:body (make-box size size 40))

  ;; Add mounting bosses BEFORE shelling
  (:on-face :direction :-z :extreme :min
    (:circular-pattern :count 4 :radius (* size 0.4)
      (:add (make-cylinder 6 8))))

  ;; Shell with top open
  (:on-face :direction :+z :extreme :max
    (:shell -3 :remove-selected-faces t))

  ;; Add mounting holes through bosses
  (:on-face :direction :-z :extreme :min :type :cylinder
    (:cut (make-cylinder 1.75 12))))

;; Example 4: Variable thickness shell
(defpart variable-wall-box ((size 100))
  (:body (make-box size size size))

  (:shell-variable
    :default-thickness -2
    :face-overrides
    ((:on-face :direction :+z :extreme :max)
     :thickness -4)
    ((:on-face :direction :-z :extreme :min)
     :thickness -5)))
```

---

## Feature 6: Mirroring

### Overview

Mirroring creates symmetric copies across planes. Used for:
- Symmetric parts (brackets, housings)
- Reducing modeling time
- Ensuring perfect symmetry
- Pattern creation

### 6.1 Mirror Implementation

#### OCCT APIs

- `BRepBuilderAPI_Transform` with mirror transformation
- `gp_Trsf::SetMirror` - Mirror transformation

#### FFI Layer

```lisp
;; src/ffi/transformations.lisp additions

(defcfun ("occt_mirror_shape" %occt-mirror-shape) :int
  "Mirror shape across plane"
  (shape-handle :pointer)
  (plane-origin :pointer)     ; (x, y, z)
  (plane-normal :pointer)     ; (nx, ny, nz)
  (out-shape :pointer)
  (err-msg :pointer))

(defun ffi-mirror (shape plane-origin plane-normal)
  "Mirror shape across a plane.

  Args:
    shape - Shape to mirror
    plane-origin - Point on mirror plane (x y z)
    plane-normal - Normal vector of plane (nx ny nz)

  Returns: Mirrored shape"

  (with-foreign-objects ((origin-arr :double 3)
                         (normal-arr :double 3)
                         (shape-ptr :pointer)
                         (err-ptr :pointer))

    ;; Fill origin
    (setf (mem-aref origin-arr :double 0) (coerce (first plane-origin) 'double-float))
    (setf (mem-aref origin-arr :double 1) (coerce (second plane-origin) 'double-float))
    (setf (mem-aref origin-arr :double 2) (coerce (third plane-origin) 'double-float))

    ;; Fill normal
    (setf (mem-aref normal-arr :double 0) (coerce (first plane-normal) 'double-float))
    (setf (mem-aref normal-arr :double 1) (coerce (second plane-normal) 'double-float))
    (setf (mem-aref normal-arr :double 2) (coerce (third plane-normal) 'double-float))

    (let ((result-code (%occt-mirror-shape
                        (occt-handle-pointer shape)
                        origin-arr
                        normal-arr
                        shape-ptr
                        err-ptr)))
      (check-occt-result result-code "mirror")
      (make-occt-handle (mem-ref shape-ptr :pointer)
                       :type (occt-handle-type shape)
                       :inc-ref nil))))
```

#### Core API

```lisp
;; src/core/mirror.lisp (new file)

(defun mirror (shape plane &key (union-original nil))
  "Mirror shape across a plane.

  Args:
    shape - Shape to mirror
    plane - Plane specification:
            - :xy, :xz, :yz for standard planes
            - (origin normal) for custom plane
    union-original - If T, union mirrored copy with original

  Returns: Mirrored shape (or union if union-original is T)

  Examples:
    ;; Mirror across YZ plane
    (mirror bracket :yz)

    ;; Mirror and union
    (mirror bracket :xz :union-original t)

    ;; Mirror across custom plane
    (mirror part '((50 50 0) (1 1 0)))"

  (let* ((origin-normal (parse-plane-spec plane))
         (origin (first origin-normal))
         (normal (second origin-normal))
         (mirrored (clad.ffi:ffi-mirror shape origin normal)))

    (if union-original
        (union shape mirrored)
        mirrored)))

(defun parse-plane-spec (spec)
  "Parse plane specification into (origin normal) pair"
  (cond
    ((eq spec :xy) '((0 0 0) (0 0 1)))
    ((eq spec :xz) '((0 0 0) (0 1 0)))
    ((eq spec :yz) '((0 0 0) (1 0 0)))
    ((listp spec) spec)
    (t (error "Invalid plane spec: ~A" spec))))

(defun mirror-and-union (shape plane)
  "Convenience: Mirror and union with original.

  Args:
    shape - Shape to mirror
    plane - Plane specification

  Returns: Union of original and mirrored copy

  Example:
    (mirror-and-union bracket :xz)"

  (mirror shape plane :union-original t))

(defun mirror-pattern (shape plane count)
  "Create pattern by repeated mirroring.

  Args:
    shape - Shape to mirror
    plane - Mirror plane
    count - Number of repetitions

  Returns: Union of all mirrored copies

  Example:
    ;; Create snowflake pattern
    (mirror-pattern arm :xy 6)"

  (loop with result = shape
        repeat (1- count)
        do (setf result (mirror-and-union result plane))
        finally (return result)))
```

#### Context API

```lisp
;; src/context/context.lisp additions

(defun mirror-op (plane &key union-original (ctx *context*))
  "Mirror current shape across plane.

  Args:
    plane - Mirror plane specification
    union-original - If T, union with original
    ctx - Context

  Returns: The context

  Example:
    (with-context ()
      (add (make-half-bracket 100))
      (mirror-op :yz :union-original t))"

  (let ((current (current-shape ctx)))
    (unless current
      (error "Cannot mirror without current shape"))

    (let* ((unwrapped (clad.shapes:unwrap-shape current))
           (mirrored (mirror unwrapped plane :union-original union-original)))
      (setf (current-shape ctx)
            (clad.shapes:wrap-shape mirrored 'clad.shapes:cad-solid))))

  ctx)
```

#### DSL Integration

```lisp
;; DSL form for mirroring
(:mirror plane &key union-original)

;; Example
(defpart symmetric-bracket ((size 100))
  ;; Model only half
  (:body (make-half-bracket size))

  ;; Mirror across YZ plane and union
  (:mirror :yz :union-original t))
```

### 6.2 Usage Examples

```lisp
;; Example 1: Symmetric bracket
(defpart symmetric-bracket ((width 100) (height 50))
  ;; Model just one side
  (:body
    (make-box (/ width 2) height 10))

  (:on-face :direction :+z :extreme :max
    (:add (translate (make-cylinder 8 15) (/ width 4) (/ height 2) 10)))

  ;; Mirror to complete
  (:mirror :yz :union-original t))

;; Example 2: Snowflake pattern
(defpart snowflake-ornament ()
  ;; Model one arm
  (:body
    (let ((arm (make-tapered-arm 30 5 2)))
      arm))

  ;; Create 6-fold symmetry
  (:mirror-pattern :xy 6))

;; Example 3: Double-ended part
(defpart connector ((length 100))
  ;; Model one end
  (:body
    (union
      (make-cylinder 20 (/ length 2))
      (translate (make-sphere 25) 0 0 0)))

  ;; Mirror to create other end
  (:mirror (list (list 0 0 (/ length 2)) (list 0 0 1))
           :union-original t))

;; Example 4: Symmetric housing with ribs
(defpart ribbed-housing ((size 120))
  ;; Base box
  (:body (make-box (/ size 2) size 40))

  ;; Add ribs on one side
  (:on-face :direction :-z :extreme :min
    (:linear-pattern :count 5 :spacing 20
      (:add (make-box 2 size 5))))

  ;; Shell
  (:on-face :direction :+z :extreme :max
    (:shell -2.5 :remove-selected-faces t))

  ;; Mirror to complete
  (:mirror :yz :union-original t))
```

---

## Feature 7: Draft Angles

### Overview

Draft angles are tapered surfaces required for:
- Injection molding (part release)
- Die casting
- Sand casting
- Forging

### 7.1 Draft Implementation

#### OCCT APIs

- `BRepOffsetAPI_DraftAngle` - Apply draft to faces
- Support for neutral plane or neutral line

#### FFI Layer

```lisp
;; src/ffi/draft.lisp (new file)

(defcfun ("occt_make_draft" %occt-make-draft) :int
  "Apply draft angle to faces"
  (shape-handle :pointer)
  (faces :pointer)            ; Array of face handles
  (num-faces :int)
  (direction :pointer)        ; Pull direction (x, y, z)
  (angle :double)             ; Draft angle in degrees
  (neutral-plane :pointer)    ; Neutral plane (origin, normal)
  (out-shape :pointer)
  (err-msg :pointer))

(defun ffi-draft (shape faces direction angle neutral-plane)
  "Apply draft angle to faces.

  Args:
    shape - Shape to draft
    faces - List of faces to draft
    direction - Pull direction (x y z)
    angle - Draft angle in degrees
    neutral-plane - (origin normal) of neutral plane

  Returns: Drafted shape"

  (unless (and (plusp angle) (< angle 45))
    (error 'occt-domain-error
           :message (format nil "Draft angle must be 0 < angle < 45: ~A" angle)))

  ;; Implementation similar to other FFI functions...
  )
```

#### Core API

```lisp
;; src/core/draft.lisp (new file)

(defun draft (shape faces direction angle &key (neutral-plane nil))
  "Apply draft angle to faces for manufacturing.

  Args:
    shape - Shape to draft
    faces - List of faces to apply draft to
    direction - Pull direction (typically mold opening direction)
    angle - Draft angle in degrees (typical range: 0.5° - 5°)
    neutral-plane - Optional neutral plane (origin normal)
                    Defaults to plane perpendicular to direction

  Returns: Drafted shape

  Examples:
    ;; Simple draft on vertical faces
    (draft housing vertical-faces '(0 0 1) 3)

    ;; Draft with specific neutral plane
    (draft part side-faces '(1 0 0) 2
           :neutral-plane '((50 0 0) (1 0 0)))"

  (let ((np (or neutral-plane
                (calculate-default-neutral-plane shape direction))))
    (clad.ffi:ffi-draft shape faces direction angle np)))
```

### 7.2 Usage Examples

```lisp
;; Example 1: Molded housing
(defpart molded-housing ((size 100))
  (:body (make-box size size 50))

  ;; Apply draft to vertical faces
  (:on-face :perpendicular :z
    (:draft '(0 0 1) 3))  ; 3° draft angle

  ;; Shell
  (:on-face :direction :+z :extreme :max
    (:shell -2.5 :remove-selected-faces t)))

;; Example 2: Cast part with parting line
(defpart cast-bracket ()
  (:body (make-complex-bracket 150))

  ;; Draft upper faces
  (:on-face :direction :+z :extreme :> 50
    (:draft '(0 0 1) 5))

  ;; Draft lower faces (opposite direction)
  (:on-face :direction :-z :extreme :< 50
    (:draft '(0 0 -1) 5)))
```

---

## DSL Integration

### Enhanced defpart Forms

```lisp
;; New DSL forms for Phase 8 features

;; Fillet and Chamfer
(:on-edge selector
  (:fillet radius))

(:on-edge selector
  (:chamfer distance [distance2]))

;; Sweep
(:sweep profile path &key twist scale)

;; Loft
(:loft section1 section2 ... &key solid ruled spine)

;; Shell
(:shell thickness &key remove-selected-faces)

;; Mirror
(:mirror plane &key union-original)

;; Draft
(:on-face selector
  (:draft direction angle &key neutral-plane))

;; Named selections
(:save-selection name)
(:on-selection name ...)
```

### Complete Example

```lisp
(defpart advanced-demo-part ((size 100))
  "Demonstration of all Phase 8 features"

  ;; Start with lofted body
  (:loft
    (make-rectangle size size)
    (translate (make-circle (/ size 2)) 0 0 50)
    (translate (make-circle (/ size 3)) 0 0 100)
    :solid t
    :ruled nil)

  ;; Save bottom face for later
  (:on-face :direction :-z :extreme :min
    (:save-selection 'bottom-face))

  ;; Add swept handle
  (:add
    (sweep (make-circle 8)
           (make-spline '((0 0 50) (30 0 70) (50 0 80)))
           :twist 0))

  ;; Shell with top opening
  (:on-face :direction :+z :extreme :max
    (:shell -2.5 :remove-selected-faces t))

  ;; Fillet all edges
  (:on-edge :type :line
    (:fillet 2))

  ;; Add mounting features to bottom
  (:on-selection 'bottom-face
    (:circular-pattern :count 4 :radius (* size 0.35)
      (:add (make-cylinder 6 10))))

  ;; Draft side faces for molding
  (:on-face :perpendicular :z
    (:draft '(0 0 1) 3))

  ;; Mirror to create symmetric part
  (:mirror :yz :union-original t))
```

---

## Testing Strategy

### Test Categories

1. **Unit Tests** - Individual operations
2. **Integration Tests** - Feature combinations
3. **Regression Tests** - Maintain compatibility
4. **Visual Tests** - Manual inspection of results
5. **Performance Tests** - Timing benchmarks

### Test Organization

```lisp
;; tests/phase8/
;;   fillet-tests.lisp
;;   chamfer-tests.lisp
;;   spline-tests.lisp
;;   sweep-tests.lisp
;;   loft-tests.lisp
;;   shell-tests.lisp
;;   mirror-tests.lisp
;;   draft-tests.lisp
;;   integration-tests.lisp
;;   selector-enhancement-tests.lisp
```

### TDD Approach

For each feature:

1. **Write failing test** - Define expected behavior
2. **Implement FFI stub** - Return dummy data
3. **Implement Core API** - Use stub
4. **Run tests** - Should pass with stubs
5. **Implement C++ wrapper** - Real OCCT calls
6. **Run tests with OCCT** - Should still pass
7. **Refactor and optimize**

### Example Test Suite Structure

```lisp
;; tests/phase8/fillet-tests.lisp

(def-suite* phase8-fillet-tests
  :in phase8-tests
  :description "Tests for fillet operations")

(test fillet-basic
  "Basic fillet on box edges"
  ...)

(test fillet-variable-radius
  "Variable radius fillet"
  ...)

(test fillet-invalid-radius
  "Error handling for invalid radius"
  (signals occt-domain-error
    (fillet box edges -5)))

(test fillet-in-context
  "Fillet via context API"
  ...)

(test fillet-in-dsl
  "Fillet via defpart DSL"
  ...)

(test fillet-with-selectors
  "Fillet with enhanced selectors"
  ...)

(test fillet-performance
  "Performance benchmark"
  (let ((box (make-box 100 100 100)))
    (time (fillet box (edges box) 5))))
```

---

## Implementation Timeline

### Week 1-2: Selector Enhancements

- **Day 1-2**: Design and implement type selectors
- **Day 3-4**: Implement size-based selectors
- **Day 5-6**: Implement radius and angle selectors
- **Day 7**: Named selections
- **Day 8-10**: Tests and documentation

### Week 3-4: Fillets and Chamfers

- **Day 1-2**: FFI bindings for fillet/chamfer
- **Day 3-4**: Core API implementation
- **Day 5-6**: Context and DSL integration
- **Day 7-8**: Comprehensive testing
- **Day 9-10**: Examples and documentation

### Week 5-6: Curves and Splines

- **Day 1-3**: FFI bindings for B-splines, Bezier, arcs
- **Day 4-5**: Wire operations
- **Day 6-7**: Core API and integration
- **Day 8-10**: Testing and examples

### Week 7-8: Sweeps and Lofts

- **Day 1-3**: Sweep FFI and Core API
- **Day 4-5**: Multi-section sweeps
- **Day 6-8**: Loft implementation
- **Day 9-10**: Testing and examples

### Week 9-10: Shelling

- **Day 1-2**: FFI bindings for shell
- **Day 3-4**: Core API with variable thickness
- **Day 5-6**: Integration and testing
- **Day 7-10**: Complex examples (bottles, housings)

### Week 11-12: Mirroring and Draft

- **Day 1-3**: Mirror implementation
- **Day 4-6**: Draft angle implementation
- **Day 7-10**: Integration testing and examples

### Week 13-14: Integration and Polish

- **Day 1-5**: Integration tests with all features
- **Day 6-10**: Performance optimization
- **Day 11-14**: Documentation and examples

### Week 15-16: Examples Library

- **Day 1-4**: Basic examples for each feature
- **Day 5-8**: Complex real-world examples
- **Day 9-12**: Tutorial examples
- **Day 13-16**: Documentation and video guides

---

## Examples Library

### Catalog of Examples

1. **Basic Operations (5 examples)**
   - Simple fillet
   - Simple chamfer
   - Basic sweep
   - Basic loft
   - Basic shell

2. **Manufacturing Examples (10 examples)**
   - Injection molded housing
   - Cast bracket
   - Machined part with chamfers
   - Sheet metal formed part
   - Forged component

3. **Consumer Products (10 examples)**
   - Perfume bottle
   - Smartphone case
   - Water bottle
   - Tool handle
   - Cosmetic container

4. **Mechanical Parts (15 examples)**
   - Gears with fillets
   - Bearing housing
   - Pipe connector
   - Flange
   - Shaft coupling
   - Turbine blade
   - Cam profile
   - Linkage
   - Mount bracket
   - Valve body
   - Pump impeller
   - Heat sink
   - Spring clip
   - Clamp
   - Knob

5. **Organic/Aesthetic (10 examples)**
   - Vase
   - Sculpture
   - Ergonomic grip
   - Jewelry piece
   - Decorative panel
   - Lamp shade
   - Furniture leg
   - Door handle
   - Trophy
   - Figurine

### Example Template

Each example should include:

1. **Source code** - Complete, commented defpart
2. **Parameter exploration** - Show variations
3. **Manufacturing notes** - Material, process
4. **Test case** - Automated test
5. **Visual render** - Screenshot/rendering
6. **STEP export** - Verification file

---

## Success Criteria

### Feature Completeness

- [ ] All 7 features implemented and tested
- [ ] 100% test coverage for new code
- [ ] All examples working and documented
- [ ] Performance meets targets (<1s for typical parts)

### Quality Metrics

- [ ] Zero memory leaks (valgrind clean)
- [ ] All STEP exports valid (FreeCAD verification)
- [ ] No regression in existing features
- [ ] Documentation complete and clear

### User Experience

- [ ] DSL is intuitive and consistent
- [ ] Error messages are helpful
- [ ] Examples cover common use cases
- [ ] Auto-rebuild works with all features

---

## Next Steps

1. **Review and approve this design**
2. **Set up test framework for Phase 8**
3. **Begin Week 1 implementation** (Selector enhancements)
4. **Establish weekly review schedule**
5. **Create example repository structure**

---

## Appendix A: OCCT API Reference

### Key OCCT Classes for Phase 8

**Fillets and Chamfers:**
- `BRepFilletAPI_MakeFillet`
- `BRepFilletAPI_MakeFillet2d`
- `BRepFilletAPI_MakeChamfer`

**Curves:**
- `Geom_BSplineCurve`
- `Geom_BezierCurve`
- `GeomAPI_PointsToBSpline`
- `GeomAPI_Interpolate`

**Sweeps and Lofts:**
- `BRepOffsetAPI_MakePipe`
- `BRepOffsetAPI_MakePipeShell`
- `BRepOffsetAPI_ThruSections`

**Shelling:**
- `BRepOffsetAPI_MakeThickSolid`
- `BRepOffsetAPI_MakeOffset`

**Mirroring:**
- `BRepBuilderAPI_Transform`
- `gp_Trsf::SetMirror`

**Draft:**
- `BRepOffsetAPI_DraftAngle`

### C++ Wrapper Patterns

```cpp
// Example pattern for Phase 8 wrappers
extern "C" {
    int occt_operation(input_params, void** out_shape, char** error_msg) {
        try {
            // Validate inputs
            if (invalid) {
                *error_msg = strdup("Error description");
                return -2;  // domain error
            }

            // Perform OCCT operation
            BRepAPI_MakeXXX maker(...);

            if (!maker.IsDone()) {
                *error_msg = strdup("Operation failed");
                return -3;  // construction error
            }

            // Return result
            TopoDS_Shape* shape = new TopoDS_Shape(maker.Shape());
            *out_shape = shape;
            return 0;  // success
        }
        catch (const Standard_Failure& e) {
            *error_msg = strdup(e.GetMessageString());
            return 1;
        }
        catch (...) {
            *error_msg = strdup("Unknown error");
            return -1;
        }
    }
}
```

---

## Appendix B: Selector Enhancement Reference

### Complete Selector Syntax

```lisp
;; Existing selectors
:direction AXIS :extreme EXTREME
:parallel AXIS
:perpendicular AXIS
(lambda (shape) ...)

;; New geometry type selectors
:type TYPE                    ; TYPE = :line, :circle, :plane, :cylinder, etc.

;; New size selectors
:length COMPARATOR VALUE [VALUE2]
:area COMPARATOR VALUE [VALUE2]
:radius COMPARATOR VALUE [VALUE2]
:volume COMPARATOR VALUE [VALUE2]
;; COMPARATOR = :>, :<, :>=, :<=, :=, :between

;; New angle selectors
:angle-with-shape SHAPE :approximately ANGLE

;; Named selections
:named NAME

;; Combinators (existing, still work)
(:and SELECTOR1 SELECTOR2 ...)
(:or SELECTOR1 SELECTOR2 ...)
(:not SELECTOR)
```

### Selector Examples

```lisp
;; Select circular edges with radius ~5mm
(select edges :type :circle :radius := 5)

;; Select large planar faces
(select faces :type :plane :area :> 1000)

;; Select short vertical edges
(select edges :parallel :z :length :< 20)

;; Complex selection with combinators
(select edges
  (:and (:type :line)
        (:parallel :z)
        (:length :between 10 50)))

;; Save and reuse selection
(save-selection 'mounting-faces
  (select faces :type :cylinder :radius := 6))

(:on-selection 'mounting-faces
  (:cut (make-cylinder 3 20)))
```

---

## Appendix C: Common Lisp Strengths Leveraged

### 1. Macros for DSL

```lisp
;; Macros enable clean, declarative syntax
(defpart example ()
  (:body ...)
  (:on-face ... (:fillet ...)))

;; Compiles to efficient code at macro-expansion time
;; No runtime interpretation overhead
```

### 2. CLOS for Extensibility

```lisp
;; Users can define custom selectors
(defclass my-custom-selector (selector)
  ((my-param :initarg :param)))

(defmethod apply-selector ((sel my-custom-selector) shapes)
  ;; Custom selection logic
  ...)
```

### 3. Conditions for Error Handling

```lisp
;; Rich error handling with restarts
(defun fillet (...)
  (restart-case
      (perform-fillet ...)
    (use-smaller-radius (new-radius)
      (fillet ... new-radius ...))
    (skip-edge ()
      (continue-without-edge ...))))
```

### 4. Higher-Order Functions

```lisp
;; Selectors are first-class functions
(defun custom-filter (predicate)
  (lambda (shapes)
    (remove-if-not predicate shapes)))

(select edges (custom-filter #'my-test-fn))
```

### 5. Multiple Dispatch

```lisp
;; Different behavior for different shape types
(defmethod fillet ((e edge) radius)
  ...)

(defmethod fillet ((f face) radius)
  ...)

(defmethod fillet ((s solid) edges radius)
  ...)
```

### 6. Symbols for Named Entities

```lisp
;; Named selections use symbols
(save-selection 'top-faces ...)
(get-selection 'top-faces)

;; Symbol properties for metadata
(setf (get 'my-part 'clad-part-metadata)
      '(:author "Jack" :date "2025-11-01"))
```

---

**End of Phase 8 Design Specification**

---

## Phase 9 Preview: 2D Sketching System

### Overview

While Phase 8 focuses on 3D operations, CAD systems require robust 2D sketching with constraints for creating profiles. This section previews the sketch system planned for Phase 9.

### CADquery Sketch Analysis

**CADquery Approach:**
- Face-based construction with boolean operations
- Edge-based construction converted via `assemble()`
- Experimental constraint solver
- String-based constraint specification
- Limited to fluent API chains

**Limitations:**
- Constraints are experimental and incomplete
- No parametric solver by default
- String-based selectors are error-prone
- Limited to 2D operations on workplanes

### CLAD Sketch System Design (Common Lisp Way)

#### 1. CLOS-Based Constraint System

```lisp
;; src/sketch/constraints.lisp

(defclass geometric-constraint ()
  ((entities :initarg :entities
             :accessor constraint-entities
             :documentation "List of sketch entities this constrains")
   (priority :initarg :priority
             :initform 100
             :accessor constraint-priority
             :type integer
             :documentation "Solving priority (lower = higher priority)"))
  (:documentation "Base class for all geometric constraints"))

(defclass distance-constraint (geometric-constraint)
  ((entity1 :initarg :entity1
            :accessor constraint-entity1)
   (entity2 :initarg :entity2
            :accessor constraint-entity2)
   (distance :initarg :distance
             :accessor constraint-distance
             :type number))
  (:documentation "Constrains distance between two points/entities"))

(defclass coincident-constraint (geometric-constraint)
  ((point1 :initarg :point1
           :accessor constraint-point1)
   (point2 :initarg :point2
           :accessor constraint-point2))
  (:documentation "Forces two points to coincide"))

(defclass parallel-constraint (geometric-constraint)
  ((line1 :initarg :line1
          :accessor constraint-line1)
   (line2 :initarg :line2
          :accessor constraint-line2))
  (:documentation "Forces two lines to be parallel"))

(defclass perpendicular-constraint (geometric-constraint)
  ((line1 :initarg :line1)
   (line2 :initarg :line2))
  (:documentation "Forces two lines to be perpendicular"))

(defclass tangent-constraint (geometric-constraint)
  ((curve1 :initarg :curve1)
   (curve2 :initarg :curve2))
  (:documentation "Forces two curves to be tangent"))

(defclass angle-constraint (geometric-constraint)
  ((line1 :initarg :line1)
   (line2 :initarg :line2)
   (angle :initarg :angle
          :accessor constraint-angle))
  (:documentation "Sets angle between two lines"))

(defclass fixed-constraint (geometric-constraint)
  ((entity :initarg :entity
           :accessor constraint-entity))
  (:documentation "Fixes an entity in place"))

(defclass horizontal-constraint (geometric-constraint)
  ((line :initarg :line
         :accessor constraint-line))
  (:documentation "Forces line to be horizontal"))

(defclass vertical-constraint (geometric-constraint)
  ((line :initarg :line
         :accessor constraint-line))
  (:documentation "Forces line to be vertical"))

(defclass equal-length-constraint (geometric-constraint)
  ((line1 :initarg :line1)
   (line2 :initarg :line2))
  (:documentation "Forces two lines to have equal length"))

(defclass equal-radius-constraint (geometric-constraint)
  ((arc1 :initarg :arc1)
   (arc2 :initarg :arc2))
  (:documentation "Forces two arcs/circles to have equal radius"))
```

#### 2. Sketch Entity System

```lisp
;; src/sketch/entities.lisp

(defclass sketch-entity ()
  ((id :initarg :id
       :accessor entity-id
       :type symbol
       :documentation "Unique identifier")
   (parameters :initarg :parameters
               :accessor entity-parameters
               :type hash-table
               :documentation "Parameter values (variable)")
   (constraints :initform '()
                :accessor entity-constraints
                :documentation "Constraints affecting this entity"))
  (:documentation "Base class for all sketch entities"))

(defclass sketch-point (sketch-entity)
  ((x :initarg :x :accessor point-x :type number)
   (y :initarg :y :accessor point-y :type number))
  (:documentation "A point in 2D space"))

(defclass sketch-line (sketch-entity)
  ((start-point :initarg :start
                :accessor line-start
                :type sketch-point)
   (end-point :initarg :end
              :accessor line-end
              :type sketch-point))
  (:documentation "Line segment between two points"))

(defclass sketch-arc (sketch-entity)
  ((center :initarg :center
           :accessor arc-center
           :type sketch-point)
   (radius :initarg :radius
           :accessor arc-radius
           :type number)
   (start-angle :initarg :start-angle
                :accessor arc-start-angle)
   (end-angle :initarg :end-angle
              :accessor arc-end-angle))
  (:documentation "Circular arc"))

(defclass sketch-circle (sketch-entity)
  ((center :initarg :center
           :accessor circle-center
           :type sketch-point)
   (radius :initarg :radius
           :accessor circle-radius
           :type number))
  (:documentation "Full circle"))

(defclass sketch-spline (sketch-entity)
  ((control-points :initarg :control-points
                   :accessor spline-control-points
                   :type list)
   (degree :initarg :degree
           :initform 3
           :accessor spline-degree))
  (:documentation "B-spline curve"))
```

#### 3. Constraint Solver (Leveraging Common Lisp)

```lisp
;; src/sketch/solver.lisp

(defclass constraint-solver ()
  ((entities :initform (make-hash-table :test 'eq)
             :accessor solver-entities
             :documentation "All sketch entities")
   (constraints :initform '()
                :accessor solver-constraints
                :documentation "All constraints")
   (degrees-of-freedom :accessor solver-dof
                       :documentation "Current DOF count")
   (solution-cache :initform (make-hash-table :test 'equal)
                   :accessor solver-cache
                   :documentation "Cached solutions for performance"))
  (:documentation "Geometric constraint solver"))

(defmethod add-entity ((solver constraint-solver) entity)
  "Add entity to solver"
  (setf (gethash (entity-id entity) (solver-entities solver))
        entity)
  (invalidate-cache solver)
  entity)

(defmethod add-constraint ((solver constraint-solver) constraint)
  "Add constraint to solver"
  (push constraint (solver-constraints solver))
  ;; Add constraint to affected entities
  (dolist (entity (constraint-entities constraint))
    (push constraint (entity-constraints entity)))
  (invalidate-cache solver)
  constraint)

(defgeneric constraint-error (constraint)
  (:documentation "Compute error function for constraint.
  Returns numerical error that solver tries to minimize."))

(defmethod constraint-error ((c distance-constraint))
  "Distance constraint error"
  (let* ((p1 (constraint-entity1 c))
         (p2 (constraint-entity2 c))
         (actual-dist (point-distance p1 p2))
         (target-dist (constraint-distance c)))
    (expt (- actual-dist target-dist) 2)))

(defmethod constraint-error ((c coincident-constraint))
  "Coincidence constraint error"
  (let ((p1 (constraint-point1 c))
        (p2 (constraint-point2 c)))
    (+ (expt (- (point-x p1) (point-x p2)) 2)
       (expt (- (point-y p1) (point-y p2)) 2))))

(defmethod constraint-error ((c angle-constraint))
  "Angle constraint error"
  (let* ((l1 (constraint-line1 c))
         (l2 (constraint-line2 c))
         (actual-angle (line-angle l1 l2))
         (target-angle (constraint-angle c)))
    (expt (- actual-angle target-angle) 2)))

(defmethod solve ((solver constraint-solver) &key (max-iterations 1000)
                                                   (tolerance 1e-6))
  "Solve the constraint system using iterative optimization.
  
  Uses Newton-Raphson method with automatic differentiation
  via Common Lisp's numerical capabilities."
  
  ;; Build parameter vector (all variable values)
  (let ((params (collect-parameters solver))
        (iteration 0)
        (converged nil))
    
    ;; Iterative solver loop
    (loop while (and (< iteration max-iterations)
                     (not converged))
          do
          ;; Compute total error
          (let ((total-error (compute-total-error solver)))
            
            (when (< total-error tolerance)
              (setf converged t)
              (return-from solve :converged))
            
            ;; Compute Jacobian (gradient of error function)
            (let ((jacobian (compute-jacobian solver params)))
              
              ;; Newton-Raphson step
              (let ((delta (solve-linear-system jacobian
                                                (error-gradient solver))))
                ;; Update parameters
                (update-parameters solver params delta)))
            
            (incf iteration)))
    
    (if converged
        (values :converged iteration)
        (values :max-iterations iteration))))

(defun compute-jacobian (solver params)
  "Compute Jacobian matrix using automatic differentiation.
  
  Common Lisp advantage: Can use numerical differentiation,
  symbolic math packages, or even compile-time code generation."
  
  (let ((n (length params))
        (m (length (solver-constraints solver)))
        (jacobian (make-array (list m n) :initial-element 0.0)))
    
    ;; For each constraint...
    (loop for constraint in (solver-constraints solver)
          for i from 0
          do
          ;; For each parameter...
          (loop for param in params
                for j from 0
                do
                ;; Compute partial derivative numerically
                (setf (aref jacobian i j)
                      (numerical-derivative
                       (lambda (p)
                         (setf (parameter-value param) p)
                         (constraint-error constraint))
                       (parameter-value param)))))
    
    jacobian))

;; Alternative: Symbolic differentiation (Common Lisp advantage!)
(defmacro define-constraint-with-gradient (name params &body error-form)
  "Define constraint with automatically computed gradient.
  
  This macro uses code-walking to compute symbolic derivatives
  at compile time - a unique Common Lisp capability!"
  
  `(progn
     (defclass ,name (geometric-constraint)
       ,(mapcar (lambda (p)
                  `(,p :initarg ,(intern (string p) :keyword)
                       :accessor ,(intern (format nil "CONSTRAINT-~A" p))))
                params))
     
     (defmethod constraint-error ((c ,name))
       ,@error-form)
     
     ;; Automatically generated gradient method
     (defmethod constraint-gradient ((c ,name))
       ,(compute-symbolic-gradient params error-form))))
```

#### 4. DSL for Sketching

```lisp
;; src/sketch/dsl.lisp

(defmacro defsketch (name &body constraints-and-entities)
  "Define a parametric 2D sketch with constraints.
  
  Example:
    (defsketch bracket-profile
      ;; Define entities
      (:line 'L1 :from '(0 0) :to '(100 0))
      (:line 'L2 :from '(100 0) :to '(100 50))
      (:arc 'A1 :center '(100 50) :radius 10)
      
      ;; Apply constraints
      (:horizontal 'L1)
      (:vertical 'L2)
      (:tangent 'L2 'A1)
      (:distance '(0 0) '(100 0) 100))"
  
  `(let ((sketch (make-instance 'sketch :name ',name)))
     ,@(mapcar #'expand-sketch-form constraints-and-entities)
     sketch))

(defun expand-sketch-form (form)
  "Expand sketch DSL form"
  (case (first form)
    (:line
     `(add-line sketch
                :id ',(second form)
                :start ',(getf (cddr form) :from)
                :end ',(getf (cddr form) :to)))
    
    (:arc
     `(add-arc sketch
               :id ',(second form)
               :center ',(getf (cddr form) :center)
               :radius ',(getf (cddr form) :radius)))
    
    (:horizontal
     `(add-constraint sketch
                      (make-instance 'horizontal-constraint
                                     :line (get-entity sketch ',(second form)))))
    
    (:vertical
     `(add-constraint sketch
                      (make-instance 'vertical-constraint
                                     :line (get-entity sketch ',(second form)))))
    
    ;; More constraint forms...
    ))
```

#### 5. Integration with 3D Operations

```lisp
;; src/sketch/integration.lisp

(defmethod extrude ((sketch sketch) distance &key (direction :+z))
  "Extrude a solved sketch to create 3D solid.
  
  Workflow:
    1. Solve sketch constraints
    2. Convert to OCCT wires
    3. Make face from wires
    4. Extrude face"
  
  (solve (sketch-solver sketch))
  (let* ((wires (sketch-to-wires sketch))
         (face (make-face-from-wires wires))
         (vector (direction-to-vector direction distance)))
    (clad.core:extrude face vector)))

(defmethod revolve ((sketch sketch) axis angle)
  "Revolve sketch around axis"
  (solve (sketch-solver sketch))
  (let ((wires (sketch-to-wires sketch)))
    (clad.core:revolve wires axis angle)))

(defmethod sweep ((sketch sketch) path)
  "Sweep sketch along path"
  (solve (sketch-solver sketch))
  (let ((profile (sketch-to-wire sketch)))
    (clad.core:sweep profile path)))
```

#### 6. Usage Example

```lisp
;; Complete example: Constrained sketch

(defsketch mounting-bracket-profile ()
  "2D profile for mounting bracket with parametric constraints"
  
  ;; Base rectangle
  (:line 'L1 :from '(0 0) :to '(100 0))
  (:line 'L2 :from '(100 0) :to '(100 50))
  (:line 'L3 :from '(100 50) :to '(0 50))
  (:line 'L4 :from '(0 50) :to '(0 0))
  
  ;; Mounting hole
  (:circle 'C1 :center '(50 25) :radius 6)
  
  ;; Constraints
  (:horizontal 'L1)
  (:horizontal 'L3)
  (:vertical 'L2)
  (:vertical 'L4)
  (:perpendicular 'L1 'L2)
  (:equal-length 'L1 'L3)
  (:equal-length 'L2 'L4)
  (:distance 'L1 100)  ; Fix base width
  (:distance 'L2 50)   ; Fix height
  
  ;; Center the hole
  (:midpoint 'C1-CENTER 'L1)  ; Horizontally centered on L1
  (:midpoint 'C1-CENTER 'L2)  ; Vertically centered on L2
  
  ;; Fix hole size
  (:radius 'C1 6))

;; Use the sketch
(defpart bracket ()
  (:body
    (extrude mounting-bracket-profile 10))
  
  ;; Add 3D features...
  (:on-face :direction :+z :extreme :max
    (:circular-pattern :count 4 :radius 35
      (:cut (make-cylinder 3 15)))))
```

### Common Lisp Advantages for Sketching

1. **CLOS Constraint Hierarchy**
   - Extensible constraint types
   - Method combination for complex constraints
   - Natural polymorphism

2. **Symbolic Math Integration**
   - Can use Maxima or similar for symbolic derivatives
   - Compile-time gradient computation via macros
   - Zero runtime overhead for analytical gradients

3. **Condition System**
   - Recoverable errors (over-constrained, under-constrained)
   - Interactive debugging via restarts
   - User can resolve conflicts interactively

4. **Metaprogramming**
   - Domain-specific constraint languages
   - Auto-generating solver code
   - Custom optimization strategies

5. **Interactive Development**
   - Live constraint modification in REPL
   - Immediate feedback on solvability
   - Visual debugging with viewer integration

---

## Phase 10 Preview: Assembly System

### Overview

Assembly systems enable multi-part designs with relationships between components. Phase 10 will implement constraint-based assembly modeling.

### CADquery Assembly Analysis

**CADquery Approach:**
- Hierarchical assembly structure
- String-based constraint specification: `"part1@faces@>Z"`
- 8 constraint types (Point, Axis, Plane, etc.)
- Sequential constraint solving via optimization
- Limited to programmatic assembly

**Limitations:**
- String parsing errors hard to debug
- No assembly-level parameters
- Limited constraint types
- No BOM generation built-in
- Python-centric serialization

### CLAD Assembly System Design (Common Lisp Way)

#### 1. CLOS Assembly Hierarchy

```lisp
;; src/assembly/classes.lisp

(defclass assembly-component ()
  ((id :initarg :id
       :accessor component-id
       :type symbol
       :documentation "Unique component identifier")
   (part :initarg :part
         :accessor component-part
         :documentation "The actual part (defpart result)")
   (transform :initform (identity-transform)
              :accessor component-transform
              :documentation "Current 4x4 transformation matrix")
   (metadata :initform (make-hash-table :test 'equal)
             :accessor component-metadata
             :documentation "Part metadata (material, cost, etc.)"))
  (:documentation "A single component in an assembly"))

(defclass assembly ()
  ((name :initarg :name
         :accessor assembly-name
         :type symbol)
   (components :initform (make-hash-table :test 'eq)
               :accessor assembly-components
               :documentation "Component registry (id -> component)")
   (constraints :initform '()
                :accessor assembly-constraints
                :documentation "Assembly constraints")
   (subassemblies :initform '()
                  :accessor assembly-subassemblies
                  :documentation "Child assemblies")
   (parameters :initform (make-hash-table :test 'eq)
               :accessor assembly-parameters
               :documentation "Assembly-level parameters")
   (bom :accessor assembly-bom
        :documentation "Bill of materials (computed)"))
  (:documentation "Top-level assembly containing components and constraints"))

(defclass assembly-constraint ()
  ((id :initarg :id
       :accessor constraint-id
       :type symbol)
   (component1 :initarg :comp1
               :accessor constraint-component1)
   (component2 :initarg :comp2
               :accessor constraint-component2)
   (priority :initarg :priority
             :initform 100
             :accessor constraint-priority))
  (:documentation "Base class for assembly constraints"))
```

#### 2. Mate Constraints (Common Lisp Way)

```lisp
;; src/assembly/constraints.lisp

(defclass mate-constraint (assembly-constraint)
  ((face1 :initarg :face1
          :accessor mate-face1
          :documentation "Face/surface on component 1")
   (face2 :initarg :face2
          :accessor mate-face2
          :documentation "Face/surface on component 2")
   (aligned :initarg :aligned
            :initform t
            :accessor mate-aligned
            :documentation "T for aligned (same direction), NIL for opposed"))
  (:documentation "Planar mate - two faces touch/align"))

(defclass axial-constraint (assembly-constraint)
  ((axis1 :initarg :axis1
          :accessor axial-axis1)
   (axis2 :initarg :axis2
          :accessor axial-axis2))
  (:documentation "Coaxial constraint - two axes align"))

(defclass point-constraint (assembly-constraint)
  ((point1 :initarg :point1
           :accessor point-constraint-point1)
   (point2 :initarg :point2
           :accessor point-constraint-point2))
  (:documentation "Point-to-point constraint"))

(defclass distance-constraint (assembly-constraint)
  ((reference1 :initarg :ref1)
   (reference2 :initarg :ref2)
   (distance :initarg :distance
             :accessor constraint-distance))
  (:documentation "Maintains fixed distance between references"))

(defclass angle-constraint (assembly-constraint)
  ((plane1 :initarg :plane1)
   (plane2 :initarg :plane2)
   (angle :initarg :angle
          :accessor constraint-angle))
  (:documentation "Sets angle between two planes"))

(defclass tangent-constraint (assembly-constraint)
  ((surface1 :initarg :surf1)
   (surface2 :initarg :surf2))
  (:documentation "Tangency between two surfaces"))
```

#### 3. Symbol-Based References (Not String-Based!)

```lisp
;; src/assembly/references.lisp

(defclass assembly-reference ()
  ((component :initarg :component
              :accessor reference-component
              :type symbol
              :documentation "Component ID (symbol)")
   (feature-type :initarg :feature-type
                 :accessor reference-feature-type
                 :type keyword
                 :documentation ":face, :edge, :vertex, :axis, etc.")
   (selector :initarg :selector
             :accessor reference-selector
             :documentation "Selector specification"))
  (:documentation "Reference to a feature on a component"))

(defun make-reference (component feature-type selector)
  "Create assembly reference using symbols and selectors.
  
  Examples:
    (make-reference 'housing :face '(:direction :+z :extreme :max))
    (make-reference 'shaft :axis '(:type :cylinder))
    (make-reference 'bolt :vertex '(:index 0))"
  
  (make-instance 'assembly-reference
                 :component component
                 :feature-type feature-type
                 :selector selector))

;; Convenience macro
(defmacro @ref (component feature-type &rest selector)
  "Shorthand for creating references.
  
  Usage:
    (@ref housing :face :direction :+z :extreme :max)
    (@ref shaft :axis :type :cylinder)
    (@ref bolt :vertex :index 0)"
  
  `(make-reference ',component ,feature-type ',selector))
```

#### 4. Assembly DSL (Declarative)

```lisp
;; src/assembly/dsl.lisp

(defmacro defassembly (name (&key parameters) &body spec)
  "Define a parametric assembly.
  
  Example:
    (defassembly door-assembly (:parameters ((width 800) (height 2000)))
      ;; Add components
      (:component 'frame (make-door-frame width height))
      (:component 'panel (make-panel (- width 20) (- height 20)))
      (:component 'hinge1 (make-hinge 100))
      (:component 'hinge2 (make-hinge 100))
      (:component 'handle (make-handle 150))
      
      ;; Metadata
      (:metadata 'frame :material \"Aluminum\" :cost 50.00)
      (:metadata 'panel :material \"Wood\" :cost 30.00)
      
      ;; Constraints
      (:mate 'frame (@ref frame :face :direction :+z :extreme :max)
             'panel (@ref panel :face :direction :-z :extreme :min)
             :aligned nil)  ; Opposed faces
      
      (:coaxial 'hinge1 (@ref hinge1 :axis :type :cylinder)
                'frame (@ref frame :edge :type :circle :at '(50 100 0)))
      
      (:distance 'hinge1 'hinge2 (- height 200)))"
  
  (let ((param-names (mapcar #'first parameters))
        (param-defaults (mapcar #'second parameters)))
    `(defun ,name (&key ,@parameters)
       (let ((assembly (make-instance 'assembly :name ',name)))
         ;; Store parameters
         ,@(mapcar (lambda (p)
                     `(setf (gethash ',p (assembly-parameters assembly))
                            ,p))
                   param-names)
         
         ;; Process assembly specification
         ,@(mapcar #'expand-assembly-form spec)
         
         ;; Solve constraints
         (solve-assembly assembly)
         
         ;; Return assembled result
         assembly))))

(defun expand-assembly-form (form)
  "Expand assembly DSL form"
  (case (first form)
    (:component
     (destructuring-bind (id part-form) (rest form)
       `(add-component assembly ,id ,part-form)))
    
    (:metadata
     (destructuring-bind (id &rest metadata) (rest form)
       `(set-metadata assembly ',id ,@metadata)))
    
    (:mate
     (destructuring-bind (comp1 ref1 comp2 ref2 &key (aligned t)) (rest form)
       `(add-constraint assembly
                        (make-instance 'mate-constraint
                                       :comp1 ',comp1
                                       :face1 ,ref1
                                       :comp2 ',comp2
                                       :face2 ,ref2
                                       :aligned ,aligned))))
    
    (:coaxial
     (destructuring-bind (comp1 ref1 comp2 ref2) (rest form)
       `(add-constraint assembly
                        (make-instance 'axial-constraint
                                       :comp1 ',comp1
                                       :axis1 ,ref1
                                       :comp2 ',comp2
                                       :axis2 ,ref2))))
    
    (:distance
     (destructuring-bind (comp1 comp2 distance) (rest form)
       `(add-constraint assembly
                        (make-instance 'distance-constraint
                                       :comp1 ',comp1
                                       :comp2 ',comp2
                                       :distance ,distance))))
    
    ;; More constraint types...
    ))
```

#### 5. Constraint Solver for Assemblies

```lisp
;; src/assembly/solver.lisp

(defun solve-assembly (assembly &key (method :sequential))
  "Solve assembly constraints to position all components.
  
  Methods:
    :sequential - Solve constraints in order (fast, may not converge)
    :simultaneous - Solve all constraints together (slow, more robust)
    :incremental - Add components one by one (best for large assemblies)"
  
  (ecase method
    (:sequential
     (solve-sequential assembly))
    (:simultaneous
     (solve-simultaneous assembly))
    (:incremental
     (solve-incremental assembly))))

(defun solve-sequential (assembly)
  "Solve constraints sequentially.
  
  Common Lisp advantage: Can use condition system for backtracking!"
  
  (let ((constraints (sort (copy-list (assembly-constraints assembly))
                          #'< :key #'constraint-priority)))
    (dolist (constraint constraints)
      (handler-case
          (apply-constraint assembly constraint)
        (unsolvable-constraint (c)
          ;; Use restarts for interactive resolution
          (restart-case
              (error c)
            (skip-constraint ()
              :report "Skip this constraint and continue"
              nil)
            (relax-constraint (new-priority)
              :report "Reduce constraint priority and retry"
              (setf (constraint-priority constraint) new-priority)
              (apply-constraint assembly constraint))
            (adjust-manually ()
              :report "Manually adjust component position"
              (manually-adjust-component assembly constraint))))))))

(defgeneric apply-constraint (assembly constraint)
  (:documentation "Apply a single constraint to reposition components"))

(defmethod apply-constraint ((asm assembly) (c mate-constraint))
  "Apply mate constraint - align two faces"
  (let* ((comp1 (gethash (constraint-component1 c)
                        (assembly-components asm)))
         (comp2 (gethash (constraint-component2 c)
                        (assembly-components asm)))
         (face1 (resolve-reference (mate-face1 c) comp1))
         (face2 (resolve-reference (mate-face2 c) comp2))
         ;; Compute transformation to mate faces
         (transform (compute-mate-transform face1 face2
                                           (mate-aligned c))))
    
    ;; Apply transformation to component 2
    (setf (component-transform comp2)
          (compose-transforms (component-transform comp2) transform))))

(defmethod apply-constraint ((asm assembly) (c axial-constraint))
  "Apply coaxial constraint"
  (let* ((comp1 (gethash (constraint-component1 c)
                        (assembly-components asm)))
         (comp2 (gethash (constraint-component2 c)
                        (assembly-components asm)))
         (axis1 (resolve-reference (axial-axis1 c) comp1))
         (axis2 (resolve-reference (axial-axis2 c) comp2))
         (transform (compute-axial-transform axis1 axis2)))
    
    (setf (component-transform comp2)
          (compose-transforms (component-transform comp2) transform))))
```

#### 6. Bill of Materials (BOM)

```lisp
;; src/assembly/bom.lisp

(defclass bom-entry ()
  ((part-id :initarg :id
            :accessor bom-part-id)
   (description :initarg :description
                :accessor bom-description)
   (quantity :initarg :quantity
             :initform 1
             :accessor bom-quantity)
   (material :initarg :material
             :accessor bom-material)
   (cost :initarg :cost
         :initform 0.0
         :accessor bom-cost)
   (supplier :initarg :supplier
             :initform nil
             :accessor bom-supplier)
   (part-number :initarg :part-number
                :initform nil
                :accessor bom-part-number))
  (:documentation "Single entry in bill of materials"))

(defmethod generate-bom ((assembly assembly))
  "Generate bill of materials from assembly.
  
  Traverses assembly hierarchy, collecting all components
  with their metadata and quantities."
  
  (let ((bom (make-hash-table :test 'eq)))
    ;; Collect components
    (labels ((collect (asm parent-quantity)
               (maphash
                (lambda (id component)
                  (let* ((metadata (component-metadata component))
                         (existing (gethash id bom)))
                    (if existing
                        ;; Increment quantity
                        (incf (bom-quantity existing) parent-quantity)
                        ;; New entry
                        (setf (gethash id bom)
                              (make-instance 'bom-entry
                                             :id id
                                             :description (gethash "description" metadata)
                                             :quantity parent-quantity
                                             :material (gethash "material" metadata)
                                             :cost (gethash "cost" metadata 0.0)
                                             :supplier (gethash "supplier" metadata)
                                             :part-number (gethash "part-number" metadata))))))
                (assembly-components asm))
               
               ;; Recurse into subassemblies
               (dolist (subasm (assembly-subassemblies asm))
                 (collect subasm parent-quantity))))
      
      (collect assembly 1))
    
    ;; Convert to list and sort
    (sort (hash-table-values bom)
          #'string< :key #'bom-part-id)))

(defmethod export-bom ((assembly assembly) format)
  "Export BOM to various formats.
  
  Formats:
    :csv - Comma-separated values
    :markdown - Markdown table
    :html - HTML table
    :lisp - S-expression format (for further processing)"
  
  (let ((bom (generate-bom assembly)))
    (ecase format
      (:csv (export-bom-csv bom))
      (:markdown (export-bom-markdown bom))
      (:html (export-bom-html bom))
      (:lisp bom))))  ; Return as-is for Lisp processing
```

#### 7. Complete Assembly Example

```lisp
;; Complete example: Parametric door assembly

(defassembly door-assembly
    (:parameters ((width 800)
                  (height 2000)
                  (thickness 40)))
  
  ;; Components
  (:component 'frame
    (make-door-frame width height thickness))
  
  (:component 'panel
    (make-panel (- width (* 2 thickness))
                (- height (* 2 thickness))
                30))
  
  (:component 'hinge-top
    (make-hinge 100 :type :heavy-duty))
  
  (:component 'hinge-bottom
    (make-hinge 100 :type :heavy-duty))
  
  (:component 'handle
    (make-lever-handle 150))
  
  (:component 'lock-body
    (make-lock-cylinder 60))
  
  ;; Metadata for BOM
  (:metadata 'frame
             :description "Aluminum door frame"
             :material "6061-T6 Aluminum"
             :cost 120.00
             :supplier "ABC Metals"
             :part-number "DF-800x2000")
  
  (:metadata 'panel
             :description "Wood core panel"
             :material "Marine plywood"
             :cost 80.00
             :supplier "Wood Supply Co"
             :part-number "PNL-760x1960")
  
  (:metadata 'hinge-top
             :description "Heavy duty hinge"
             :material "Stainless steel"
             :cost 25.00
             :supplier "Hardware Inc"
             :part-number "HNG-HD-100")
  
  (:metadata 'hinge-bottom
             :description "Heavy duty hinge"
             :material "Stainless steel"
             :cost 25.00
             :supplier "Hardware Inc"
             :part-number "HNG-HD-100")
  
  (:metadata 'handle
             :description "Lever handle"
             :material "Brushed nickel"
             :cost 35.00
             :supplier "Hardware Inc"
             :part-number "LVR-150-BN")
  
  (:metadata 'lock-body
             :description "Cylinder lock"
             :material "Brass"
             :cost 45.00
             :supplier "Security Systems"
             :part-number "CYL-60-BR")
  
  ;; Assembly constraints
  
  ;; Mate panel to frame (centered)
  (:mate 'frame (@ref frame :face :direction :+z :extreme :max)
         'panel (@ref panel :face :direction :-z :extreme :min)
         :aligned nil)
  
  ;; Position hinges on frame
  (:coaxial 'frame (@ref frame :edge :at (list thickness 200 0) :type :line)
            'hinge-top (@ref hinge-top :axis :type :cylinder))
  
  (:coaxial 'frame (@ref frame :edge :at (list thickness (- height 200) 0) :type :line)
            'hinge-bottom (@ref hinge-bottom :axis :type :cylinder))
  
  ;; Position handle on panel
  (:point 'panel (@ref panel :vertex :at (list (- width 100) (/ height 2) 0))
          'handle (@ref handle :vertex :at '(0 0 0)))
  
  ;; Mate handle to panel surface
  (:mate 'panel (@ref panel :face :direction :+z :extreme :max)
         'handle (@ref handle :face :direction :-z :extreme :min)
         :aligned t)
  
  ;; Position lock below handle
  (:distance 'handle (@ref handle :vertex :at '(0 0 0))
             'lock-body (@ref lock-body :vertex :at '(0 0 0))
             100)
  
  ;; Coaxial lock with handle
  (:coaxial 'handle (@ref handle :axis :type :cylinder)
            'lock-body (@ref lock-body :axis :type :cylinder)))

;; Use the assembly
(let ((door (door-assembly :width 900 :height 2100 :thickness 50)))
  ;; Export BOM
  (export-bom door :markdown)
  
  ;; Export assembly to STEP
  (export-assembly door "door.step")
  
  ;; Visualize
  (view-assembly door))
```

### Common Lisp Advantages for Assemblies

1. **Symbol-Based References** (Not Strings!)
   - Compile-time checking of component IDs
   - IDE autocomplete works
   - Type-safe references
   - No parsing errors

2. **CLOS Constraint Hierarchy**
   - Extensible constraint types
   - User-defined constraints
   - Constraint priority via method combination

3. **Condition System for Solving**
   - Recoverable errors (over-constrained, under-constrained)
   - Interactive conflict resolution
   - Restart capabilities for backtracking

4. **Parametric Assemblies**
   - True parametric relationships
   - Assembly-level parameters propagate to components
   - Recompute on parameter change

5. **Metadata as First-Class**
   - Hash tables for arbitrary metadata
   - Symbol properties for type information
   - BOM generation is trivial

6. **Hierarchical Assemblies**
   - Natural tree structure via subassemblies
   - Recursive parameter propagation
   - Automatic quantity calculation in BOM

### Assembly DSL Example Comparison

**CADquery:**
```python
door = (cq.Assembly()
    .add(frame, name="frame")
    .add(panel, name="panel")
    .constrain("frame@faces@>Z", "panel@faces@<Z", "Plane"))
```

**CLAD (Better!):**
```lisp
(defassembly door-assembly ()
  (:component 'frame (make-frame))
  (:component 'panel (make-panel))
  (:mate 'frame (@ref frame :face :direction :+z :extreme :max)
         'panel (@ref panel :face :direction :-z :extreme :min)))
```

**Advantages:**
- No string parsing!
- Compile-time checking
- Better error messages
- IDE support
- Extensible via CLOS

---

## Updated Implementation Roadmap

### Original Plan (Phase 8):
- Weeks 1-16: Advanced 3D features

### Extended Plan (Including Phases 9-10):

**Phase 8: Advanced 3D Operations (Weeks 1-16)** - As specified above

**Phase 9: 2D Sketching System (Weeks 17-24)**
- Week 17-18: Sketch entity system and basic constraints
- Week 19-20: Constraint solver implementation
- Week 21-22: DSL and integration with 3D operations
- Week 23-24: Testing, examples, and documentation

**Phase 10: Assembly System (Weeks 25-32)**
- Week 25-26: Assembly component system and hierarchy
- Week 27-28: Constraint types and solver
- Week 29-30: DSL and BOM generation
- Week 31-32: Testing, examples, and documentation

**Total Timeline:** 32 weeks (~8 months) for complete production-ready CAD system

---

## Conclusion: Why CLAD Will Surpass CADquery

### 1. **Better Selectors**
- Type-safe, composable, extensible
- No string parsing
- Full IDE support

### 2. **Constraint-Based Sketching**
- Proper parametric 2D sketches
- CLOS-based constraint extensibility
- Symbolic differentiation via macros

### 3. **Assembly System**
- Symbol-based references (not strings!)
- Condition system for conflict resolution
- Natural BOM generation
- Hierarchical assemblies

### 4. **Common Lisp Power**
- Macros for zero-overhead DSL
- CLOS for extensibility
- Conditions for recovery
- Interactive development

### 5. **REPL-Driven Workflow**
- C-c C-c for instant updates
- Works with sketches and assemblies
- Live constraint editing
- Interactive debugging

CLAD will be the first truly Lispy CAD system, leveraging Common Lisp's unique strengths to create something Python-based systems like CADquery cannot match!

---

**End of Extended Phase 8+ Design Specification**
**Including Phase 9 (Sketching) and Phase 10 (Assembly) Previews**
