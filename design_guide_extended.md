# CLAD Design Guide
## A Source-Code Based CAD Design System in Common Lisp

**Version:** 1.0  
**Date:** 2025-10-23  
**Status:** Design Specification

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [System Architecture](#system-architecture)
3. [Core Components](#core-components)
4. [Implementation Phases](#implementation-phases)
5. [API Design](#api-design)
6. [Extension Mechanisms](#extension-mechanisms)
7. [Development Workflow](#development-workflow)
8. [Testing Strategy](#testing-strategy)
9. [Future Roadmap](#future-roadmap)

---

## Executive Summary

### Vision

CLAD is a parametric, source-code-based CAD system built on Common Lisp and OpenCASCADE Technology (OCCT). It combines the expressiveness of Lisp with the industrial-strength geometry kernel of OCCT to create a powerful, extensible platform for mechanical design.

### Key Design Goals

1. **Ergonomic DSL**: Declarative syntax that reads like natural design intent
2. **REPL-Driven Development**: Instant feedback loop with auto-rebuilding
3. **Parametric by Default**: All designs are inherently parametric
4. **Extensible**: User-defined features and reusable components
5. **Industrial Grade**: STEP export compatible with professional CAD tools
6. **Interactive**: Web-based 3D visualization during development

### Core Principles

- **Functional Foundation**: Pure functions at the base layer enable composition and testing
- **Contextual Convenience**: Stateful contexts for ergonomics without sacrificing clarity
- **Declarative DSL**: Macros provide high-level syntax that compiles to functional core
- **Safety First**: C++ exceptions properly wrapped, memory automatically managed
- **Incremental Complexity**: Simple things simple, complex things possible

---

## System Architecture

### Layered Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Layer 5: DSL (Declarative Macros)                          │
│  - defpart macro with declarative syntax                    │
│  - Auto-rebuild integration                                 │
│  - High-level feature definitions                           │
├─────────────────────────────────────────────────────────────┤
│  Layer 4: Context API (Stateful Convenience)               │
│  - with-context for sequential operations                   │
│  - Workplane management                                     │
│  - Selection stack operations                               │
├─────────────────────────────────────────────────────────────┤
│  Layer 3: CLOS Wrapper (Object-Oriented Interface)         │
│  - shape, edge, face, solid classes                        │
│  - Generic functions for operations                         │
│  - Selector system                                          │
│  - Unit conversion                                          │
├─────────────────────────────────────────────────────────────┤
│  Layer 2: Functional Core (Pure Functions)                 │
│  - make-box, make-cylinder, etc.                           │
│  - Boolean operations (union, cut, intersect)              │
│  - Transformations (translate, rotate, mirror)             │
│  - Returns immutable values                                 │
├─────────────────────────────────────────────────────────────┤
│  Layer 1: CFFI Bindings (Foreign Interface)                │
│  - Direct OCCT function wrappers                           │
│  - Exception handling boundary                              │
│  - Memory management (Handle<> integration)                 │
├─────────────────────────────────────────────────────────────┤
│  Layer 0: OpenCASCADE Technology (C++ Kernel)              │
│  - B-rep geometry kernel                                    │
│  - Boolean operations                                       │
│  - STEP/IGES import/export                                 │
└─────────────────────────────────────────────────────────────┘
```

### Data Flow

```
User DSL Code
    ↓
  Macro Expansion (compile time)
    ↓
  CLOS Method Dispatch (runtime)
    ↓
  Functional Core Operations
    ↓
  CFFI Foreign Calls
    ↓
  OCCT C++ Kernel
    ↓
  STEP File / Mesh / Visualization
```

### Package Structure

```lisp
;; Package hierarchy
clad.ffi          ; CFFI bindings, low-level
clad.core         ; Functional core API
clad.shapes       ; CLOS shape classes
clad.selectors    ; Selection system
clad.workplane    ; Coordinate systems
clad.units        ; Unit conversion
clad.context      ; Context-based API
clad.dsl          ; High-level DSL macros
clad.auto-rebuild ; REPL integration
clad.viewer       ; Web viewer
clad.export       ; STEP/STL export
clad             ; Main user package (re-exports)
```

---

## Core Components

### 1. CFFI Bindings Layer

#### Design Philosophy

- **Exception Safety**: All OCCT calls wrapped in try-catch at C++ boundary
- **Memory Management**: Integrate with OCCT's Handle<> reference counting
- **Error Propagation**: Convert C++ exceptions to Lisp conditions

#### Critical Implementation Details

**Exception Handling Wrapper Pattern:**

```cpp
// C wrapper layer (must be compiled, not SWIG-generated for full control)
extern "C" {
    // Return codes: 0 = success, 1 = known error, -1 = unknown error
    int occt_make_box(double w, double h, double d, 
                      void** out_shape, 
                      char** error_msg) {
        try {
            BRepPrimAPI_MakeBox maker(w, h, d);
            TopoDS_Shape* shape = new TopoDS_Shape(maker.Shape());
            *out_shape = shape;
            return 0;
        } 
        catch (const Standard_Failure& e) {
            *error_msg = strdup(e.GetMessageString());
            return 1;
        } 
        catch (...) {
            *error_msg = strdup("Unknown OCCT error");
            return -1;
        }
    }
    
    void occt_release_shape(void* shape) {
        delete static_cast<TopoDS_Shape*>(shape);
    }
}
```

**Lisp CFFI Wrapper:**

```lisp
(defpackage :clad.ffi
  (:use :cl :cffi))

(in-package :clad.ffi)

;; Define error conditions
(define-condition occt-error (error)
  ((message :initarg :message :reader error-message)
   (operation :initarg :operation :reader error-operation)))

;; Safe foreign call wrapper
(defmacro with-occt-call ((result-var error-var) call &body body)
  `(with-foreign-objects ((,result-var :pointer)
                          (,error-var :pointer))
     (let ((return-code ,call))
       (case return-code
         (0 ,@body)
         (1 (error 'occt-error
                   :message (foreign-string-to-lisp 
                            (mem-ref ,error-var :pointer))
                   :operation ',call))
         (t (error 'occt-error
                   :message "Unknown OCCT error"
                   :operation ',call))))))

;; Example usage
(defun ffi-make-box (width height depth)
  "Low-level FFI call to create box"
  (with-occt-call (shape-ptr err-ptr)
      (occt-make-box (coerce width 'double-float)
                     (coerce height 'double-float)
                     (coerce depth 'double-float)
                     shape-ptr err-ptr)
    (mem-ref shape-ptr :pointer)))
```

#### Memory Management Strategy

**OCCT Handle Integration:**

```lisp
;; Track handles for finalization
(defvar *occt-handles* (make-hash-table :weakness :key)
  "Weak hash table tracking OCCT handles for debugging")

(defun register-occt-handle (lisp-object foreign-ptr)
  "Register a handle for finalization"
  (setf (gethash lisp-object *occt-handles*) foreign-ptr)
  (trivial-garbage:finalize lisp-object
    (lambda ()
      (unless (null-pointer-p foreign-ptr)
        (occt-release-shape foreign-ptr)))))

;; For debugging memory leaks
(defun count-active-handles ()
  "Count active OCCT handles (for debugging)"
  (hash-table-count *occt-handles*))
```

### 2. Functional Core Layer

#### Design Philosophy

- **Pure Functions**: No side effects, referentially transparent
- **Immutable Values**: Operations return new shapes, don't modify inputs
- **Composability**: Functions easily combined via standard Lisp composition
- **Type Safety**: Use CLOS types for dispatch and documentation

#### Core Primitives

```lisp
(defpackage :clad.core
  (:use :cl)
  (:import-from :clad.ffi)
  (:export #:make-box #:make-cylinder #:make-sphere #:make-cone
           #:union #:cut #:intersect
           #:translate #:rotate #:mirror #:scale
           #:fillet #:chamfer
           #:make-spline #:make-arc))

(in-package :clad.core)

;; Basic primitives - all return new shape objects
(defun make-box (width height depth &key (center nil))
  "Create a box primitive.
  If CENTER is true, box is centered at origin."
  (let ((shape (ffi-make-box width height depth)))
    (if center
        (translate shape 
                   (- (/ width 2)) 
                   (- (/ height 2)) 
                   (- (/ depth 2)))
        shape)))

(defun make-cylinder (radius height &key (center nil))
  "Create a cylinder primitive.
  If CENTER is true, cylinder is centered at origin."
  (let ((shape (ffi-make-cylinder radius height)))
    (if center
        (translate shape 0 0 (- (/ height 2)))
        shape)))

;; Boolean operations - pure functions
(defun union (&rest shapes)
  "Compute union of multiple shapes"
  (reduce #'ffi-boolean-union shapes))

(defun cut (base-shape &rest tool-shapes)
  "Subtract tool shapes from base shape"
  (reduce #'ffi-boolean-cut tool-shapes :initial-value base-shape))

(defun intersect (&rest shapes)
  "Compute intersection of multiple shapes"
  (reduce #'ffi-boolean-intersect shapes))

;; Transformations - return new transformed shapes
(defun translate (shape dx dy dz)
  "Translate shape by (dx, dy, dz)"
  (ffi-transform shape (make-translation-matrix dx dy dz)))

(defun rotate (shape axis angle &key (center-x 0) (center-y 0) (center-z 0))
  "Rotate shape around axis by angle (degrees)
  AXIS can be :x, :y, :z, or a vector"
  (ffi-transform shape 
                 (make-rotation-matrix axis angle 
                                       center-x center-y center-z)))
```

### 3. CLOS Shape Classes

#### Class Hierarchy

```lisp
(defpackage :clad.shapes
  (:use :cl)
  (:export #:shape #:vertex #:edge #:wire #:face #:shell #:solid #:compound
           #:shape-type #:shape-handle
           #:vertices #:edges #:wires #:faces #:shells #:solids
           #:bounding-box #:center-of-mass #:volume #:area #:length
           #:is-valid #:geom-type))

(in-package :clad.shapes)

;; Base class for all OCCT shapes
(defclass shape ()
  ((handle :initarg :handle
           :accessor shape-handle
           :type foreign-pointer
           :documentation "Foreign pointer to OCCT TopoDS_Shape")
   (shape-type :initarg :shape-type
               :accessor shape-type
               :type symbol
               :documentation "Type of shape: :vertex, :edge, :face, etc."))
  (:documentation "Base class for all OCCT geometric shapes"))

;; Specific shape classes
(defclass vertex (shape) 
  ()
  (:default-initargs :shape-type :vertex))

(defclass edge (shape) 
  ()
  (:default-initargs :shape-type :edge))

(defclass wire (shape) 
  ()
  (:default-initargs :shape-type :wire))

(defclass face (shape) 
  ()
  (:default-initargs :shape-type :face))

(defclass shell (shape) 
  ()
  (:default-initargs :shape-type :shell))

(defclass solid (shape) 
  ()
  (:default-initargs :shape-type :solid))

(defclass compound (shape) 
  ()
  (:default-initargs :shape-type :compound))

;; Initialization and finalization
(defmethod initialize-instance :after ((s shape) &key)
  "Register shape for finalization"
  (clad.ffi:register-occt-handle s (shape-handle s)))

;; Generic functions for shape queries
(defgeneric vertices (shape)
  (:documentation "Return list of vertices in shape"))

(defgeneric edges (shape)
  (:documentation "Return list of edges in shape"))

(defgeneric faces (shape)
  (:documentation "Return list of faces in shape"))

(defgeneric bounding-box (shape)
  (:documentation "Return bounding box as (min-x min-y min-z max-x max-y max-z)"))

(defgeneric center-of-mass (shape)
  (:documentation "Return center of mass as (x y z)"))

(defgeneric volume (shape)
  (:documentation "Return volume (for solids)"))

(defgeneric area (shape)
  (:documentation "Return area (for faces/shells)"))

(defgeneric geom-type (shape)
  (:documentation "Return geometry type: :line, :circle, :bspline, :plane, etc."))

;; Implementation of generic functions
(defmethod vertices ((s shape))
  (mapcar #'make-vertex-from-handle
          (ffi-get-vertices (shape-handle s))))

(defmethod edges ((s shape))
  (mapcar #'make-edge-from-handle
          (ffi-get-edges (shape-handle s))))

(defmethod faces ((s shape))
  (mapcar #'make-face-from-handle
          (ffi-get-faces (shape-handle s))))

(defmethod bounding-box ((s shape))
  (multiple-value-bind (xmin ymin zmin xmax ymax zmax)
      (ffi-get-bounding-box (shape-handle s))
    (list xmin ymin zmin xmax ymax zmax)))

(defmethod volume ((s solid))
  (ffi-get-volume (shape-handle s)))

(defmethod area ((s face))
  (ffi-get-area (shape-handle s)))

(defmethod geom-type ((e edge))
  "Return geometric type of edge"
  (ffi-get-edge-geom-type (shape-handle e)))
```

### 4. Selector System

#### Design Philosophy

- **Keyword-Based**: Clear, discoverable syntax
- **Composable**: Selectors combine with AND, OR, NOT
- **Function-Based**: Selectors are first-class functions
- **Extensible**: Users can define custom selectors

#### Selector Implementation

```lisp
(defpackage :clad.selectors
  (:use :cl)
  (:export #:select #:direction-selector #:parallel-selector
           #:perpendicular-selector #:type-selector
           #:and-selector #:or-selector #:not-selector
           #:custom-selector))

(in-package :clad.selectors)

;; Base selector protocol
(defgeneric apply-selector (selector shape-list)
  (:documentation "Apply selector to list of shapes, return filtered list"))

;; Direction selector (e.g., ">Z", "<X")
(defclass direction-selector ()
  ((axis :initarg :axis :accessor selector-axis)
   (extreme :initarg :extreme :accessor selector-extreme
            :type (member :max :min)
            :documentation ":max for >, :min for <")
   (tolerance :initarg :tolerance :initform 1e-6)))

(defmethod apply-selector ((sel direction-selector) shape-list)
  (let ((axis-vector (axis-to-vector (selector-axis sel)))
        (compare-fn (if (eq (selector-extreme sel) :max) #'> #'<)))
    ;; Find shapes with maximum/minimum projection on axis
    (let* ((projections (mapcar (lambda (shape)
                                  (cons shape 
                                        (dot-product (center-of-mass shape)
                                                    axis-vector)))
                                shape-list))
           (extreme-val (funcall (if (eq (selector-extreme sel) :max) 
                                    #'maximum #'minimum)
                                projections :key #'cdr)))
      (remove-if-not (lambda (shape)
                       (< (abs (- (cdr (assoc shape projections))
                                 extreme-val))
                          (slot-value sel 'tolerance)))
                     shape-list))))

;; Parallel selector (e.g., "|Z")
(defclass parallel-selector ()
  ((axis :initarg :axis :accessor selector-axis)
   (tolerance :initarg :tolerance :initform 1e-6)))

(defmethod apply-selector ((sel parallel-selector) shape-list)
  (let ((axis-vector (axis-to-vector (selector-axis sel))))
    (remove-if-not (lambda (shape)
                     (is-parallel-p shape axis-vector 
                                   (slot-value sel 'tolerance)))
                   shape-list)))

;; Combinator selectors
(defclass and-selector ()
  ((selectors :initarg :selectors :accessor selector-list)))

(defmethod apply-selector ((sel and-selector) shape-list)
  (reduce (lambda (shapes selector)
            (apply-selector selector shapes))
          (selector-list sel)
          :initial-value shape-list))

(defclass or-selector ()
  ((selectors :initarg :selectors :accessor selector-list)))

(defmethod apply-selector ((sel or-selector) shape-list)
  (remove-duplicates
   (apply #'append
          (mapcar (lambda (selector)
                    (apply-selector selector shape-list))
                  (selector-list sel)))))

;; High-level API
(defun select (shape-list selector-spec)
  "Apply selector specification to list of shapes.
  
  Selector specs:
    :direction :+z :extreme :max  => '>Z' top face
    :direction :-x :extreme :min  => '<X' leftmost face
    :parallel :z                  => '|Z' faces parallel to Z
    :perpendicular :x             => '#X' faces perpendicular to X
    (:and sel1 sel2 ...)          => Intersection of selectors
    (:or sel1 sel2 ...)           => Union of selectors
    (:not sel)                    => Complement of selector
    (lambda (shape) ...)          => Custom predicate
  
  Examples:
    (select (faces box) :direction :+z :extreme :max)
    (select (edges box) :parallel :z)
    (select (faces box) (:and (:parallel :z) (:not (:direction :+z :extreme :max))))"
  (let ((selector (parse-selector-spec selector-spec)))
    (apply-selector selector shape-list)))

(defun parse-selector-spec (spec)
  "Parse selector specification into selector object"
  (cond
    ;; Keyword-based direction selector
    ((and (eq (first spec) :direction)
          (eq (third spec) :extreme))
     (make-instance 'direction-selector
                    :axis (second spec)
                    :extreme (fourth spec)))
    
    ;; Parallel selector
    ((eq (first spec) :parallel)
     (make-instance 'parallel-selector :axis (second spec)))
    
    ;; Combinator selectors
    ((eq (first spec) :and)
     (make-instance 'and-selector
                    :selectors (mapcar #'parse-selector-spec (rest spec))))
    
    ((eq (first spec) :or)
     (make-instance 'or-selector
                    :selectors (mapcar #'parse-selector-spec (rest spec))))
    
    ;; Custom predicate
    ((functionp spec)
     (make-instance 'custom-selector :predicate spec))
    
    (t (error "Invalid selector specification: ~S" spec))))

;; Convenience macros for common patterns
(defmacro faces-selector (shape &rest selector-spec)
  `(select (faces ,shape) ,@selector-spec))

(defmacro edges-selector (shape &rest selector-spec)
  `(select (edges ,shape) ,@selector-spec))
```

### 5. Workplane System

#### Design Philosophy

- **Local Coordinates**: All 2D operations in plane-local coordinates
- **Transformation Stack**: Planes can be nested/transformed
- **Face-Based**: Create planes from selected faces
- **CadQuery-Compatible**: Similar conceptual model

#### Workplane Implementation

```lisp
(defpackage :clad.workplane
  (:use :cl)
  (:export #:workplane #:make-workplane #:workplane-from-face
           #:origin #:x-dir #:y-dir #:z-dir #:normal
           #:transform-to-global #:transform-to-local
           #:offset-workplane))

(in-package :clad.workplane)

(defclass workplane ()
  ((origin :initarg :origin :accessor origin
           :type (simple-array double-float (3))
           :documentation "Origin point in global coordinates")
   (x-direction :initarg :x-dir :accessor x-dir
                :type (simple-array double-float (3))
                :documentation "X-axis direction (normalized)")
   (z-direction :initarg :z-dir :accessor z-dir
                :type (simple-array double-float (3))
                :documentation "Z-axis direction (normalized, normal to plane)")
   (y-direction :accessor y-dir
                :documentation "Y-axis direction (computed from X and Z)"))
  (:documentation "Represents a local coordinate system"))

(defmethod initialize-instance :after ((wp workplane) &key)
  "Compute Y direction from X and Z via cross product"
  (setf (slot-value wp 'y-direction)
        (cross-product (z-dir wp) (x-dir wp))))

;; Standard plane constructors
(defun make-workplane (&key (plane-type :xy) 
                           origin x-dir z-dir)
  "Create a standard workplane or custom plane.
  
  Standard planes:
    :xy => XY plane (Z up)
    :xz => XZ plane (Y up)
    :yz => YZ plane (X up)
    :front => XZ plane
    :back => XZ plane (inverted)
    :left => YZ plane
    :right => YZ plane (inverted)
    :top => XY plane
    :bottom => XY plane (inverted)
  
  Custom plane:
    Specify origin, x-dir, and z-dir vectors"
  (cond
    ((eq plane-type :xy)
     (make-instance 'workplane
                    :origin (or origin #(0.0d0 0.0d0 0.0d0))
                    :x-dir #(1.0d0 0.0d0 0.0d0)
                    :z-dir #(0.0d0 0.0d0 1.0d0)))
    
    ((eq plane-type :xz)
     (make-instance 'workplane
                    :origin (or origin #(0.0d0 0.0d0 0.0d0))
                    :x-dir #(1.0d0 0.0d0 0.0d0)
                    :z-dir #(0.0d0 1.0d0 0.0d0)))
    
    ;; Custom plane
    ((and origin x-dir z-dir)
     (make-instance 'workplane
                    :origin origin
                    :x-dir (normalize x-dir)
                    :z-dir (normalize z-dir)))
    
    (t (error "Invalid workplane specification"))))

(defun workplane-from-face (face &key (origin-mode :center))
  "Create workplane from a face.
  
  ORIGIN-MODE:
    :center => Place at center of mass
    :min => Place at minimum corner of bounding box"
  (let* ((normal (face-normal face))
         (tangent (face-tangent face))
         (origin-point (ecase origin-mode
                         (:center (center-of-mass face))
                         (:min (bounding-box-min face)))))
    (make-instance 'workplane
                   :origin origin-point
                   :x-dir tangent
                   :z-dir normal)))

(defun offset-workplane (wp distance)
  "Create new workplane offset along normal by distance"
  (make-instance 'workplane
                 :origin (v+ (origin wp) 
                            (v* (z-dir wp) distance))
                 :x-dir (x-dir wp)
                 :z-dir (z-dir wp)))

;; Coordinate transformations
(defun transform-to-global (wp local-point)
  "Transform point from workplane local coords to global coords"
  (v+ (origin wp)
      (v+ (v* (x-dir wp) (aref local-point 0))
          (v+ (v* (y-dir wp) (aref local-point 1))
              (v* (z-dir wp) (aref local-point 2))))))

(defun transform-to-local (wp global-point)
  "Transform point from global coords to workplane local coords"
  (let ((relative (v- global-point (origin wp))))
    (make-array 3 :initial-contents
                (list (dot-product relative (x-dir wp))
                      (dot-product relative (y-dir wp))
                      (dot-product relative (z-dir wp))))))
```

### 6. Units System

#### Design Philosophy

- **Compile-Time Conversion**: Zero runtime overhead for known units
- **Dynamic Context**: `with-units` for temporary unit changes
- **OCCT Native**: All values stored in mm (OCCT default)
- **Type Safety**: Unit-aware dimension type (optional)

#### Units Implementation

```lisp
(defpackage :clad.units
  (:use :cl)
  (:export #:*default-units* #:with-units #:dim
           #:convert-units #:define-unit))

(in-package :clad.units)

(defvar *default-units* :mm
  "Default units for dimensional values")

;; Unit conversion table (all relative to mm)
(defvar *unit-conversions*
  '((:mm . 1.0)
    (:cm . 10.0)
    (:m . 1000.0)
    (:in . 25.4)
    (:ft . 304.8)
    (:thou . 0.0254)  ; thousandths of an inch
    (:mil . 0.0254))
  "Conversion factors from each unit to mm")

(defun convert-units (value from-unit to-unit)
  "Convert VALUE from FROM-UNIT to TO-UNIT"
  (let ((from-factor (or (cdr (assoc from-unit *unit-conversions*))
                        (error "Unknown unit: ~A" from-unit)))
        (to-factor (or (cdr (assoc to-unit *unit-conversions*))
                      (error "Unknown unit: ~A" to-unit))))
    (* value (/ from-factor to-factor))))

;; Compile-time unit conversion macro
(defmacro dim (value &optional (unit nil unit-provided-p))
  "Dimensional value with optional unit.
  Converts to OCCT native units (mm) at compile time.
  
  Usage:
    (dim 10)         => 10 (uses *default-units*)
    (dim 10 :in)     => 254.0 (converts inches to mm)
    (with-units (:in)
      (dim 10))      => 254.0 (uses dynamic context)"
  (let ((source-unit (if unit-provided-p unit '*default-units*)))
    (if (keywordp source-unit)
        ;; Compile-time conversion
        `(coerce ,(convert-units value source-unit :mm) 'double-float)
        ;; Runtime conversion
        `(coerce (convert-units ,value ,source-unit :mm) 'double-float))))

(defmacro with-units (unit &body body)
  "Execute BODY with *default-units* bound to UNIT"
  `(let ((*default-units* ,unit))
     ,@body))

;; Define custom units
(defun define-unit (name factor-to-mm)
  "Define a custom unit with conversion factor to mm"
  (push (cons name factor-to-mm) *unit-conversions*))

;; Example custom units
(define-unit :yard 914.4)
(define-unit :mile 1609344.0)
```

### 7. Context API Layer

#### Design Philosophy

- **Sequential Operations**: Natural flow for building complex parts
- **Stack-Based Selection**: Maintain selection context
- **Workplane Management**: Track current working plane
- **Convenience**: Hide boilerplate without magic

#### Context Implementation

```lisp
(defpackage :clad.context
  (:use :cl)
  (:export #:with-context #:add-solid #:select-faces #:select-edges
           #:current-workplane #:set-workplane #:get-result
           #:cut-feature #:union-feature #:transform-feature))

(in-package :clad.context)

(defclass modeling-context ()
  ((solid :initform nil :accessor context-solid
          :documentation "Current solid being built")
   (selection-stack :initform nil :accessor selection-stack
                    :documentation "Stack of selected sub-shapes")
   (workplane :initarg :workplane :accessor current-workplane
              :documentation "Current working plane")
   (history :initform nil :accessor operation-history
            :documentation "History of operations (for parametric updates)"))
  (:documentation "Context for sequential modeling operations"))

(defmacro with-context ((ctx &key (plane :xy)) &body operations)
  "Execute modeling operations in a context.
  
  Usage:
    (with-context (ctx :plane :xy)
      (add-solid ctx (make-box 10 10 10))
      (select-faces ctx :direction :+z :extreme :max)
      (cut-feature ctx (make-cylinder 3 12))
      (get-result ctx))"
  `(let ((,ctx (make-instance 'modeling-context
                              :workplane (make-workplane :plane-type ,plane))))
     ,@operations))

(defun add-solid (ctx solid)
  "Add/union a solid to the context"
  (if (context-solid ctx)
      (setf (context-solid ctx) 
            (clad.core:union (context-solid ctx) solid))
      (setf (context-solid ctx) solid))
  (push (list :add-solid solid) (operation-history ctx))
  ctx)

(defun select-faces (ctx &rest selector-spec)
  "Select faces and push to selection stack"
  (let* ((current (or (car (selection-stack ctx))
                     (context-solid ctx)))
         (selected (apply #'clad.selectors:select 
                         (faces current)
                         selector-spec)))
    (push selected (selection-stack ctx))
    (push (list :select-faces selector-spec) (operation-history ctx))
    ctx))

(defun select-edges (ctx &rest selector-spec)
  "Select edges and push to selection stack"
  (let* ((current (or (car (selection-stack ctx))
                     (context-solid ctx)))
         (selected (apply #'clad.selectors:select 
                         (edges current)
                         selector-spec)))
    (push selected (selection-stack ctx))
    (push (list :select-edges selector-spec) (operation-history ctx))
    ctx))

(defun cut-feature (ctx tool-solid)
  "Cut tool solid from context solid"
  (setf (context-solid ctx)
        (clad.core:cut (context-solid ctx) tool-solid))
  (push (list :cut-feature tool-solid) (operation-history ctx))
  ctx)

(defun union-feature (ctx add-solid)
  "Union solid with context solid"
  (setf (context-solid ctx)
        (clad.core:union (context-solid ctx) add-solid))
  (push (list :union-feature add-solid) (operation-history ctx))
  ctx)

(defun set-workplane (ctx workplane)
  "Set current working plane"
  (setf (current-workplane ctx) workplane)
  ctx)

(defun get-result (ctx)
  "Get the final solid from context"
  (context-solid ctx))
```

### 8. DSL Layer

#### Design Philosophy

- **Declarative**: Describe WHAT, not HOW
- **Hierarchical**: Natural nesting of features
- **Readable**: Code reads like design intent
- **Compiled**: Expands to efficient functional code
- **Auto-Rebuild**: Integrates with REPL workflow

#### DSL Implementation

```lisp
(defpackage :clad.dsl
  (:use :cl)
  (:export #:defpart #:part #:body #:feature 
           #:on-face #:on-edge #:pattern
           #:dim #:with-units))

(in-package :clad.dsl)

(defmacro defpart (name params &body body)
  "Define a parametric part with auto-rebuild support.
  
  Syntax:
    (defpart name (param1 param2 ...) [docstring]
      (:body form)
      (:on-face selector
        (:feature operation form))
      (:pattern pattern-spec
        (:feature operation form))
      ...)
  
  Example:
    (defpart bracket ((width 50) (height 30) (thickness 10))
      \"A simple mounting bracket\"
      (:body
        (box :width width :height height :depth thickness))
      (:on-face :direction :+z :extreme :max
        (:feature :cut
          (cylinder :diameter 6 :depth 12)))
      (:all-edges
        (:filter (lambda (e) (< (edge-length e) 40)))
        (fillet 2)))"
  
  (let* ((docstring (when (stringp (car body)) (car body)))
         (real-body (if docstring (cdr body) body))
         (fn-name (if (listp name) (car name) name)))
    
    `(progn
       ;; Define the actual function
       (defun ,name ,params
         ,@(when docstring (list docstring))
         (expand-part-body ,@real-body))
       
       ;; Mark as a part for auto-rebuild
       (setf (get ',fn-name 'part-function) t)
       
       ;; Trigger auto-rebuild if this is the active part
       (when (and clad.auto-rebuild:*auto-rebuild*
                  (eq clad.auto-rebuild:*current-part* ',fn-name))
         (clad.auto-rebuild:rebuild))
       
       ',fn-name)))

(defun expand-part-body (&rest body-forms)
  "Expand declarative part body into functional code"
  (let ((ctx (gensym "CTX")))
    `(with-context (,ctx)
       ,@(mapcar (lambda (form) (expand-part-form ctx form))
                body-forms)
       (get-result ,ctx))))

(defun expand-part-form (ctx form)
  "Expand a single part form"
  (case (car form)
    (:body
     `(add-solid ,ctx ,(expand-body-form (cadr form))))
    
    (:on-face
     (destructuring-bind (selector &rest operations) (cdr form)
       `(progn
          (select-faces ,ctx ,@(parse-selector selector))
          ,@(mapcar (lambda (op) (expand-operation ctx op))
                   operations))))
    
    (:on-edge
     (destructuring-bind (selector &rest operations) (cdr form)
       `(progn
          (select-edges ,ctx ,@(parse-selector selector))
          ,@(mapcar (lambda (op) (expand-operation ctx op))
                   operations))))
    
    (:pattern
     (destructuring-bind (pattern-type &rest pattern-spec) (cdr form)
       (expand-pattern ctx pattern-type pattern-spec)))
    
    (:all-edges
     `(progn
        (select-edges ,ctx :all)
        ,@(mapcar (lambda (op) (expand-operation ctx op))
                 (cdr form))))
    
    (t (error "Unknown part form: ~S" form))))

(defun expand-body-form (form)
  "Expand body form into shape creation"
  (case (car form)
    (box `(make-box :width ,(getf (cdr form) :width)
                    :height ,(getf (cdr form) :height)
                    :depth ,(getf (cdr form) :depth)
                    ,@(when (getf (cdr form) :center)
                        (list :center t))))
    
    (cylinder `(make-cylinder :radius ,(/ (getf (cdr form) :diameter) 2)
                              :height ,(getf (cdr form) :depth)
                              ,@(when (getf (cdr form) :center)
                                  (list :center t))))
    
    (sphere `(make-sphere :radius ,(getf (cdr form) :radius)))
    
    (t form)))

(defun expand-operation (ctx operation-form)
  "Expand an operation (feature, fillet, etc.)"
  (case (caar operation-form)
    (:feature
     (let ((op-type (cadr (car operation-form)))
           (shape-form (cadr operation-form)))
       (case op-type
         (:cut `(cut-feature ,ctx ,(expand-body-form shape-form)))
         (:add `(union-feature ,ctx ,(expand-body-form shape-form)))
         (t (error "Unknown feature type: ~S" op-type)))))
    
    (fillet
     `(fillet-selected ,ctx ,(cadr operation-form)))
    
    (chamfer
     `(chamfer-selected ,ctx ,(cadr operation-form)))
    
    (:filter
     ;; Filter current selection
     `(filter-selection ,ctx ,(cadr operation-form)))
    
    (t (error "Unknown operation: ~S" operation-form))))

(defun expand-pattern (ctx pattern-type pattern-spec)
  "Expand pattern operations"
  (case pattern-type
    (:circular
     (destructuring-bind (&key count radius angle-start angle-end 
                               &allow-other-keys)
         pattern-spec
       `(circular-pattern ,ctx 
                         :count ,count 
                         :radius ,radius
                         ,@(when angle-start `(:angle-start ,angle-start))
                         ,@(when angle-end `(:angle-end ,angle-end))
                         :feature ,(expand-operation ctx (car pattern-spec)))))
    
    (:linear
     (destructuring-bind (&key count direction spacing &allow-other-keys)
         pattern-spec
       `(linear-pattern ,ctx
                       :count ,count
                       :direction ,direction
                       :spacing ,spacing
                       :feature ,(expand-operation ctx (car pattern-spec)))))
    
    (t (error "Unknown pattern type: ~S" pattern-type))))
```

### 9. Auto-Rebuild System

#### Complete Implementation

```lisp
(defpackage :clad.auto-rebuild
  (:use :cl)
  (:export #:*auto-rebuild* #:*current-part* #:*viewer-connection*
           #:show #:rebuild #:watch #:toggle-auto-rebuild
           #:update-viewer))

(in-package :clad.auto-rebuild)

(defvar *auto-rebuild* t
  "Whether to automatically rebuild on part redefinition")

(defvar *current-part* nil
  "Current part function or symbol being displayed")

(defvar *viewer-connection* nil
  "WebSocket or HTTP connection to viewer")

(defvar *file-watchers* (make-hash-table :test 'equal)
  "Active file watchers")

(defun show (part-or-fn &key (make-current t))
  "Display a part. If MAKE-CURRENT, set as auto-rebuild target.
  
  Usage:
    (show 'my-part)           ; Show part, enable auto-rebuild
    (show #'my-part)          ; Show using function
    (show (my-part 20 30))    ; Show with parameters (no auto-rebuild)"
  (let ((part (etypecase part-or-fn
                (function (funcall part-or-fn))
                (symbol (funcall (symbol-function part-or-fn)))
                (clad.shapes:shape part-or-fn))))
    
    (when make-current
      (setf *current-part* 
            (etypecase part-or-fn
              (function part-or-fn)
              (symbol part-or-fn)
              (t (lambda () part)))))
    
    (update-viewer part)
    
    (format t "~&╔════════════════════════════════════════╗~%")
    (format t "~&║ Part displayed successfully           ║~%")
    (format t "~&║ Auto-rebuild: ~A~23@A║~%"
            (if make-current "ENABLED " "disabled")
            "")
    (when make-current
      (format t "~&║ Current part: ~A~15@A║~%"
              (if (symbolp part-or-fn)
                  part-or-fn
                  "<function>")
              ""))
    (format t "~&╚════════════════════════════════════════╝~%")
    
    part))

(defun rebuild ()
  "Rebuild and display the current part"
  (unless *current-part*
    (warn "No current part set. Use (show 'part-name) first.")
    (return-from rebuild))
  
  (format t "~&[Rebuilding...")
  (finish-output)
  
  (let* ((start-time (get-internal-real-time))
         (part (handler-case
                   (etypecase *current-part*
                     (function (funcall *current-part*))
                     (symbol (funcall (symbol-function *current-part*))))
                 (error (e)
                   (format t " FAILED]~%")
                   (format t "~&Error: ~A~%" e)
                   (return-from rebuild nil))))
         (elapsed (/ (- (get-internal-real-time) start-time)
                    internal-time-units-per-second)))
    
    (update-viewer part)
    (format t " done in ~,3Fs]~%" elapsed)
    part))

(defun update-viewer (part)
  "Send part to viewer for display"
  (cond
    ;; WebSocket viewer (future)
    (*viewer-connection*
     (send-to-viewer *viewer-connection* part))
    
    ;; HTTP-based viewer (simple)
    ((find-package :hunchentoot)
     (export-for-web-viewer part))
    
    ;; Fallback: Export STEP file
    (t
     (let ((filename "/tmp/clad-preview.step"))
       (clad.export:export-step part filename)
       (format t "~&Exported to ~A~%" filename)))))

(defun watch (filepath part-symbol &key (interval 0.5))
  "Watch a file and auto-rebuild when it changes.
  
  Usage:
    (watch \"~/projects/bracket.lisp\" 'bracket)
  
  Returns: watcher thread"
  (let ((watcher 
         (bt:make-thread
          (lambda ()
            (let ((last-modified (file-write-date filepath)))
              (loop
                (sleep interval)
                (let ((current (file-write-date filepath)))
                  (when (and current (> current last-modified))
                    (setf last-modified current)
                    (format t "~&[File changed: ~A]~%" filepath)
                    (handler-case
                        (progn
                          (load filepath)
                          (when (eq *current-part* part-symbol)
                            (rebuild)))
                      (error (e)
                        (format t "~&Error loading file: ~A~%" e))))))))
          :name (format nil "watcher:~A" filepath))))
    
    (setf (gethash filepath *file-watchers*) watcher)
    (format t "~&Watching ~A for changes~%" filepath)
    watcher))

(defun stop-watching (filepath)
  "Stop watching a file"
  (let ((watcher (gethash filepath *file-watchers*)))
    (when watcher
      (bt:destroy-thread watcher)
      (remhash filepath *file-watchers*)
      (format t "~&Stopped watching ~A~%" filepath))))

(defun toggle-auto-rebuild ()
  "Toggle automatic rebuilding on/off"
  (setf *auto-rebuild* (not *auto-rebuild*))
  (format t "~&Auto-rebuild: ~A~%" 
          (if *auto-rebuild* "ENABLED" "disabled")))

;; Integration with SLIME/Sly (optional)
#+swank
(defun slime-compile-hook ()
  "Hook for SLIME compilation events"
  (when *auto-rebuild*
    (rebuild)))

#+swank
(push 'slime-compile-hook swank:*after-compilation-functions*)
```

---

## Implementation Phases

### Phase 1: Foundation (Weeks 1-3)

**Goal**: Basic OCCT integration working, can create and export simple primitives

**Tasks**:
1. Fork and update awolven/oc to OCCT 7.8+
2. Implement exception-safe C wrapper layer
3. Create CFFI bindings with proper error handling
4. Implement memory management with finalizers
5. Basic functional core: primitives, booleans, transforms
6. STEP export working
7. Write comprehensive tests

**Deliverable**: Can create box, cylinder, sphere, do boolean ops, export STEP

**Test**:
```lisp
(let ((part (clad.core:cut
              (clad.core:make-box 100 100 100)
              (clad.core:make-cylinder 25 120 :center t))))
  (clad.export:export-step part "test.step"))
;; Open in FreeCAD - should see box with cylinder hole
```

### Phase 2: Object System (Weeks 4-5)

**Goal**: CLOS wrapper with shape hierarchy and basic queries

**Tasks**:
1. Implement shape class hierarchy
2. Generic functions for queries (vertices, edges, faces)
3. Geometric queries (bounding-box, center-of-mass, volume)
4. Shape validation and type checking
5. Property access methods

**Deliverable**: Object-oriented interface to shapes

**Test**:
```lisp
(let ((box (make-box 100 50 30)))
  (print (bounding-box box))
  (print (volume box))
  (print (length (faces box)))
  (print (length (edges box))))
```

### Phase 3: Selectors (Week 6)

**Goal**: Powerful selector system for finding faces/edges

**Tasks**:
1. Implement base selector protocol
2. Direction selectors (>Z, <X, etc.)
3. Parallel/perpendicular selectors
4. Combinator selectors (AND, OR, NOT)
5. Custom predicate selectors
6. Extensive selector tests

**Deliverable**: Can select specific faces/edges easily

**Test**:
```lisp
(let* ((box (make-box 100 100 100))
       (top-face (car (select (faces box) 
                             :direction :+z :extreme :max)))
       (vertical-edges (select (edges box)
                              :parallel :z)))
  ...)
```

### Phase 4: Workplanes & Context (Weeks 7-8)

**Goal**: Local coordinate systems and context API

**Tasks**:
1. Implement workplane class and transformations
2. Standard plane constructors
3. Face-to-workplane conversion
4. Context API with selection stack
5. Workplane-based 2D sketching (basic)

**Deliverable**: Can work in local coordinates naturally

**Test**:
```lisp
(with-context (ctx :plane :xy)
  (add-solid ctx (make-box 100 100 20))
  (select-faces ctx :direction :+z :extreme :max)
  (set-workplane ctx (workplane-from-selected ctx))
  (cut-feature ctx (make-cylinder 10 25 :center t)))
```

### Phase 5: Units & DSL (Weeks 9-10)

**Goal**: Units system and declarative DSL

**Tasks**:
1. Implement unit conversion system
2. Compile-time unit conversion
3. `with-units` macro
4. `defpart` macro with declarative syntax
5. Pattern operations (circular, linear)
6. Feature nesting and composition

**Deliverable**: Beautiful declarative syntax

**Test**:
```lisp
(defpart bracket ((width 50) (mount-dia 6))
  "Mounting bracket"
  (:body
    (box :width (dim width :mm) 
         :height (dim 30 :mm) 
         :depth (dim 10 :mm)))
  (:on-face :direction :+z :extreme :max
    (:pattern :circular :count 4 :radius (dim 20 :mm)
      (:feature :cut
        (cylinder :diameter (dim mount-dia :mm) 
                  :depth (dim 15 :mm))))))
```

### Phase 6: Auto-Rebuild (Week 11)

**Goal**: REPL-driven development with instant feedback

**Tasks**:
1. Implement `show` and `rebuild` functions
2. Function redefinition hooks
3. File watching capability
4. SLIME/Sly integration
5. Status display and error recovery

**Deliverable**: Seamless REPL workflow

**Test**:
```lisp
CL-USER> (defpart test-part () (box 10 10 10))
CL-USER> (show 'test-part)
CL-USER> (defpart test-part () (box 20 20 10))
;; Auto-rebuilds and updates!
```

### Phase 7: Visualization (Weeks 12-13)

**Goal**: Web-based 3D viewer

**Tasks**:
1. Implement tessellation (BREP to mesh)
2. JSON export for Three.js
3. Simple Hunchentoot web server
4. Three.js client with basic viewer
5. WebSocket for live updates (optional)

**Deliverable**: View parts in browser during development

### Phase 8: Advanced Features (Weeks 14-16)

**Goal**: Production-ready features

**Tasks**:
1. Fillets and chamfers
2. Splines and complex curves
3. Lofting and sweeping
4. Shelling
5. Advanced patterns
6. Mirroring and arrays
7. Extensive example library

**Deliverable**: Can build complex real-world parts

---

## API Design

### Naming Conventions

- **Predicates**: End in `-p` (e.g., `valid-p`, `parallel-p`)
- **Destructive ops**: End in `!` (avoid when possible)
- **Constructors**: Start with `make-` (e.g., `make-box`)
- **Type conversions**: Format `from-to` (e.g., `shape-to-mesh`)
- **Classes**: Lowercase with dashes (e.g., `solid`, `workplane`)

### API Layers Summary

**Layer 1 (FFI)**: Prefix with `ffi-` or `%`
```lisp
clad.ffi:ffi-make-box
clad.ffi:%boolean-union
```

**Layer 2 (Functional)**: Clean function names
```lisp
clad.core:make-box
clad.core:union
clad.core:translate
```

**Layer 3 (CLOS)**: Generic functions and methods
```lisp
(faces shape)
(volume solid)
(bounding-box shape)
```

**Layer 4 (Context)**: Context-aware operations
```lisp
(with-context (ctx) ...)
(add-solid ctx ...)
```

**Layer 5 (DSL)**: Declarative macros
```lisp
(defpart name params ...)
(:body ...)
(:on-face ...)
```

---

## Extension Mechanisms

### 1. User-Defined Features

```lisp
(deffeature mounting-hole ((diameter 6) (depth 10) (counterbore-dia 12))
  "Standard mounting hole with counterbore"
  (list
    ;; Main hole
    (cylinder :diameter diameter :depth depth)
    ;; Counterbore
    (cylinder :diameter counterbore-dia :depth (/ depth 2))))

;; Use in parts
(defpart bracket ()
  (:body (box 100 50 10))
  (:on-face ">Z"
    (:feature :cut (mounting-hole :diameter 6 :depth 12))))
```

### 2. Custom Selectors

```lisp
(defun hole-selector (min-diameter max-diameter)
  "Select cylindrical holes within diameter range"
  (lambda (face)
    (and (eq (geom-type face) :cylinder)
         (<= min-diameter (face-diameter face) max-diameter))))

;; Use
(select (faces part) (hole-selector 5 10))
```

### 3. Custom Operations

```lisp
(defgeneric custom-operation (shape &key options))

(defmethod custom-operation ((s solid) &key options)
  ;; Your implementation
  ...)
```

### 4. Plugin System (Future)

```lisp
(define-cad-plugin :my-plugin
  :requires (:clad.core :clad.dsl)
  :exports (:my-feature :my-selector))
```

---

## Development Workflow

### Typical Session

```lisp
;; 1. Start REPL
$ sbcl
* (ql:quickload :clad)

;; 2. Define part
* (defpart gear-housing ((shaft-dia 30))
    (:body (box 100 100 20))
    (:on-face ">Z"
      (:feature :cut (cylinder :d shaft-dia :h 25))))

;; 3. Show it
* (show 'gear-housing)
;; Browser opens with 3D view

;; 4. Iterate
* (defpart gear-housing ((shaft-dia 30))
    (:body (box 120 120 20))  ; Made it bigger
    (:on-face ">Z"
      (:feature :cut (cylinder :d shaft-dia :h 25))))
;; Auto-rebuilds in browser!

;; 5. Export when ready
* (export-step (gear-housing 30) "gear-housing.step")
```

### File-Based Workflow

```lisp
;; In gear-housing.lisp
(in-package :clad-user)

(defpart gear-housing ((shaft-dia 30) (bolt-dia 6))
  "Housing for 30mm shaft with mounting holes"
  (:body
    (box :width 100 :height 100 :depth 20))
  
  (:on-face :direction :+z :extreme :max
    (:feature :cut
      (cylinder :diameter shaft-dia :depth 25)))
  
  (:on-face :direction :+z :extreme :max
    (:pattern :circular :count 4 :radius 40
      (:feature :cut
        (cylinder :diameter bolt-dia :depth 15)))))
```

```lisp
;; In REPL
* (watch "~/projects/gear-housing.lisp" 'gear-housing)
* (show 'gear-housing)
;; Now edit file in your editor, save, and it auto-rebuilds!
```

---

## Testing Strategy

### Unit Tests

Test each layer independently:

```lisp
;; tests/ffi-tests.lisp
(deftest test-box-creation ()
  (let ((box (ffi-make-box 10.0d0 20.0d0 30.0d0)))
    (is (not (null-pointer-p box)))
    (ffi-release-shape box)))

;; tests/core-tests.lisp
(deftest test-boolean-union ()
  (let* ((box1 (make-box 10 10 10))
         (box2 (translate (make-box 10 10 10) 5 0 0))
         (result (union box1 box2)))
    (is (> (volume result) (volume box1)))
    (is (< (volume result) (* 2 (volume box1))))))

;; tests/selector-tests.lisp
(deftest test-direction-selector ()
  (let* ((box (make-box 10 10 10))
         (top-face (car (select (faces box) 
                               :direction :+z :extreme :max))))
    (is (not (null top-face)))
    (is (> (aref (center-of-mass top-face) 2) 9))))
```

### Integration Tests

Test complete workflows:

```lisp
(deftest test-full-part-workflow ()
  (let ((part (with-context (ctx)
                (add-solid ctx (make-box 100 100 20))
                (select-faces ctx :direction :+z :extreme :max)
                (cut-feature ctx (make-cylinder 10 25))
                (get-result ctx))))
    (is (typep part 'solid))
    (is (< (volume part) (* 100 100 20)))
    (export-step part "/tmp/test.step")
    (is (probe-file "/tmp/test.step"))))
```

### Visual Tests

For complex parts, visual verification:

```lisp
(deftest test-gear-housing-visual ()
  "Visual test - check exported STEP file manually"
  (let ((part (gear-housing 30)))
    (export-step part "/tmp/gear-housing-test.step")
    (format t "~%Check /tmp/gear-housing-test.step in FreeCAD~%")))
```

---

## Future Roadmap

### Phase 9: Assembly Support

- **Multi-part models**: Multiple solids in one model
- **Constraints**: Mate, align, tangent relationships
- **Solver**: Constraint satisfaction
- **Exploded views**: Assembly animations
- **BOM generation**: Parts list with metadata

### Phase 10: Sketching

- **2D sketch mode**: Full 2D constraint solver
- **Sketch primitives**: Lines, arcs, splines, circles
- **Constraints**: Horizontal, vertical, tangent, parallel, etc.
- **Dimensions**: Driven dimensions with solver
- **Sketch-based features**: Extrude, revolve from sketch

### Phase 11: Advanced Surfacing

- **NURBS surfaces**: Direct surface creation
- **Surface continuity**: G1, G2, G3 continuity
- **Surface trimming**: Trim surfaces with curves
- **Surface filleting**: Complex fillet surfaces
- **Patch surfaces**: Fill holes with surfaces

### Phase 12: Analysis Integration

- **FEA integration**: Export to CalculiX, Code_Aster
- **CFD preparation**: Fluid domains, boundary conditions
- **Mass properties**: Detailed inertia calculations
- **Interference checking**: Clash detection
- **Tolerance analysis**: Stack-up analysis

### Phase 13: Generative Design

- **Topology optimization**: Lightweight structure generation
- **Parametric optimization**: Multi-objective optimization
- **Genetic algorithms**: Evolutionary design
- **Shape grammars**: Rule-based generation
- **AI integration**: ML-based design suggestions

### Phase 14: Collaboration

- **Version control**: Git-friendly format
- **Diff visualization**: Visual diffs of geometry
- **Merge strategies**: Handling conflicts
- **Comments**: Inline design comments
- **Review workflow**: PR-style design reviews

---

## Appendix A: File Structure

```
clad/
├── README.md
├── LICENSE
├── clad.asd                    # ASDF system definition
├── docs/
│   ├── getting-started.md
│   ├── api-reference.md
│   ├── tutorial.md
│   └── examples/
├── src/
│   ├── packages.lisp               # Package definitions
│   ├── ffi/
│   │   ├── bindings.lisp          # CFFI bindings
│   │   ├── error-handling.lisp    # Exception wrapping
│   │   └── memory.lisp            # Memory management
│   ├── core/
│   │   ├── primitives.lisp        # Basic shapes
│   │   ├── booleans.lisp          # Boolean operations
│   │   ├── transforms.lisp        # Transformations
│   │   └── queries.lisp           # Shape queries
│   ├── shapes/
│   │   ├── classes.lisp           # CLOS shape hierarchy
│   │   ├── methods.lisp           # Generic functions
│   │   └── properties.lisp        # Property queries
│   ├── selectors/
│   │   ├── base.lisp              # Selector protocol
│   │   ├── direction.lisp         # Direction selectors
│   │   ├── combinators.lisp       # AND/OR/NOT
│   │   └── custom.lisp            # User selectors
│   ├── workplane/
│   │   ├── workplane.lisp         # Workplane class
│   │   └── transforms.lisp        # Coordinate transforms
│   ├── units/
│   │   └── units.lisp             # Unit conversion
│   ├── context/
│   │   └── context.lisp           # Context API
│   ├── dsl/
│   │   ├── macros.lisp            # DSL macros
│   │   └── expanders.lisp         # Form expansion
│   ├── auto-rebuild/
│   │   ├── rebuild.lisp           # Auto-rebuild system
│   │   └── watchers.lisp          # File watchers
│   ├── viewer/
│   │   ├── server.lisp            # Web server
│   │   ├── tessellation.lisp     # Mesh generation
│   │   └── static/                # HTML/JS/CSS
│   └── export/
│       ├── step.lisp              # STEP export
│       ├── stl.lisp               # STL export
│       └── formats.lisp           # Other formats
├── tests/
│   ├── ffi-tests.lisp
│   ├── core-tests.lisp
│   ├── shapes-tests.lisp
│   ├── selector-tests.lisp
│   └── integration-tests.lisp
├── examples/
│   ├── basic-primitives.lisp
│   ├── boolean-operations.lisp
│   ├── parametric-gear.lisp
│   ├── mounting-bracket.lisp
│   └── assembly-example.lisp
└── c-wrapper/
    ├── CMakeLists.txt
    ├── occt-wrapper.h
    ├── occt-wrapper.cpp           # Exception-safe wrappers
    └── build.sh
```

---

## Appendix B: Key Design Decisions Summary

### Memory Management
- **Decision**: Use OCCT's Handle<> with Lisp finalizers
- **Rationale**: Automatic, safe, no manual cleanup needed
- **Trade-off**: Weak pointer tracking adds slight overhead

### Exception Handling
- **Decision**: C wrapper layer with try-catch, return error codes
- **Rationale**: C++ exceptions cannot safely cross FFI boundary
- **Trade-off**: Extra C wrapper layer to maintain

### API Style
- **Decision**: Functional core + Context convenience + Declarative DSL
- **Rationale**: Maximum flexibility and expressiveness
- **Trade-off**: Three ways to do things (but clear layering)

### Selector Syntax
- **Decision**: Keyword-based instead of string-based
- **Rationale**: More Lispy, better IDE support, type-safe
- **Trade-off**: Slightly more verbose than ">Z" strings

### Units System
- **Decision**: Compile-time conversion with dynamic context
- **Rationale**: Zero runtime overhead for known units
- **Trade-off**: Runtime conversion for dynamic units

### Auto-Rebuild
- **Decision**: Function redefinition hooks + file watching
- **Rationale**: REPL-native, feels natural in Lisp
- **Trade-off**: Some magic in defpart macro

### Visualization
- **Decision**: Web-based Three.js viewer
- **Rationale**: Cross-platform, no installation, fast enough
- **Trade-off**: Requires tessellation, network latency

---

## Appendix C: Critical Implementation Notes

### OCCT Version Compatibility
- Target OCCT 7.8+
- Key modules: BRepPrimAPI, BRepAlgoAPI, BRepFilletAPI
- STEP export via STEPControl_Writer
- Mesh generation via BRepMesh_IncrementalMesh

### Lisp Implementation Support
- Primary: SBCL (best CFFI support)
- Secondary: CCL, ECL
- Windows: Requires OCCT binaries or manual build

### Performance Considerations
- OCCT operations are typically fast (<100ms for simple parts)
- Tessellation can be slow for complex parts (tune parameters)
- Selector operations scale with face/edge count
- Consider caching for repeated operations

### Thread Safety
- OCCT Handle<> is thread-safe (atomic ref counting)
- Shape operations are NOT thread-safe
- Serialize access to same shape from multiple threads
- Context objects are single-threaded

### Common Pitfalls
1. **Null pointers**: Always check after FFI calls
2. **Circular references**: Break cycles manually if needed
3. **Tolerance issues**: OCCT uses tolerances (default 1e-7)
4. **Invalid geometry**: Always validate shapes after boolean ops
5. **Memory leaks**: Ensure finalizers are registered

---

## Conclusion

This design provides a solid foundation for a production-quality, source-code-based CAD system in Common Lisp. The layered architecture ensures:

- **Maintainability**: Clear separation of concerns
- **Extensibility**: Multiple extension points
- **Safety**: Proper error handling and memory management
- **Usability**: Ergonomic DSL and REPL integration
- **Performance**: Direct OCCT integration, minimal overhead

The phased implementation approach allows for incremental development with working prototypes at each stage. Each phase builds on the previous, with clear deliverables and tests.

The system is designed for mechanical engineers and programmers who want the power of a real CAD kernel with the flexibility of a programming language, combining the best of both worlds.

---

**Next Steps**: Begin Phase 1 by forking awolven/oc and implementing the exception-safe C wrapper layer.
