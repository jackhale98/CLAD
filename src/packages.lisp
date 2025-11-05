;;;; packages.lisp --- Package definitions for CLAD

(in-package :cl-user)

;;; ============================================================================
;;; Layer 1: FFI Bindings
;;; ============================================================================

(defpackage #:clad.ffi
  (:use #:cl #:cffi)
  (:documentation "Foreign function interface to OpenCASCADE Technology")
  (:export
   ;; Exception handling
   #:with-occt-error-handling
   #:occt-error
   #:occt-error-message
   #:occt-domain-error
   #:occt-construction-error
   #:occt-null-object-error
   #:error-code-to-condition
   #:check-occt-result
   #:*occt-available-p*
   #:load-occt-libraries
   ;; Error codes
   #:+occt-success+
   #:+occt-error-unknown+
   #:+occt-error-domain+
   #:+occt-error-construction+
   #:+occt-error-null-object+

   ;; Memory management
   #:occt-handle
   #:make-occt-handle
   #:handle-valid-p
   #:handle-ptr
   #:handle-type
   #:handle-null-p
   #:copy-occt-handle
   #:ensure-valid-handle
   #:occt-handle-ref-count
   #:clear-handle-cache

   ;; Primitive constructors
   #:ffi-make-box
   #:ffi-make-cylinder
   #:ffi-make-sphere
   #:ffi-make-cone

   ;; Boolean operations
   #:ffi-union
   #:ffi-cut
   #:ffi-intersect
   #:ffi-fuse

   ;; Transformations
   #:ffi-translate
   #:ffi-rotate
   #:ffi-mirror
   #:ffi-scale

   ;; Export
   #:ffi-export-step
   #:ffi-export-stl
   #:ffi-export-gltf

   ;; Fillet operations (Phase 8)
   #:ffi-fillet
   #:ffi-chamfer

   ;; Curve operations (Phase 8)
   #:ffi-make-interpolated-curve
   #:ffi-make-bezier-curve
   #:ffi-make-arc-3points
   #:ffi-make-arc-center-radius
   #:ffi-make-wire
   #:ffi-wire-is-closed

   ;; Advanced operations (Phase 8)
   #:ffi-make-pipe
   #:ffi-make-pipe-shell
   #:ffi-make-loft
   #:ffi-make-shell
   #:ffi-mirror-shape

   ;; Shape queries
   #:ffi-get-bounding-box
   #:ffi-get-volume
   #:ffi-get-area
   #:ffi-get-length
   #:ffi-get-center-of-mass
   #:ffi-get-shapes
   #:ffi-get-face-normal
   #:ffi-get-face-center

   ;; Shape type constants
   #:+shape-type-vertex+
   #:+shape-type-edge+
   #:+shape-type-wire+
   #:+shape-type-face+
   #:+shape-type-shell+
   #:+shape-type-solid+
   #:+shape-type-compound+

   ;; Phase 8: Geometric type queries
   #:ffi-get-edge-geom-type
   #:ffi-get-face-geom-type
   #:+geom-type-line+
   #:+geom-type-circle+
   #:+geom-type-ellipse+
   #:+geom-type-hyperbola+
   #:+geom-type-parabola+
   #:+geom-type-bezier+
   #:+geom-type-bspline+
   #:+geom-type-other-curve+
   #:+geom-type-plane+
   #:+geom-type-cylinder+
   #:+geom-type-cone+
   #:+geom-type-sphere+
   #:+geom-type-torus+
   #:+geom-type-bezier-surface+
   #:+geom-type-bspline-surface+
   #:+geom-type-other-surface+))

;;; ============================================================================
;;; Layer 2: Functional Core
;;; ============================================================================

(defpackage #:clad.core
  (:use #:cl)
  (:import-from #:clad.ffi
                #:with-occt-error-handling
                #:occt-handle
                #:handle-type
                #:handle-null-p
                #:ensure-valid-handle)
  (:documentation "Pure functional CAD operations")
  (:export
   ;; Shape type
   #:shape
   #:shape-p
   #:shape-handle
   #:shape-metadata
   #:valid-shape-p
   #:ensure-shape
   #:make-shape

   ;; Primitive constructors
   #:make-box
   #:make-cylinder
   #:make-sphere
   #:make-cone

   ;; Boolean operations
   #:union-shapes
   #:cut-shapes
   #:intersect-shapes
   #:fuse-shapes

   ;; Transformations
   #:translate
   #:rotate
   #:mirror
   #:scale-shape

   ;; Fillet operations (Phase 8)
   #:fillet
   #:fillet-chain

   ;; Chamfer operations (Phase 8)
   #:chamfer
   #:chamfer-chain

   ;; Curve operations (Phase 8)
   #:make-spline
   #:make-bezier
   #:make-arc-3points
   #:make-arc
   #:make-wire
   #:wire-closed-p
   #:make-line

   ;; Advanced operations (Phase 8)
   #:make-sweep
   #:make-pipe
   #:make-loft
   #:make-shell
   #:mirror-shape
   #:make-circle-wire))

;;; ============================================================================
;;; Layer 3: Units System
;;; ============================================================================

(defpackage #:clad.units
  (:use #:cl)
  (:documentation "Unit conversion and dimension tracking system")
  (:export
   ;; Unit context variables
   #:*default-units*
   #:*file-units*
   #:effective-units

   ;; Unit types
   #:unit-p
   #:valid-unit-p

   ;; Conversion functions
   #:convert-units
   #:define-unit-conversion

   ;; Dimension class and accessors
   #:toleranced-dimension
   #:make-toleranced-dimension
   #:dimension-nominal
   #:dimension-source-value
   #:dimension-source-units
   #:dimension-tolerance
   #:dimension-display-units

   ;; Dimension macro
   #:dim
   #:with-units

   ;; Tolerance specifications
   #:tolerance-spec
   #:make-tolerance-spec
   #:bilateral-tolerance
   #:unilateral-tolerance
   #:symmetric-tolerance

   ;; Conversion utilities
   #:mm->in
   #:in->mm
   #:mm->cm
   #:cm->mm
   #:mm->m
   #:m->mm
   #:unit-symbol))

;;; ============================================================================
;;; Layer 3.5: CLOS Shape Classes
;;; ============================================================================

(defpackage #:clad.shapes
  (:use #:cl)
  (:import-from #:clad.ffi
                #:occt-handle
                #:handle-ptr
                #:handle-valid-p
                #:ensure-valid-handle)
  (:import-from #:clad.core
                #:shape
                #:shape-handle)
  (:documentation "CLOS wrapper for OCCT shape hierarchy with queries")
  (:export
   ;; Shape classes
   #:cad-shape
   #:cad-vertex
   #:cad-edge
   #:cad-wire
   #:cad-face
   #:cad-shell
   #:cad-solid
   #:cad-compound

   ;; Shape accessors
   #:shape-type
   #:shape-valid-p

   ;; Generic query functions
   #:vertices
   #:edges
   #:wires
   #:faces
   #:shells
   #:solids

   ;; Geometric property queries
   #:bounding-box
   #:center-of-mass
   #:volume
   #:area
   #:shape-length

   ;; Type checking
   #:vertex-p
   #:edge-p
   #:wire-p
   #:face-p
   #:shell-p
   #:solid-p
   #:compound-p

   ;; Wrapping/unwrapping utilities
   #:wrap-shape
   #:unwrap-shape

   ;; Geometry type queries
   #:geom-type))

;;; ============================================================================
;;; Layer 4: Selectors (Phase 3)
;;; ============================================================================

(defpackage #:clad.selectors
  (:use #:cl)
  (:import-from #:clad.shapes
                #:faces #:edges #:vertices #:wires
                #:center-of-mass #:area #:volume
                #:bounding-box
                #:cad-shape #:cad-face #:cad-edge
                #:unwrap-shape)
  (:import-from #:clad.core
                #:shape-handle)
  (:import-from #:clad.ffi
                #:ffi-get-face-normal)
  (:documentation "Shape selection system for finding faces, edges, etc.")
  (:export
   ;; Main API
   #:select

   ;; Selector classes (exported for testing, not typical user API)
   #:base-selector
   #:direction-selector
   #:parallel-selector
   #:perpendicular-selector
   #:type-selector          ; Phase 8
   #:size-selector          ; Phase 8
   #:and-selector
   #:or-selector
   #:not-selector
   #:custom-selector

   ;; Protocol
   #:apply-selector

   ;; Tolerance control
   #:*default-tolerance*
   #:selector-tolerance))

;;; ============================================================================
;;; Layer 4.5: Workplanes (Phase 4)
;;; ============================================================================

(defpackage #:clad.workplane
  (:use #:cl)
  (:import-from #:clad.ffi
                #:ffi-get-face-center
                #:ffi-get-face-normal)
  (:import-from #:clad.core
                #:shape-handle)
  (:import-from #:clad.shapes
                #:unwrap-shape)
  (:documentation "Workplane system for local coordinate systems")
  (:export
   ;; Workplane class and constructor
   #:workplane
   #:make-workplane

   ;; Accessors
   #:workplane-origin
   #:workplane-x-dir
   #:workplane-y-dir
   #:workplane-z-dir

   ;; Standard plane constructors
   #:xy-plane
   #:xz-plane
   #:yz-plane

   ;; Coordinate transformations
   #:local-to-global
   #:global-to-local

   ;; Workplane from face
   #:workplane-from-face

   ;; Workplane operations
   #:offset-workplane
   #:rotate-workplane))

;;; ============================================================================
;;; Layer 4.75: Context API (Phase 4)
;;; ============================================================================

(defpackage #:clad.context
  (:use #:cl)
  (:import-from #:clad.core
                #:shape #:shape-handle
                #:make-box #:make-cylinder #:make-sphere
                #:union-shapes #:cut-shapes #:intersect-shapes
                #:translate #:rotate #:scale-shape)
  (:import-from #:clad.shapes
                #:faces #:edges #:vertices
                #:cad-shape #:unwrap-shape)
  (:import-from #:clad.selectors
                #:select)
  (:import-from #:clad.workplane
                #:workplane #:xy-plane
                #:workplane-origin #:local-to-global)
  (:documentation "Stateful modeling context for sequential CAD operations")
  (:export
   ;; Context class
   #:modeling-context
   #:make-context
   #:*context*

   ;; Context macro
   #:with-context

   ;; State queries
   #:current-shape
   #:current-selection
   #:current-workplane

   ;; Shape operations
   #:add
   #:union-op
   #:cut-op
   #:intersect-op

   ;; Selection operations
   #:select-faces
   #:select-edges
   #:push-selection
   #:pop-selection

   ;; Workplane operations
   #:set-workplane
   #:push-workplane
   #:pop-workplane

   ;; Fillet operations (Phase 8)
   #:fillet-selected

   ;; Chamfer operations (Phase 8)
   #:chamfer-selected

   ;; Result extraction
   #:get-result))

;;; ============================================================================
;;; Layer 5: DSL (Declarative Macros - Phase 5)
;;; ============================================================================

(defpackage #:clad.dsl
  (:use #:cl)
  (:import-from #:clad.core
                #:make-box #:make-cylinder #:make-sphere #:make-cone
                #:translate #:rotate #:scale-shape
                #:union-shapes #:cut-shapes #:intersect-shapes)
  (:import-from #:clad.shapes
                #:cad-shape #:unwrap-shape #:wrap-shape
                #:solid-p #:volume)
  (:import-from #:clad.context
                #:with-context #:add #:cut-op #:union-op
                #:select-faces #:select-edges
                #:get-result)
  (:import-from #:clad.selectors
                #:select)
  (:documentation "Declarative DSL for parametric part definition")
  (:export
   ;; Part definition macros
   #:defpart
   #:deffeature

   ;; Pattern operations
   #:circular-pattern
   #:linear-pattern
   #:grid-pattern

   ;; Feature registry (Cycle 9)
   #:*feature-registry*
   #:register-feature
   #:lookup-feature
   #:list-features
   #:clear-features))

;;; ============================================================================
;;; Layer 5: Export
;;; ============================================================================

(defpackage #:clad.export
  (:use #:cl)
  (:import-from #:clad.core #:shape #:shape-handle)
  (:import-from #:clad.ffi #:ffi-export-step #:ffi-export-stl #:ffi-export-gltf)
  (:documentation "CAD file export functionality")
  (:export
   #:export-step
   #:export-stl
   #:export-gltf
   #:export-iges))

;;; ============================================================================
;;; Layer 5: Viewer (Web-based visualization)
;;; ============================================================================

(defpackage #:clad.viewer
  (:use #:cl)
  (:import-from #:clad.core #:shape #:valid-shape-p)
  (:import-from #:clad.export #:export-gltf)
  (:documentation "Web-based 3D viewer for CLAD shapes")
  (:export
   #:start-viewer
   #:stop-viewer
   #:view
   #:*viewer-port*
   #:*viewer-running-p*))

;;; ============================================================================
;;; Layer 6: Auto-Rebuild (REPL-Driven Development - Phase 6)
;;; ============================================================================

(defpackage #:clad.auto-rebuild
  (:use #:cl)
  (:import-from #:clad.core #:shape #:valid-shape-p)
  (:import-from #:clad.shapes #:cad-shape #:unwrap-shape)
  (:import-from #:clad.viewer #:view #:start-viewer #:*viewer-running-p*)
  (:documentation "Auto-rebuild system for REPL-driven CAD development")
  (:export
   ;; State variables
   #:*auto-rebuild*
   #:*current-part*
   #:*current-part-args*

   ;; Main API
   #:show
   #:rebuild
   #:toggle-auto-rebuild

   ;; File watching
   #:watch
   #:stop-watching
   #:stop-all-watchers
   #:*file-watchers*

   ;; Errors
   #:no-current-part-error))

;;; ============================================================================
;;; Main User Package
;;; ============================================================================

(defpackage #:clad
  (:use #:cl)
  (:import-from #:clad.core
                #:shape
                #:make-box
                #:make-cylinder
                #:make-sphere
                #:make-cone
                #:union-shapes
                #:cut-shapes
                #:intersect-shapes)
  (:import-from #:clad.units
                #:*default-units*
                #:*file-units*
                #:dim
                #:with-units)
  (:import-from #:clad.export
                #:export-step
                #:export-stl
                #:export-gltf)
  (:import-from #:clad.viewer
                #:start-viewer
                #:stop-viewer
                #:view)
  (:import-from #:clad.auto-rebuild
                #:show
                #:rebuild
                #:toggle-auto-rebuild
                #:watch
                #:stop-watching
                #:*auto-rebuild*)
  (:import-from #:clad.dsl
                #:defpart
                #:deffeature)
  (:documentation "Main user package - re-exports all public APIs")
  (:export
   ;; From core
   #:shape
   #:make-box
   #:make-cylinder
   #:make-sphere
   #:make-cone
   #:union-shapes
   #:cut-shapes
   #:intersect-shapes

   ;; From units
   #:*default-units*
   #:*file-units*
   #:dim
   #:with-units

   ;; From export
   #:export-step
   #:export-stl
   #:export-gltf

   ;; From viewer
   #:view
   #:start-viewer
   #:stop-viewer

   ;; From auto-rebuild
   #:show
   #:rebuild
   #:toggle-auto-rebuild
   #:watch
   #:stop-watching
   #:*auto-rebuild*

   ;; From DSL
   #:defpart
   #:deffeature))

;;; ============================================================================
;;; Test Package
;;; ============================================================================

(defpackage #:clad.tests
  (:use #:cl #:fiveam #:clad)
  (:documentation "Test suite for CLAD")
  (:export
   #:clad-tests
   #:sketch-tests
   #:run-tests))

;;; ============================================================================
;;; Layer 9: Sketch System (Phase 9)
;;; ============================================================================

(defpackage #:clad.sketch
  (:use #:cl)
  (:documentation "2D constraint-based sketching system")
  (:export
   ;; Sketch entities
   #:sketch-entity
   #:point-2d
   #:line-2d
   #:arc-2d
   #:circle-2d
   #:spline-2d
   
   ;; Entity constructors
   #:make-point-2d
   #:make-line-2d
   #:make-arc-2d
   #:make-circle-2d
   #:make-spline-2d
   
   ;; Entity accessors
   #:entity-name
   #:entity-parameters
   #:point-x
   #:point-y
   #:point-fixed-p
   #:line-start
   #:line-end
   #:line-length
   #:arc-center
   #:arc-radius
   #:arc-start-angle
   #:arc-end-angle
   #:circle-center
   #:circle-radius
   #:spline-points
   #:spline-control-points
   #:spline-closed-p

   ;; Sketch container
   #:sketch
   #:make-sketch
   #:sketch-name
   #:sketch-entities
   #:sketch-constraints
   #:add-entity
   #:add-constraint
   #:find-entity

   ;; Parameters
   #:sketch-parameter
   #:param-name
   #:param-value
   #:param-fixed-p

   ;; Sketch-to-3D conversion (Week 7-8)
   #:sketch-plane
   #:make-sketch-plane
   #:make-sketch-plane-from-face  ; Week 11-12
   #:plane-origin
   #:plane-x-axis
   #:plane-y-axis
   #:plane-name
   #:sketch-to-wire
   #:sketch-to-face
   #:extrude-sketch
   #:revolve-sketch))

(defpackage #:clad.sketch.constraints
  (:use #:cl #:clad.sketch)
  (:documentation "Constraint types for 2D sketches")
  (:export
   ;; Constraint classes
   #:constraint
   #:fixed-constraint
   #:coincident-constraint
   #:distance-constraint
   #:horizontal-constraint
   #:vertical-constraint
   #:parallel-constraint
   #:perpendicular-constraint
   #:angle-constraint
   #:tangent-constraint

   ;; Constraint constructors
   #:make-constraint
   #:make-fixed-constraint
   #:make-coincident-constraint
   #:make-distance-constraint
   #:make-horizontal-constraint
   #:make-vertical-constraint
   #:make-parallel-constraint
   #:make-perpendicular-constraint
   #:make-angle-constraint
   #:make-tangent-constraint

   ;; Constraint accessors
   #:constraint-entities
   #:constraint-type
   #:constraint-parameters
   #:constraint-weight
   #:constraint-target-x
   #:constraint-target-y
   #:constraint-target-distance
   #:constraint-target-angle

   ;; Constraint evaluation
   #:constraint-error
   #:constraint-jacobian
   #:apply-constraint))

(defpackage #:clad.sketch.solver
  (:use #:cl #:clad.sketch #:clad.sketch.constraints)
  (:documentation "Constraint solver for 2D sketches")
  (:export
   ;; Solver
   #:solve-sketch
   #:solver-options
   #:make-solver-options
   
   ;; Solver configuration
   #:solver-max-iterations
   #:solver-tolerance
   #:solver-method
   
   ;; Errors
   #:over-constrained-error
   #:under-constrained-error
   #:solver-failed-error))

(defpackage #:clad.sketch.dsl
  (:use #:cl)
  (:import-from #:clad.sketch
                #:make-sketch #:add-entity #:find-entity)
  (:import-from #:clad.sketch.constraints
                #:make-constraint)
  (:import-from #:clad.sketch.solver
                #:solve-sketch)
  (:documentation "Declarative DSL for parametric sketches")
  (:export
   ;; Sketch definition macro
   #:defsketch
   
   ;; Sketch to 3D operations
   #:extrude-sketch
   #:revolve-sketch))

;;; ============================================================================
;;; Layer 10: Assembly System (Phase 10)
;;; ============================================================================

(defpackage #:clad.assembly
  (:use #:cl)
  (:import-from #:clad.core #:shape)
  (:documentation "Assembly modeling with mates and constraints")
  (:export
   ;; Assembly classes
   #:assembly
   #:component

   ;; Assembly constructors
   #:make-assembly
   #:assembly-p
   #:add-component
   #:get-component
   #:remove-component
   #:list-components

   ;; Assembly accessors
   #:assembly-name
   #:assembly-description
   #:assembly-components
   #:assembly-constraints
   #:assembly-parameters
   #:assembly-metadata
   #:set-parameter
   #:get-parameter

   ;; Component accessors
   #:component-name
   #:component-part
   #:component-assembly
   #:component-position
   #:component-rotation
   #:component-transform
   #:component-fixed-p
   #:component-quantity
   #:component-metadata
   #:set-component-position
   #:set-component-rotation))

(defpackage #:clad.assembly.constraints
  (:use #:cl #:clad.assembly)
  (:documentation "Mate constraints for assemblies")
  (:export
   ;; Mate constraint class
   #:mate-constraint

   ;; Mate constructors
   #:add-mate
   #:list-constraints
   #:remove-constraint
   #:clear-constraints

   ;; Mate accessors
   #:mate-type
   #:mate-component1
   #:mate-component2
   #:mate-reference1
   #:mate-reference2
   #:mate-offset

   ;; Mate evaluation
   #:mate-error
   #:apply-mate
   #:mate-compatible-p

   ;; DOF calculation
   #:assembly-dof
   #:assembly-status))

(defpackage #:clad.assembly.solver
  (:use #:cl #:clad.assembly #:clad.assembly.constraints)
  (:documentation "Assembly constraint solver")
  (:export
   ;; Solver
   #:solve-assembly
   #:solver-status-string

   ;; Errors
   #:assembly-solver-error
   #:over-constrained-error
   #:under-constrained-error))

(defpackage #:clad.assembly.bom
  (:use #:cl #:clad.assembly)
  (:documentation "Bill of materials generation")
  (:export
   ;; BOM classes
   #:bom-entry
   #:bom-entry-name
   #:bom-entry-part
   #:bom-entry-quantity
   #:bom-entry-metadata

   ;; BOM generation
   #:generate-bom
   #:print-bom

   ;; BOM export
   #:export-bom-csv
   #:export-bom-json))

(defpackage #:clad.assembly.dsl
  (:use #:cl)
  (:import-from #:clad.assembly
                #:make-assembly #:add-component #:set-parameter)
  (:import-from #:clad.assembly.constraints
                #:add-mate)
  (:import-from #:clad.assembly.solver
                #:solve-assembly)
  (:documentation "Declarative DSL for assemblies")
  (:export
   ;; Assembly definition macro
   #:defassembly

   ;; Reference macro
   #:@ref

   ;; Utilities
   #:with-assembly))
