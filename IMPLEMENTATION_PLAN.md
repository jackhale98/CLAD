# CLAD Enhancement Implementation Plan
## Test-Driven Development Roadmap

**Date:** 2025-01-13
**Target:** Autonomous AI Agent Implementation
**Methodology:** Test-Driven Development (TDD)

---

## Executive Summary

This plan implements four major enhancements to CLAD:
1. **Boolean Selector Combinators** - Logical AND/OR/NOT for complex selections
2. **Position-Based Selectors** - Select by coordinate values and bounding boxes
3. **Selector Debugging/Inspection** - Tools for understanding selections
4. **Lightweight Workplane Operations** - Simple 2D operations without full sketch system

Each enhancement follows strict TDD:
- Write tests first
- Implement minimal code to pass
- Refactor for clarity
- Document thoroughly

---

## Current State Analysis

### Assembly Reference System

**Finding:** Assembly mate references like `:face-top`, `:bottom-face`, `:axis` are **symbolic placeholders** only.

```lisp
;; From examples/05-assemblies.lisp:64
(clad.assembly.constraints:add-mate
  assy :coincident
  :base :face-top        ; ← Just a symbol, not resolved
  :top :face-bottom)     ; ← Stored as-is in constraint
```

**Implementation Status:**
- Symbols stored in `mate-reference1` and `mate-reference2` slots
- No resolution to actual geometry yet
- Marked as stub for Week 13-14 assembly solver

**Recommendation:** Create reference resolution system that maps symbols to selector specifications.

### Face-Centered Positioning

**Finding:** Features are NOT automatically centered on selected faces - manual positioning required.

```lisp
;; From examples/01-basic-dsl.lisp:44
(:on-face :direction :+z :extreme :max
  (:cut (clad.core:translate
          (clad.core:make-cylinder (/ hole-diameter 2) (* size 1.2))
          0 0 (- (/ size 10)))))  ; ← Manual global coordinates!
```

**Current Behavior:**
- Shapes positioned at global (0,0,0) or user-specified coordinates
- No automatic translation to face center
- No automatic alignment to face normal

**Need:** Convenience operations for face-relative positioning.

### Multi-Select

**Finding:** Multi-select DOES work - selectors return lists of all matching entities.

```lisp
;; From src/context/context.lisp:259
(let* ((all-faces (faces current))
       (selected (apply #'select all-faces selector-spec args)))
  (push selected (selection-stack ctx))  ; ← List of faces
```

**Confirmed:** Operations like `:fillet` work on all selected edges.

### Sketch System

**Finding:** Full constraint-based sketch system exists, but NO lightweight 2D operations.

**Current:** Must create named sketch with entities → solve → extrude
**Missing:** Simple operations like `(:circle 10) (:extrude 5)` on a face

---

## Implementation Phases

### Phase 1: Boolean Selector Combinators (Week 1)

**Goal:** Enable logical combination of selectors for complex queries.

**Deliverables:**
- `(:and selector1 selector2 ...)` combinator
- `(:or selector1 selector2 ...)` combinator
- `(:not selector)` combinator
- Nested combinations support
- Full test coverage

**TDD Cycle:**

#### Cycle 1.1: AND Combinator (2-3 hours)

**Test File:** `tests/selector-combinator-tests.lisp`

```lisp
(test and-combinator-basic
  "AND combinator selects entities matching ALL criteria"
  (let* ((box (make-box 100 100 50))
         (faces (clad.shapes:faces box))
         ;; Select planar faces pointing up with area > 5000
         (selected (select faces
                          (:and :type :planar
                                :direction :+z
                                :area :> 5000))))
    (is (= 1 (length selected)))  ; Only top face
    (is (> (face-area (first selected)) 5000))))

(test and-combinator-empty
  "AND combinator returns empty when no matches"
  (let* ((box (make-box 100 100 50))
         (faces (clad.shapes:faces box))
         ;; Impossible: planar AND cylindrical
         (selected (select faces
                          (:and :type :planar
                                :type :cylindrical))))
    (is (null selected))))

(test and-combinator-nested
  "AND combinator supports nesting with OR"
  (let* ((box (make-box 100 100 50))
         (edges (clad.shapes:edges box))
         ;; Straight edges that are parallel to X OR Y
         (selected (select edges
                          (:and :type :line
                                (:or :parallel :x
                                     :parallel :y)))))
    (is (= 8 (length selected)))))  ; 4 X-parallel + 4 Y-parallel
```

**Implementation:**

```lisp
;;;; src/selectors/combinators.lisp

(defclass and-combinator (selector)
  ((selectors :initarg :selectors
              :accessor combinator-selectors
              :documentation "List of selectors to AND together"))
  (:documentation "AND combinator - matches entities that satisfy ALL sub-selectors"))

(defmethod apply-selector ((sel and-combinator) shape-list)
  "Apply AND logic: keep shapes matching all selectors"
  (let ((result shape-list))
    (dolist (sub-selector (combinator-selectors sel) result)
      (setf result (apply-selector sub-selector result)))))

;; Update src/selectors/api.lisp
(defun select (shape-list selector-spec &rest args)
  (cond
    ;; Existing cases...

    ;; New: AND combinator
    ((eq selector-spec :and)
     (let ((sub-selectors (mapcar #'parse-selector-spec args)))
       (apply-selector (make-instance 'and-combinator
                                      :selectors sub-selectors)
                       shape-list)))
    ...))

(defun parse-selector-spec (spec &rest args)
  "Convert selector specification to selector instance"
  (cond
    ((keywordp spec)
     ;; Recursively parse: :direction :+z → direction-selector instance
     (apply #'select nil spec args))
    ((listp spec)
     ;; Nested: (:and ...) or (:or ...)
     (apply #'select nil spec))
    ((typep spec 'selector)
     spec)))
```

**Files to Create/Modify:**
1. `tests/selector-combinator-tests.lisp` - New test file
2. `src/selectors/combinators.lisp` - Enhance existing file
3. `src/selectors/api.lisp` - Add combinator parsing

**Success Criteria:**
- All tests pass
- Handles 2-10 selectors in AND
- Nested combinators work
- Empty result handling
- No performance regression on single selectors

#### Cycle 1.2: OR Combinator (1-2 hours)

**Tests:**

```lisp
(test or-combinator-basic
  "OR combinator selects entities matching ANY criteria"
  (let* ((box (make-box 100 100 50))
         (edges (clad.shapes:edges box))
         ;; Parallel to X OR parallel to Z
         (selected (select edges
                          (:or :parallel :x
                               :parallel :z))))
    (is (= 8 (length selected)))))  ; 4 X + 4 Z

(test or-combinator-single-match
  "OR combinator with only one match"
  (let* ((box (make-box 100 100 50))
         (faces (clad.shapes:faces box))
         ;; Cylindrical (none) OR top face
         (selected (select faces
                          (:or :type :cylindrical
                               (:and :direction :+z :extreme :max)))))
    (is (= 1 (length selected)))))
```

**Implementation:**

```lisp
(defclass or-combinator (selector)
  ((selectors :initarg :selectors
              :accessor combinator-selectors))
  (:documentation "OR combinator - matches entities that satisfy ANY sub-selector"))

(defmethod apply-selector ((sel or-combinator) shape-list)
  "Apply OR logic: accumulate shapes matching any selector (no duplicates)"
  (let ((result '()))
    (dolist (sub-selector (combinator-selectors sel))
      (let ((matches (apply-selector sub-selector shape-list)))
        ;; Union: add unique matches
        (dolist (match matches)
          (pushnew match result :test #'eq))))
    result))
```

#### Cycle 1.3: NOT Combinator (1-2 hours)

**Tests:**

```lisp
(test not-combinator-basic
  "NOT combinator inverts selection"
  (let* ((box (make-box 100 100 50))
         (faces (clad.shapes:faces box))
         ;; All faces EXCEPT top face
         (selected (select faces
                          (:not (:and :direction :+z :extreme :max)))))
    (is (= 5 (length selected)))))  ; 6 faces - 1 top = 5

(test not-combinator-with-type
  "NOT combinator with type selector"
  (let* ((cylinder (make-cylinder 20 50))
         (faces (clad.shapes:faces cylinder))
         ;; All faces except planar (so only cylindrical wall)
         (selected (select faces
                          (:not :type :planar))))
    (is (= 1 (length selected)))  ; Only cylindrical side
    (is (eq :cylindrical (face-type (first selected))))))
```

**Implementation:**

```lisp
(defclass not-combinator (selector)
  ((selector :initarg :selector
             :accessor combinator-selector))
  (:documentation "NOT combinator - matches entities that DON'T satisfy sub-selector"))

(defmethod apply-selector ((sel not-combinator) shape-list)
  "Apply NOT logic: remove shapes matching selector"
  (let ((matches (apply-selector (combinator-selector sel) shape-list)))
    (set-difference shape-list matches :test #'eq)))
```

#### Cycle 1.4: DSL Integration (2 hours)

**Goal:** Make combinators work in `defpart` syntax.

**Tests:**

```lisp
(test combinator-in-defpart
  "Combinators work in defpart DSL"
  (let ((part (test-part-with-combinator)))
    (is (not (null part)))
    ;; Verify fillets applied only to horizontal straight edges
    ...))

(defpart test-part-with-combinator ()
  (:body (make-box 100 100 50))

  ;; Fillet horizontal straight edges only
  (:on-edge (:and :type :line
                  (:or :parallel :x :parallel :y))
    (:fillet 3.0d0)))
```

**Implementation:** Modify `src/dsl/defpart.lisp` to parse combinator syntax in `:on-face` and `:on-edge` forms.

**Deliverables:**
- All combinator tests pass
- Works in both imperative (context API) and declarative (defpart DSL) modes
- Documentation updated
- Examples added

---

### Phase 2: Position-Based Selectors (Week 2)

**Goal:** Select entities by coordinate values and spatial relationships.

**Deliverables:**
- `:at-x`, `:at-y`, `:at-z` selectors with tolerance
- `:between-x`, `:between-y`, `:between-z` range selectors
- `:within-box` bounding box selector
- `:near-point` proximity selector
- Full test coverage

#### Cycle 2.1: Single-Axis Position Selectors (3-4 hours)

**Test File:** `tests/position-selector-tests.lisp`

```lisp
(test at-x-selector
  "Select faces at specific X coordinate"
  (let* ((box (make-box 100 100 50))
         (faces (clad.shapes:faces box))
         ;; Right face at X=50 (box is centered: -50 to +50)
         (selected (select faces :at-x 50.0 :tolerance 0.1)))
    (is (= 1 (length selected)))
    (is (approximately= 50.0 (face-center-x (first selected)) 0.1))))

(test at-y-with-no-matches
  "at-y returns empty when no faces at coordinate"
  (let* ((box (make-box 100 100 50))
         (faces (clad.shapes:faces box))
         ;; No face at Y=1000
         (selected (select faces :at-y 1000.0 :tolerance 0.1)))
    (is (null selected))))

(test at-z-multiple-matches
  "at-z can match multiple faces at same Z"
  (let* ((part (complex-multi-level-part))  ; Has multiple faces at Z=20
         (faces (clad.shapes:faces part))
         (selected (select faces :at-z 20.0 :tolerance 0.1)))
    (is (>= (length selected) 2))))  ; At least 2 faces at Z=20
```

**Implementation:**

```lisp
;;;; src/selectors/position.lisp (NEW FILE)

(defclass position-selector (selector)
  ((axis :initarg :axis
         :accessor position-axis
         :documentation "Axis to check: :x, :y, or :z")
   (value :initarg :value
          :accessor position-value
          :type double-float
          :documentation "Coordinate value to match")
   (tolerance :initarg :tolerance
              :initform 0.01d0
              :accessor position-tolerance
              :type double-float
              :documentation "Matching tolerance in mm"))
  (:documentation "Selector for entities at specific coordinate value"))

(defmethod apply-selector ((sel position-selector) shape-list)
  "Select shapes whose center is at specified coordinate (within tolerance)"
  (let ((axis (position-axis sel))
        (value (position-value sel))
        (tolerance (position-tolerance sel)))
    (remove-if-not
      (lambda (shape)
        (let* ((center (shape-center shape))  ; Returns (x y z)
               (coord (case axis
                        (:x (first center))
                        (:y (second center))
                        (:z (third center)))))
          (< (abs (- coord value)) tolerance)))
      shape-list)))

;; Helper function - add to src/shapes/methods.lisp
(defun shape-center (shape)
  "Get center point of shape's bounding box as (x y z) list"
  (let* ((bbox (bounding-box shape))
         (center-x (/ (+ (bbox-min-x bbox) (bbox-max-x bbox)) 2.0))
         (center-y (/ (+ (bbox-min-y bbox) (bbox-max-y bbox)) 2.0))
         (center-z (/ (+ (bbox-min-z bbox) (bbox-max-z bbox)) 2.0)))
    (list center-x center-y center-z)))
```

**API Integration:**

```lisp
;;;; src/selectors/api.lisp

(defun select (shape-list selector-spec &rest args)
  (cond
    ;; Existing cases...

    ;; Position selectors
    ((member selector-spec '(:at-x :at-y :at-z))
     (destructuring-bind (value &key (tolerance 0.01d0)) args
       (let ((axis (case selector-spec
                     (:at-x :x)
                     (:at-y :y)
                     (:at-z :z))))
         (apply-selector
           (make-instance 'position-selector
                          :axis axis
                          :value (coerce value 'double-float)
                          :tolerance (coerce tolerance 'double-float))
           shape-list))))
    ...))
```

#### Cycle 2.2: Range Selectors (2-3 hours)

**Tests:**

```lisp
(test between-z-range
  "Select faces between two Z coordinates"
  (let* ((stepped-part (make-stepped-part))  ; Has faces at Z=10, 20, 30, 40
         (faces (clad.shapes:faces stepped-part))
         ;; Faces between Z=15 and Z=35
         (selected (select faces :between-z 15.0 35.0)))
    ;; Should match faces at Z=20 and Z=30
    (is (>= (length selected) 2))))

(test between-x-empty
  "between-x returns empty when no faces in range"
  (let* ((box (make-box 100 100 50))
         (faces (clad.shapes:faces box))
         ;; Range outside box
         (selected (select faces :between-x 200.0 300.0)))
    (is (null selected))))
```

**Implementation:**

```lisp
(defclass range-selector (selector)
  ((axis :initarg :axis
         :accessor range-axis)
   (min-value :initarg :min
              :accessor range-min)
   (max-value :initarg :max
              :accessor range-max))
  (:documentation "Selector for entities within coordinate range"))

(defmethod apply-selector ((sel range-selector) shape-list)
  (let ((axis (range-axis sel))
        (min-val (range-min sel))
        (max-val (range-max sel)))
    (remove-if-not
      (lambda (shape)
        (let* ((center (shape-center shape))
               (coord (case axis
                        (:x (first center))
                        (:y (second center))
                        (:z (third center)))))
          (and (>= coord min-val)
               (<= coord max-val))))
      shape-list)))
```

#### Cycle 2.3: Bounding Box Selector (2 hours)

**Tests:**

```lisp
(test within-box-selector
  "Select entities within bounding box"
  (let* ((assembly (make-multi-part-assembly))
         (faces (all-faces-in-assembly assembly))
         ;; Select only faces in corner region
         (selected (select faces
                          :within-box '(0 0 0) '(50 50 50))))
    (is (> (length selected) 0))
    (dolist (face selected)
      (let ((center (shape-center face)))
        (is (and (<= 0 (first center) 50)
                 (<= 0 (second center) 50)
                 (<= 0 (third center) 50)))))))
```

**Implementation:**

```lisp
(defclass bbox-selector (selector)
  ((min-corner :initarg :min
               :accessor bbox-min)
   (max-corner :initarg :max
               :accessor bbox-max))
  (:documentation "Selector for entities within bounding box"))

(defmethod apply-selector ((sel bbox-selector) shape-list)
  (let ((min-corner (bbox-min sel))
        (max-corner (bbox-max sel)))
    (remove-if-not
      (lambda (shape)
        (let ((center (shape-center shape)))
          (and (<= (first min-corner) (first center) (first max-corner))
               (<= (second min-corner) (second center) (second max-corner))
               (<= (third min-corner) (third center) (third max-corner)))))
      shape-list)))
```

#### Cycle 2.4: Proximity Selector (2 hours)

**Tests:**

```lisp
(test near-point-selector
  "Select entities near a point"
  (let* ((box (make-box 100 100 50))
         (faces (clad.shapes:faces box))
         ;; Faces within 10mm of origin
         (selected (select faces
                          :near-point '(0 0 0) :radius 10.0)))
    (is (> (length selected) 0))))
```

**Implementation:**

```lisp
(defclass proximity-selector (selector)
  ((point :initarg :point
          :accessor proximity-point)
   (radius :initarg :radius
           :accessor proximity-radius))
  (:documentation "Selector for entities near a point"))

(defmethod apply-selector ((sel proximity-selector) shape-list)
  (let ((point (proximity-point sel))
        (radius (proximity-radius sel)))
    (remove-if-not
      (lambda (shape)
        (let ((center (shape-center shape)))
          (< (distance-3d point center) radius)))
      shape-list)))

(defun distance-3d (p1 p2)
  "Euclidean distance between two 3D points"
  (sqrt (+ (expt (- (first p1) (first p2)) 2)
           (expt (- (second p1) (second p2)) 2)
           (expt (- (third p1) (third p2)) 2))))
```

**Deliverables:**
- All position selector tests pass
- Works with combinators: `(:and :at-z 50 :type :planar)`
- DSL integration
- Documentation and examples

---

### Phase 3: Selector Debugging & Inspection (Week 3)

**Goal:** Tools to understand what selectors are selecting.

**Deliverables:**
- `inspect-selection` REPL function
- `:debug-selection` DSL form
- `:debug-highlight` viewer integration
- Selection statistics and reporting
- Full test coverage

#### Cycle 3.1: REPL Inspection Tools (2-3 hours)

**Tests:**

```lisp
(test inspect-selection-basic
  "inspect-selection returns detailed information"
  (let* ((box (make-box 100 100 50))
         (report (clad.selectors:inspect-selection
                   box :on-face :type :planar)))
    (is (plist-p report))
    (is (= 6 (getf report :count)))
    (is (listp (getf report :shapes)))
    (is (every #'stringp (getf report :descriptions)))))

(test inspect-selection-empty
  "inspect-selection handles empty selection gracefully"
  (let* ((box (make-box 100 100 50))
         (report (clad.selectors:inspect-selection
                   box :on-face :type :cylindrical)))
    (is (zerop (getf report :count)))
    (is (null (getf report :shapes)))))
```

**Implementation:**

```lisp
;;;; src/selectors/inspection.lisp (NEW FILE)

(defun inspect-selection (shape selector-type selector-spec &rest args)
  "Inspect what a selector would select, returning detailed report.

  Args:
    shape - Shape to select from
    selector-type - :on-face or :on-edge
    selector-spec - Selector specification
    args - Additional selector arguments

  Returns: Property list with:
    :count - Number of entities selected
    :shapes - List of selected shape objects
    :descriptions - Human-readable descriptions
    :centers - Center points of each entity
    :areas - Areas (for faces) or lengths (for edges)
    :types - Geometry types of each entity"

  (let* ((entities (case selector-type
                     (:on-face (clad.shapes:faces shape))
                     (:on-edge (clad.shapes:edges shape))
                     (t (error "Unknown selector type: ~A" selector-type))))
         (selected (apply #'clad.selectors:select entities selector-spec args)))

    (list :count (length selected)
          :shapes selected
          :descriptions (mapcar #'describe-entity selected)
          :centers (mapcar #'shape-center selected)
          :areas (mapcar #'entity-measure selected)
          :types (mapcar #'entity-type selected))))

(defun describe-entity (entity)
  "Generate human-readable description of entity"
  (format nil "~A at (~{~,2F~^, ~}) with ~A ~,2F"
          (entity-type entity)
          (shape-center entity)
          (if (face-p entity) "area" "length")
          (entity-measure entity)))

(defun entity-measure (entity)
  "Get area (face) or length (edge) of entity"
  (cond
    ((face-p entity) (face-area entity))
    ((edge-p entity) (edge-length entity))
    (t 0.0)))

(defun entity-type (entity)
  "Get geometry type as keyword"
  (cond
    ((face-p entity)
     (cond
       ((planar-face-p entity) :planar)
       ((cylindrical-face-p entity) :cylindrical)
       ((spherical-face-p entity) :spherical)
       (t :other)))
    ((edge-p entity)
     (cond
       ((line-edge-p entity) :line)
       ((circle-edge-p entity) :circle)
       ((arc-edge-p entity) :arc)
       (t :curve)))
    (t :unknown)))

;; Pretty printing
(defun print-selection-report (report &optional (stream *standard-output*))
  "Pretty-print inspection report"
  (format stream "~%Selection Report:~%")
  (format stream "  Count: ~A~%" (getf report :count))
  (when (> (getf report :count) 0)
    (format stream "  Entities:~%")
    (loop for desc in (getf report :descriptions)
          for i from 1
          do (format stream "    ~A. ~A~%" i desc))))
```

**REPL Usage:**

```lisp
;; Interactive debugging in REPL
CL-USER> (let ((box (clad.core:make-box 100 100 50)))
           (clad.selectors:inspect-selection
             box :on-face :type :planar))

Selection Report:
  Count: 6
  Entities:
    1. PLANAR at (50.00, 0.00, 25.00) with area 5000.00
    2. PLANAR at (-50.00, 0.00, 25.00) with area 5000.00
    3. PLANAR at (0.00, 50.00, 25.00) with area 5000.00
    ...

CL-USER> (clad.selectors:inspect-selection
           box :on-edge (:and :type :line :parallel :z))

Selection Report:
  Count: 4
  Entities:
    1. LINE at (50.00, 50.00, 25.00) with length 50.00
    2. LINE at (-50.00, 50.00, 25.00) with length 50.00
    ...
```

#### Cycle 3.2: DSL Debug Forms (2-3 hours)

**Tests:**

```lisp
(test debug-selection-in-defpart
  "debug-selection form prints selection info during build"
  (let ((output (with-output-to-string (*standard-output*)
                  (test-part-with-debug-selection))))
    (is (search "Selected 6 faces" output))
    (is (search "Selected 4 edges" output))))

(defpart test-part-with-debug-selection ()
  (:body (make-box 100 100 50))

  (:debug-selection :on-face :type :planar)  ; Prints during build

  (:on-edge :parallel :z
    (:debug-selection)  ; Debug current selection
    (:fillet 3.0d0)))
```

**Implementation:**

```lisp
;;;; src/dsl/defpart.lisp - Add new form type

(defun expand-part-form-at-compile-time (form)
  (case (first form)
    ;; Existing cases...

    (:debug-selection
     (let ((args (rest form)))
       (if (null args)
           ;; No args: debug current selection from context
           `(let ((selection (clad.context:current-selection)))
              (format t "~%DEBUG: Current selection has ~A entities~%"
                      (length selection))
              (dolist (entity selection)
                (format t "  - ~A~%" (clad.selectors.inspection:describe-entity entity))))
           ;; Args provided: run selector and debug result
           `(let* ((current (clad.context:current-shape))
                   (report (clad.selectors:inspect-selection
                             current ,@args)))
              (clad.selectors.inspection:print-selection-report report)))))

    ...))
```

#### Cycle 3.3: Viewer Highlighting (3-4 hours)

**Goal:** Visual debugging in 3D viewer.

**Tests:**

```lisp
(test debug-highlight-creates-markers
  "debug-highlight adds visual markers to viewer"
  (let ((part (test-part-with-highlighting)))
    ;; Verify highlighting data was generated
    (is (not (null (clad.viewer:get-highlights part))))))

(defpart test-part-with-highlighting ()
  (:body (make-box 100 100 50))

  (:on-face :type :planar
    (:debug-highlight :color :red)
    (:fillet 2.0d0)))
```

**Implementation:**

```lisp
;;;; src/viewer/server.lisp - Add highlighting support

(defparameter *debug-highlights* (make-hash-table :test 'eq)
  "Stores highlight data for shapes")

(defun add-highlight (shape entities color)
  "Register entities for highlighting in viewer"
  (push (list :entities entities :color color)
        (gethash shape *debug-highlights* '())))

(defun get-highlights (shape)
  "Get all highlight data for shape"
  (gethash shape *debug-highlights*))

;;;; src/dsl/defpart.lisp - Add :debug-highlight form

(defun expand-feature-form-at-compile-time (feature-form)
  (case (first feature-form)
    ;; Existing cases...

    (:debug-highlight
     (let ((args (rest feature-form)))
       (destructuring-bind (&key (color :red)) args
         `(let ((selection (clad.context:current-selection)))
            (clad.viewer:add-highlight
              (clad.context:current-shape)
              selection
              ,color)))))

    ...))
```

**Viewer Protocol Extension:**

Add to GLB export to include highlight metadata, rendered as colored wireframe overlay in viewer.

**Deliverables:**
- `inspect-selection` working in REPL
- `:debug-selection` printing during defpart build
- `:debug-highlight` showing selections in viewer
- Documentation with examples

---

### Phase 4: Lightweight Workplane Operations (Week 4-5)

**Goal:** Simple 2D operations on face planes without full sketch system.

**Deliverables:**
- `:on-face-plane` DSL form
- Auto-centering on face
- Auto-alignment to face normal
- 2D operations: circle, rectangle, polygon
- Extrude/cut perpendicular to face
- Full test coverage

#### Cycle 4.1: Face Plane Context (2-3 hours)

**Design Decision:** Add face-local coordinate system to context.

**Tests:**

```lisp
(test face-plane-basic
  "on-face-plane establishes local coordinate system"
  (let ((part (test-face-plane-part)))
    (is (not (null part)))))

(defpart test-face-plane-part ()
  (:body (make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; Now in local coords where (0,0) is face center, Z is face normal
    (:cut-circle 10 :depth 15)))  ; Circle at face center, cut downward
```

**Implementation:**

```lisp
;;;; src/context/workplane.lisp (ENHANCE EXISTING)

(defclass face-workplane (workplane)
  ((face :initarg :face
         :accessor workplane-face
         :documentation "Reference face for this workplane")
   (origin :initarg :origin
           :accessor workplane-origin
           :documentation "Origin point (face center)")
   (normal :initarg :normal
           :accessor workplane-normal
           :documentation "Z-axis (face normal)")
   (u-axis :initarg :u-axis
           :accessor workplane-u-axis
           :documentation "X-axis in plane")
   (v-axis :initarg :v-axis
           :accessor workplane-v-axis
           :documentation "Y-axis in plane"))
  (:documentation "Workplane aligned to a face"))

(defun make-face-workplane (face)
  "Create workplane from face geometry"
  (let* ((center (face-center face))
         (normal (face-normal face))
         ;; Choose U axis perpendicular to normal
         (u-axis (perpendicular-vector normal))
         ;; V axis completes right-handed system
         (v-axis (cross-product normal u-axis)))
    (make-instance 'face-workplane
                   :face face
                   :origin center
                   :normal normal
                   :u-axis u-axis
                   :v-axis v-axis)))

(defun perpendicular-vector (v)
  "Find any vector perpendicular to v"
  (let ((x (first v))
        (y (second v))
        (z (third v)))
    (if (< (abs z) 0.9)
        ;; Normal not too vertical, use (0,0,1) x normal
        (normalize (cross-product '(0 0 1) v))
        ;; Normal too vertical, use (1,0,0) x normal
        (normalize (cross-product '(1 0 0) v)))))

(defun transform-to-face-coords (workplane point-2d)
  "Transform 2D point in face plane to 3D world coordinates"
  (let ((origin (workplane-origin workplane))
        (u (workplane-u-axis workplane))
        (v (workplane-v-axis workplane))
        (x (first point-2d))
        (y (second point-2d)))
    (vector-add origin
                (vector-add (vector-scale u x)
                            (vector-scale v y)))))

(defun transform-to-world-coords (workplane shape-2d depth)
  "Transform 2D shape in face plane to 3D, extruded by depth"
  ;; Create 3D shape by extruding 2D profile along normal
  (let* ((profile-3d (map-2d-to-3d workplane shape-2d))
         (extrude-vec (vector-scale (workplane-normal workplane) depth)))
    (extrude-along-vector profile-3d extrude-vec)))
```

#### Cycle 4.2: Simple 2D Operations (3-4 hours)

**Tests:**

```lisp
(test cut-circle-on-face
  "cut-circle creates centered hole on face"
  (let ((part (part-with-face-circle)))
    ;; Verify hole exists at face center
    ...))

(defpart part-with-face-circle ()
  (:body (make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    (:cut-circle 10 :depth 15)))

(test add-rectangle-on-face
  "add-rectangle adds boss on face"
  (let ((part (part-with-face-rectangle)))
    ...))

(defpart part-with-face-rectangle ()
  (:body (make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    (:add-rectangle 40 30 :height 10)))
```

**Implementation:**

```lisp
;;;; src/dsl/defpart.lisp - Add :on-face-plane form

(defun expand-part-form-at-compile-time (form)
  (case (first form)
    ;; Existing cases...

    (:on-face-plane
     (let* ((args (rest form))
            (selector-spec '())
            (operation-forms '()))
       ;; Parse selector and operations
       (dolist (arg args)
         (if (and (not operation-forms) (not (listp arg)))
             (push arg selector-spec)
             (push arg operation-forms)))
       (setf selector-spec (nreverse selector-spec))
       (setf operation-forms (nreverse operation-forms))

       ;; Generate code
       `(progn
          ;; Select face
          (clad.context:select-faces ,@selector-spec)
          ;; Get first selected face
          (let* ((selection (clad.context:current-selection))
                 (face (first selection)))
            (unless face
              (error "No face selected for workplane"))
            ;; Create workplane from face
            (let ((workplane (clad.context:make-face-workplane face)))
              ;; Push onto workplane stack
              (clad.context:push-workplane workplane)
              ;; Execute operations in face-local coordinates
              ,@(mapcar #'expand-face-plane-operation operation-forms)
              ;; Pop workplane
              (clad.context:pop-workplane))))))

    ...))

(defun expand-face-plane-operation (op-form)
  "Expand operations in face-plane context"
  (case (first op-form)
    (:cut-circle
     (destructuring-bind (radius &key depth) (rest op-form)
       `(let* ((wp (clad.context:current-workplane))
               ;; Create 2D circle at origin (face center)
               (circle-2d (make-circle-2d-profile ,radius))
               ;; Transform to 3D and extrude
               (circle-3d (clad.context:transform-to-world-coords
                            wp circle-2d (- ,depth))))
          (clad.context:cut-op circle-3d))))

    (:add-circle
     (destructuring-bind (radius &key height) (rest op-form)
       `(let* ((wp (clad.context:current-workplane))
               (circle-2d (make-circle-2d-profile ,radius))
               (circle-3d (clad.context:transform-to-world-coords
                            wp circle-2d ,height)))
          (clad.context:union-op circle-3d))))

    (:cut-rectangle
     (destructuring-bind (width height &key depth) (rest op-form)
       `(let* ((wp (clad.context:current-workplane))
               (rect-2d (make-rectangle-2d-profile ,width ,height))
               (rect-3d (clad.context:transform-to-world-coords
                          wp rect-2d (- ,depth))))
          (clad.context:cut-op rect-3d))))

    (:add-rectangle
     (destructuring-bind (width height &key height-val) (rest op-form)
       `(let* ((wp (clad.context:current-workplane))
               (rect-2d (make-rectangle-2d-profile ,width ,height))
               (rect-3d (clad.context:transform-to-world-coords
                          wp rect-2d ,height-val)))
          (clad.context:union-op rect-3d))))

    (t (error "Unknown face-plane operation: ~A" (first op-form)))))

(defun make-circle-2d-profile (radius)
  "Create 2D circular profile for extrusion"
  ;; Uses OpenCASCADE to create wire in XY plane
  (clad.core:make-circle-wire '(0 0 0) radius :axis '(0 0 1)))

(defun make-rectangle-2d-profile (width height)
  "Create 2D rectangular profile for extrusion"
  (clad.core:make-rectangle-wire '(0 0 0) width height))
```

#### Cycle 4.3: Pattern Integration (2-3 hours)

**Goal:** Patterns work in face-plane context.

**Tests:**

```lisp
(test circular-pattern-on-face-plane
  "Circular pattern works in face-plane context"
  (let ((part (part-with-bolt-circle)))
    ;; Verify 8 holes around center
    ...))

(defpart part-with-bolt-circle ()
  (:body (make-box 150 150 20))

  (:on-face-plane :direction :+z :extreme :max
    (:circular-pattern :count 8 :radius 50
      (:cut-circle 3 :depth 15))))
```

**Implementation:** Patterns in face-plane context use 2D coordinates.

**Deliverables:**
- `:on-face-plane` working in DSL
- Auto-centering on face
- Auto-alignment to face normal
- Circle, rectangle operations
- Pattern support
- Full documentation

---

## Assembly Reference Resolution System

**Goal:** Resolve symbolic references like `:face-top` to actual geometry.

**Implementation Strategy:**

```lisp
;;;; src/assembly/references.lisp (NEW FILE)

(defclass component-reference ()
  ((component :initarg :component
              :accessor ref-component
              :documentation "Component name")
   (selector :initarg :selector
             :accessor ref-selector
             :documentation "Selector specification for feature"))
  (:documentation "Reference to a feature on a component"))

(defun resolve-reference (assembly component-name ref-spec)
  "Resolve symbolic reference to actual geometry.

  Examples:
    :face-top        → (:direction :+z :extreme :max)
    :face-bottom     → (:direction :-z :extreme :min)
    :axis            → (center line of cylindrical part)
    :hole            → (first cylindrical face)
    (:face :direction :+x)  → Custom selector"

  (let ((component (clad.assembly:get-component assembly component-name)))
    (unless component
      (error "Component not found: ~A" component-name))

    (let ((part (clad.assembly:component-part component)))
      (cond
        ;; Symbolic shortcuts
        ((eq ref-spec :face-top)
         (resolve-reference assembly component-name
                           '(:face :direction :+z :extreme :max)))

        ((eq ref-spec :face-bottom)
         (resolve-reference assembly component-name
                           '(:face :direction :-z :extreme :min)))

        ((eq ref-spec :axis)
         ;; For cylindrical parts, get center axis
         (get-cylindrical-axis part))

        ;; Explicit selector
        ((and (listp ref-spec) (eq (first ref-spec) :face))
         (let ((faces (clad.shapes:faces part))
               (selector-args (rest ref-spec)))
           (apply #'clad.selectors:select faces selector-args)))

        ((and (listp ref-spec) (eq (first ref-spec) :edge))
         (let ((edges (clad.shapes:edges part))
               (selector-args (rest ref-spec)))
           (apply #'clad.selectors:select edges selector-args)))

        (t (error "Unknown reference format: ~A" ref-spec))))))

;; Predefined reference shortcuts
(defparameter *reference-shortcuts*
  '((:face-top . (:face :direction :+z :extreme :max))
    (:face-bottom . (:face :direction :-z :extreme :min))
    (:face-front . (:face :direction :+y :extreme :max))
    (:face-back . (:face :direction :-y :extreme :min))
    (:face-right . (:face :direction :+x :extreme :max))
    (:face-left . (:face :direction :-x :extreme :min))
    (:top-edge . (:edge :at-z :max))
    (:bottom-edge . (:edge :at-z :min))))
```

---

## File Structure

### New Files to Create

```
tests/
  selector-combinator-tests.lisp    # Boolean combinator tests
  position-selector-tests.lisp       # Position selector tests
  selector-inspection-tests.lisp     # Debug/inspection tests
  face-plane-tests.lisp              # Lightweight workplane tests

src/selectors/
  position.lisp                      # NEW - Position-based selectors
  inspection.lisp                    # NEW - Debug/inspection tools

src/assembly/
  references.lisp                    # NEW - Reference resolution

examples/
  07-advanced-selectors.lisp         # NEW - Combinator examples
  08-face-operations.lisp            # NEW - Face-plane examples
```

### Files to Modify

```
src/selectors/
  combinators.lisp                   # Add AND/OR/NOT classes
  api.lisp                           # Add combinator parsing

src/context/
  workplane.lisp                     # Add face-workplane class
  context.lisp                       # Add face-plane operations

src/dsl/
  defpart.lisp                       # Add :on-face-plane, :debug-selection

src/viewer/
  server.lisp                        # Add highlighting support

clad.asd                             # Add new files to system definition
```

---

## Testing Strategy

### Test Organization

Each phase has dedicated test file:
- `selector-combinator-tests.lisp` - ~30 tests
- `position-selector-tests.lisp` - ~25 tests
- `selector-inspection-tests.lisp` - ~15 tests
- `face-plane-tests.lisp` - ~20 tests

**Total:** ~90 new tests

### Test Categories

1. **Unit Tests** - Test individual selector classes
2. **Integration Tests** - Test selectors in defpart DSL
3. **Regression Tests** - Ensure existing functionality unchanged
4. **Performance Tests** - Verify no slowdowns on large parts

### Coverage Goals

- **Line Coverage:** > 90%
- **Branch Coverage:** > 85%
- **Critical Paths:** 100%

### CI Integration

```lisp
;; Run all tests
(asdf:test-system :clad)

;; Run specific suite
(fiveam:run! :clad-selector-tests)
```

---

## Documentation Requirements

### Code Documentation

Each function must have:
- Docstring with description
- Parameter documentation
- Return value description
- Usage examples
- Notes about edge cases

Example:

```lisp
(defun select (shape-list selector-spec &rest args)
  "Select shapes from SHAPE-LIST matching SELECTOR-SPEC.

  Args:
    shape-list - List of shapes to filter
    selector-spec - Selector type (:direction, :and, :or, :at-x, etc.)
    args - Arguments depending on selector type

  Returns: Filtered list of shapes

  Examples:
    ;; Direction selector
    (select faces :direction :+z :extreme :max)

    ;; AND combinator
    (select faces (:and :type :planar :area :> 1000))

    ;; Position selector
    (select faces :at-z 50.0 :tolerance 0.1)

  Notes:
    - Returns empty list if no matches
    - Preserves order of input list
    - Thread-safe (pure function)"
  ...)
```

### User Documentation

1. **SELECTOR_REFERENCE.md** - Update with new selectors
2. **USER_GUIDE.md** - Add sections on combinators and debugging
3. **examples/** - Add 2 new example files
4. **API_REFERENCE.md** (new) - Complete API documentation

### Example Code

Each feature needs working examples:

```lisp
;;;; examples/07-advanced-selectors.lisp

;; Boolean Combinators
(defpart advanced-filtering ()
  (:body (make-complex-part))

  ;; Fillet large horizontal faces only
  (:on-face (:and :type :planar
                  :area :> 1000
                  (:or :direction :+z :direction :-z))
    (:fillet 5.0d0)))

;; Position-Based Selection
(defpart multi-level-part ()
  (:body (make-stepped-tower))

  ;; Operations only on middle level (Z between 20 and 40)
  (:on-face (:and :between-z 20 40 :type :planar)
    (:cut-circle 10 :depth 15)))

;; Debugging
(defpart debug-example ()
  (:body (make-box 100 100 50))

  (:debug-selection :on-face :type :planar)  ; Print count

  (:on-face :type :planar
    (:debug-highlight :color :red)           ; Show in viewer
    (:fillet 2.0d0)))
```

---

## Success Criteria

### Phase 1 Complete When:

- [ ] All combinator tests pass (30+ tests)
- [ ] Works in both context API and defpart DSL
- [ ] Documentation updated
- [ ] Examples added
- [ ] No performance regression
- [ ] Code review passed

### Phase 2 Complete When:

- [ ] All position selector tests pass (25+ tests)
- [ ] Works with combinators
- [ ] Integrated in DSL
- [ ] Documentation complete
- [ ] Examples demonstrate all features

### Phase 3 Complete When:

- [ ] REPL inspection tools work
- [ ] DSL debug forms work
- [ ] Viewer highlighting functional
- [ ] Documentation with screenshots
- [ ] Video tutorial created (optional)

### Phase 4 Complete When:

- [ ] Face-plane operations work
- [ ] Auto-centering and alignment correct
- [ ] Pattern integration complete
- [ ] All tests pass
- [ ] User guide updated

### Overall Project Complete When:

- [ ] All 4 phases complete
- [ ] All tests pass (90+ tests)
- [ ] Documentation comprehensive
- [ ] Examples demonstrate all features
- [ ] Performance benchmarks met
- [ ] No regressions in existing functionality
- [ ] Code review approved
- [ ] Ready for user testing

---

## Risk Mitigation

### Technical Risks

**Risk:** OpenCASCADE API limitations for face geometry queries

**Mitigation:**
- Test with simple geometries first
- Add fallback methods for complex cases
- Document known limitations

**Risk:** Performance degradation with complex combinators

**Mitigation:**
- Profile early and often
- Optimize hot paths
- Add caching where appropriate
- Benchmark against target parts

**Risk:** Workplane coordinate transforms break on non-planar faces

**Mitigation:**
- Validate face is planar before creating workplane
- Provide clear error messages
- Add approximation for near-planar faces

### Schedule Risks

**Risk:** Phase takes longer than estimated

**Mitigation:**
- Each phase is independently useful
- Can ship incrementally
- Buffer time built into estimates
- Prioritize core features over nice-to-haves

---

## Future Extensions

**Not in this plan, but natural follow-ups:**

1. **Topological Selectors**
   - `:adjacent-to` - Faces adjacent to selection
   - `:opposite-to` - Face opposite to selected
   - `:connected-by` - Edges connecting two faces

2. **Custom Predicates in DSL**
   ```lisp
   (:on-face (:lambda (f) (> (face-area f) 1000))
     (:fillet 2.0d0))
   ```

3. **Selection History**
   - Named selections: `(:save-selection :bolt-holes)`
   - Reuse later: `(:load-selection :bolt-holes)`

4. **Solver-Based Face Positioning**
   - Constraint-based placement on faces
   - Automatic centering, alignment

5. **Sketch Integration with Face-Plane**
   ```lisp
   (:on-face-plane :direction :+z :extreme :max
     (:sketch "bolt-pattern"
       (:circular-pattern :count 8 :radius 50
         (:circle :radius 3))))
   ```

---

## Autonomous Implementation Notes

**For AI Agent:**

1. **Start with tests** - Write all tests for a cycle before implementing
2. **Minimal implementation** - Make tests pass with simplest code
3. **Refactor** - Clean up after tests pass
4. **Document** - Add docstrings and examples
5. **Verify** - Run full test suite after each cycle
6. **Commit** - Git commit after each successful cycle

**Red-Green-Refactor Cycle:**

```
1. RED: Write failing test
   → Run: (fiveam:run! :clad-selector-tests)
   → Verify failure

2. GREEN: Implement minimal code
   → Run tests until pass
   → No refactoring yet!

3. REFACTOR: Improve code quality
   → Extract functions
   → Remove duplication
   → Improve names
   → Run tests (must still pass)

4. DOCUMENT: Add docstrings, examples

5. COMMIT: Git commit with descriptive message
```

**Each commit message should include:**
- What: Concise description
- Why: Reason for change
- Tests: Which tests now pass

Example:
```
Add AND combinator for selectors

Implements logical AND to combine multiple selector criteria.
Useful for complex queries like "planar faces with area > 1000".

Tests passing:
- and-combinator-basic
- and-combinator-empty
- and-combinator-nested
```

---

## Appendix: Helper Functions Needed

### Geometry Queries

```lisp
;; Add to src/shapes/methods.lisp

(defun face-center (face)
  "Get center point of face's bounding box"
  ...)

(defun face-normal (face)
  "Get unit normal vector of planar face"
  ...)

(defun face-area (face)
  "Get surface area of face"
  ...)

(defun edge-length (edge)
  "Get length of edge"
  ...)

(defun planar-face-p (face)
  "Test if face is planar"
  ...)

(defun cylindrical-face-p (face)
  "Test if face is cylindrical"
  ...)

(defun line-edge-p (edge)
  "Test if edge is straight line"
  ...)

(defun circle-edge-p (edge)
  "Test if edge is circle"
  ...)
```

### Vector Math

```lisp
;; Add to src/core/math-utils.lisp (new file)

(defun vector-add (v1 v2)
  "Add two 3D vectors"
  ...)

(defun vector-scale (v scalar)
  "Scale vector by scalar"
  ...)

(defun cross-product (v1 v2)
  "Cross product of two 3D vectors"
  ...)

(defun normalize (v)
  "Normalize vector to unit length"
  ...)

(defun distance-3d (p1 p2)
  "Euclidean distance between points"
  ...)
```

---

**End of Implementation Plan**

This plan is designed for autonomous AI agent execution following strict TDD methodology. Each phase is independently testable and valuable. Implementation can proceed incrementally with frequent verification.
