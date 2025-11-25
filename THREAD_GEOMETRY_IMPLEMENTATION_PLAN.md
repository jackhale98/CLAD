# 3D Thread Geometry Implementation Plan

**Project:** CLAD - Production-Ready Thread Modeling
**Date:** November 2025
**Approach:** Test-Driven Development (TDD)

## Executive Summary

This plan outlines the implementation of **geometrically accurate 3D thread modeling** in CLAD to enable production-ready CAD work. Currently, CLAD uses simplified placeholder geometries (basic cylinders). This implementation will create actual helical thread profiles that accurately represent real-world threaded features.

**Goal:** Generate threads that:
- Match ISO 68-1, ISO 724, and ASME B1.1 standards exactly
- Can be manufactured from the exported STEP files
- Display correctly in all CAD viewers
- Perform well (no excessive geometry complexity)

---

## 1. Technical Background

### 1.1 Thread Geometry Fundamentals

#### ISO Metric Thread Profile (ISO 68-1)

**Key Dimensions:**
```
P     = Pitch (axial distance between crests)
D     = Major diameter (nominal diameter, e.g., M6 = 6mm)
H     = Fundamental triangle height = P × (√3/2) ≈ P × 0.866025
D2    = Pitch diameter = D - (3/8) × H = D - 0.6495 × P
D1    = Minor diameter = D - (5/8) × H = D - 1.0825 × P

Thread angle = 60° (symmetric V-profile)
Crest truncation = H/8 (flat at top)
Root truncation = H/4 (flat at bottom)
```

**Profile Geometry:**
- External threads: Truncated V-profile pointing outward
- Internal threads: Truncated V-profile pointing inward
- The profile repeats helically with pitch P

### 1.2 OpenCascade Implementation Strategy

**Approach: Helical Sweep**

1. **Create Thread Profile** (2D wire):
   - Construct truncated V-shape in YZ plane
   - Use line segments with precise coordinates
   - Profile width = pitch P
   - Profile depth based on thread type

2. **Create Helical Path** (3D curve):
   - Use `GCE_MakeHelix` or construct parametric helix
   - Start point, axis, pitch, height
   - Radius = pitch diameter for accuracy

3. **Sweep Profile Along Helix**:
   - Use `BRepOffsetAPI_MakePipe` with auxiliary spine
   - Keep profile perpendicular to helix path
   - Generate helical surface

4. **Boolean Operations**:
   - External threads: Intersect with base cylinder (D_major)
   - Internal threads: Cut helical shape from hole cylinder
   - Ensure clean topology

---

## 2. Implementation Phases (TDD Approach)

### Phase 1: Profile Geometry Engine (Week 1)

**Module:** `src/features/thread-profile.lisp`

#### Tests to Write First:

```lisp
;; File: tests/thread-profile-tests.lisp

(deftest test-thread-profile-m6-dimensions
  "Verify M6 thread profile has correct dimensions"
  (let* ((profile (clad.features.thread-profile:make-iso-metric-profile :m6 :external))
         (params (clad.features.thread-profile:profile-parameters profile)))
    (assert-equal 6.0 (getf params :major-diameter) 0.001)
    (assert-equal 1.0 (getf params :pitch))
    (assert-equal 4.917 (getf params :minor-diameter) 0.001)
    (assert-equal 5.350 (getf params :pitch-diameter) 0.001)))

(deftest test-thread-profile-vertices
  "Verify thread profile geometry vertices are correct"
  (let* ((profile (clad.features.thread-profile:make-iso-metric-profile :m6 :external))
         (vertices (clad.features.thread-profile:get-profile-vertices profile)))
    ;; External thread profile should have 6 vertices (truncated V)
    (assert-equal 6 (length vertices))
    ;; First vertex at root (0, minor-radius)
    (assert-point-equal '(0.0 2.4585 0.0) (first vertices) 0.001)
    ;; Verify V-angle is 60 degrees
    (let ((angle (calculate-v-angle vertices)))
      (assert-equal 60.0 angle 0.1))))

(deftest test-internal-vs-external-profile
  "Verify internal thread profile is inverted from external"
  (let ((ext (clad.features.thread-profile:make-iso-metric-profile :m8 :external))
        (int (clad.features.thread-profile:make-iso-metric-profile :m8 :internal)))
    ;; Internal profile should point outward (larger at thread)
    (assert-true (is-profile-inverted ext int))))

(deftest test-thread-profile-to-wire
  "Verify profile converts to valid OCCT wire"
  (let* ((profile (clad.features.thread-profile:make-iso-metric-profile :m6 :external))
         (wire (clad.features.thread-profile:profile-to-wire profile)))
    (assert-true (occt-is-valid-wire wire))
    (assert-true (occt-is-closed-wire wire))))
```

#### Implementation:

```lisp
;; File: src/features/thread-profile.lisp

(in-package :clad.features.thread-profile)

(defclass thread-profile ()
  ((spec :initarg :spec :reader profile-spec)
   (type :initarg :type :reader profile-type) ; :external or :internal
   (vertices :initarg :vertices :reader profile-vertices)
   (parameters :initarg :parameters :reader profile-parameters)))

(defun make-iso-metric-profile (thread-spec profile-type)
  "Create ISO 68-1 metric thread profile geometry.

  THREAD-SPEC: Thread specification keyword (e.g., :m6, :m8x1.0)
  PROFILE-TYPE: Either :external or :internal

  Returns: THREAD-PROFILE object with calculated vertices"

  (let* ((params (clad.features:get-thread-spec thread-spec))
         (pitch (getf params :pitch))
         (major-d (getf params :major-diameter))
         (h (* pitch (sqrt 3) 0.5))  ; Fundamental triangle height
         (minor-d (- major-d (* 1.0825 pitch)))  ; D - 5H/8
         (pitch-d (- major-d (* 0.6495 pitch)))  ; D - 3H/8

         ;; Calculate profile vertices in YZ plane (sweep will be along X)
         (vertices (calculate-profile-vertices
                    major-d minor-d pitch-d pitch h profile-type)))

    (make-instance 'thread-profile
                   :spec thread-spec
                   :type profile-type
                   :vertices vertices
                   :parameters (list :major-diameter major-d
                                    :minor-diameter minor-d
                                    :pitch-diameter pitch-d
                                    :pitch pitch
                                    :fundamental-height h))))

(defun calculate-profile-vertices (major-d minor-d pitch-d pitch h type)
  "Calculate the 6 vertices of truncated V-profile.

  Profile layout (external thread, YZ plane):
       Y axis (radial)
       ^
       |     /\    <- Crest (truncated by H/8)
       |    /  \
       |   /    \
       |  /      \
       | /________\ <- Root (truncated by H/4)
       +-----------> Z axis (along thread)

  Returns: List of 6 (Y Z) coordinate pairs"

  (let* ((major-r (/ major-d 2.0))
         (minor-r (/ minor-d 2.0))
         (pitch-r (/ pitch-d 2.0))

         ;; Truncation amounts
         (crest-flat (* h 0.125))   ; H/8
         (root-flat (* h 0.25))     ; H/4

         ;; V-angle is 60°, so half-angle is 30°
         (half-angle (/ pi 6.0))    ; 30° in radians

         ;; Calculate radial heights
         (thread-depth (* h 0.625)) ; 5H/8
         (crest-r (if (eq type :external)
                      (- major-r crest-flat)
                      (+ minor-r root-flat)))
         (root-r (if (eq type :external)
                     (+ minor-r root-flat)
                     (- major-r crest-flat))))

    (if (eq type :external)
        ;; External thread profile (6 vertices, one pitch period)
        (list
         (list root-r 0.0)                          ; Start at root, left
         (list crest-r (* pitch 0.25))              ; Climb to crest
         (list crest-r (* pitch 0.75))              ; Crest flat
         (list root-r pitch)                        ; Descend to root
         (list root-r pitch)                        ; Root continues to next thread
         (list root-r 0.0))                         ; Close (connects to start)

        ;; Internal thread profile (inverted)
        (list
         (list crest-r 0.0)
         (list root-r (* pitch 0.25))
         (list root-r (* pitch 0.75))
         (list crest-r pitch)
         (list crest-r pitch)
         (list crest-r 0.0)))))

(defun profile-to-wire (profile &optional (z-position 0.0))
  "Convert thread profile to OpenCascade wire.

  PROFILE: Thread profile object
  Z-POSITION: Axial position for the profile (default 0.0)

  Returns: OCCT TopoDS_Wire"

  (let* ((vertices (profile-vertices profile))
         (edges '()))

    ;; Create line segments between consecutive vertices
    (loop for i from 0 below (1- (length vertices))
          for v1 = (nth i vertices)
          for v2 = (nth (1+ i) vertices)
          do (let ((p1 (occt-make-gp-pnt (first v1) z-position (second v1)))
                   (p2 (occt-make-gp-pnt (first v2) z-position (second v2))))
               (push (occt-make-edge p1 p2) edges)))

    ;; Close the wire
    (let* ((first-v (first vertices))
           (last-v (car (last vertices)))
           (p-first (occt-make-gp-pnt (first first-v) z-position (second first-v)))
           (p-last (occt-make-gp-pnt (first last-v) z-position (second last-v))))
      (push (occt-make-edge p-last p-first) edges))

    ;; Build wire from edges
    (occt-make-wire (reverse edges))))
```

**Success Criteria:**
- All profile dimension tests pass
- Profile geometry is mathematically correct
- Both external and internal profiles generate properly
- Wire conversion works without errors

---

### Phase 2: Helical Path Generation (Week 2)

**Module:** `src/features/helical-path.lisp`

#### Tests to Write First:

```lisp
;; File: tests/helical-path-tests.lisp

(deftest test-helix-creation-basic
  "Verify basic helix creation with correct pitch and height"
  (let ((helix (clad.features.helical-path:make-helix
                :pitch 1.0
                :radius 2.5
                :height 10.0
                :axis '(0 0 1))))
    (assert-true (occt-is-valid-edge helix))
    (assert-equal 10.0 (helix-total-height helix) 0.001)
    (assert-equal 10 (helix-number-of-turns helix))))

(deftest test-helix-start-point
  "Verify helix starts at correct position"
  (let ((helix (clad.features.helical-path:make-helix
                :pitch 1.5
                :radius 3.0
                :height 15.0
                :start-angle 0.0)))
    (let ((start-pt (occt-edge-first-point helix)))
      (assert-point-equal '(3.0 0.0 0.0) start-pt 0.001))))

(deftest test-helix-handedness
  "Verify right-hand thread helix direction"
  (let ((helix (clad.features.helical-path:make-helix
                :pitch 1.0
                :radius 2.5
                :height 10.0
                :right-handed t)))
    ;; After 90°, should be at (0, 2.5, 0.25)
    (let ((pt-90 (occt-edge-point-at-parameter helix 0.25)))
      (assert-point-equal '(0.0 2.5 0.25) pt-90 0.05))))

(deftest test-helix-for-thread-length
  "Verify helix extends properly for given thread length"
  (let ((helix (clad.features.helical-path:make-helix-for-thread
                :thread-spec :m6
                :length 30.0)))
    ;; M6 has pitch 1.0, so 30mm length = 30 turns
    (assert-equal 30 (helix-number-of-turns helix))))
```

#### Implementation:

```lisp
;; File: src/features/helical-path.lisp

(in-package :clad.features.helical-path)

(defun make-helix (&key pitch radius height
                        (axis '(0 0 1))
                        (start-point '(0 0 0))
                        (start-angle 0.0)
                        (right-handed t))
  "Create a helical curve using OpenCascade.

  PITCH: Axial advance per full revolution (mm)
  RADIUS: Helix radius (mm)
  HEIGHT: Total axial height (mm)
  AXIS: Direction vector for helix axis
  START-POINT: Starting point of helix
  START-ANGLE: Starting angle in radians (default 0)
  RIGHT-HANDED: T for right-hand helix, NIL for left-hand

  Returns: OCCT TopoDS_Edge representing helix"

  (let* ((num-turns (/ height pitch))
         (total-angle (* 2 pi num-turns))
         (total-angle-deg (/ (* total-angle 180.0) pi)))

    ;; Create helix using parametric approach
    ;; Parameter t ∈ [0, 1]
    ;; x(t) = radius * cos(total-angle * t + start-angle)
    ;; y(t) = radius * sin(total-angle * t + start-angle) * (if right-handed 1 -1)
    ;; z(t) = height * t

    (create-parametric-helix-edge
     radius total-angle height start-angle right-handed)))

(defun create-parametric-helix-edge (radius total-angle height start-angle right-handed)
  "Create helix using OCCT Geom_BSplineCurve.

  This creates a mathematically precise helical curve using control points."

  (let* ((num-points 100) ; Sufficient for smooth helix
         (points '())
         (hand-factor (if right-handed 1.0 -1.0)))

    ;; Generate helix points
    (dotimes (i (1+ num-points))
      (let* ((t-param (/ (float i) num-points))
             (angle (+ start-angle (* total-angle t-param)))
             (x (* radius (cos angle)))
             (y (* radius (sin angle) hand-factor))
             (z (* height t-param))
             (pt (occt-make-gp-pnt x y z)))
        (push pt points)))

    ;; Create B-spline curve through points
    (occt-make-bspline-through-points (reverse points))))

(defun make-helix-for-thread (&key thread-spec length (lead-in-turns 0.25) (lead-out-turns 0.25))
  "Create helix specifically for thread modeling.

  THREAD-SPEC: Thread specification (e.g., :m6)
  LENGTH: Thread length in mm
  LEAD-IN-TURNS: Partial turns at start for smooth entry (default 0.25)
  LEAD-OUT-TURNS: Partial turns at end for smooth exit (default 0.25)

  Returns: OCCT TopoDS_Edge representing thread helix"

  (let* ((params (clad.features:get-thread-spec thread-spec))
         (pitch (getf params :pitch))
         (pitch-diameter (getf params :pitch-diameter))
         (radius (/ pitch-diameter 2.0))

         ;; Total height includes lead-in and lead-out
         (total-turns (+ (/ length pitch) lead-in-turns lead-out-turns))
         (total-height (* total-turns pitch)))

    (make-helix :pitch pitch
                :radius radius
                :height total-height
                :right-handed t))) ; ISO metric threads are right-handed
```

**Success Criteria:**
- Helix geometry is mathematically correct
- Pitch and radius match specifications
- Start and end points are accurate
- Both right and left-handed helices work

---

### Phase 3: Helical Sweep Operation (Week 3)

**Module:** `src/features/helical-sweep.lisp`

#### Tests to Write First:

```lisp
;; File: tests/helical-sweep-tests.lisp

(deftest test-sweep-profile-along-helix
  "Verify sweeping thread profile along helical path"
  (let* ((profile-wire (make-test-thread-profile :m6))
         (helix (make-test-helix :m6 30.0))
         (swept-surface (clad.features.helical-sweep:sweep-profile-along-path
                         profile-wire helix)))
    (assert-true (occt-is-valid-shape swept-surface))
    (assert-equal 'shell (occt-shape-type swept-surface))))

(deftest test-thread-surface-normals
  "Verify swept surface has correct normal directions"
  (let* ((thread-surface (create-test-thread-surface :m6 :external))
         (mid-point-normal (get-surface-normal-at-midpoint thread-surface)))
    ;; Normal should point outward for external thread
    (assert-true (vector-points-outward mid-point-normal))))

(deftest test-sweep-preserves-profile-shape
  "Verify profile shape is preserved during sweep"
  (let* ((profile (make-test-thread-profile :m8))
         (helix (make-test-helix :m8 20.0))
         (swept (clad.features.helical-sweep:sweep-profile-along-path profile helix)))
    ;; Sample cross-sections at different heights should match original profile
    (assert-profiles-match profile (sample-cross-section swept 10.0) 0.01)))
```

#### Implementation:

```lisp
;; File: src/features/helical-sweep.lisp

(in-package :clad.features.helical-sweep)

(defun sweep-profile-along-path (profile-wire helix-edge &key (auxiliary-spine nil))
  "Sweep a profile wire along a helical path using BRepOffsetAPI_MakePipe.

  PROFILE-WIRE: TopoDS_Wire defining the thread profile cross-section
  HELIX-EDGE: TopoDS_Edge defining the helical sweep path
  AUXILIARY-SPINE: Optional auxiliary curve for orientation control

  Returns: TopoDS_Shape (typically a shell)"

  (let ((pipe-maker (occt-make-pipe-object)))

    ;; Set the spine (helical path)
    (occt-pipe-set-spine pipe-maker helix-edge)

    ;; Set the profile (thread cross-section)
    (occt-pipe-set-profile pipe-maker profile-wire)

    ;; Use Frenet frame mode to keep profile perpendicular to helix
    ;; This is critical for thread geometry
    (occt-pipe-set-mode pipe-maker :frenet)

    ;; Build the pipe
    (occt-pipe-build pipe-maker)

    ;; Check for errors
    (unless (occt-pipe-is-done pipe-maker)
      (error "Helical sweep failed: ~A" (occt-pipe-error pipe-maker)))

    ;; Return the resulting shape
    (occt-pipe-shape pipe-maker)))

(defun make-thread-surface (thread-spec length profile-type)
  "Create a helical thread surface by sweeping the profile.

  THREAD-SPEC: Thread specification (e.g., :m6)
  LENGTH: Thread length in mm
  PROFILE-TYPE: :external or :internal

  Returns: TopoDS_Shape representing the thread surface"

  (let* ((profile (clad.features.thread-profile:make-iso-metric-profile
                   thread-spec profile-type))
         (profile-wire (clad.features.thread-profile:profile-to-wire profile))
         (helix (clad.features.helical-path:make-helix-for-thread
                 :thread-spec thread-spec
                 :length length))
         (thread-surface (sweep-profile-along-path profile-wire helix)))

    thread-surface))
```

**Success Criteria:**
- Sweep operation completes without errors
- Resulting surface is valid
- Profile shape is preserved along the helix
- Surface normals are correct

---

### Phase 4: Boolean Integration for Final Threads (Week 4)

**Module:** `src/features/thread-boolean.lisp`

#### Tests to Write First:

```lisp
;; File: tests/thread-boolean-tests.lisp

(deftest test-external-thread-creation
  "Verify complete external thread creation"
  (let ((threaded-shaft (clad.features:make-external-thread
                         :m6 :length 30.0 :full-geometry t)))
    (assert-true (occt-is-valid-solid threaded-shaft))
    ;; Check major diameter
    (assert-equal 6.0 (get-max-diameter threaded-shaft) 0.1)
    ;; Check minor diameter
    (assert-equal 4.917 (get-min-diameter threaded-shaft) 0.1)
    ;; Check overall length
    (assert-equal 30.0 (get-shape-height threaded-shaft) 0.5)))

(deftest test-internal-thread-creation
  "Verify complete internal thread (tapped hole) creation"
  (let ((tapped-hole (clad.features:make-internal-thread
                      :m8 :depth 25.0 :full-geometry t)))
    (assert-true (occt-is-valid-solid tapped-hole))
    ;; This should be a negative space (hole)
    (assert-true (is-negative-volume tapped-hole))))

(deftest test-thread-boolean-with-base
  "Verify threading a pre-existing cylinder"
  (let* ((base-cylinder (clad.core:make-cylinder 3.0 30.0))
         (threaded (clad.features:add-external-thread-to-shape
                    base-cylinder :m6 :length 25.0 :full-geometry t)))
    (assert-true (occt-is-valid-solid threaded))
    ;; Volume should be less than original (material removed)
    (assert-true (< (shape-volume threaded) (shape-volume base-cylinder)))))

(deftest test-thread-geometry-export
  "Verify threaded parts export to STEP correctly"
  (let ((threaded-bolt (clad.features:make-external-thread :m6 :length 30.0 :full-geometry t))
        (temp-file "/tmp/test-thread.step"))
    (clad.export:export-step threaded-bolt temp-file)
    (assert-true (probe-file temp-file))
    ;; Verify file is valid STEP and contains thread geometry
    (let ((reimported (clad.import:import-step temp-file)))
      (assert-true (occt-is-valid-solid reimported)))))
```

#### Implementation:

```lisp
;; File: src/features/thread-boolean.lisp

(in-package :clad.features.thread-boolean)

(defun make-external-thread-solid (thread-spec length &key (full-geometry t))
  "Create a complete external thread (bolt/stud) as a solid.

  THREAD-SPEC: Thread specification (e.g., :m6)
  LENGTH: Thread length in mm
  FULL-GEOMETRY: T to create actual helical geometry, NIL for simplified cylinder

  Returns: TopoDS_Solid representing the threaded shaft"

  (if (not full-geometry)
      ;; Simplified representation (current implementation)
      (let* ((params (clad.features:get-thread-spec thread-spec))
             (major-d (getf params :major-diameter)))
        (clad.core:make-cylinder (/ major-d 2.0) length))

      ;; Full helical geometry
      (let* ((params (clad.features:get-thread-spec thread-spec))
             (major-d (getf params :major-diameter))
             (minor-d (getf params :minor-diameter))

             ;; Create base cylinder at major diameter
             (major-cylinder (clad.core:make-cylinder (/ major-d 2.0) length))

             ;; Create helical thread surface
             (thread-surface (clad.features.helical-sweep:make-thread-surface
                             thread-spec length :external))

             ;; Convert surface to solid (thicken slightly)
             (thread-solid (occt-make-solid-from-shell thread-surface))

             ;; Intersect thread with major cylinder to get final shape
             (final-thread (clad.core:intersect-shapes major-cylinder thread-solid)))

        ;; Ensure we have a valid solid
        (unless (occt-is-valid-solid final-thread)
          (error "Failed to create valid thread solid for ~A" thread-spec))

        final-thread)))

(defun make-internal-thread-solid (thread-spec depth &key (full-geometry t) (hole-diameter nil))
  "Create a complete internal thread (tapped hole) as a cutting solid.

  THREAD-SPEC: Thread specification (e.g., :m8)
  DEPTH: Thread depth in mm
  FULL-GEOMETRY: T to create actual helical geometry, NIL for simplified cylinder
  HOLE-DIAMETER: Pre-drilled hole diameter (defaults to tap drill size)

  Returns: TopoDS_Solid representing the threaded hole (for cutting)"

  (if (not full-geometry)
      ;; Simplified representation
      (let* ((params (clad.features:get-thread-spec thread-spec))
             (major-d (getf params :major-diameter)))
        (clad.core:make-cylinder (/ major-d 2.0) depth))

      ;; Full helical geometry
      (let* ((params (clad.features:get-thread-spec thread-spec))
             (major-d (getf params :major-diameter))
             (tap-drill (or hole-diameter (clad.features:tap-drill-size thread-spec)))

             ;; Create hole cylinder at major diameter (clearance for threads)
             (hole-cylinder (clad.core:make-cylinder (/ major-d 2.0) depth))

             ;; Create helical thread surface (internal profile)
             (thread-surface (clad.features.helical-sweep:make-thread-surface
                             thread-spec depth :internal))

             ;; Convert to solid
             (thread-solid (occt-make-solid-from-shell thread-surface))

             ;; Union thread with hole cylinder
             (final-thread (clad.core:fuse-shapes hole-cylinder thread-solid)))

        ;; Ensure we have a valid solid for cutting
        (unless (occt-is-valid-solid final-thread)
          (error "Failed to create valid internal thread solid for ~A" thread-spec))

        final-thread)))
```

**Success Criteria:**
- External threads create valid solids
- Internal threads create valid cutting solids
- Boolean operations complete successfully
- Threads integrate with existing geometry
- STEP export/import works correctly

---

### Phase 5: DSL Integration (Week 5)

#### Update existing defpart DSL to support full geometry flag:

```lisp
;; File: src/features/threads.lisp (update existing functions)

(defun make-external-thread (thread-spec &key (length 10) (full-geometry *default-thread-geometry*))
  "Create external thread (bolt/stud).

  THREAD-SPEC: Keyword like :m6, :m8, :1/4-20
  LENGTH: Thread length in mm
  FULL-GEOMETRY: T for helical geometry, NIL for simplified (default from *default-thread-geometry*)

  Returns: TopoDS_Shape"

  (clad.features.thread-boolean:make-external-thread-solid thread-spec length :full-geometry full-geometry))

(defun make-internal-thread (thread-spec &key (depth 10) (full-geometry *default-thread-geometry*))
  "Create internal thread (tapped hole).

  THREAD-SPEC: Keyword like :m6, :m8
  DEPTH: Thread depth in mm
  FULL-GEOMETRY: T for helical geometry, NIL for simplified (default from *default-thread-geometry*)

  Returns: TopoDS_Shape (for cutting operation)"

  (clad.features.thread-boolean:make-internal-thread-solid thread-spec depth :full-geometry full-geometry))

;; Global setting for thread geometry detail level
(defvar *default-thread-geometry* nil
  "Default thread geometry mode:
  NIL - Simplified cylinders (fast, for design work)
  T   - Full helical geometry (slow, for production)")

(defun set-thread-geometry-mode (mode)
  "Set global thread geometry mode.

  MODE: :simplified or :full

  Example:
    (set-thread-geometry-mode :full)    ; Enable production geometry
    (set-thread-geometry-mode :simplified) ; Fast design mode"

  (setf *default-thread-geometry* (eq mode :full))
  (format t "Thread geometry mode set to: ~A~%" mode))
```

#### Integration with defpart:

```lisp
;; Example usage in defpart

(clad.dsl:defpart production-bolt
    ((thread-spec :m8)
     (thread-length 30)
     (head-diameter 13)
     (head-height 5))
  "M8 bolt with hex head and full thread geometry"

  ;; Hex head
  (:body (clad.core:make-cylinder (/ head-diameter 2) head-height))

  ;; Threaded shaft with FULL geometry
  (:on-face :direction :-z :extreme :min
    (:add (clad.features:make-external-thread thread-spec
                                               :length thread-length
                                               :full-geometry t))))
```

**Success Criteria:**
- DSL seamlessly supports both simplified and full geometry
- Global mode switch works correctly
- Backward compatibility maintained
- Documentation updated

---

## 3. Performance Optimization (Week 6)

### 3.1 Geometry Simplification Options

```lisp
(defvar *thread-geometry-detail-level* :medium
  "Control thread geometry tessellation:
  :low    - 20 points per turn (fast, approximate)
  :medium - 50 points per turn (balanced)
  :high   - 100 points per turn (precise)
  :ultra  - 200 points per turn (production)")

(defun adjust-helix-resolution (detail-level num-turns)
  "Calculate number of points for B-spline based on detail level"
  (let ((points-per-turn (case detail-level
                           (:low 20)
                           (:medium 50)
                           (:high 100)
                           (:ultra 200)
                           (otherwise 50))))
    (* points-per-turn num-turns)))
```

### 3.2 Caching Strategy

```lisp
(defvar *thread-geometry-cache* (make-hash-table :test 'equal))

(defun get-cached-thread-geometry (thread-spec length profile-type)
  "Return cached thread geometry if available"
  (let ((key (list thread-spec length profile-type)))
    (gethash key *thread-geometry-cache*)))

(defun cache-thread-geometry (thread-spec length profile-type geometry)
  "Cache thread geometry for reuse"
  (let ((key (list thread-spec length profile-type)))
    (setf (gethash key *thread-geometry-cache*) geometry)))
```

### 3.3 Level-of-Detail (LOD) System

```lisp
(defun make-thread-with-lod (thread-spec length &key (lod-mode :auto))
  "Create thread with automatic level-of-detail selection.

  LOD-MODE:
    :auto       - Automatic based on thread size
    :simplified - Always use simple cylinder
    :full       - Always use full helical geometry
    :hybrid     - Full geometry for first/last turns, simplified middle"

  (case lod-mode
    (:auto (if (thread-is-small-p thread-spec)
               (make-full-thread thread-spec length)
               (make-simplified-thread thread-spec length)))
    (:simplified (make-simplified-thread thread-spec length))
    (:full (make-full-thread thread-spec length))
    (:hybrid (make-hybrid-thread thread-spec length))))
```

---

## 4. Testing Strategy

### 4.1 Unit Tests (Per Phase)

Each phase includes comprehensive unit tests (shown above in phase descriptions).

**Test Categories:**
- Geometry calculations (dimensions, angles)
- OCCT API integration (wire creation, sweeps)
- Boolean operations (intersections, unions)
- Edge cases (very small threads, very long threads)
- Invalid inputs (error handling)

### 4.2 Integration Tests

```lisp
;; File: tests/thread-integration-tests.lisp

(deftest test-full-external-thread-workflow
  "End-to-end test: Create M6x30 bolt, export to STEP, reimport, validate"
  (let* ((bolt (clad.features:make-external-thread :m6 :length 30 :full-geometry t))
         (temp-file "/tmp/integration-bolt.step"))

    ;; Export
    (clad.export:export-step bolt temp-file)

    ;; Reimport
    (let ((reimported (clad.import:import-step temp-file)))

      ;; Validate geometry preserved
      (assert-true (occt-is-valid-solid reimported))
      (assert-equal 6.0 (get-max-diameter reimported) 0.2)
      (assert-equal 30.0 (get-height reimported) 1.0)

      ;; Cleanup
      (delete-file temp-file))))

(deftest test-thread-in-assembly
  "Verify threaded parts work in assemblies"
  (let* ((bolt (clad.features:make-external-thread :m8 :length 40 :full-geometry t))
         (nut (clad.features:make-nut :m8 :full-geometry t))
         (assembly (clad.assembly:make-assembly :name "Bolt-Nut")))

    (clad.assembly:add-component assembly :bolt bolt)
    (clad.assembly:add-component assembly :nut nut)
    (clad.assembly:view-assembly assembly :name "threaded-assembly")

    (assert-true (clad.assembly:validate-assembly assembly))))
```

### 4.3 Visual Verification Tests

```lisp
(defun visual-test-threads ()
  "Generate sample threads for visual inspection in viewer"

  (clad:start-viewer)

  ;; Test various thread sizes
  (dolist (spec '(:m3 :m6 :m8 :m10 :m12))
    (let ((thread (clad.features:make-external-thread spec :length 20 :full-geometry t)))
      (clad:view thread :name (format nil "~A-external" spec))))

  ;; Test internal threads
  (dolist (spec '(:m3 :m6 :m8))
    (let* ((block (clad.core:make-box 20 20 15))
           (threaded-block (clad.features:add-internal-thread
                            block spec :depth 12 :full-geometry t)))
      (clad:view threaded-block :name (format nil "~A-internal" spec)))))
```

### 4.4 Performance Benchmarks

```lisp
(defun benchmark-thread-creation ()
  "Benchmark thread creation performance"

  (format t "~%Thread Creation Benchmarks:~%")
  (format t "===========================~%")

  (dolist (mode '(:simplified :full))
    (format t "~%Mode: ~A~%" mode)

    (dolist (spec '(:m3 :m6 :m10))
      (let ((start-time (get-internal-real-time)))

        (dotimes (i 10)
          (clad.features:make-external-thread spec :length 30
                                               :full-geometry (eq mode :full)))

        (let* ((end-time (get-internal-real-time))
               (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
          (format t "  ~A: ~,3F seconds (avg ~,3F s per thread)~%"
                  spec elapsed (/ elapsed 10.0)))))))
```

---

## 5. Documentation Updates

### 5.1 User Guide Section

Add to `USER_GUIDE.md`:

```markdown
## Thread Modeling - Production Geometry

CLAD supports two modes for thread modeling:

### Simplified Mode (Default - Fast)

For design and iteration work, CLAD uses simplified cylindrical representations:
- External threads: Cylinder at major diameter
- Internal threads: Cylinder at major diameter
- Fast creation and rendering
- Suitable for most CAD work

```lisp
;; Default: Simplified
(clad.features:make-external-thread :m6 :length 30)
```

### Full Geometry Mode (Production)

For manufacturing, export to machining, or detailed analysis, enable full helical thread geometry:

```lisp
;; Enable full geometry globally
(clad.features:set-thread-geometry-mode :full)

;; Or per-thread
(clad.features:make-external-thread :m6 :length 30 :full-geometry t)
```

Full geometry mode creates:
- Mathematically accurate ISO 68-1 thread profiles
- Helical surfaces with correct pitch
- Exact major, pitch, and minor diameters
- Thread crests and roots matching standards
- STEP files suitable for CNC machining

### Performance Considerations

Full thread geometry is computationally expensive:
- 10-100x slower creation time
- Larger file sizes (STEP exports)
- More complex rendering

**Recommendations:**
- Use simplified mode during design
- Switch to full geometry for final export
- Cache full geometry threads for reuse
- Use LOD (level-of-detail) for large assemblies
```

### 5.2 API Documentation

```lisp
(defun make-external-thread (thread-spec &key (length 10) (full-geometry nil))
  "Create external thread (bolt, stud, threaded rod).

  Parameters:
    THREAD-SPEC - Thread specification keyword:
                  ISO Metric: :m3, :m6, :m8, :m10, :m12, etc.
                  ISO Fine: :m8x1.0, :m10x1.25, etc.
                  UNC: :1/4-20, :5/16-18, :3/8-16, etc.
    LENGTH - Thread length in millimeters (default 10mm)
    FULL-GEOMETRY - Boolean flag:
                    NIL: Simplified cylinder (default, fast)
                    T: Full helical geometry (slow, production-ready)

  Returns:
    TopoDS_Solid - Threaded shaft geometry

  Examples:
    ;; Quick design iteration (simplified)
    (make-external-thread :m6 :length 30)

    ;; Production geometry for machining
    (make-external-thread :m6 :length 30 :full-geometry t)

    ;; Fine thread for precision work
    (make-external-thread :m10x1.25 :length 50 :full-geometry t)

  Notes:
    - Full geometry mode creates accurate ISO 68-1 helical threads
    - Simplified mode is 10-100x faster for design work
    - Use (set-thread-geometry-mode :full) to change default globally
    - Thread standards: ISO 68-1 (Metric), ISO 965-1 (Tolerances),
                       ASME B1.1 (Unified), ASME B1.13M (Metric)

  See Also:
    make-internal-thread - Create tapped holes
    tap-drill-size - Calculate drill size for internal threads
    thread-minor-diameter - Get thread root diameter"
  ...)
```

---

## 6. Migration Path

### 6.1 Backward Compatibility

The current simplified thread system continues to work:

```lisp
;; Existing code still works (simplified mode)
(clad.features:make-external-thread :m6 :length 30)
(clad.features:make-internal-thread :m8 :depth 25)
```

### 6.2 Opt-In Full Geometry

Users can enable full geometry when needed:

```lisp
;; Per-thread basis
(clad.features:make-external-thread :m6 :length 30 :full-geometry t)

;; Or globally
(clad.features:set-thread-geometry-mode :full)
```

### 6.3 Gradual Rollout

**Phase 1:** Implement as optional feature (`:full-geometry t`)
**Phase 2:** Add to examples with clear documentation
**Phase 3:** Performance optimize based on user feedback
**Phase 4:** Consider making full geometry default (with option to simplify)

---

## 7. Success Metrics

### Correctness Metrics
- [ ] Thread dimensions match ISO 68-1 standard within 0.1%
- [ ] All unit tests pass (100% coverage for thread modules)
- [ ] Integration tests pass (STEP export/import roundtrip)
- [ ] Visual inspection confirms correct helical geometry

### Performance Metrics
- [ ] Simplified threads: <10ms creation time
- [ ] Full geometry threads: <500ms for M6x30 (acceptable for production)
- [ ] Memory usage: <50MB for typical threaded assembly

### Quality Metrics
- [ ] STEP files import correctly into SolidWorks, Fusion 360, FreeCAD
- [ ] Thread geometry passes CAD validation tools
- [ ] Threads display correctly in web viewer
- [ ] Documentation is clear and includes examples

---

## 8. Timeline and Milestones

| Week | Phase | Deliverable | Tests |
|------|-------|-------------|-------|
| 1 | Profile Geometry | Thread profile calculation working | 15 unit tests |
| 2 | Helical Path | Helix generation working | 10 unit tests |
| 3 | Helical Sweep | Profile sweep along helix working | 12 unit tests |
| 4 | Boolean Integration | Complete thread creation working | 15 unit tests |
| 5 | DSL Integration | Full API integrated with defpart | 8 integration tests |
| 6 | Optimization | Performance tuning and caching | Benchmarks documented |
| 7 | Documentation | User guide and examples complete | Examples tested |
| 8 | Testing & Polish | All tests passing, edge cases handled | Full test suite |

**Total Duration:** 8 weeks
**Test Coverage Goal:** >90% for all thread-related modules

---

## 9. Risk Mitigation

### Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| OCCT helical sweep complexity | Medium | High | Research OCCT examples early, prototype in week 1-2 |
| Performance too slow | Medium | Medium | Implement caching, LOD system, optimize in week 6 |
| Boolean operations fail | Low | High | Robust error handling, fallback to simplified mode |
| Thread profile calculations incorrect | Low | High | Validate against ISO standards, extensive unit tests |
| STEP export incompatibility | Medium | Medium | Test with multiple CAD tools, adjust export settings |

### Process Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Scope creep (too many thread types) | Medium | Medium | Focus on ISO Metric first, add others in follow-up |
| Integration breaks existing code | Low | High | Comprehensive regression tests, backward compatibility |
| Documentation insufficient | Medium | Low | Write docs alongside code (TDD for docs too) |

---

## 10. Future Enhancements (Post-V1)

After initial implementation, consider:

1. **Additional Thread Standards:**
   - BSW/BSF (British Standard Whitworth)
   - NPT/NPTF (National Pipe Thread)
   - ACME threads (trapezoidal)
   - Buttress threads

2. **Advanced Features:**
   - Thread relief grooves
   - Thread runout modeling
   - Damaged/worn thread simulation
   - Thread gauging tools

3. **Manufacturing Integration:**
   - CNC toolpath generation for threading
   - Thread milling cutter selection
   - Thread rolling simulation
   - Thread inspection reports

4. **Smart Threading:**
   - Auto-detect mating thread sizes
   - Thread engagement length calculator
   - Strength analysis (stripping, tensile)
   - Material-specific thread recommendations

---

## 11. References

### Standards
- ISO 68-1: ISO general purpose screw threads — Basic profile — Metric screw threads
- ISO 724: ISO general purpose metric screw threads — Basic dimensions
- ISO 965-1: ISO general purpose metric screw threads — Tolerances
- ASME B1.1: Unified Inch Screw Threads (UN and UNR Thread Form)
- ASME B1.13M: Metric Screw Threads: M Profile

### Technical Resources
- OpenCascade Modeling Algorithms Guide (Helical Sweeps)
- OpenCascade Tutorial - Making a Bottle (Thread Creation Example)
- Engineers Edge: ISO 68-1 Metric Thread Profile Specifications
- Machinery's Handbook (Thread Standards Reference)

### CAD Best Practices
- FreeCAD ThreadProfile Workbench (Reference Implementation)
- SolidWorks: Creating Cosmetic vs. Helical Threads
- Fusion 360: Thread Modeling Guidelines

---

## Conclusion

This implementation plan provides a comprehensive, test-driven approach to adding production-ready 3D thread geometry to CLAD. The phased approach ensures:

1. **Correctness** - TDD ensures geometric accuracy at each step
2. **Performance** - Optimization phase prevents slowdowns
3. **Usability** - DSL integration makes it easy to use
4. **Compatibility** - Backward compatibility and STEP export work
5. **Maintainability** - Well-tested, documented code

By following this plan, CLAD will offer both fast simplified threads for design work AND accurate helical threads for production manufacturing, making it suitable for real-world engineering projects.
