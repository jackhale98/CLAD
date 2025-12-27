;;;; tests/sketch-tests.lisp --- Comprehensive sketch system tests
;;;;
;;;; Tests for:
;;;; - 2D sketch entities (point, line, arc, circle, spline)
;;;; - Constraints (fixed, coincident, distance, horizontal, vertical, etc.)
;;;; - Constraint solver
;;;; - Sketch to wire/face conversion
;;;; - Extrusion and revolution operations

(in-package :clad.tests)

(in-suite clad-tests)

;;; ============================================================================
;;; Sketch Entity Creation Tests
;;; ============================================================================

(test test-make-point-2d
  "Test 2D point creation"
  (let ((point (clad.sketch:make-point-2d 10.0 20.0)))
    (is (typep point 'clad.sketch:point-2d))
    (is (= 10.0d0 (clad.sketch:point-x point)))
    (is (= 20.0d0 (clad.sketch:point-y point)))))

(test test-make-point-2d-fixed
  "Test fixed 2D point creation"
  (let ((point (clad.sketch:make-point-2d 10.0 20.0 :fixed t)))
    (is (clad.sketch:point-fixed-p point))))

(test test-make-line-2d
  "Test 2D line creation"
  (let* ((p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 10 20))
         (line (clad.sketch:make-line-2d p1 p2)))
    (is (typep line 'clad.sketch:line-2d))
    (is (eq p1 (clad.sketch:line-start line)))
    (is (eq p2 (clad.sketch:line-end line)))))

(test test-line-length
  "Test line length calculation"
  (let* ((p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 3 4))
         (line (clad.sketch:make-line-2d p1 p2)))
    (is (= 5.0d0 (clad.sketch:line-length line)))))

(test test-make-circle-2d
  "Test 2D circle creation"
  (let* ((center (clad.sketch:make-point-2d 50 50))
         (circle (clad.sketch:make-circle-2d center 25.0)))
    (is (typep circle 'clad.sketch:circle-2d))
    (is (eq center (clad.sketch:circle-center circle)))
    (is (= 25.0d0 (clad.sketch:circle-radius circle)))))

(test test-make-arc-2d
  "Test 2D arc creation"
  (let* ((center (clad.sketch:make-point-2d 0 0))
         (arc (clad.sketch:make-arc-2d center 10.0 0.0 (/ pi 2))))
    (is (typep arc 'clad.sketch:arc-2d))
    (is (= 10.0d0 (clad.sketch:arc-radius arc)))
    (is (= 0.0d0 (clad.sketch:arc-start-angle arc)))))

(test test-make-spline-2d
  "Test 2D spline creation"
  (let* ((pts (list (clad.sketch:make-point-2d 0 0)
                    (clad.sketch:make-point-2d 10 5)
                    (clad.sketch:make-point-2d 20 0)
                    (clad.sketch:make-point-2d 30 5)))
         (spline (clad.sketch:make-spline-2d pts)))
    (is (typep spline 'clad.sketch:spline-2d))
    (is (= 4 (length (clad.sketch:spline-points spline))))))

;;; ============================================================================
;;; Sketch Container Tests
;;; ============================================================================

(test test-make-sketch
  "Test sketch container creation"
  (let ((sketch (clad.sketch:make-sketch :name "test-sketch")))
    (is (typep sketch 'clad.sketch:sketch))
    (is (null (clad.sketch:sketch-entities sketch)))
    (is (null (clad.sketch:sketch-constraints sketch)))))

(test test-add-entity-to-sketch
  "Test adding entities to sketch"
  (let ((sketch (clad.sketch:make-sketch))
        (point (clad.sketch:make-point-2d 10 20)))
    (clad.sketch:add-entity sketch point)
    (is (= 1 (length (clad.sketch:sketch-entities sketch))))
    (is (eq point (first (clad.sketch:sketch-entities sketch))))))

(test test-find-entity-by-name
  "Test finding entity by name"
  (let ((sketch (clad.sketch:make-sketch))
        (point (clad.sketch:make-point-2d 10 20 :name "origin")))
    (clad.sketch:add-entity sketch point)
    (is (eq point (clad.sketch:find-entity sketch "origin")))
    (is (null (clad.sketch:find-entity sketch "nonexistent")))))

;;; ============================================================================
;;; Constraint Creation Tests
;;; ============================================================================

(test test-fixed-constraint
  "Test fixed constraint creation"
  (let* ((point (clad.sketch:make-point-2d 5 5))
         (constraint (clad.sketch.constraints:make-fixed-constraint point 10.0 20.0)))
    (is (typep constraint 'clad.sketch.constraints:fixed-constraint))
    (is (= 10.0d0 (clad.sketch.constraints:constraint-target-x constraint)))
    (is (= 20.0d0 (clad.sketch.constraints:constraint-target-y constraint)))))

(test test-coincident-constraint
  "Test coincident constraint creation"
  (let* ((p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 5 5))
         (constraint (clad.sketch.constraints:make-coincident-constraint p1 p2)))
    (is (typep constraint 'clad.sketch.constraints:coincident-constraint))))

(test test-distance-constraint
  "Test distance constraint creation"
  (let* ((p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 10 0))
         (constraint (clad.sketch.constraints:make-distance-constraint p1 p2 25.0)))
    (is (typep constraint 'clad.sketch.constraints:distance-constraint))
    (is (= 25.0d0 (clad.sketch.constraints:constraint-target-distance constraint)))))

(test test-horizontal-constraint
  "Test horizontal constraint creation"
  (let* ((p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 10 5))
         (line (clad.sketch:make-line-2d p1 p2))
         (constraint (clad.sketch.constraints:make-horizontal-constraint line)))
    (is (typep constraint 'clad.sketch.constraints:horizontal-constraint))))

(test test-vertical-constraint
  "Test vertical constraint creation"
  (let* ((p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 5 10))
         (line (clad.sketch:make-line-2d p1 p2))
         (constraint (clad.sketch.constraints:make-vertical-constraint line)))
    (is (typep constraint 'clad.sketch.constraints:vertical-constraint))))

(test test-parallel-constraint
  "Test parallel constraint creation"
  (let* ((l1 (clad.sketch:make-line-2d
              (clad.sketch:make-point-2d 0 0)
              (clad.sketch:make-point-2d 10 0)))
         (l2 (clad.sketch:make-line-2d
              (clad.sketch:make-point-2d 0 10)
              (clad.sketch:make-point-2d 10 15)))
         (constraint (clad.sketch.constraints:make-parallel-constraint l1 l2)))
    (is (typep constraint 'clad.sketch.constraints:parallel-constraint))))

(test test-perpendicular-constraint
  "Test perpendicular constraint creation"
  (let* ((l1 (clad.sketch:make-line-2d
              (clad.sketch:make-point-2d 0 0)
              (clad.sketch:make-point-2d 10 0)))
         (l2 (clad.sketch:make-line-2d
              (clad.sketch:make-point-2d 5 0)
              (clad.sketch:make-point-2d 5 10)))
         (constraint (clad.sketch.constraints:make-perpendicular-constraint l1 l2)))
    (is (typep constraint 'clad.sketch.constraints:perpendicular-constraint))))

;;; ============================================================================
;;; Constraint Error Calculation Tests
;;; ============================================================================

(test test-fixed-constraint-error
  "Test fixed constraint error calculation"
  (let* ((point (clad.sketch:make-point-2d 10 20))
         (constraint (clad.sketch.constraints:make-fixed-constraint point 10.0 20.0)))
    ;; Point is at target, error should be 0
    (is (< (clad.sketch.constraints:constraint-error constraint) 0.001))))

(test test-fixed-constraint-error-nonzero
  "Test fixed constraint error when point is off target"
  (let* ((point (clad.sketch:make-point-2d 0 0))
         (constraint (clad.sketch.constraints:make-fixed-constraint point 3.0 4.0)))
    ;; Error should be (3^2 + 4^2) = 25
    (is (= 25.0d0 (clad.sketch.constraints:constraint-error constraint)))))

(test test-coincident-constraint-error
  "Test coincident constraint error calculation"
  (let* ((p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 3 4))
         (constraint (clad.sketch.constraints:make-coincident-constraint p1 p2)))
    ;; Error should be (3^2 + 4^2) = 25
    (is (= 25.0d0 (clad.sketch.constraints:constraint-error constraint)))))

(test test-distance-constraint-error
  "Test distance constraint error calculation"
  (let* ((p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 5 0))  ; distance = 5
         (constraint (clad.sketch.constraints:make-distance-constraint p1 p2 5.0)))
    ;; Distance matches, error should be 0
    (is (< (clad.sketch.constraints:constraint-error constraint) 0.001))))

(test test-horizontal-constraint-error
  "Test horizontal constraint error calculation"
  (let* ((p1 (clad.sketch:make-point-2d 0 10))
         (p2 (clad.sketch:make-point-2d 20 10))  ; same Y = horizontal
         (line (clad.sketch:make-line-2d p1 p2))
         (constraint (clad.sketch.constraints:make-horizontal-constraint line)))
    ;; Line is horizontal, error should be 0
    (is (< (clad.sketch.constraints:constraint-error constraint) 0.001))))

(test test-vertical-constraint-error
  "Test vertical constraint error calculation"
  (let* ((p1 (clad.sketch:make-point-2d 10 0))
         (p2 (clad.sketch:make-point-2d 10 20))  ; same X = vertical
         (line (clad.sketch:make-line-2d p1 p2))
         (constraint (clad.sketch.constraints:make-vertical-constraint line)))
    ;; Line is vertical, error should be 0
    (is (< (clad.sketch.constraints:constraint-error constraint) 0.001))))

;;; ============================================================================
;;; Sketch Plane Tests
;;; ============================================================================

(test test-make-sketch-plane-xy
  "Test XY sketch plane creation"
  (let ((plane (clad.sketch:make-sketch-plane :type :xy)))
    (is (typep plane 'clad.sketch:sketch-plane))
    (is (equal '(1.0d0 0.0d0 0.0d0) (clad.sketch:plane-x-axis plane)))
    (is (equal '(0.0d0 1.0d0 0.0d0) (clad.sketch:plane-y-axis plane)))))

(test test-make-sketch-plane-yz
  "Test YZ sketch plane creation"
  (let ((plane (clad.sketch:make-sketch-plane :type :yz)))
    (is (equal '(0.0d0 1.0d0 0.0d0) (clad.sketch:plane-x-axis plane)))
    (is (equal '(0.0d0 0.0d0 1.0d0) (clad.sketch:plane-y-axis plane)))))

(test test-make-sketch-plane-xz
  "Test XZ sketch plane creation"
  (let ((plane (clad.sketch:make-sketch-plane :type :xz)))
    (is (equal '(1.0d0 0.0d0 0.0d0) (clad.sketch:plane-x-axis plane)))
    (is (equal '(0.0d0 0.0d0 1.0d0) (clad.sketch:plane-y-axis plane)))))

(test test-sketch-plane-origin
  "Test sketch plane with custom origin"
  (let ((plane (clad.sketch:make-sketch-plane :type :xy :origin '(10.0d0 20.0d0 30.0d0))))
    (is (equal '(10.0d0 20.0d0 30.0d0) (clad.sketch:plane-origin plane)))))

(test test-transform-2d-to-3d
  "Test 2D to 3D coordinate transformation"
  (let ((plane (clad.sketch:make-sketch-plane :type :xy)))
    (is (equal '(5.0d0 10.0d0 0.0d0) (clad.sketch:transform-2d-to-3d 5 10 plane)))))

(test test-transform-2d-to-3d-with-origin
  "Test 2D to 3D transformation with offset origin"
  (let ((plane (clad.sketch:make-sketch-plane :type :xy :origin '(100.0d0 0.0d0 0.0d0))))
    (is (equal '(110.0d0 20.0d0 0.0d0) (clad.sketch:transform-2d-to-3d 10 20 plane)))))

;;; ============================================================================
;;; Constraint Solver Tests
;;; ============================================================================

(test test-solver-options
  "Test solver options creation"
  (let ((options (clad.sketch.solver:make-solver-options
                  :max-iterations 1000
                  :tolerance 0.001d0)))
    (is (= 1000 (clad.sketch.solver:solver-max-iterations options)))
    (is (= 0.001d0 (clad.sketch.solver:solver-tolerance options)))))

(test test-solve-simple-fixed
  "Test solver with simple fixed constraint"
  (let* ((sketch (clad.sketch:make-sketch))
         (point (clad.sketch:make-point-2d 0 0))
         (constraint (clad.sketch.constraints:make-fixed-constraint point 10.0 20.0)))
    (clad.sketch:add-entity sketch point)
    (clad.sketch:add-constraint sketch constraint)
    (clad.sketch.solver:solve-sketch sketch)
    ;; Point should have moved to target
    (is (< (abs (- 10.0d0 (clad.sketch:point-x point))) 0.1))
    (is (< (abs (- 20.0d0 (clad.sketch:point-y point))) 0.1))))

(test test-solve-coincident
  "Test solver with coincident constraint"
  (let* ((sketch (clad.sketch:make-sketch))
         (p1 (clad.sketch:make-point-2d 0 0 :fixed t))
         (p2 (clad.sketch:make-point-2d 10 10))
         (constraint (clad.sketch.constraints:make-coincident-constraint p1 p2)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-constraint sketch constraint)
    (clad.sketch.solver:solve-sketch sketch)
    ;; p2 should have moved to p1 (which is fixed)
    (is (< (abs (clad.sketch:point-x p2)) 0.1))
    (is (< (abs (clad.sketch:point-y p2)) 0.1))))

;;; ============================================================================
;;; Sketch to Wire Conversion Tests
;;; ============================================================================

(test test-circle-to-wire
  "Test converting circle sketch to 3D wire"
  (let* ((sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 0 0))
         (circle (clad.sketch:make-circle-2d center 10.0)))
    (clad.sketch:add-entity sketch circle)
    (let ((wire (clad.sketch:sketch-to-wire sketch)))
      (is (not (null wire))))))

(test test-line-to-wire
  "Test converting line sketch to 3D wire"
  (let* ((sketch (clad.sketch:make-sketch))
         (p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 100 50))
         (line (clad.sketch:make-line-2d p1 p2)))
    (clad.sketch:add-entity sketch line)
    (let ((wire (clad.sketch:sketch-to-wire sketch)))
      (is (not (null wire))))))

;;; ============================================================================
;;; Sketch to Face Conversion Tests (with real FFI)
;;; ============================================================================

(test test-circle-to-face
  "Test converting circle sketch to 3D face"
  (let* ((sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 0 0))
         (circle (clad.sketch:make-circle-2d center 10.0)))
    (clad.sketch:add-entity sketch circle)
    (let ((face (clad.sketch:sketch-to-face sketch)))
      (is (not (null face)))
      ;; Check it's a valid handle
      (is (clad.ffi:handle-valid-p face)))))

;;; ============================================================================
;;; Extrusion Tests (with real FFI)
;;; ============================================================================

(test test-extrude-circle
  "Test extruding circle to cylinder"
  (let* ((sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 0 0))
         (circle (clad.sketch:make-circle-2d center 10.0)))
    (clad.sketch:add-entity sketch circle)
    (let ((solid (clad.sketch:extrude-sketch sketch 20.0)))
      (is (not (null solid)))
      (is (clad.ffi:handle-valid-p solid)))))

(test test-extrude-with-direction
  "Test extruding with custom direction"
  (let* ((sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 0 0))
         (circle (clad.sketch:make-circle-2d center 10.0)))
    (clad.sketch:add-entity sketch circle)
    (let ((solid (clad.sketch:extrude-sketch sketch 20.0 :direction '(0 0 1))))
      (is (not (null solid)))
      (is (clad.ffi:handle-valid-p solid)))))

(test test-extrude-on-yz-plane
  "Test extruding on YZ plane"
  (let* ((plane (clad.sketch:make-sketch-plane :type :yz))
         (sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 0 0))
         (circle (clad.sketch:make-circle-2d center 10.0)))
    (clad.sketch:add-entity sketch circle)
    (let ((solid (clad.sketch:extrude-sketch sketch 20.0 :plane plane)))
      (is (not (null solid)))
      (is (clad.ffi:handle-valid-p solid)))))

;;; ============================================================================
;;; Revolution Tests (with real FFI)
;;; ============================================================================

(test test-revolve-circle
  "Test revolving circle to torus"
  (let* ((sketch (clad.sketch:make-sketch))
         ;; Circle offset from Y axis
         (center (clad.sketch:make-point-2d 20 0))
         (circle (clad.sketch:make-circle-2d center 5.0)))
    (clad.sketch:add-entity sketch circle)
    ;; Revolve around Y axis (default)
    (let ((solid (clad.sketch:revolve-sketch sketch)))
      (is (not (null solid)))
      (is (clad.ffi:handle-valid-p solid)))))

(test test-revolve-partial
  "Test partial revolution"
  (let* ((sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 20 0))
         (circle (clad.sketch:make-circle-2d center 5.0)))
    (clad.sketch:add-entity sketch circle)
    ;; Revolve 90 degrees
    (let ((solid (clad.sketch:revolve-sketch sketch :angle (/ pi 2))))
      (is (not (null solid)))
      (is (clad.ffi:handle-valid-p solid)))))

(test test-revolve-around-z
  "Test revolution around Z axis"
  (let* ((sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 20 0))
         (circle (clad.sketch:make-circle-2d center 5.0)))
    (clad.sketch:add-entity sketch circle)
    ;; Revolve around Z axis
    (let ((solid (clad.sketch:revolve-sketch sketch :axis-direction '(0 0 1))))
      (is (not (null solid)))
      (is (clad.ffi:handle-valid-p solid)))))

;;; ============================================================================
;;; Integration Tests - Full Workflow
;;; ============================================================================

(test test-full-extrude-workflow
  "Test complete sketch-to-solid workflow with extrusion"
  (let* ((sketch (clad.sketch:make-sketch))
         ;; Create a rectangle from 4 lines
         (p1 (clad.sketch:make-point-2d 0 0))
         (p2 (clad.sketch:make-point-2d 100 0))
         (p3 (clad.sketch:make-point-2d 100 50))
         (p4 (clad.sketch:make-point-2d 0 50)))
    ;; Add lines forming rectangle
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d p1 p2))
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d p2 p3))
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d p3 p4))
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d p4 p1))
    ;; Extrude to solid
    (let ((solid (clad.sketch:extrude-sketch sketch 30.0)))
      (is (not (null solid)))
      (is (clad.ffi:handle-valid-p solid)))))

(test test-extrude-and-export
  "Test extrusion followed by STEP export"
  (let* ((sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 0 0))
         (circle (clad.sketch:make-circle-2d center 15.0)))
    (clad.sketch:add-entity sketch circle)
    (let ((solid (clad.sketch:extrude-sketch sketch 25.0)))
      (is (clad.ffi:handle-valid-p solid))
      ;; Export to STEP
      (let ((path "/tmp/sketch-test-extrusion.step"))
        (clad.ffi:ffi-export-step solid path)
        (is (probe-file path))))))

(test test-revolve-and-export
  "Test revolution followed by STEP export"
  (let* ((sketch (clad.sketch:make-sketch))
         (center (clad.sketch:make-point-2d 30 0))
         (circle (clad.sketch:make-circle-2d center 8.0)))
    (clad.sketch:add-entity sketch circle)
    (let ((solid (clad.sketch:revolve-sketch sketch)))
      (is (clad.ffi:handle-valid-p solid))
      ;; Export to STEP
      (let ((path "/tmp/sketch-test-revolution.step"))
        (clad.ffi:ffi-export-step solid path)
        (is (probe-file path))))))
