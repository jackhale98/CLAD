;;;; sketch-tests.lisp --- Tests for Phase 9: 2D Sketch System

(in-package #:clad.tests)

;;; ==============================================================================
;;; Test Suite Definition
;;; ==============================================================================

(def-suite sketch-tests
  :description "Tests for 2D constraint-based sketch system (Phase 9)"
  :in clad-tests)

(in-suite sketch-tests)

;;; ==============================================================================
;;; Week 1: Basic 2D Entities (Tests 1-15)
;;; ==============================================================================

;;; Test 1: Point-2D Entity Creation
(test test-point-2d-creation
  "Test creating a 2D point entity"
  (let ((p (clad.sketch:make-point-2d 10.0d0 20.0d0 :name "P1")))
    (is (typep p 'clad.sketch:point-2d)
        "make-point-2d should return a point-2d instance")
    (is (= 10.0d0 (clad.sketch:point-x p))
        "point-x should return correct X coordinate")
    (is (= 20.0d0 (clad.sketch:point-y p))
        "point-y should return correct Y coordinate")
    (is (equal "P1" (clad.sketch:entity-name p))
        "entity-name should return correct name")))

;;; Test 2: Line-2D Entity Creation
(test test-line-2d-creation
  "Test creating a 2D line entity"
  (let ((line (clad.sketch:make-line-2d
               (clad.sketch:make-point-2d 0.0d0 0.0d0)
               (clad.sketch:make-point-2d 10.0d0 0.0d0)
               :name "L1")))
    (is (typep line 'clad.sketch:line-2d)
        "make-line-2d should return a line-2d instance")
    (is (equal "L1" (clad.sketch:entity-name line))
        "entity-name should return correct name")
    (let ((start (clad.sketch:line-start line))
          (end (clad.sketch:line-end line)))
      (is (= 0.0d0 (clad.sketch:point-x start))
          "Line start point X should be correct")
      (is (= 10.0d0 (clad.sketch:point-x end))
          "Line end point X should be correct"))))

;;; Test 3: Point Fixed Parameter
(test test-point-fixed-parameter
  "Test point fixed/unfixed parameter"
  (let ((p1 (clad.sketch:make-point-2d 5.0d0 5.0d0 :fixed t))
        (p2 (clad.sketch:make-point-2d 3.0d0 3.0d0 :fixed nil)))
    (is (clad.sketch:point-fixed-p p1)
        "Point with :fixed t should be fixed")
    (is (not (clad.sketch:point-fixed-p p2))
        "Point with :fixed nil should not be fixed")))

;;; Test 4: Line Length Calculation
(test test-line-length-calculation
  "Test automatic line length calculation"
  (let ((line (clad.sketch:make-line-2d
               (clad.sketch:make-point-2d 0.0d0 0.0d0)
               (clad.sketch:make-point-2d 3.0d0 4.0d0))))
    (is (= 5.0d0 (clad.sketch:line-length line))
        "Line length for 3-4-5 triangle should be 5.0")))

;;; Test 5: Arc-2D Entity Creation
(test test-arc-2d-creation
  "Test creating a 2D arc entity"
  (let ((arc (clad.sketch:make-arc-2d
              (clad.sketch:make-point-2d 10.0d0 10.0d0)  ; center
              5.0d0                                        ; radius
              0.0d0                                        ; start-angle
              (/ pi 2)                                     ; end-angle
              :name "A1")))
    (is (typep arc 'clad.sketch:arc-2d)
        "make-arc-2d should return an arc-2d instance")
    (is (equal "A1" (clad.sketch:entity-name arc))
        "entity-name should return correct name")
    (is (= 5.0d0 (clad.sketch:arc-radius arc))
        "arc-radius should return correct radius")
    (is (= 0.0d0 (clad.sketch:arc-start-angle arc))
        "arc-start-angle should return correct start angle")
    (is (= (/ pi 2) (clad.sketch:arc-end-angle arc))
        "arc-end-angle should return correct end angle")))

;;; Test 6: Circle-2D Entity Creation
(test test-circle-2d-creation
  "Test creating a 2D circle entity"
  (let ((circle (clad.sketch:make-circle-2d
                 (clad.sketch:make-point-2d 15.0d0 15.0d0)
                 8.0d0
                 :name "C1")))
    (is (typep circle 'clad.sketch:circle-2d)
        "make-circle-2d should return a circle-2d instance")
    (is (equal "C1" (clad.sketch:entity-name circle))
        "entity-name should return correct name")
    (is (= 8.0d0 (clad.sketch:circle-radius circle))
        "circle-radius should return correct radius")
    (let ((center (clad.sketch:circle-center circle)))
      (is (= 15.0d0 (clad.sketch:point-x center))
          "Circle center X should be correct")
      (is (= 15.0d0 (clad.sketch:point-y center))
          "Circle center Y should be correct"))))

;;; Test 7: Spline-2D Entity Creation
(test test-spline-2d-creation
  "Test creating a 2D spline entity"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (p2 (clad.sketch:make-point-2d 10.0d0 5.0d0))
         (p3 (clad.sketch:make-point-2d 20.0d0 0.0d0))
         (spline (clad.sketch:make-spline-2d (list p1 p2 p3) :name "S1")))
    (is (typep spline 'clad.sketch:spline-2d)
        "make-spline-2d should return a spline-2d instance")
    (is (equal "S1" (clad.sketch:entity-name spline))
        "entity-name should return correct name")
    (is (= 3 (length (clad.sketch:spline-points spline)))
        "Spline should have 3 control points")
    (is (not (clad.sketch:spline-closed-p spline))
        "Spline should not be closed by default")))

;;; Test 8: Sketch Container Creation
(test test-sketch-creation
  "Test creating an empty sketch container"
  (let ((sketch (clad.sketch:make-sketch :name "TestSketch")))
    (is (typep sketch 'clad.sketch:sketch)
        "make-sketch should return a sketch instance")
    (is (equal "TestSketch" (clad.sketch:sketch-name sketch))
        "sketch-name should return correct name")
    (is (null (clad.sketch:sketch-entities sketch))
        "New sketch should have no entities")
    (is (null (clad.sketch:sketch-constraints sketch))
        "New sketch should have no constraints")))

;;; Test 9: Multiple Circles with Different Radii
(test test-multiple-circles
  "Test creating multiple circles with different properties"
  (let ((c1 (clad.sketch:make-circle-2d
             (clad.sketch:make-point-2d 0.0d0 0.0d0)
             5.0d0
             :name "SmallCircle"))
        (c2 (clad.sketch:make-circle-2d
             (clad.sketch:make-point-2d 20.0d0 20.0d0)
             15.0d0
             :name "LargeCircle")))
    (is (< (clad.sketch:circle-radius c1) (clad.sketch:circle-radius c2))
        "SmallCircle should have smaller radius than LargeCircle")
    (is (not (equal (clad.sketch:entity-name c1) (clad.sketch:entity-name c2)))
        "Circles should have different names")))

;;; Test 10: Closed Spline
(test test-closed-spline
  "Test creating a closed spline"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0))
         (p3 (clad.sketch:make-point-2d 10.0d0 10.0d0))
         (p4 (clad.sketch:make-point-2d 0.0d0 10.0d0))
         (spline (clad.sketch:make-spline-2d (list p1 p2 p3 p4)
                                              :name "ClosedSpline"
                                              :closed t)))
    (is (clad.sketch:spline-closed-p spline)
        "Spline with :closed t should be closed")
    (is (= 4 (length (clad.sketch:spline-points spline)))
        "Closed spline should have 4 control points")))

;;; Test 11: Add Entity to Sketch
(test test-add-entity
  "Test adding an entity to a sketch"
  (let ((sketch (clad.sketch:make-sketch :name "MySketch"))
        (point (clad.sketch:make-point-2d 5.0d0 5.0d0 :name "P1")))
    (clad.sketch:add-entity sketch point)
    (is (= 1 (length (clad.sketch:sketch-entities sketch)))
        "Sketch should have 1 entity after adding")
    (is (member point (clad.sketch:sketch-entities sketch))
        "Added entity should be in sketch")))

;;; Test 12: Add Constraint to Sketch
(test test-add-constraint
  "Test adding a constraint to a sketch (placeholder)"
  (let ((sketch (clad.sketch:make-sketch :name "ConstrainedSketch"))
        (constraint :placeholder-constraint))
    (clad.sketch:add-constraint sketch constraint)
    (is (= 1 (length (clad.sketch:sketch-constraints sketch)))
        "Sketch should have 1 constraint after adding")
    (is (member constraint (clad.sketch:sketch-constraints sketch))
        "Added constraint should be in sketch")))

;;; Test 13: Find Entity by Name
(test test-find-entity
  "Test finding an entity in sketch by name"
  (let ((sketch (clad.sketch:make-sketch))
        (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "Point1"))
        (p2 (clad.sketch:make-point-2d 10.0d0 10.0d0 :name "Point2")))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (let ((found (clad.sketch:find-entity sketch "Point1")))
      (is (not (null found))
          "Should find entity by name")
      (is (eq found p1)
          "Should return the correct entity"))))

;;; Test 14: Count Entities in Sketch
(test test-count-entities
  "Test counting entities in a sketch"
  (let ((sketch (clad.sketch:make-sketch :name "CountTest")))
    (clad.sketch:add-entity sketch (clad.sketch:make-point-2d 0.0d0 0.0d0))
    (clad.sketch:add-entity sketch (clad.sketch:make-point-2d 10.0d0 10.0d0))
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d
                                     (clad.sketch:make-point-2d 0.0d0 0.0d0)
                                     (clad.sketch:make-point-2d 10.0d0 10.0d0)))
    (is (= 3 (length (clad.sketch:sketch-entities sketch)))
        "Sketch should have 3 entities")))

;;; Test 15: Multiple Entity Types in Sketch
(test test-multiple-entity-types
  "Test adding different entity types to a sketch"
  (let ((sketch (clad.sketch:make-sketch :name "Mixed")))
    (clad.sketch:add-entity sketch (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P"))
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d
                                     (clad.sketch:make-point-2d 0.0d0 0.0d0)
                                     (clad.sketch:make-point-2d 10.0d0 0.0d0)
                                     :name "L"))
    (clad.sketch:add-entity sketch (clad.sketch:make-circle-2d
                                     (clad.sketch:make-point-2d 5.0d0 5.0d0)
                                     3.0d0
                                     :name "C"))
    (is (= 3 (length (clad.sketch:sketch-entities sketch)))
        "Sketch should contain 3 different entity types")
    (let ((entities (clad.sketch:sketch-entities sketch)))
      (is (some (lambda (e) (typep e 'clad.sketch:point-2d)) entities)
          "Should have a point")
      (is (some (lambda (e) (typep e 'clad.sketch:line-2d)) entities)
          "Should have a line")
      (is (some (lambda (e) (typep e 'clad.sketch:circle-2d)) entities)
          "Should have a circle"))))

;;; ==============================================================================
;;; Week 3-4: Basic Constraints (Tests 16-27)
;;; ==============================================================================

;;; Test 16: Fixed Constraint Creation
(test test-fixed-constraint-creation
  "Test creating a fixed constraint on a point"
  (let* ((p (clad.sketch:make-point-2d 10.0d0 20.0d0 :name "P1"))
         (c (clad.sketch.constraints:make-fixed-constraint p 10.0d0 20.0d0)))
    (is (typep c 'clad.sketch.constraints:fixed-constraint)
        "make-fixed-constraint should return a fixed-constraint instance")
    (is (= 1 (length (clad.sketch.constraints:constraint-entities c)))
        "Fixed constraint should have 1 entity")
    (is (eq p (first (clad.sketch.constraints:constraint-entities c)))
        "Constraint should reference the point")))

;;; Test 17: Fixed Constraint Error Calculation
(test test-fixed-constraint-error
  "Test fixed constraint error measurement"
  (let* ((p (clad.sketch:make-point-2d 10.0d0 20.0d0 :name "P1"))
         (c (clad.sketch.constraints:make-fixed-constraint p 10.0d0 20.0d0)))
    ;; Point is at target - should have zero error
    (is (< (clad.sketch.constraints:constraint-error c) 1.0d-6)
        "Constraint should be satisfied when point is at target position")))

;;; Test 18: Fixed Constraint Violation
(test test-fixed-constraint-violation
  "Test fixed constraint error when violated"
  (let* ((p (clad.sketch:make-point-2d 15.0d0 25.0d0 :name "P1"))
         (c (clad.sketch.constraints:make-fixed-constraint p 10.0d0 20.0d0)))
    ;; Point is not at target - should have non-zero error
    (is (> (clad.sketch.constraints:constraint-error c) 0.0d0)
        "Constraint should have error when point is not at target")
    ;; Error should be squared distance: (15-10)^2 + (25-20)^2 = 25 + 25 = 50
    (is (< (abs (- (clad.sketch.constraints:constraint-error c) 50.0d0)) 1.0d-6)
        "Error should be squared distance from target")))

;;; Test 19: Coincident Constraint Creation
(test test-coincident-constraint-creation
  "Test creating a coincident constraint between two points"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 10.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-coincident-constraint p1 p2)))
    (is (typep c 'clad.sketch.constraints:coincident-constraint)
        "make-coincident-constraint should return a coincident-constraint instance")
    (is (= 2 (length (clad.sketch.constraints:constraint-entities c)))
        "Coincident constraint should have 2 entities")
    (is (eq p1 (first (clad.sketch.constraints:constraint-entities c)))
        "First entity should be first point")
    (is (eq p2 (second (clad.sketch.constraints:constraint-entities c)))
        "Second entity should be second point")))

;;; Test 20: Coincident Constraint Satisfied
(test test-coincident-constraint-satisfied
  "Test coincident constraint when points are coincident"
  (let* ((p1 (clad.sketch:make-point-2d 5.0d0 5.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 5.0d0 5.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-coincident-constraint p1 p2)))
    (is (< (clad.sketch.constraints:constraint-error c) 1.0d-6)
        "Constraint should be satisfied when points coincide")))

;;; Test 21: Coincident Constraint Violation
(test test-coincident-constraint-violation
  "Test coincident constraint error when violated"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 3.0d0 4.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-coincident-constraint p1 p2)))
    ;; Points are 3-4-5 triangle apart, squared distance = 25
    (is (> (clad.sketch.constraints:constraint-error c) 0.0d0)
        "Constraint should have error when points are not coincident")
    (is (< (abs (- (clad.sketch.constraints:constraint-error c) 25.0d0)) 1.0d-6)
        "Error should be squared distance between points")))

;;; Test 22: Distance Constraint Creation
(test test-distance-constraint-creation
  "Test creating a distance constraint between two points"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
    (is (typep c 'clad.sketch.constraints:distance-constraint)
        "make-distance-constraint should return a distance-constraint instance")
    (is (= 2 (length (clad.sketch.constraints:constraint-entities c)))
        "Distance constraint should have 2 entities")
    (is (= 10.0d0 (clad.sketch.constraints:constraint-target-distance c))
        "Target distance should be correct")))

;;; Test 23: Distance Constraint Satisfied
(test test-distance-constraint-satisfied
  "Test distance constraint when satisfied"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
    (is (< (clad.sketch.constraints:constraint-error c) 1.0d-6)
        "Constraint should be satisfied when distance is correct")))

;;; Test 24: Distance Constraint Violation
(test test-distance-constraint-violation
  "Test distance constraint error when violated"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 15.0d0 0.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
    ;; Actual distance is 15, target is 10, error = (15-10)^2 = 25
    (is (> (clad.sketch.constraints:constraint-error c) 0.0d0)
        "Constraint should have error when distance is wrong")
    (is (< (abs (- (clad.sketch.constraints:constraint-error c) 25.0d0)) 1.0d-6)
        "Error should be squared difference from target distance")))

;;; Test 25: Horizontal Constraint Creation
(test test-horizontal-constraint-creation
  "Test creating a horizontal constraint on a line"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 5.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (c (clad.sketch.constraints:make-horizontal-constraint line)))
    (is (typep c 'clad.sketch.constraints:horizontal-constraint)
        "make-horizontal-constraint should return a horizontal-constraint instance")
    (is (= 1 (length (clad.sketch.constraints:constraint-entities c)))
        "Horizontal constraint should have 1 entity")))

;;; Test 26: Vertical Constraint Creation
(test test-vertical-constraint-creation
  "Test creating a vertical constraint on a line"
  (let* ((p1 (clad.sketch:make-point-2d 5.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 5.0d0 10.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (c (clad.sketch.constraints:make-vertical-constraint line)))
    (is (typep c 'clad.sketch.constraints:vertical-constraint)
        "make-vertical-constraint should return a vertical-constraint instance")
    (is (= 1 (length (clad.sketch.constraints:constraint-entities c)))
        "Vertical constraint should have 1 entity")))

;;; Test 27: Constraint in Sketch Integration
(test test-constraint-in-sketch
  "Test adding constraints to a sketch"
  (let* ((sketch (clad.sketch:make-sketch :name "ConstrainedSketch"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1")))
    ;; Add entities
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch line)
    ;; Add constraint
    (let ((c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
      (clad.sketch:add-constraint sketch c)
      (is (= 1 (length (clad.sketch:sketch-constraints sketch)))
          "Sketch should have 1 constraint")
      (is (member c (clad.sketch:sketch-constraints sketch))
          "Constraint should be in sketch"))))

;;; ==============================================================================
;;; Week 4: Advanced Constraints (Tests 28-39)
;;; ==============================================================================

;;; Test 28: Parallel Constraint Creation
(test test-parallel-constraint-creation
  "Test creating a parallel constraint between two lines"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 5.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-parallel-constraint line1 line2)))
    (is (typep c 'clad.sketch.constraints:parallel-constraint)
        "make-parallel-constraint should return a parallel-constraint instance")
    (is (= 2 (length (clad.sketch.constraints:constraint-entities c)))
        "Parallel constraint should have 2 entities")))

;;; Test 29: Parallel Constraint Satisfied
(test test-parallel-constraint-satisfied
  "Test parallel constraint when lines are parallel"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 5.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-parallel-constraint line1 line2)))
    (is (< (clad.sketch.constraints:constraint-error c) 1.0d-6)
        "Constraint should be satisfied when lines are parallel")))

;;; Test 30: Parallel Constraint Violation
(test test-parallel-constraint-violation
  "Test parallel constraint error when violated"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 5.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 5.0d0 10.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-parallel-constraint line1 line2)))
    (is (> (clad.sketch.constraints:constraint-error c) 0.0d0)
        "Constraint should have error when lines are not parallel")))

;;; Test 31: Perpendicular Constraint Creation
(test test-perpendicular-constraint-creation
  "Test creating a perpendicular constraint between two lines"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 0.0d0 10.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-perpendicular-constraint line1 line2)))
    (is (typep c 'clad.sketch.constraints:perpendicular-constraint)
        "make-perpendicular-constraint should return a perpendicular-constraint instance")
    (is (= 2 (length (clad.sketch.constraints:constraint-entities c)))
        "Perpendicular constraint should have 2 entities")))

;;; Test 32: Perpendicular Constraint Satisfied
(test test-perpendicular-constraint-satisfied
  "Test perpendicular constraint when lines are perpendicular"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 0.0d0 10.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-perpendicular-constraint line1 line2)))
    (is (< (clad.sketch.constraints:constraint-error c) 1.0d-6)
        "Constraint should be satisfied when lines are perpendicular")))

;;; Test 33: Perpendicular Constraint Violation
(test test-perpendicular-constraint-violation
  "Test perpendicular constraint error when violated"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-perpendicular-constraint line1 line2)))
    (is (> (clad.sketch.constraints:constraint-error c) 0.0d0)
        "Constraint should have error when lines are not perpendicular")))

;;; Test 34: Angle Constraint Creation
(test test-angle-constraint-creation
  "Test creating an angle constraint between two lines"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 10.0d0 10.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-angle-constraint line1 line2 (/ pi 4))))
    (is (typep c 'clad.sketch.constraints:angle-constraint)
        "make-angle-constraint should return an angle-constraint instance")
    (is (= 2 (length (clad.sketch.constraints:constraint-entities c)))
        "Angle constraint should have 2 entities")
    (is (= (/ pi 4) (clad.sketch.constraints:constraint-target-angle c))
        "Target angle should be correct")))

;;; Test 35: Angle Constraint Satisfied
(test test-angle-constraint-satisfied
  "Test angle constraint when angle is correct"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 10.0d0 10.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-angle-constraint line1 line2 (/ pi 4))))
    (is (< (clad.sketch.constraints:constraint-error c) 1.0d-6)
        "Constraint should be satisfied when angle is 45 degrees")))

;;; Test 36: Angle Constraint Violation
(test test-angle-constraint-violation
  "Test angle constraint error when violated"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 0.0d0 10.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p3 p4 :name "L2"))
         (c (clad.sketch.constraints:make-angle-constraint line1 line2 (/ pi 4))))
    ;; Lines are at 90 degrees, target is 45 degrees
    (is (> (clad.sketch.constraints:constraint-error c) 0.0d0)
        "Constraint should have error when angle is wrong")))

;;; Test 37: Tangent Constraint Creation (Line-Circle)
(test test-tangent-constraint-creation-line-circle
  "Test creating a tangent constraint between line and circle"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 5.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (center (clad.sketch:make-point-2d 5.0d0 0.0d0 :name "Center"))
         (circle (clad.sketch:make-circle-2d center 5.0d0 :name "C1"))
         (c (clad.sketch.constraints:make-tangent-constraint line circle)))
    (is (typep c 'clad.sketch.constraints:tangent-constraint)
        "make-tangent-constraint should return a tangent-constraint instance")
    (is (= 2 (length (clad.sketch.constraints:constraint-entities c)))
        "Tangent constraint should have 2 entities")))

;;; Test 38: Tangent Constraint Satisfied (Line-Circle)
(test test-tangent-constraint-satisfied-line-circle
  "Test tangent constraint when line is tangent to circle"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 5.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (center (clad.sketch:make-point-2d 5.0d0 0.0d0 :name "Center"))
         (circle (clad.sketch:make-circle-2d center 5.0d0 :name "C1"))
         (c (clad.sketch.constraints:make-tangent-constraint line circle)))
    (is (< (clad.sketch.constraints:constraint-error c) 1.0d-6)
        "Constraint should be satisfied when line is tangent to circle")))

;;; Test 39: Tangent Constraint Creation (Circle-Circle)
(test test-tangent-constraint-creation-circle-circle
  "Test creating a tangent constraint between two circles"
  (let* ((center1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "C1"))
         (center2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "C2"))
         (circle1 (clad.sketch:make-circle-2d center1 3.0d0 :name "Circle1"))
         (circle2 (clad.sketch:make-circle-2d center2 7.0d0 :name "Circle2"))
         (c (clad.sketch.constraints:make-tangent-constraint circle1 circle2)))
    (is (typep c 'clad.sketch.constraints:tangent-constraint)
        "make-tangent-constraint should return a tangent-constraint instance")
    (is (= 2 (length (clad.sketch.constraints:constraint-entities c)))
        "Tangent constraint should have 2 entities")))

;;; ==============================================================================
;;; Week 5-6: Constraint Solver (Tests 40-45)
;;; ==============================================================================

;;; Test 40: Solver Function Exists
(test test-solver-exists
  "Test that solve-sketch function exists"
  (let* ((sketch (clad.sketch:make-sketch :name "TestSketch"))
         (p (clad.sketch:make-point-2d 5.0d0 5.0d0 :name "P1" :fixed t)))
    (clad.sketch:add-entity sketch p)
    ;; Just calling it should not error, even if it doesn't do anything yet
    (is (not (null (clad.sketch.solver:solve-sketch sketch)))
        "solve-sketch should return non-nil")))

;;; Test 41: Fixed Point Remains Fixed
(test test-solver-fixed-point
  "Test that solver does not move fixed points"
  (let* ((sketch (clad.sketch:make-sketch :name "FixedSketch"))
         (p (clad.sketch:make-point-2d 10.0d0 20.0d0 :name "P1" :fixed t)))
    (clad.sketch:add-entity sketch p)
    (clad.sketch.solver:solve-sketch sketch)
    ;; Fixed point should not have moved
    (is (= 10.0d0 (clad.sketch:point-x p))
        "Fixed point X should not change")
    (is (= 20.0d0 (clad.sketch:point-y p))
        "Fixed point Y should not change")))

;;; Test 42: Simple Distance Constraint Solving
(test test-solver-simple-distance
  "Test solving a simple distance constraint"
  (let* ((sketch (clad.sketch:make-sketch :name "DistanceSketch"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 5.0d0 0.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-constraint sketch c)
    ;; Solve
    (clad.sketch.solver:solve-sketch sketch)
    ;; P1 should be fixed at origin
    (is (< (abs (clad.sketch:point-x p1)) 1.0d-6)
        "P1 X should be at origin")
    (is (< (abs (clad.sketch:point-y p1)) 1.0d-6)
        "P1 Y should be at origin")
    ;; Distance from P1 to P2 should be 10
    (let ((dx (- (clad.sketch:point-x p2) (clad.sketch:point-x p1)))
          (dy (- (clad.sketch:point-y p2) (clad.sketch:point-y p1))))
      (is (< (abs (- (sqrt (+ (* dx dx) (* dy dy))) 10.0d0)) 0.01d0)
          "Distance should be approximately 10 after solving"))))

;;; Test 43: Multiple Constraints
(test test-solver-multiple-constraints
  "Test solving with multiple constraints"
  (let* ((sketch (clad.sketch:make-sketch :name "MultiSketch"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 5.0d0 5.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P3"))
         (c1 (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0))
         (c2 (clad.sketch.constraints:make-distance-constraint p2 p3 5.0d0)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch p3)
    (clad.sketch:add-constraint sketch c1)
    (clad.sketch:add-constraint sketch c2)
    ;; Solve
    (clad.sketch.solver:solve-sketch sketch)
    ;; Check that constraints are satisfied
    (is (< (clad.sketch.constraints:constraint-error c1) 0.1d0)
        "First constraint should be nearly satisfied")
    (is (< (clad.sketch.constraints:constraint-error c2) 0.1d0)
        "Second constraint should be nearly satisfied")))

;;; Test 44: Horizontal Line Constraint
(test test-solver-horizontal-line
  "Test solving a horizontal line constraint"
  (let* ((sketch (clad.sketch:make-sketch :name "HorzSketch"))
         (p1 (clad.sketch:make-point-2d 0.0d0 5.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 7.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (c (clad.sketch.constraints:make-horizontal-constraint line)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch line)
    (clad.sketch:add-constraint sketch c)
    ;; Solve
    (clad.sketch.solver:solve-sketch sketch)
    ;; Line should be horizontal (same Y coordinates)
    (is (< (abs (- (clad.sketch:point-y p1) (clad.sketch:point-y p2))) 0.01d0)
        "Line should be horizontal after solving")))

;;; Test 45: Solver Convergence
(test test-solver-convergence
  "Test that solver converges on a solvable system"
  (let* ((sketch (clad.sketch:make-sketch :name "ConvergeSketch"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 100.0d0 100.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-constraint sketch c)
    ;; Initial error should be large
    (let ((initial-error (clad.sketch.constraints:constraint-error c)))
      (is (> initial-error 1.0d0)
          "Initial error should be large")
      ;; Solve
      (clad.sketch.solver:solve-sketch sketch)
      ;; Final error should be much smaller
      (let ((final-error (clad.sketch.constraints:constraint-error c)))
        (is (< final-error initial-error)
            "Error should decrease after solving")
        (is (< final-error 0.1d0)
            "Error should converge to near zero")))))

;;; ==============================================================================
;;; Week 7-8: Sketch-to-3D Integration (Tests 46-55)
;;; ==============================================================================

;;; Test 46: Convert Circle Sketch to Wire
(test test-sketch-circle-to-wire
  "Test converting a circle sketch to a 3D wire"
  (let* ((center (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (circle (clad.sketch:make-circle-2d center 10.0d0 :name "C1"))
         (sketch (clad.sketch:make-sketch :name "CircleSketch")))
    (clad.sketch:add-entity sketch circle)
    ;; Convert to 3D wire on XY plane
    (let ((wire (clad.sketch:sketch-to-wire sketch)))
      (is (not (null wire))
          "sketch-to-wire should return a wire")
      (is (clad.core:shape-p wire)
          "Result should be a valid shape"))))

;;; Test 47: Convert Rectangle Sketch to Wire
(test test-sketch-rectangle-to-wire
  "Test converting a rectangular sketch to a closed wire"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 20.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 20.0d0 15.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 0.0d0 15.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2))
         (line2 (clad.sketch:make-line-2d p2 p3))
         (line3 (clad.sketch:make-line-2d p3 p4))
         (line4 (clad.sketch:make-line-2d p4 p1))
         (sketch (clad.sketch:make-sketch :name "RectSketch")))
    (clad.sketch:add-entity sketch line1)
    (clad.sketch:add-entity sketch line2)
    (clad.sketch:add-entity sketch line3)
    (clad.sketch:add-entity sketch line4)
    ;; Convert to wire
    (let ((wire (clad.sketch:sketch-to-wire sketch)))
      (is (not (null wire))
          "Should create a wire from 4 connected lines")
      (is (clad.core:shape-p wire)
          "Result should be a valid shape"))))

;;; Test 48: Convert Sketch to Face
(test test-sketch-to-face
  "Test converting a closed sketch to a face"
  (let* ((center (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (circle (clad.sketch:make-circle-2d center 10.0d0 :name "C1"))
         (sketch (clad.sketch:make-sketch :name "CircleSketch")))
    (clad.sketch:add-entity sketch circle)
    ;; Convert to face (closed region)
    (let ((face (clad.sketch:sketch-to-face sketch)))
      (is (not (null face))
          "sketch-to-face should return a face")
      (is (clad.core:shape-p face)
          "Result should be a valid shape"))))

;;; Test 49: Extrude Sketch to Solid
(test test-extrude-sketch
  "Test extruding a sketch to create a 3D solid"
  (let* ((center (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (circle (clad.sketch:make-circle-2d center 5.0d0 :name "C1"))
         (sketch (clad.sketch:make-sketch :name "ExtrudeSketch")))
    (clad.sketch:add-entity sketch circle)
    ;; Extrude 20mm in Z direction
    (let ((solid (clad.sketch:extrude-sketch sketch 20.0d0)))
      (is (not (null solid))
          "extrude-sketch should return a solid")
      (is (clad.core:shape-p solid)
          "Result should be a valid shape")
      ;; Check that it's a cylinder (volume should be pi*r^2*h)
      (let ((expected-volume (* pi (* 5.0d0 5.0d0) 20.0d0)))
        (is (< (abs (- (clad.shapes:volume solid) expected-volume)) 1.0d0)
            "Extruded circle should create cylinder with correct volume")))))

;;; Test 50: Extrude Rectangular Sketch
(test test-extrude-rectangle-sketch
  "Test extruding a rectangular sketch"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 10.0d0 8.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 0.0d0 8.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2))
         (line2 (clad.sketch:make-line-2d p2 p3))
         (line3 (clad.sketch:make-line-2d p3 p4))
         (line4 (clad.sketch:make-line-2d p4 p1))
         (sketch (clad.sketch:make-sketch :name "BoxSketch")))
    (clad.sketch:add-entity sketch line1)
    (clad.sketch:add-entity sketch line2)
    (clad.sketch:add-entity sketch line3)
    (clad.sketch:add-entity sketch line4)
    ;; Extrude 12mm
    (let ((solid (clad.sketch:extrude-sketch sketch 12.0d0)))
      (is (not (null solid))
          "Should create a box solid")
      (let ((expected-volume (* 10.0d0 8.0d0 12.0d0)))
        (is (< (abs (- (clad.shapes:volume solid) expected-volume)) 1.0d0)
            "Extruded rectangle should create box with correct volume")))))

;;; Test 51: Revolve Sketch to Solid
(test test-revolve-sketch
  "Test revolving a sketch to create a solid of revolution"
  (let* ((p1 (clad.sketch:make-point-2d 5.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 10.0d0 15.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 5.0d0 15.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2))
         (line2 (clad.sketch:make-line-2d p2 p3))
         (line3 (clad.sketch:make-line-2d p3 p4))
         (line4 (clad.sketch:make-line-2d p4 p1))
         (sketch (clad.sketch:make-sketch :name "RevolveSketch")))
    (clad.sketch:add-entity sketch line1)
    (clad.sketch:add-entity sketch line2)
    (clad.sketch:add-entity sketch line3)
    (clad.sketch:add-entity sketch line4)
    ;; Revolve around Y axis (vertical) 360 degrees
    (let ((solid (clad.sketch:revolve-sketch sketch :axis '(0 1 0) :angle (* 2 pi))))
      (is (not (null solid))
          "revolve-sketch should return a solid")
      (is (clad.core:shape-p solid)
          "Result should be a valid shape"))))

;;; Test 52: Sketch Plane - XY Plane
(test test-sketch-plane-xy
  "Test creating a sketch on the XY plane"
  (let* ((plane (clad.sketch:make-sketch-plane :xy))
         (center (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (circle (clad.sketch:make-circle-2d center 8.0d0))
         (sketch (clad.sketch:make-sketch :name "XYSketch" :plane plane)))
    (clad.sketch:add-entity sketch circle)
    (let ((solid (clad.sketch:extrude-sketch sketch 10.0d0)))
      (is (not (null solid))
          "Should extrude on XY plane")
      ;; Cylinder should extend in +Z direction
      (is (clad.core:shape-p solid)
          "Result should be a valid shape"))))

;;; Test 53: Sketch Plane - YZ Plane
(test test-sketch-plane-yz
  "Test creating a sketch on the YZ plane"
  (let* ((plane (clad.sketch:make-sketch-plane :yz))
         (center (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (circle (clad.sketch:make-circle-2d center 6.0d0))
         (sketch (clad.sketch:make-sketch :name "YZSketch" :plane plane)))
    (clad.sketch:add-entity sketch circle)
    (let ((solid (clad.sketch:extrude-sketch sketch 10.0d0)))
      (is (not (null solid))
          "Should extrude on YZ plane")
      (is (clad.core:shape-p solid)
          "Result should be a valid shape"))))

;;; Test 54: Sketch with Constraints, Then Extrude
(test test-constrained-sketch-extrude
  "Test extruding a constrained sketch"
  (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 12.0d0 3.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 12.0d0 12.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 0.0d0 12.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2))
         (line2 (clad.sketch:make-line-2d p2 p3))
         (line3 (clad.sketch:make-line-2d p3 p4))
         (line4 (clad.sketch:make-line-2d p4 p1))
         (c1 (clad.sketch.constraints:make-horizontal-constraint line1))
         (c2 (clad.sketch.constraints:make-vertical-constraint line2))
         (sketch (clad.sketch:make-sketch :name "ConstrainedBox")))
    ;; Add entities and constraints
    (clad.sketch:add-entity sketch line1)
    (clad.sketch:add-entity sketch line2)
    (clad.sketch:add-entity sketch line3)
    (clad.sketch:add-entity sketch line4)
    (clad.sketch:add-constraint sketch c1)
    (clad.sketch:add-constraint sketch c2)
    ;; Solve constraints
    (clad.sketch.solver:solve-sketch sketch)
    ;; Now extrude the constrained sketch
    (let ((solid (clad.sketch:extrude-sketch sketch 10.0d0)))
      (is (not (null solid))
          "Should extrude constrained sketch")
      (is (clad.core:shape-p solid)
          "Result should be a valid shape"))))

;;; Test 55: Sketch-Based Boolean Operations
(test test-sketch-boolean-operations
  "Test using extruded sketches in boolean operations"
  (let* (;; Create base box
         (base-center (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (base-circle (clad.sketch:make-circle-2d base-center 20.0d0))
         (base-sketch (clad.sketch:make-sketch :name "Base"))
         ;; Create hole sketch
         (hole-center (clad.sketch:make-point-2d 0.0d0 0.0d0))
         (hole-circle (clad.sketch:make-circle-2d hole-center 8.0d0))
         (hole-sketch (clad.sketch:make-sketch :name "Hole")))
    (clad.sketch:add-entity base-sketch base-circle)
    (clad.sketch:add-entity hole-sketch hole-circle)
    ;; Extrude both
    (let ((base-solid (clad.sketch:extrude-sketch base-sketch 15.0d0))
          (hole-solid (clad.sketch:extrude-sketch hole-sketch 20.0d0)))
      ;; Perform cut operation
      (let ((result (clad.core:cut-shapes base-solid hole-solid)))
        (is (not (null result))
            "Should be able to cut hole from base")
        (is (clad.core:shape-p result)
            "Result should be a valid shape")
        ;; Volume should be base minus hole
        (let* ((base-volume (* pi (* 20.0d0 20.0d0) 15.0d0))
               (hole-volume (* pi (* 8.0d0 8.0d0) 15.0d0))
               (expected-volume (- base-volume hole-volume)))
          (is (< (abs (- (clad.shapes:volume result) expected-volume)) 10.0d0)
              "Result should have correct volume"))))))

;;; ==============================================================================
;;; Week 9-10: Sketch Validation (Tests 56-65)
;;; ==============================================================================

;;; Test 56: Well-Constrained Sketch Detection
(test test-well-constrained-sketch
  "Test detecting a well-constrained sketch"
  (let* ((sketch (clad.sketch:make-sketch :name "WellConstrained"))
         ;; Two points with distance constraint, one fixed
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-constraint sketch c)
    ;; Validate the sketch
    (let ((status (clad.sketch.validation:validate-sketch sketch)))
      (is (eq status :well-constrained)
          "Sketch with fixed point and distance constraint should be well-constrained"))))

;;; Test 57: Underconstrained Detection - Floating Point
(test test-underconstrained-floating-point
  "Test detecting an underconstrained sketch with floating point"
  (let* ((sketch (clad.sketch:make-sketch :name "Underconstrained"))
         ;; Single unfixed point - has 2 DOF
         (p (clad.sketch:make-point-2d 5.0d0 5.0d0 :name "P1")))
    (clad.sketch:add-entity sketch p)
    ;; Validate the sketch
    (let ((status (clad.sketch.validation:validate-sketch sketch)))
      (is (eq status :under-constrained)
          "Sketch with single unfixed point should be underconstrained"))))

;;; Test 58: Underconstrained Detection - Partially Constrained Line
(test test-underconstrained-line
  "Test detecting underconstrained line"
  (let* ((sketch (clad.sketch:make-sketch :name "UnderLine"))
         ;; Line with one fixed endpoint but no other constraints
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 5.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1")))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch line)
    ;; P2 is not constrained (has 2 DOF)
    (let ((status (clad.sketch.validation:validate-sketch sketch)))
      (is (eq status :under-constrained)
          "Sketch with floating endpoint should be underconstrained"))))

;;; Test 59: Overconstrained Detection - Simple Case
(test test-overconstrained-simple
  "Test detecting overconstrained sketch with redundant constraints"
  (let* ((sketch (clad.sketch:make-sketch :name "OverSimple"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2" :fixed t)))
    ;; Both points fixed = 0 DOF, but we also add distance constraint
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    ;; This distance constraint is redundant
    (let ((c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
      (clad.sketch:add-constraint sketch c))
    (let ((status (clad.sketch.validation:validate-sketch sketch)))
      (is (eq status :over-constrained)
          "Sketch with two fixed points and distance constraint should be overconstrained"))))

;;; Test 60: Overconstrained Detection - Line with Conflicting Angle
(test test-overconstrained-line-angle
  "Test detecting overconstrained line with too many angle constraints"
  (let* ((sketch (clad.sketch:make-sketch :name "OverLine"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1"))
         ;; Add horizontal constraint
         (c1 (clad.sketch.constraints:make-horizontal-constraint line))
         ;; Add distance constraint
         (c2 (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch line)
    (clad.sketch:add-constraint sketch c1)
    (clad.sketch:add-constraint sketch c2)
    ;; Line has P1 fixed (removes 2 DOF), P2 has 2 DOF
    ;; Horizontal removes 1 DOF, distance removes 1 DOF = fully constrained
    ;; Now add one more constraint to make it overconstrained
    (let ((p3 (clad.sketch:make-point-2d 0.0d0 10.0d0 :name "P3" :fixed t))
          (c3 (clad.sketch.constraints:make-distance-constraint p2 p3 14.142d0)))
      (clad.sketch:add-entity sketch p3)
      (clad.sketch:add-constraint sketch c3)
      (let ((status (clad.sketch.validation:validate-sketch sketch)))
        (is (eq status :over-constrained)
            "Sketch with redundant constraints should be overconstrained")))))

;;; Test 61: Conflicting Constraints Detection
(test test-conflicting-constraints
  "Test detecting conflicting constraints that cannot be satisfied"
  (let* ((sketch (clad.sketch:make-sketch :name "Conflicting"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2" :fixed t))
         ;; These points are 10 units apart, but we'll require 20
         (c (clad.sketch.constraints:make-distance-constraint p1 p2 20.0d0)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-constraint sketch c)
    ;; This should be detected as conflicting
    (let ((status (clad.sketch.validation:validate-sketch sketch)))
      (is (or (eq status :conflicting) (eq status :over-constrained))
          "Fixed points with impossible distance should be conflicting or overconstrained"))))

;;; Test 62: Conflicting Angular Constraints
(test test-conflicting-angular-constraints
  "Test detecting conflicting angular constraints on same line"
  (let* ((sketch (clad.sketch:make-sketch :name "AngularConflict"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (line (clad.sketch:make-line-2d p1 p2 :name "L1"))
         ;; Add horizontal constraint (requires Y1 = Y2)
         (c1 (clad.sketch.constraints:make-horizontal-constraint line))
         ;; Add vertical constraint (requires X1 = X2) - conflicts with horizontal
         (c2 (clad.sketch.constraints:make-vertical-constraint line)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch line)
    (clad.sketch:add-constraint sketch c1)
    (clad.sketch:add-constraint sketch c2)
    ;; Horizontal and vertical constraints on same line are conflicting
    (let ((status (clad.sketch.validation:validate-sketch sketch)))
      (is (or (eq status :conflicting) (eq status :over-constrained))
          "Line cannot be both horizontal and vertical"))))

;;; Test 63: Degree of Freedom Calculation
(test test-degree-of-freedom-calculation
  "Test calculating degrees of freedom for a sketch"
  (let* ((sketch (clad.sketch:make-sketch :name "DOFTest"))
         ;; Two points: 4 DOF total
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1"))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2")))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    ;; Initial DOF should be 4 (2 points x 2 coords each)
    (let ((dof (clad.sketch.validation:degrees-of-freedom sketch)))
      (is (= dof 4)
          "Two unconstrained points should have 4 DOF"))
    ;; Fix one point (removes 2 DOF)
    (setf (clad.sketch:point-fixed-p p1) t)
    (let ((dof (clad.sketch.validation:degrees-of-freedom sketch)))
      (is (= dof 2)
          "One fixed point and one free point should have 2 DOF"))
    ;; Add distance constraint (removes 1 DOF)
    (let ((c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
      (clad.sketch:add-constraint sketch c)
      (let ((dof (clad.sketch.validation:degrees-of-freedom sketch)))
        (is (= dof 1)
            "Fixed point + distance constraint should leave 1 DOF")))))

;;; Test 64: Validation Status Reporting
(test test-validation-status-reporting
  "Test that validation returns detailed status information"
  (let* ((sketch (clad.sketch:make-sketch :name "StatusTest"))
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0 :name "P2"))
         (c (clad.sketch.constraints:make-distance-constraint p1 p2 10.0d0)))
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-constraint sketch c)
    ;; Get validation report
    (let ((report (clad.sketch.validation:validation-report sketch)))
      (is (not (null report))
          "validation-report should return non-nil")
      (is (consp report)
          "Validation report should be a data structure")
      ;; Report should contain status
      (is (member :status report)
          "Report should contain :status field"))))

;;; Test 65: Complex Validation Scenario
(test test-complex-validation-scenario
  "Test validation on a complex sketch with multiple entity types"
  (let* ((sketch (clad.sketch:make-sketch :name "Complex"))
         ;; Create a rectangle with constraints
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 20.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 20.0d0 15.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 0.0d0 15.0d0 :name "P4"))
         (line1 (clad.sketch:make-line-2d p1 p2 :name "L1"))
         (line2 (clad.sketch:make-line-2d p2 p3 :name "L2"))
         (line3 (clad.sketch:make-line-2d p3 p4 :name "L3"))
         (line4 (clad.sketch:make-line-2d p4 p1 :name "L4"))
         ;; Add constraints: horizontal, vertical, and dimensions
         (c1 (clad.sketch.constraints:make-horizontal-constraint line1))
         (c2 (clad.sketch.constraints:make-vertical-constraint line2))
         (c3 (clad.sketch.constraints:make-horizontal-constraint line3))
         (c4 (clad.sketch.constraints:make-vertical-constraint line4))
         (c5 (clad.sketch.constraints:make-distance-constraint p1 p2 20.0d0))
         (c6 (clad.sketch.constraints:make-distance-constraint p2 p3 15.0d0)))
    ;; Add all entities
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch p3)
    (clad.sketch:add-entity sketch p4)
    (clad.sketch:add-entity sketch line1)
    (clad.sketch:add-entity sketch line2)
    (clad.sketch:add-entity sketch line3)
    (clad.sketch:add-entity sketch line4)
    ;; Add all constraints
    (clad.sketch:add-constraint sketch c1)
    (clad.sketch:add-constraint sketch c2)
    (clad.sketch:add-constraint sketch c3)
    (clad.sketch:add-constraint sketch c4)
    (clad.sketch:add-constraint sketch c5)
    (clad.sketch:add-constraint sketch c6)
    ;; Validate
    (let ((status (clad.sketch.validation:validate-sketch sketch)))
      (is (not (null status))
          "Should return a validation status")
      (is (member status '(:well-constrained :under-constrained :over-constrained :conflicting))
          "Status should be one of the valid types"))))

;;; ==============================================================================
;;; Week 11-12: Advanced Integration (Tests 66-75)
;;; ==============================================================================

;;; Test 66: Sketch Plane from Face - Box Top Face
(test test-sketch-plane-from-box-face
  "Test creating a sketch plane from a box's top face"
  (let* ((box (clad.core:make-box 50.0d0 50.0d0 20.0d0))
         ;; Get top face (+Z direction, max extreme)
         (top-faces (clad.context:select-faces box :direction :+z :extreme :max))
         (top-face (first top-faces)))
    (is (not (null top-face))
        "Should find top face of box")
    ;; Create sketch plane from this face
    (let ((plane (clad.sketch:make-sketch-plane-from-face top-face)))
      (is (not (null plane))
          "Should create sketch plane from face")
      ;; Create a sketch on this plane
      (let* ((center (clad.sketch:make-point-2d 25.0d0 25.0d0))
             (circle (clad.sketch:make-circle-2d center 10.0d0))
             (sketch (clad.sketch:make-sketch :name "OnTopFace" :plane plane)))
        (clad.sketch:add-entity sketch circle)
        ;; Extrude should work from this plane
        (let ((solid (clad.sketch:extrude-sketch sketch 15.0d0)))
          (is (not (null solid))
              "Should extrude sketch from arbitrary face plane")
          (is (clad.core:shape-p solid)
              "Result should be a valid shape"))))))

;;; Test 67: Sketch Plane from Face - Cylinder Side
(test test-sketch-plane-from-cylinder-face
  "Test creating a sketch plane from a cylindrical face"
  (let* ((cylinder (clad.core:make-cylinder 20.0d0 50.0d0))
         ;; Get top face (planar face at +Z max)
         (top-faces (clad.context:select-faces cylinder :direction :+z :extreme :max))
         (top-face (first top-faces)))
    (is (not (null top-face))
        "Should find top face of cylinder")
    ;; Create sketch plane from this face
    (let ((plane (clad.sketch:make-sketch-plane-from-face top-face)))
      (is (not (null plane))
          "Should create sketch plane from cylindrical top face")
      ;; Create a simple sketch on this plane
      (let* ((p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :fixed t))
             (p2 (clad.sketch:make-point-2d 10.0d0 0.0d0))
             (line (clad.sketch:make-line-2d p1 p2))
             (sketch (clad.sketch:make-sketch :name "OnCylinderTop" :plane plane)))
        (clad.sketch:add-entity sketch line)
        (is (= 1 (length (clad.sketch:sketch-entities sketch)))
            "Should be able to add entities to sketch on cylinder face")))))

;;; Test 68: Multiple Sketch Planes on Same Part
(test test-multiple-sketch-planes-on-part
  "Test creating multiple sketches on different faces of the same part"
  (let* ((base-box (clad.core:make-box 100.0d0 100.0d0 20.0d0))
         ;; Get top and front faces
         (top-faces (clad.context:select-faces base-box :direction :+z :extreme :max))
         (front-faces (clad.context:select-faces base-box :direction :+y :extreme :max))
         (top-face (first top-faces))
         (front-face (first front-faces))
         ;; Create planes from faces
         (top-plane (clad.sketch:make-sketch-plane-from-face top-face))
         (front-plane (clad.sketch:make-sketch-plane-from-face front-face)))
    (is (not (null top-plane))
        "Should create plane from top face")
    (is (not (null front-plane))
        "Should create plane from front face")
    ;; Create sketches on both planes
    (let* ((sketch1 (clad.sketch:make-sketch :name "TopSketch" :plane top-plane))
           (sketch2 (clad.sketch:make-sketch :name "FrontSketch" :plane front-plane))
           (circle1 (clad.sketch:make-circle-2d (clad.sketch:make-point-2d 50.0d0 50.0d0) 15.0d0))
           (circle2 (clad.sketch:make-circle-2d (clad.sketch:make-point-2d 50.0d0 10.0d0) 8.0d0)))
      (clad.sketch:add-entity sketch1 circle1)
      (clad.sketch:add-entity sketch2 circle2)
      (is (not (null (clad.sketch:sketch-plane sketch1)))
          "First sketch should have plane")
      (is (not (null (clad.sketch:sketch-plane sketch2)))
          "Second sketch should have plane"))))

;;; Test 69: Multiple Sketches in Workflow - Base + Boss
(test test-multiple-sketches-workflow-base-boss
  "Test creating a part using multiple sketches in sequence"
  (let* (;; Sketch 1: Base profile (rectangle)
         (base-sketch (clad.sketch:make-sketch :name "Base"))
         (bp1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :fixed t))
         (bp2 (clad.sketch:make-point-2d 50.0d0 0.0d0))
         (bp3 (clad.sketch:make-point-2d 50.0d0 30.0d0))
         (bp4 (clad.sketch:make-point-2d 0.0d0 30.0d0))
         (bl1 (clad.sketch:make-line-2d bp1 bp2))
         (bl2 (clad.sketch:make-line-2d bp2 bp3))
         (bl3 (clad.sketch:make-line-2d bp3 bp4))
         (bl4 (clad.sketch:make-line-2d bp4 bp1)))
    ;; Add entities to base sketch
    (clad.sketch:add-entity base-sketch bl1)
    (clad.sketch:add-entity base-sketch bl2)
    (clad.sketch:add-entity base-sketch bl3)
    (clad.sketch:add-entity base-sketch bl4)
    ;; Extrude base
    (let ((base-solid (clad.sketch:extrude-sketch base-sketch 10.0d0)))
      (is (not (null base-solid))
          "Should create base solid from first sketch")
      ;; Sketch 2: Boss profile (circle) on top face
      (let* ((top-faces (clad.context:select-faces base-solid :direction :+z :extreme :max))
             (top-face (first top-faces))
             (boss-plane (clad.sketch:make-sketch-plane-from-face top-face))
             (boss-sketch (clad.sketch:make-sketch :name "Boss" :plane boss-plane))
             (boss-circle (clad.sketch:make-circle-2d (clad.sketch:make-point-2d 25.0d0 15.0d0) 8.0d0)))
        (clad.sketch:add-entity boss-sketch boss-circle)
        ;; Extrude boss
        (let ((boss-solid (clad.sketch:extrude-sketch boss-sketch 12.0d0)))
          (is (not (null boss-solid))
              "Should create boss solid from second sketch")
          ;; Combine them
          (let ((final-part (clad.core:fuse base-solid boss-solid)))
            (is (not (null final-part))
                "Should fuse base and boss")
            (is (clad.core:shape-p final-part)
                "Result should be valid shape")))))))

;;; Test 70: Multiple Sketches - Base with Cut
(test test-multiple-sketches-workflow-base-cut
  "Test using multiple sketches for additive and subtractive operations"
  (let* (;; Sketch 1: Base cylinder
         (base-sketch (clad.sketch:make-sketch :name "Base"))
         (base-circle (clad.sketch:make-circle-2d (clad.sketch:make-point-2d 0.0d0 0.0d0) 20.0d0)))
    (clad.sketch:add-entity base-sketch base-circle)
    ;; Extrude base
    (let ((base-solid (clad.sketch:extrude-sketch base-sketch 30.0d0)))
      (is (not (null base-solid))
          "Should create base cylinder")
      ;; Sketch 2: Hole profile on top face
      (let* ((top-faces (clad.context:select-faces base-solid :direction :+z :extreme :max))
             (top-face (first top-faces))
             (hole-plane (clad.sketch:make-sketch-plane-from-face top-face))
             (hole-sketch (clad.sketch:make-sketch :name "Hole" :plane hole-plane))
             (hole-circle (clad.sketch:make-circle-2d (clad.sketch:make-point-2d 0.0d0 0.0d0) 8.0d0)))
        (clad.sketch:add-entity hole-sketch hole-circle)
        ;; Extrude hole (through the part)
        (let ((hole-solid (clad.sketch:extrude-sketch hole-sketch 40.0d0)))
          (is (not (null hole-solid))
              "Should create hole solid from second sketch")
          ;; Cut the hole from base
          (let ((final-part (clad.core:cut-shapes base-solid hole-solid)))
            (is (not (null final-part))
                "Should cut hole from base")
            (is (clad.core:shape-p final-part)
                "Result should be valid shape")
            ;; Check volume (base - hole)
            (let* ((base-volume (* pi (* 20.0d0 20.0d0) 30.0d0))
                   (hole-volume (* pi (* 8.0d0 8.0d0) 30.0d0))
                   (expected-volume (- base-volume hole-volume)))
              (is (< (abs (- (clad.shapes:volume final-part) expected-volume)) 10.0d0)
                  "Cut part should have correct volume"))))))))

;;; Test 71: Chained Sketch Workflow
(test test-chained-sketch-workflow
  "Test creating a complex part with a chain of sketch operations"
  (let* (;; Step 1: Base plate
         (base-sketch (clad.sketch:make-sketch :name "BasePlate"))
         (bp1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :fixed t))
         (bp2 (clad.sketch:make-point-2d 60.0d0 0.0d0))
         (bp3 (clad.sketch:make-point-2d 60.0d0 40.0d0))
         (bp4 (clad.sketch:make-point-2d 0.0d0 40.0d0))
         (bl1 (clad.sketch:make-line-2d bp1 bp2))
         (bl2 (clad.sketch:make-line-2d bp2 bp3))
         (bl3 (clad.sketch:make-line-2d bp3 bp4))
         (bl4 (clad.sketch:make-line-2d bp4 bp1)))
    (clad.sketch:add-entity base-sketch bl1)
    (clad.sketch:add-entity base-sketch bl2)
    (clad.sketch:add-entity base-sketch bl3)
    (clad.sketch:add-entity base-sketch bl4)
    (let ((part (clad.sketch:extrude-sketch base-sketch 8.0d0)))
      (is (not (null part))
          "Step 1: Base plate created")
      ;; Step 2: Add boss on top
      (let* ((top-faces (clad.context:select-faces part :direction :+z :extreme :max))
             (boss-plane (clad.sketch:make-sketch-plane-from-face (first top-faces)))
             (boss-sketch (clad.sketch:make-sketch :name "Boss" :plane boss-plane))
             (boss-circ (clad.sketch:make-circle-2d (clad.sketch:make-point-2d 30.0d0 20.0d0) 10.0d0)))
        (clad.sketch:add-entity boss-sketch boss-circ)
        (let ((boss (clad.sketch:extrude-sketch boss-sketch 15.0d0)))
          (setf part (clad.core:fuse part boss))
          (is (not (null part))
              "Step 2: Boss added")
          ;; Step 3: Cut hole through boss
          (let* ((boss-top-faces (clad.context:select-faces part :direction :+z :extreme :max))
                 (hole-plane (clad.sketch:make-sketch-plane-from-face (first boss-top-faces)))
                 (hole-sketch (clad.sketch:make-sketch :name "Hole" :plane hole-plane))
                 (hole-circ (clad.sketch:make-circle-2d (clad.sketch:make-point-2d 30.0d0 20.0d0) 4.0d0)))
            (clad.sketch:add-entity hole-sketch hole-circ)
            (let ((hole (clad.sketch:extrude-sketch hole-sketch 25.0d0)))
              (setf part (clad.core:cut-shapes part hole))
              (is (not (null part))
                  "Step 3: Hole cut")
              (is (clad.core:shape-p part)
                  "Final part should be valid shape"))))))))

;;; Test 72: Sketch DSL Integration - Basic Sketch in Defpart
(test test-sketch-dsl-basic
  "Test defining a simple sketch within defpart using DSL"
  ;; Define a part with an inline sketch definition
  (clad.dsl:defpart simple-sketch-part ()
    "Part created from a sketch defined in DSL"
    (:sketch :name "BaseProfile" :plane :xy
      (:circle :center (0 0) :radius 15))
    (:extrude-sketch "BaseProfile" 20))
  ;; Create the part
  (let ((part (simple-sketch-part)))
    (is (not (null part))
        "Should create part from DSL sketch")
    (is (clad.core:shape-p part)
        "Result should be valid shape")
    ;; Check volume (cylinder: pi*r^2*h)
    (let ((expected-volume (* pi (* 15.0d0 15.0d0) 20.0d0)))
      (is (< (abs (- (clad.shapes:volume part) expected-volume)) 10.0d0)
          "Part should have correct volume"))))

;;; Test 73: Sketch DSL Integration - Constrained Sketch
(test test-sketch-dsl-constrained
  "Test defining a constrained sketch within defpart"
  (clad.dsl:defpart constrained-sketch-part ()
    "Part with constrained sketch defined in DSL"
    (:sketch :name "ConstrainedRect" :plane :xy
      (:point :name "P1" :at (0 0) :fixed t)
      (:point :name "P2" :at (20 0))
      (:point :name "P3" :at (20 15))
      (:point :name "P4" :at (0 15))
      (:line :name "L1" :from "P1" :to "P2")
      (:line :name "L2" :from "P2" :to "P3")
      (:line :name "L3" :from "P3" :to "P4")
      (:line :name "L4" :from "P4" :to "P1")
      (:constraint :horizontal "L1")
      (:constraint :vertical "L2")
      (:constraint :distance "P1" "P2" 20)
      (:constraint :distance "P2" "P3" 15))
    (:solve-sketch "ConstrainedRect")
    (:extrude-sketch "ConstrainedRect" 10))
  ;; Create the part
  (let ((part (constrained-sketch-part)))
    (is (not (null part))
        "Should create part from constrained DSL sketch")
    (is (clad.core:shape-p part)
        "Result should be valid shape")
    ;; Check volume (20x15x10 box)
    (let ((expected-volume (* 20.0d0 15.0d0 10.0d0)))
      (is (< (abs (- (clad.shapes:volume part) expected-volume)) 10.0d0)
          "Part should have correct volume"))))

;;; Test 74: Sketch DSL Integration - Sketch on Face
(test test-sketch-dsl-on-face
  "Test defining a sketch on an existing face using DSL"
  (clad.dsl:defpart sketch-on-face-part ()
    "Part with sketch on top face"
    (:body (clad.core:make-box 50 50 10))
    (:on-face :direction :+z :extreme :max
      (:sketch :name "TopCircle"
        (:circle :center (25 25) :radius 12))
      (:extrude-sketch "TopCircle" 8)))
  ;; Create the part
  (let ((part (sketch-on-face-part)))
    (is (not (null part))
        "Should create part with sketch on face")
    (is (clad.core:shape-p part)
        "Result should be valid shape")))

;;; Test 75: Complex Integration - Multiple Sketches with DSL
(test test-complex-sketch-dsl-integration
  "Test complex part creation using multiple sketches in DSL"
  (clad.dsl:defpart complex-sketch-part ()
    "Complex part with multiple sketches"
    ;; Base from sketch
    (:sketch :name "Base" :plane :xy
      (:point :name "P1" :at (0 0) :fixed t)
      (:point :name "P2" :at (40 0))
      (:point :name "P3" :at (40 30))
      (:point :name "P4" :at (0 30))
      (:line :from "P1" :to "P2")
      (:line :from "P2" :to "P3")
      (:line :from "P3" :to "P4")
      (:line :from "P4" :to "P1"))
    (:extrude-sketch "Base" 8)
    ;; Boss on top face
    (:on-face :direction :+z :extreme :max
      (:sketch :name "Boss"
        (:circle :center (20 15) :radius 8))
      (:extrude-sketch "Boss" 12))
    ;; Hole through boss
    (:on-face :direction :+z :extreme :max
      (:sketch :name "Hole"
        (:circle :center (20 15) :radius 3))
      (:cut-extrude-sketch "Hole" 25)))
  ;; Create the part
  (let ((part (complex-sketch-part)))
    (is (not (null part))
        "Should create complex part from multiple sketches")
    (is (clad.core:shape-p part)
        "Result should be valid shape")
    ;; Part should be base + boss - hole
    (let* ((base-volume (* 40.0d0 30.0d0 8.0d0))
           (boss-volume (* pi (* 8.0d0 8.0d0) 12.0d0))
           (hole-volume (* pi (* 3.0d0 3.0d0) 20.0d0))
           (expected-volume (- (+ base-volume boss-volume) hole-volume)))
      (is (< (abs (- (clad.shapes:volume part) expected-volume)) 50.0d0)
          "Part should have approximately correct volume"))))
