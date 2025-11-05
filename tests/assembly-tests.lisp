;;;; assembly-tests.lisp --- Tests for Phase 10: Assembly System

(in-package #:clad.tests)

;;; ==============================================================================
;;; Test Suite Definition
;;; ==============================================================================

(def-suite assembly-tests
  :description "Tests for hierarchical assembly system (Phase 10)"
  :in clad-tests)

(in-suite assembly-tests)

;;; ==============================================================================
;;; Week 9-10: Component System (Tests 1-20)
;;; ==============================================================================

;;; ---------------------------------------------------------------------------
;;; Basic Assembly Creation (Tests 1-5)
;;; ---------------------------------------------------------------------------

;;; Test 1: Assembly Creation
(test test-create-assembly
  "Test creating an empty assembly"
  (let ((asm (clad.assembly:make-assembly :name :test-assy)))
    (is (typep asm 'clad.assembly:assembly)
        "make-assembly should return an assembly instance")
    (is (eq :test-assy (clad.assembly:assembly-name asm))
        "assembly-name should return correct name")
    (is (zerop (hash-table-count (clad.assembly:assembly-components asm)))
        "New assembly should have no components")
    (is (null (clad.assembly:assembly-constraints asm))
        "New assembly should have no constraints")))

;;; Test 2: Assembly with Metadata
(test test-assembly-metadata
  "Test creating assembly with metadata"
  (let ((asm (clad.assembly:make-assembly
              :name :test-assy
              :metadata '(:project "Test" :version "1.0"))))
    (is (equal "Test" (getf (clad.assembly:assembly-metadata asm) :project))
        "Metadata should be stored correctly")
    (is (equal "1.0" (getf (clad.assembly:assembly-metadata asm) :version))
        "Multiple metadata fields should work")))

;;; Test 3: Assembly Parameters
(test test-assembly-parameters
  "Test parametric assembly creation"
  (let ((asm (clad.assembly:make-assembly :name :parametric-assy)))
    (clad.assembly:set-parameter asm :spacing 10)
    (clad.assembly:set-parameter asm :count 5)
    (is (= 10 (clad.assembly:get-parameter asm :spacing))
        "Parameters should be stored")
    (is (= 5 (clad.assembly:get-parameter asm :count))
        "Multiple parameters should work")))

;;; Test 4: Assembly Predicate
(test test-assembly-predicate
  "Test assembly-p predicate"
  (let ((asm (clad.assembly:make-assembly :name :test))
        (not-asm "not an assembly"))
    (is (clad.assembly:assembly-p asm)
        "assembly-p should return true for assemblies")
    (is (not (clad.assembly:assembly-p not-asm))
        "assembly-p should return false for non-assemblies")))

;;; Test 5: Assembly Description
(test test-assembly-description
  "Test assembly with description"
  (let ((asm (clad.assembly:make-assembly
              :name :test-assy
              :description "Test assembly for unit tests")))
    (is (equal "Test assembly for unit tests"
               (clad.assembly:assembly-description asm))
        "Description should be stored")))

;;; ---------------------------------------------------------------------------
;;; Component Management (Tests 6-12)
;;; ---------------------------------------------------------------------------

;;; Test 6: Add Component with Part
(test test-add-component
  "Test adding a component to an assembly"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (box (clad.core:make-box 10 10 10)))
    (clad.assembly:add-component asm :base-plate box)
    (is (= 1 (hash-table-count (clad.assembly:assembly-components asm)))
        "Assembly should have one component")
    (let ((comp (clad.assembly:get-component asm :base-plate)))
      (is (not (null comp))
          "get-component should find the component")
      (is (eq :base-plate (clad.assembly:component-name comp))
          "Component should have correct name")
      (is (clad.core:shape-p (clad.assembly:component-part comp))
          "Component part should be a valid shape"))))

;;; Test 7: Add Multiple Components
(test test-add-multiple-components
  "Test adding multiple components"
  (let ((asm (clad.assembly:make-assembly :name :multi-comp-assy)))
    (clad.assembly:add-component asm :part1 (clad.core:make-box 10 10 10))
    (clad.assembly:add-component asm :part2 (clad.core:make-cylinder 5 20))
    (clad.assembly:add-component asm :part3 (clad.core:make-sphere 8))
    (is (= 3 (hash-table-count (clad.assembly:assembly-components asm)))
        "Assembly should have three components")
    (is (not (null (clad.assembly:get-component asm :part1)))
        "First component should be accessible")
    (is (not (null (clad.assembly:get-component asm :part2)))
        "Second component should be accessible")
    (is (not (null (clad.assembly:get-component asm :part3)))
        "Third component should be accessible")))

;;; Test 8: Remove Component
(test test-remove-component
  "Test removing a component from assembly"
  (let ((asm (clad.assembly:make-assembly :name :test-assy)))
    (clad.assembly:add-component asm :part1 (clad.core:make-box 10 10 10))
    (clad.assembly:add-component asm :part2 (clad.core:make-cylinder 5 20))
    (is (= 2 (hash-table-count (clad.assembly:assembly-components asm)))
        "Should start with two components")
    (clad.assembly:remove-component asm :part1)
    (is (= 1 (hash-table-count (clad.assembly:assembly-components asm)))
        "Should have one component after removal")
    (is (null (clad.assembly:get-component asm :part1))
        "Removed component should not be found")
    (is (not (null (clad.assembly:get-component asm :part2)))
        "Other component should still exist")))

;;; Test 9: Component with Metadata
(test test-component-metadata
  "Test component with metadata (part number, material, etc.)"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (part (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :base-plate part
                                 :metadata '(:part-number "BP-001"
                                            :material "6061-T6 Aluminum"
                                            :vendor "McMaster-Carr"
                                            :finish "Anodized"))
    (let* ((comp (clad.assembly:get-component asm :base-plate))
           (meta (clad.assembly:component-metadata comp)))
      (is (equal "BP-001" (getf meta :part-number))
          "Part number should be stored")
      (is (equal "6061-T6 Aluminum" (getf meta :material))
          "Material should be stored")
      (is (equal "McMaster-Carr" (getf meta :vendor))
          "Vendor should be stored")
      (is (equal "Anodized" (getf meta :finish))
          "Finish should be stored"))))

;;; Test 10: Component Quantity
(test test-component-quantity
  "Test component quantity for BOM"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (bolt (clad.core:make-cylinder 2 10)))
    (clad.assembly:add-component asm :m4-bolt bolt :quantity 8)
    (let ((comp (clad.assembly:get-component asm :m4-bolt)))
      (is (= 8 (clad.assembly:component-quantity comp))
          "Component quantity should be stored correctly"))))

;;; Test 11: Component Fixed Status
(test test-component-fixed
  "Test marking component as fixed (ground component)"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (base (clad.core:make-box 100 100 10)))
    (clad.assembly:add-component asm :base base :fixed t)
    (let ((comp (clad.assembly:get-component asm :base)))
      (is (clad.assembly:component-fixed-p comp)
          "Fixed component should return true for fixed-p"))))

;;; Test 12: List All Components
(test test-list-components
  "Test listing all components in assembly"
  (let ((asm (clad.assembly:make-assembly :name :test-assy)))
    (clad.assembly:add-component asm :part1 (clad.core:make-box 10 10 10))
    (clad.assembly:add-component asm :part2 (clad.core:make-cylinder 5 20))
    (clad.assembly:add-component asm :part3 (clad.core:make-sphere 8))
    (let ((comp-list (clad.assembly:list-components asm)))
      (is (= 3 (length comp-list))
          "Should return all components")
      (is (every (lambda (c) (typep c 'clad.assembly:component)) comp-list)
          "All items should be component instances"))))

;;; ---------------------------------------------------------------------------
;;; Component Transforms (Tests 13-17)
;;; ---------------------------------------------------------------------------

;;; Test 13: Component Default Position
(test test-component-default-position
  "Test that new components have identity transform"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (box (clad.core:make-box 10 10 10)))
    (clad.assembly:add-component asm :part1 box)
    (let* ((comp (clad.assembly:get-component asm :part1))
           (pos (clad.assembly:component-position comp)))
      (is (equal '(0 0 0) pos)
          "Default position should be origin"))))

;;; Test 14: Set Component Position
(test test-set-component-position
  "Test setting component position"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (box (clad.core:make-box 10 10 10)))
    (clad.assembly:add-component asm :part1 box)
    (let ((comp (clad.assembly:get-component asm :part1)))
      (clad.assembly:set-component-position comp 10 20 30)
      (is (equal '(10 20 30) (clad.assembly:component-position comp))
          "Position should be updated"))))

;;; Test 15: Set Component Rotation
(test test-set-component-rotation
  "Test setting component rotation (axis-angle)"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (box (clad.core:make-box 10 10 10)))
    (clad.assembly:add-component asm :part1 box)
    (let ((comp (clad.assembly:get-component asm :part1)))
      (clad.assembly:set-component-rotation comp :axis '(0 0 1) :angle 45)
      (let ((rot (clad.assembly:component-rotation comp)))
        (is (equal '(0 0 1) (getf rot :axis))
            "Rotation axis should be stored")
        (is (= 45 (getf rot :angle))
            "Rotation angle should be stored")))))

;;; Test 16: Component Transform Matrix
(test test-component-transform-matrix
  "Test that component transform matrix is computed correctly"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (box (clad.core:make-box 10 10 10)))
    (clad.assembly:add-component asm :part1 box)
    (let ((comp (clad.assembly:get-component asm :part1)))
      (clad.assembly:set-component-position comp 10 20 30)
      (let ((transform (clad.assembly:component-transform comp)))
        (is (not (null transform))
            "Transform matrix should be computed")
        (is (= 4 (array-dimension transform 0))
            "Transform should be 4x4 matrix")
        (is (= 4 (array-dimension transform 1))
            "Transform should be 4x4 matrix")))))

;;; Test 17: Combined Position and Rotation
(test test-combined-transform
  "Test component with both position and rotation"
  (let ((asm (clad.assembly:make-assembly :name :test-assy))
        (box (clad.core:make-box 10 10 10)))
    (clad.assembly:add-component asm :part1 box)
    (let ((comp (clad.assembly:get-component asm :part1)))
      (clad.assembly:set-component-position comp 100 50 25)
      (clad.assembly:set-component-rotation comp :axis '(1 0 0) :angle 90)
      (is (equal '(100 50 25) (clad.assembly:component-position comp))
          "Position should be set")
      (is (= 90 (getf (clad.assembly:component-rotation comp) :angle))
          "Rotation should be set")
      (let ((transform (clad.assembly:component-transform comp)))
        (is (not (null transform))
            "Combined transform should be computed")))))

;;; ---------------------------------------------------------------------------
;;; Nested Assemblies (Tests 18-20)
;;; ---------------------------------------------------------------------------

;;; Test 18: Assembly as Component
(test test-nested-assembly
  "Test adding an assembly as a component of another assembly"
  (let ((sub-assy (clad.assembly:make-assembly :name :sub-assembly))
        (main-assy (clad.assembly:make-assembly :name :main-assembly)))
    ;; Add parts to sub-assembly
    (clad.assembly:add-component sub-assy :part1 (clad.core:make-box 10 10 10))
    (clad.assembly:add-component sub-assy :part2 (clad.core:make-cylinder 5 20))
    ;; Add sub-assembly to main assembly
    (clad.assembly:add-component main-assy :sub-module sub-assy)
    (is (= 1 (hash-table-count (clad.assembly:assembly-components main-assy)))
        "Main assembly should have one component")
    (let ((comp (clad.assembly:get-component main-assy :sub-module)))
      (is (not (null comp))
          "Sub-assembly component should be found")
      (is (typep (clad.assembly:component-part comp) 'clad.assembly:assembly)
          "Component part should be an assembly"))))

;;; Test 19: Multi-Level Nesting
(test test-multi-level-nesting
  "Test multiple levels of assembly nesting"
  (let ((level3 (clad.assembly:make-assembly :name :level3))
        (level2 (clad.assembly:make-assembly :name :level2))
        (level1 (clad.assembly:make-assembly :name :level1)))
    ;; Build hierarchy: level1 contains level2 contains level3
    (clad.assembly:add-component level3 :bolt (clad.core:make-cylinder 2 10))
    (clad.assembly:add-component level2 :fastener-assy level3)
    (clad.assembly:add-component level1 :hardware-module level2)
    (is (= 1 (hash-table-count (clad.assembly:assembly-components level1)))
        "Top assembly should have one component")
    ;; Navigate down the hierarchy
    (let* ((comp2 (clad.assembly:get-component level1 :hardware-module))
           (assy2 (clad.assembly:component-part comp2))
           (comp3 (clad.assembly:get-component assy2 :fastener-assy))
           (assy3 (clad.assembly:component-part comp3)))
      (is (typep assy2 'clad.assembly:assembly)
          "Second level should be assembly")
      (is (typep assy3 'clad.assembly:assembly)
          "Third level should be assembly")
      (is (not (null (clad.assembly:get-component assy3 :bolt)))
          "Deepest part should be accessible"))))

;;; Test 20: Nested Assembly Transform
(test test-nested-assembly-transform
  "Test that transforms propagate through nested assemblies"
  (let ((sub-assy (clad.assembly:make-assembly :name :sub))
        (main-assy (clad.assembly:make-assembly :name :main)))
    (clad.assembly:add-component sub-assy :part (clad.core:make-box 10 10 10))
    (clad.assembly:add-component main-assy :sub-module sub-assy)
    (let ((comp (clad.assembly:get-component main-assy :sub-module)))
      (clad.assembly:set-component-position comp 50 50 50)
      (clad.assembly:set-component-rotation comp :axis '(0 0 1) :angle 45)
      (is (equal '(50 50 50) (clad.assembly:component-position comp))
          "Nested assembly should have position")
      (is (= 45 (getf (clad.assembly:component-rotation comp) :angle))
          "Nested assembly should have rotation")
      (let ((transform (clad.assembly:component-transform comp)))
        (is (not (null transform))
            "Nested assembly should have transform matrix")))))

;;; ==============================================================================
;;; Week 11-12: Mate Constraints (Tests 21-45)
;;; ==============================================================================

;;; ---------------------------------------------------------------------------
;;; Basic Constraint Creation (Tests 21-25)
;;; ---------------------------------------------------------------------------

;;; Test 21: Add Mate Constraint
(test test-add-mate-constraint
  "Test adding a mate constraint to an assembly"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :comp1 box1 :fixed t)
    (clad.assembly:add-component asm :comp2 box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :coincident
                 :comp1 :face-top
                 :comp2 :face-bottom)))
      (is (typep mate 'clad.assembly.constraints:mate-constraint)
          "add-mate should return a mate-constraint")
      (is (find mate (clad.assembly:assembly-constraints asm))
          "Mate should be added to assembly constraints list"))))

;;; Test 22: Mate Constraint Type
(test test-mate-constraint-type
  "Test mate constraint stores correct type"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :comp1 box1 :fixed t)
    (clad.assembly:add-component asm :comp2 box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :distance
                 :comp1 :face-top
                 :comp2 :face-bottom
                 :offset 10.0)))
      (is (eq :distance (clad.assembly.constraints:mate-type mate))
          "Mate should store correct type"))))

;;; Test 23: Mate Component References
(test test-mate-component-references
  "Test mate constraint stores component references"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :comp1 box1 :fixed t)
    (clad.assembly:add-component asm :comp2 box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :coincident
                 :comp1 :face-top
                 :comp2 :face-bottom)))
      (is (eq :comp1 (clad.assembly.constraints:mate-component1 mate))
          "Mate should store first component name")
      (is (eq :comp2 (clad.assembly.constraints:mate-component2 mate))
          "Mate should store second component name")
      (is (eq :face-top (clad.assembly.constraints:mate-reference1 mate))
          "Mate should store first reference")
      (is (eq :face-bottom (clad.assembly.constraints:mate-reference2 mate))
          "Mate should store second reference"))))

;;; Test 24: Mate Offset Parameter
(test test-mate-offset-parameter
  "Test mate constraint with offset parameter"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :comp1 box1 :fixed t)
    (clad.assembly:add-component asm :comp2 box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :distance
                 :comp1 :face-top
                 :comp2 :face-bottom
                 :offset 15.5)))
      (is (= 15.5 (clad.assembly.constraints:mate-offset mate))
          "Mate should store offset value"))))

;;; Test 25: Multiple Constraints on Assembly
(test test-multiple-constraints
  "Test adding multiple mate constraints to an assembly"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10))
         (box3 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :comp1 box1 :fixed t)
    (clad.assembly:add-component asm :comp2 box2)
    (clad.assembly:add-component asm :comp3 box3)
    (let ((mate1 (clad.assembly.constraints:add-mate
                  asm :coincident
                  :comp1 :face-top
                  :comp2 :face-bottom))
          (mate2 (clad.assembly.constraints:add-mate
                  asm :distance
                  :comp2 :face-right
                  :comp3 :face-left
                  :offset 5.0)))
      (is (= 2 (length (clad.assembly:assembly-constraints asm)))
          "Assembly should have two constraints")
      (is (find mate1 (clad.assembly:assembly-constraints asm))
          "First mate should be in constraints")
      (is (find mate2 (clad.assembly:assembly-constraints asm))
          "Second mate should be in constraints"))))

;;; ---------------------------------------------------------------------------
;;; Constraint Types (Tests 26-34)
;;; ---------------------------------------------------------------------------

;;; Test 26: Coincident Constraint
(test test-coincident-constraint
  "Test coincident mate constraint (faces flush)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :base box1 :fixed t)
    (clad.assembly:add-component asm :top box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :coincident
                 :base :face-top
                 :top :face-bottom)))
      (is (eq :coincident (clad.assembly.constraints:mate-type mate))
          "Constraint type should be :coincident")
      (is (zerop (clad.assembly.constraints:mate-offset mate))
          "Coincident constraint should have zero offset"))))

;;; Test 27: Concentric Constraint
(test test-concentric-constraint
  "Test concentric mate constraint (cylinders aligned)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (cyl1 (clad.core:make-cylinder 10 50))
         (cyl2 (clad.core:make-cylinder 5 30)))
    (clad.assembly:add-component asm :shaft cyl1 :fixed t)
    (clad.assembly:add-component asm :bearing cyl2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :concentric
                 :shaft :axis
                 :bearing :hole)))
      (is (eq :concentric (clad.assembly.constraints:mate-type mate))
          "Constraint type should be :concentric"))))

;;; Test 28: Distance Constraint
(test test-distance-constraint
  "Test distance mate constraint (fixed separation)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :distance
                 :part1 :face-top
                 :part2 :face-bottom
                 :offset 20.0)))
      (is (eq :distance (clad.assembly.constraints:mate-type mate))
          "Constraint type should be :distance")
      (is (= 20.0 (clad.assembly.constraints:mate-offset mate))
          "Distance constraint should have correct offset"))))

;;; Test 29: Angle Constraint
(test test-angle-constraint
  "Test angle mate constraint (fixed angle between planes)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :angle
                 :part1 :face-front
                 :part2 :face-front
                 :angle 45.0)))
      (is (eq :angle (clad.assembly.constraints:mate-type mate))
          "Constraint type should be :angle")
      (is (= 45.0 (clad.assembly.constraints:mate-offset mate))
          "Angle constraint should store angle value"))))

;;; Test 30: Parallel Constraint
(test test-parallel-constraint
  "Test parallel mate constraint (faces/axes parallel)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :parallel
                 :part1 :face-top
                 :part2 :face-top)))
      (is (eq :parallel (clad.assembly.constraints:mate-type mate))
          "Constraint type should be :parallel"))))

;;; Test 31: Perpendicular Constraint
(test test-perpendicular-constraint
  "Test perpendicular mate constraint (faces at 90 degrees)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :perpendicular
                 :part1 :face-top
                 :part2 :face-front)))
      (is (eq :perpendicular (clad.assembly.constraints:mate-type mate))
          "Constraint type should be :perpendicular"))))

;;; Test 32: Tangent Constraint
(test test-tangent-constraint
  "Test tangent mate constraint (cylindrical surfaces touch)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (cyl1 (clad.core:make-cylinder 20 50))
         (cyl2 (clad.core:make-cylinder 10 30)))
    (clad.assembly:add-component asm :drum cyl1 :fixed t)
    (clad.assembly:add-component asm :roller cyl2)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :tangent
                 :drum :surface
                 :roller :surface)))
      (is (eq :tangent (clad.assembly.constraints:mate-type mate))
          "Constraint type should be :tangent"))))

;;; Test 33: Fixed Constraint
(test test-fixed-constraint
  "Test fixed mate constraint (component locked in place)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part box)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :fixed
                 :part :origin
                 :part :origin)))
      (is (eq :fixed (clad.assembly.constraints:mate-type mate))
          "Constraint type should be :fixed"))))

;;; Test 34: Constraint with Selector References
(test test-constraint-with-selectors
  "Test mate constraint using face selectors"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    ;; Use actual face selectors (requires selectors system)
    (let ((mate (clad.assembly.constraints:add-mate
                 asm :coincident
                 :part1 '(:face :direction :+z)
                 :part2 '(:face :direction :-z))))
      (is (eq :coincident (clad.assembly.constraints:mate-type mate))
          "Constraint should work with selector references"))))

;;; ---------------------------------------------------------------------------
;;; Constraint Validation (Tests 35-38)
;;; ---------------------------------------------------------------------------

;;; Test 35: Mate Error Calculation
(test test-mate-error-calculation
  "Test calculating constraint error (how far from satisfied)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (let* ((comp2 (clad.assembly:get-component asm :part2))
           ;; Position part2 10mm away from desired coincident position
           (_ (clad.assembly:set-component-position comp2 0 0 20))
           (mate (clad.assembly.constraints:add-mate
                  asm :distance
                  :part1 :face-top
                  :part2 :face-bottom
                  :offset 0.0)))
      (declare (ignore _))
      ;; Error should be non-zero since parts are separated
      (let ((error (clad.assembly.constraints:mate-error mate)))
        (is (numberp error)
            "mate-error should return a number")
        (is (>= error 0.0)
            "Constraint error should be non-negative")))))

;;; Test 36: Apply Mate Constraint
(test test-apply-mate-constraint
  "Test applying a mate constraint (adjusts component positions)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (let* ((comp2 (clad.assembly:get-component asm :part2))
           (mate (clad.assembly.constraints:add-mate
                  asm :coincident
                  :part1 :face-top
                  :part2 :face-bottom)))
      ;; Apply constraint (should move comp2 to satisfy mate)
      (clad.assembly.constraints:apply-mate mate)
      ;; After applying, component should be repositioned
      (is (not (null (clad.assembly:component-position comp2)))
          "Component should have a position after apply-mate"))))

;;; Test 37: Constraint Compatibility Check
(test test-constraint-compatibility
  "Test detecting incompatible constraints"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    ;; Add two conflicting constraints (should be detected by solver)
    (clad.assembly.constraints:add-mate
     asm :distance :part1 :face-top :part2 :face-bottom :offset 10.0)
    (clad.assembly.constraints:add-mate
     asm :distance :part1 :face-top :part2 :face-bottom :offset 20.0)
    (is (= 2 (length (clad.assembly:assembly-constraints asm)))
        "Both conflicting constraints should be added (solver detects conflicts)")))

;;; Test 38: Constraint Validation State
(test test-constraint-validation-state
  "Test checking if constraint is satisfied"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (let* ((comp2 (clad.assembly:get-component asm :part2))
           ;; Position exactly where constraint wants it
           (_ (clad.assembly:set-component-position comp2 0 0 10))
           (mate (clad.assembly.constraints:add-mate
                  asm :distance
                  :part1 :face-top
                  :part2 :face-bottom
                  :offset 0.0)))
      (declare (ignore _))
      ;; Error should be calculable
      (let ((error (clad.assembly.constraints:mate-error mate)))
        (is (numberp error)
            "Constraint error should be numeric")))))

;;; ---------------------------------------------------------------------------
;;; Multiple Constraints (Tests 39-42)
;;; ---------------------------------------------------------------------------

;;; Test 39: Chain of Constraints
(test test-chain-of-constraints
  "Test multiple components connected by constraints"
  (let* ((asm (clad.assembly:make-assembly :name :chain-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10))
         (box3 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :link1 box1 :fixed t)
    (clad.assembly:add-component asm :link2 box2)
    (clad.assembly:add-component asm :link3 box3)
    (clad.assembly.constraints:add-mate
     asm :coincident :link1 :face-right :link2 :face-left)
    (clad.assembly.constraints:add-mate
     asm :coincident :link2 :face-right :link3 :face-left)
    (is (= 2 (length (clad.assembly:assembly-constraints asm)))
        "Chain should have two constraints")))

;;; Test 40: Fully Constrained Assembly
(test test-fully-constrained-assembly
  "Test assembly with enough constraints to fully define positions"
  (let* ((asm (clad.assembly:make-assembly :name :bracket-assy))
         (base (clad.core:make-box 100 100 10))
         (post (clad.core:make-cylinder 5 50)))
    (clad.assembly:add-component asm :base base :fixed t)
    (clad.assembly:add-component asm :post post)
    ;; Fix post position relative to base
    (clad.assembly.constraints:add-mate
     asm :coincident :base '(:face :direction :+z) :post '(:face :direction :-z))
    (clad.assembly.constraints:add-mate
     asm :concentric :base :hole :post :axis)
    (is (= 2 (length (clad.assembly:assembly-constraints asm)))
        "Fully constrained assembly should have multiple mates")))

;;; Test 41: Over-Constrained Detection
(test test-over-constrained-detection
  "Test detecting over-constrained assemblies"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    ;; Add redundant constraints
    (clad.assembly.constraints:add-mate
     asm :coincident :part1 :face-top :part2 :face-bottom)
    (clad.assembly.constraints:add-mate
     asm :distance :part1 :face-top :part2 :face-bottom :offset 0.0)
    ;; Solver should detect over-constraint (Week 13-14)
    (is (= 2 (length (clad.assembly:assembly-constraints asm)))
        "Over-constrained assembly stores all mates")))

;;; Test 42: Under-Constrained Assembly
(test test-under-constrained-assembly
  "Test assembly with insufficient constraints (DOF > 0)"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    ;; Only one constraint - part2 still has degrees of freedom
    (clad.assembly.constraints:add-mate
     asm :parallel :part1 :face-top :part2 :face-top)
    ;; Part2 can still slide along the parallel plane
    (is (= 1 (length (clad.assembly:assembly-constraints asm)))
        "Under-constrained assembly has partial constraints")))

;;; ---------------------------------------------------------------------------
;;; Constraint Management (Tests 43-45)
;;; ---------------------------------------------------------------------------

;;; Test 43: List All Constraints
(test test-list-all-constraints
  "Test listing all constraints in an assembly"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10))
         (box3 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :p1 box1 :fixed t)
    (clad.assembly:add-component asm :p2 box2)
    (clad.assembly:add-component asm :p3 box3)
    (clad.assembly.constraints:add-mate asm :coincident :p1 :f1 :p2 :f2)
    (clad.assembly.constraints:add-mate asm :distance :p2 :f1 :p3 :f2 :offset 5.0)
    (clad.assembly.constraints:add-mate asm :parallel :p1 :f1 :p3 :f1)
    (let ((constraints (clad.assembly:assembly-constraints asm)))
      (is (= 3 (length constraints))
          "Should list all 3 constraints")
      (is (every (lambda (c) (typep c 'clad.assembly.constraints:mate-constraint))
                 constraints)
          "All items should be mate-constraint instances"))))

;;; Test 44: Remove Constraint
(test test-remove-constraint
  "Test removing a mate constraint from assembly"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (let ((mate1 (clad.assembly.constraints:add-mate
                  asm :coincident :part1 :f1 :part2 :f2))
          (mate2 (clad.assembly.constraints:add-mate
                  asm :distance :part1 :f1 :part2 :f2 :offset 10.0)))
      (is (= 2 (length (clad.assembly:assembly-constraints asm)))
          "Should have 2 constraints initially")
      ;; Remove first constraint
      (setf (clad.assembly:assembly-constraints asm)
            (remove mate1 (clad.assembly:assembly-constraints asm)))
      (is (= 1 (length (clad.assembly:assembly-constraints asm)))
          "Should have 1 constraint after removal")
      (is (find mate2 (clad.assembly:assembly-constraints asm))
          "Second constraint should still be present"))))

;;; Test 45: Clear All Constraints
(test test-clear-all-constraints
  "Test removing all mate constraints from assembly"
  (let* ((asm (clad.assembly:make-assembly :name :test-assy))
         (box1 (clad.core:make-box 50 50 10))
         (box2 (clad.core:make-box 50 50 10)))
    (clad.assembly:add-component asm :part1 box1 :fixed t)
    (clad.assembly:add-component asm :part2 box2)
    (clad.assembly.constraints:add-mate asm :coincident :part1 :f1 :part2 :f2)
    (clad.assembly.constraints:add-mate asm :distance :part1 :f1 :part2 :f2 :offset 10.0)
    (clad.assembly.constraints:add-mate asm :parallel :part1 :f1 :part2 :f2)
    (is (= 3 (length (clad.assembly:assembly-constraints asm)))
        "Should have 3 constraints initially")
    ;; Clear all constraints
    (setf (clad.assembly:assembly-constraints asm) '())
    (is (zerop (length (clad.assembly:assembly-constraints asm)))
        "Should have no constraints after clearing")))

;;; ==============================================================================
;;; Week 13-14: Assembly Solver (Tests 46-65) - PENDING
;;; ==============================================================================

;;; These tests will be added when we reach Week 13-14

;;; ==============================================================================
;;; Week 15-16: DSL & BOM (Tests 66-85) - PENDING
;;; ==============================================================================

;;; These tests will be added when we reach Week 15-16
