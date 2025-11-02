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
