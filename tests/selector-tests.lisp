;;;; tests/selector-tests.lisp --- Test suite for selector system

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite selector-tests
  :description "Tests for the selector system"
  :in clad-tests)

(in-suite selector-tests)

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defun make-test-box ()
  "Create a standard test box (100x100x100mm) for selector testing"
  (clad.shapes:wrap-shape
   (clad.core:make-box 100 100 100)
   'clad.shapes:cad-solid))

(defun make-test-cylinder ()
  "Create a standard test cylinder (radius 50mm, height 100mm)"
  (clad.shapes:wrap-shape
   (clad.core:make-cylinder 50 100)
   'clad.shapes:cad-solid))

;;; ============================================================================
;;; TDD Cycle 1: Basic Selector Protocol
;;; ============================================================================

(test basic-selector-protocol
  "Test that we can create and apply a basic selector"
  ;; RED: This test will fail initially
  (let ((selector (make-instance 'clad.selectors::base-selector)))
    (is (not (null selector)))
    ;; Applying to empty list should return empty list
    (is (null (clad.selectors::apply-selector selector nil)))))

;;; ============================================================================
;;; TDD Cycle 2: Direction Selector - Top Face
;;; ============================================================================

(test direction-selector-top-face
  "Test selecting the top face of a box using +Z direction"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         (selector (make-instance 'clad.selectors::direction-selector
                                  :axis :+z
                                  :extreme :max))
         (top-faces (clad.selectors::apply-selector selector all-faces)))

    ;; Should find exactly 1 face
    (is (= 1 (length top-faces)))

    ;; The face should be at the top (Z ≈ 100)
    ;; Use bounding box center since center-of-mass doesn't work for faces yet
    (let* ((top-face (first top-faces))
           (bbox (clad.shapes:bounding-box top-face))
           (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
      (is (> center-z 95))  ; Z coordinate should be near 100
      (is (< center-z 105)))))

;;; ============================================================================
;;; TDD Cycle 3: Direction Selector - All Directions
;;; ============================================================================

(test direction-selector-bottom-face
  "Test selecting the bottom face using -Z direction"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         (selector (make-instance 'clad.selectors::direction-selector
                                  :axis :+z
                                  :extreme :min))
         (bottom-faces (clad.selectors::apply-selector selector all-faces)))

    (is (= 1 (length bottom-faces)))

    ;; Use bounding box center
    (let* ((bottom-face (first bottom-faces))
           (bbox (clad.shapes:bounding-box bottom-face))
           (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
      (is (< center-z 5))    ; Z should be near 0
      (is (> center-z -5)))))

(test direction-selector-all-six-directions
  "Test selecting faces in all 6 cardinal directions"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box)))

    ;; Test each direction
    (dolist (dir-spec '((:+x :max 0) (:+x :min 1)
                        (:+y :max 0) (:+y :min 1)
                        (:+z :max 2) (:+z :min 2)))
      (let* ((axis (first dir-spec))
             (extreme (second dir-spec))
             (coord-index (third dir-spec))
             (selector (make-instance 'clad.selectors::direction-selector
                                      :axis axis
                                      :extreme extreme))
             (selected (clad.selectors::apply-selector selector all-faces)))

        ;; Should find exactly 1 face in each direction
        (is (= 1 (length selected))
            "Should find 1 face for ~A ~A" axis extreme)))))

;;; ============================================================================
;;; TDD Cycle 4: Face Normal Vectors (FFI Extension)
;;; ============================================================================

(test face-normal-extraction
  "Test that we can extract normal vectors from faces"
  ;; This test validates the FFI layer
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         ;; Get a face we know - let's get the top face
         (selector (make-instance 'clad.selectors::direction-selector
                                  :axis :+z :extreme :max))
         (top-faces (clad.selectors::apply-selector selector all-faces))
         (top-face (first top-faces)))

    ;; Get the normal vector
    (multiple-value-bind (nx ny nz)
        (clad.ffi:ffi-get-face-normal
         (clad.core:shape-handle (clad.shapes:unwrap-shape top-face)))

      ;; Normal vectors should be normalized (length = 1)
      (let ((length (sqrt (+ (* nx nx) (* ny ny) (* nz nz)))))
        (is (< (abs (- length 1.0)) 0.01)
            "Normal vector should be normalized"))

      ;; Top face should have normal pointing up (+Z)
      (is (> (abs nz) 0.9)
          "Top face normal should point primarily in Z direction"))))

;;; ============================================================================
;;; TDD Cycle 5: Parallel Selector
;;; ============================================================================

(test parallel-selector-z-axis
  "Test selecting faces parallel to Z axis"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         (selector (make-instance 'clad.selectors::parallel-selector
                                  :axis :z))
         (parallel-faces (clad.selectors::apply-selector selector all-faces)))

    ;; A box has 2 faces parallel to Z (top and bottom)
    (is (= 2 (length parallel-faces))
        "Box should have 2 faces parallel to Z")))

(test parallel-selector-x-axis
  "Test selecting faces parallel to X axis"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         (selector (make-instance 'clad.selectors::parallel-selector
                                  :axis :x))
         (parallel-faces (clad.selectors::apply-selector selector all-faces)))

    ;; A box has 2 faces parallel to X
    (is (= 2 (length parallel-faces)))))

;;; ============================================================================
;;; TDD Cycle 6: Perpendicular Selector
;;; ============================================================================

(test perpendicular-selector-z-axis
  "Test selecting faces perpendicular to Z axis"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         (selector (make-instance 'clad.selectors::perpendicular-selector
                                  :axis :z))
         (perp-faces (clad.selectors::apply-selector selector all-faces)))

    ;; A box has 4 faces perpendicular to Z (the 4 sides)
    (is (= 4 (length perp-faces))
        "Box should have 4 faces perpendicular to Z")))

;;; ============================================================================
;;; TDD Cycle 7: AND Combinator
;;; ============================================================================

(test and-combinator-two-selectors
  "Test combining two selectors with AND"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         ;; Select faces that are parallel to Z AND not at the top
         (parallel-z (make-instance 'clad.selectors::parallel-selector :axis :z))
         (not-top (make-instance 'clad.selectors::direction-selector
                                 :axis :+z
                                 :extreme :min))
         (and-selector (make-instance 'clad.selectors::and-selector
                                      :selectors (list parallel-z not-top)))
         (result (clad.selectors::apply-selector and-selector all-faces)))

    ;; Should find only the bottom face
    (is (= 1 (length result))
        "Should find exactly 1 face (bottom)")))

;;; ============================================================================
;;; TDD Cycle 8: OR Combinator
;;; ============================================================================

(test or-combinator-two-selectors
  "Test combining two selectors with OR"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         ;; Select faces that are either top OR bottom
         (top-sel (make-instance 'clad.selectors::direction-selector
                                 :axis :+z :extreme :max))
         (bottom-sel (make-instance 'clad.selectors::direction-selector
                                    :axis :+z :extreme :min))
         (or-selector (make-instance 'clad.selectors::or-selector
                                     :selectors (list top-sel bottom-sel)))
         (result (clad.selectors::apply-selector or-selector all-faces)))

    ;; Should find 2 faces (top and bottom)
    (is (= 2 (length result))
        "Should find 2 faces (top and bottom)")))

;;; ============================================================================
;;; TDD Cycle 9: NOT Combinator
;;; ============================================================================

(test not-combinator
  "Test NOT combinator to exclude shapes"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         ;; Select all faces EXCEPT the top
         (top-sel (make-instance 'clad.selectors::direction-selector
                                 :axis :+z :extreme :max))
         (not-selector (make-instance 'clad.selectors::not-selector
                                      :selector top-sel
                                      :universe all-faces))
         (result (clad.selectors::apply-selector not-selector all-faces)))

    ;; Should find 5 faces (all except top)
    (is (= 5 (length result))
        "Should find 5 faces (excluding top)")))

;;; ============================================================================
;;; TDD Cycle 10: Custom Predicate Selector
;;; ============================================================================

(test custom-predicate-selector
  "Test custom predicate selector with user-defined function"
  (let* ((box (make-test-box))
         (all-faces (clad.shapes:faces box))
         ;; Select faces with area greater than 9000 mm²
         ;; (100x100 faces have area 10000, should select 2)
         (selector (make-instance 'clad.selectors::custom-selector
                                  :predicate (lambda (face)
                                              (> (clad.shapes:area face) 9000))))
         (result (clad.selectors::apply-selector selector all-faces)))

    ;; A 100x100x100 box has different face areas depending on orientation
    ;; All faces are 100x100 = 10000 mm²
    (is (= 6 (length result))
        "All faces of a cube have the same area")))

;;; ============================================================================
;;; TDD Cycle 11: High-Level Select Function
;;; ============================================================================

(test select-function-direction
  "Test high-level select function with direction keyword"
  (let* ((box (make-test-box))
         (top-faces (clad.selectors:select (clad.shapes:faces box)
                                           :direction :+z :extreme :max)))
    (is (= 1 (length top-faces)))
    ;; Verify it's the top face using bounding box
    (let* ((bbox (clad.shapes:bounding-box (first top-faces)))
           (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
      (is (> center-z 95)))))

(test select-function-parallel
  "Test high-level select function with parallel keyword"
  (let* ((box (make-test-box))
         (z-faces (clad.selectors:select (clad.shapes:faces box)
                                         :parallel :z)))
    (is (= 2 (length z-faces)))))

(test select-function-custom-predicate
  "Test high-level select function with custom lambda"
  (let* ((box (make-test-box))
         (large-faces (clad.selectors:select (clad.shapes:faces box)
                                             (lambda (f)
                                               (> (clad.shapes:area f) 5000)))))
    (is (= 6 (length large-faces)))))

#|
(test select-function-combinator
  "Test high-level select function with combinator syntax"
  (let* ((box (make-test-box))
         ;; Select faces parallel to Z but not the top
         (result (clad.selectors:select (clad.shapes:faces box)
                                        (:and (:parallel :z)
                                              (:not (:direction :+z :extreme :max))))))
    (is (= 1 (length result)))))
|#
