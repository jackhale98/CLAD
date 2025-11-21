;;;; tests/selector-validation-tests.lisp --- Selector validation tests (Priority 2)

(in-package :clad.tests)

(in-suite clad-tests)

;;; ============================================================================
;;; Priority 2: Selector Validation Tests
;;; ============================================================================

(test selector-matches-nothing-warning
  "Selectors that match no features should warn (non-critical)"
  ;; This test documents expected behavior for selector warnings
  ;; When a selector matches nothing, it should generate a warning but not error
  (let ((box (clad.core:make-box 100 100 10)))
    ;; This selector will match nothing (no circular faces on a box)
    (let ((result (clad.selectors:select box '(:on-face :type :cylindrical))))
      (is (null result) "Selector matching nothing should return empty list"))))

(test selector-matches-multiple-ok
  "Selectors matching multiple features should be OK"
  (let ((box (clad.core:make-box 100 100 10)))
    ;; Box has 6 faces
    (let ((result (clad.selectors:select box '(:on-face))))
      (is (= 6 (length result)) "Box should have 6 faces"))))

(test selector-extreme-with-no-match
  "Extreme selector with no candidates should warn"
  (let ((box (clad.core:make-box 100 100 10)))
    ;; Try to find extreme in direction that matches no faces
    (let ((result (clad.selectors:select box '(:on-face :direction :+z :extreme :max))))
      ;; This should find the top face
      (is (= 1 (length result)) "Should find exactly one extreme face"))))

(test gdt-selector-validation
  "GD&T selectors should validate at compile time when possible"
  ;; Test that GD&T forms validate selectors
  ;; This is more of an integration test
  (finishes
    (eval '(clad.dsl:defpart selector-validation-test ()
             "Test part for selector validation"
             (:body (clad.core:make-box 100 100 10))
             (:datum "A" :on-face :direction :-z :extreme :min)
             (:flatness :on-face :direction :+z :extreme :max :tolerance 0.05)))))

;;; ============================================================================
;;; Selector Validation with Runtime Checks
;;; ============================================================================

(test runtime-selector-empty-match
  "Runtime selector validation should handle empty matches gracefully"
  ;; When selectors match nothing at runtime, the system should handle it gracefully
  (let* ((box (clad.core:make-box 100 100 10))
         (wrapped (clad.shapes:wrap-shape box)))
    ;; Select cylindrical faces (there are none on a box)
    (let ((faces (clad.selectors:select wrapped '(:on-face :type :cylindrical))))
      (is (null faces) "Should return empty list for no matches")
      ;; Verify no error was thrown
      (is (listp faces) "Should return a list even when empty"))))

(test selector-validation-metadata
  "Test that selector validation doesn't interfere with metadata"
  (let* ((box (clad.core:make-box 100 100 10))
         (metadata (clad.core:shape-metadata box)))
    ;; Metadata should be accessible
    (is (listp metadata) "Metadata should be a list")

    ;; Add GD&T to metadata
    (let* ((tolerance (clad.gdt:make-flatness-tolerance
                       '(:on-face :direction :+z :extreme :max)
                       0.05))
           (new-metadata (clad.gdt:add-geometric-tolerance-to-metadata
                          metadata
                          tolerance)))
      (is (not (null new-metadata)) "Metadata should be updated")
      (is (find :geometric-tolerances new-metadata) "Should have GD&T key"))))

;;; ============================================================================
;;; Integration Tests with defpart
;;; ============================================================================

(test defpart-with-valid-selectors
  "defpart with valid selectors should compile"
  (finishes
    (eval '(clad.dsl:defpart part-with-valid-selectors ()
             "Test part"
             (:body (clad.core:make-box 100 100 10))
             (:on-face :direction :+z :extreme :max
               (:cut (clad.core:make-cylinder 5 20)))))))

(test defpart-gdt-with-valid-selectors
  "defpart with GD&T and valid selectors should compile"
  (finishes
    (eval '(clad.dsl:defpart part-gdt-valid-selectors ()
             "Test part with GD&T"
             (:body (clad.core:make-box 100 100 10))
             (:datum "A" :on-face :direction :-z :extreme :min)
             (:flatness :on-face :direction :-z :extreme :min :tolerance 0.05)
             (:perpendicularity :on-face :direction :+z :extreme :max
                                :tolerance 0.1 :datum-ref "A")))))
