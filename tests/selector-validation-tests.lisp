;;;; tests/selector-validation-tests.lisp --- Selector validation tests

(in-package :clad.tests)

(in-suite clad-tests)

;;; ============================================================================
;;; Selector API Tests
;;; ============================================================================

(test selector-matches-nothing-empty-list
  "Selectors that match no features should return empty list"
  (let* ((box (clad.core:make-box 100 100 10))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped)))
    ;; Select cylindrical faces (there are none on a box) using type selector
    (let ((result (clad.selectors:select faces :type :cylindrical)))
      (is (null result) "Selector matching nothing should return empty list"))))

(test selector-matches-multiple-faces
  "Selectors matching multiple features should work"
  (let* ((box (clad.core:make-box 100 100 10))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped)))
    ;; Box has 6 planar faces - :type :plane matches planar faces (see type-selectors.lisp)
    (let ((result (clad.selectors:select faces :type :plane)))
      ;; All 6 faces are planar
      (is (= 6 (length result)) "Box should have 6 planar faces"))))

(test selector-direction-extreme
  "Direction selector with extreme should find one face"
  (let* ((box (clad.core:make-box 100 100 10))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped)))
    ;; Find the top face (highest Z)
    (let ((result (clad.selectors:select faces :direction :+z :extreme :max)))
      (is (= 1 (length result)) "Should find exactly one extreme face"))))

(test selector-direction-without-extreme
  "Direction selector without extreme should find all matching faces"
  (let* ((box (clad.core:make-box 100 100 10))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped)))
    ;; Find faces with normal pointing in +Z or -Z direction
    (let ((result-up (clad.selectors:select faces :direction :+z))
          (result-down (clad.selectors:select faces :direction :-z)))
      ;; Box has top and bottom faces
      (is (>= (length result-up) 1) "Should find at least one +Z face")
      (is (>= (length result-down) 1) "Should find at least one -Z face"))))

;;; ============================================================================
;;; GD&T Selector Validation
;;; ============================================================================

(test gdt-selector-validation
  "GD&T selectors should validate at compile time when possible"
  ;; Test that GD&T forms with selectors compile
  (finishes
    (eval '(clad.dsl:defpart selector-validation-test ()
             "Test part for selector validation"
             (:body (clad.core:make-box 100 100 10))
             (:datum "A" :on-face :direction :-z :extreme :min)
             (:flatness :on-face :direction :+z :extreme :max :tolerance 0.05)))))

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
