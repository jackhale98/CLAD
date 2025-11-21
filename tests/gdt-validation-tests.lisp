;;;; tests/gdt-validation-tests.lisp --- GD&T validation tests

(in-package :clad.tests)

(in-suite clad-tests)

;;; ============================================================================
;;; Priority 1: GD&T Validation Tests (TDD RED Phase)
;;; ============================================================================

(test form-tolerance-with-datum-error
  "Form tolerances should not reference datums per ASME Y14.5"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :datum-refs '("A")
     :tolerance-zone 0.05)))

(test form-tolerance-without-datum-ok
  "Form tolerances without datums should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :datum-refs nil
     :tolerance-zone 0.05)))

(test orientation-tolerance-without-datum-error
  "Orientation tolerances require datum references per ASME Y14.5"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :perpendicularity
     :datum-refs nil
     :tolerance-zone 0.1)))

(test orientation-tolerance-with-datum-ok
  "Orientation tolerances with datums should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :perpendicularity
     :datum-refs '("A")
     :tolerance-zone 0.1)))

(test location-tolerance-without-datum-error
  "Location tolerances require datum references per ASME Y14.5"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :position
     :datum-refs nil
     :tolerance-zone 0.2)))

(test location-tolerance-with-datum-ok
  "Location tolerances with datums should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :position
     :datum-refs '("A" "B" "C")
     :tolerance-zone 0.2)))

(test runout-tolerance-without-datum-error
  "Runout tolerances require datum axis reference"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :circular-runout
     :datum-ref nil
     :tolerance-zone 0.05)))

(test runout-tolerance-with-datum-ok
  "Runout tolerances with datum should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :circular-runout
     :datum-ref "A"
     :tolerance-zone 0.05)))

(test negative-tolerance-zone-error
  "Tolerance zones must be positive"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :datum-refs nil
     :tolerance-zone -0.05)))

(test zero-tolerance-zone-error
  "Tolerance zones must be positive (non-zero)"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :datum-refs nil
     :tolerance-zone 0.0)))

(test profile-bilateral-validation
  "Profile tolerances with bilateral flag should validate correctly"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :profile-surface
     :datum-refs '("A")
     :tolerance-zone 0.15
     :bilateral t)))

(test profile-unilateral-validation
  "Profile tolerances with unilateral (bilateral=nil) should validate correctly"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :profile-surface
     :datum-refs '("A")
     :tolerance-zone 0.15
     :bilateral nil)))

(test angularity-without-basic-angle-warning
  "Angularity should ideally specify basic angle (warning, not error)"
  ;; This test documents expected behavior - angularity works without angle
  ;; but best practice is to specify it
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :angularity
     :datum-refs '("A")
     :tolerance-zone 0.1
     :basic-angle nil)))

(test datum-reference-frame-321-validation
  "3-2-1 datum reference frame should validate correctly"
  (finishes
    (clad.gdt:validate-datum-reference-frame
     '("A" "B" "C"))))

(test datum-reference-frame-single-datum
  "Single datum reference should be valid"
  (finishes
    (clad.gdt:validate-datum-reference-frame
     '("A"))))

(test datum-reference-frame-duplicate-error
  "Duplicate datums in reference frame should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-datum-reference-frame
     '("A" "B" "A"))))

(test material-condition-with-form-tolerance-error
  "Material condition modifiers don't apply to form tolerances"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :datum-refs nil
     :tolerance-zone 0.05
     :material-condition :mmc)))

(test material-condition-with-position-ok
  "Material condition modifiers are valid for position tolerances"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :position
     :datum-refs '("A" "B" "C")
     :tolerance-zone 0.2
     :material-condition :mmc)))

(test conflicting-tolerances-on-same-feature
  "Multiple tolerances of same type on same feature should warn/error"
  ;; This test validates that we can detect potential conflicts
  ;; Implementation may warn rather than error
  (let ((tolerances
          (list
           (clad.gdt:make-flatness-tolerance
            '(:on-face :direction :+z :extreme :max)
            0.05)
           (clad.gdt:make-flatness-tolerance
            '(:on-face :direction :+z :extreme :max)
            0.1))))
    ;; Check for conflicts - may signal error or warning
    (signals clad.gdt:gdt-validation-error
      (clad.gdt:check-tolerance-conflicts tolerances))))

(test valid-iso-fit-h7
  "ISO H7 fit within valid size range should validate"
  (finishes
    (clad.gdt:validate-iso-fit "H7" 25.0)))

(test invalid-iso-fit-class
  "Unknown ISO fit class should error"
  (signals clad.units:iso-fit-error
    (clad.gdt:validate-iso-fit "X99" 25.0)))

(test iso-fit-outside-table-range
  "ISO fit outside defined size ranges should error"
  (signals clad.units:iso-fit-error
    (clad.gdt:validate-iso-fit "H7" 500.0)))

;;; ============================================================================
;;; Tolerance Zone Type Validation
;;; ============================================================================

(test position-tolerance-zone-types
  "Position tolerance accepts diametrical, spherical, or cylindrical zones"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :position
     :datum-refs '("A" "B" "C")
     :tolerance-zone 0.2
     :zone-type :diametrical))
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :position
     :datum-refs '("A" "B" "C")
     :tolerance-zone 0.2
     :zone-type :cylindrical)))

(test flatness-with-invalid-zone-type-error
  "Flatness only accepts planar tolerance zones"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :datum-refs nil
     :tolerance-zone 0.05
     :zone-type :diametrical)))

;;; ============================================================================
;;; Integration Tests with DSL
;;; ============================================================================

(test dsl-validation-form-with-datum-error
  "DSL should catch form tolerance with datum at compile-time"
  (signals clad.gdt:gdt-validation-error
    (eval '(clad.dsl:defpart invalid-flatness-datum ()
             "Test part with invalid flatness specification"
             (:body (clad.core:make-box 100 100 10))
             (:datum "A" :on-face :direction :-z :extreme :min)
             (:flatness :on-face :direction :-z :extreme :min
                        :tolerance 0.05 :datum-ref "A")))))

(test dsl-validation-orientation-without-datum-error
  "DSL should catch orientation tolerance without datum"
  (signals clad.gdt:gdt-validation-error
    (eval '(clad.dsl:defpart invalid-perpendicularity ()
             "Test part with invalid perpendicularity specification"
             (:body (clad.core:make-box 100 100 10))
             (:perpendicularity :on-face :direction :+z :extreme :max
                                :tolerance 0.1)))))

(test dsl-validation-negative-tolerance-error
  "DSL should catch negative tolerance values"
  (signals clad.gdt:gdt-validation-error
    (eval '(clad.dsl:defpart invalid-negative-tolerance ()
             "Test part with negative tolerance"
             (:body (clad.core:make-box 100 100 10))
             (:flatness :on-face :direction :-z :extreme :min
                        :tolerance -0.05)))))

(test dsl-validation-valid-gdt-ok
  "DSL should accept valid GD&T specifications"
  (finishes
    (eval '(clad.dsl:defpart valid-gdt-part ()
             "Test part with valid GD&T"
             (:body (clad.core:make-box 100 100 10))
             (:datum "A" :on-face :direction :-z :extreme :min)
             (:flatness :on-face :direction :-z :extreme :min
                        :tolerance 0.05)
             (:perpendicularity :on-face :direction :+z :extreme :max
                                :tolerance 0.1 :datum-ref "A")))))
