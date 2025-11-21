;;;; tests/gdt-edge-cases-tests.lisp --- Comprehensive GD&T edge case tests (Priority 4)

(in-package :clad.tests)

(in-suite clad-tests)

;;; ============================================================================
;;; Priority 4: Edge Case Test Suite
;;; ============================================================================

;;; Tolerance Edge Cases
;;;=============================================================================

(test tolerance-very-small-value
  "Very small but positive tolerance values should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :tolerance-zone 0.001  ; 1 micron
     :datum-refs nil)))

(test tolerance-very-large-value
  "Very large tolerance values should be valid (though unusual)"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :tolerance-zone 1000.0  ; 1 meter tolerance (unusual but valid)
     :datum-refs nil)))

(test tolerance-exact-zero-error
  "Exactly zero tolerance should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :tolerance-zone 0.0
     :datum-refs nil)))

(test tolerance-negative-error
  "Negative tolerance should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :tolerance-zone -0.1
     :datum-refs nil)))

;;; Datum Reference Frame Edge Cases
;;; ============================================================================

(test datum-single-letter-valid
  "Single datum reference should be valid"
  (finishes
    (clad.gdt:validate-datum-reference-frame '("A"))))

(test datum-321-frame-valid
  "3-2-1 datum reference frame should be valid"
  (finishes
    (clad.gdt:validate-datum-reference-frame '("A" "B" "C"))))

(test datum-duplicate-error
  "Duplicate datums should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-datum-reference-frame '("A" "A"))))

(test datum-triple-duplicate-error
  "Multiple duplicates should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-datum-reference-frame '("A" "B" "A" "C" "B"))))

(test datum-empty-list-ok
  "Empty datum list should be OK (for form tolerances)"
  (finishes
    (clad.gdt:validate-datum-reference-frame nil)))

(test datum-many-references
  "More than 3 datums should be valid (unusual but allowed)"
  (finishes
    (clad.gdt:validate-datum-reference-frame '("A" "B" "C" "D" "E"))))

;;; Material Condition Edge Cases
;;; ============================================================================

(test material-condition-rfs-with-form-ok
  "RFS with form tolerance should be OK (RFS is default, means no modifier)"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :tolerance-zone 0.05
     :material-condition :rfs)))

(test material-condition-mmc-with-form-error
  "MMC with form tolerance should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :tolerance-zone 0.05
     :material-condition :mmc)))

(test material-condition-lmc-with-form-error
  "LMC with form tolerance should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :flatness
     :tolerance-zone 0.05
     :material-condition :lmc)))

(test material-condition-mmc-with-position-ok
  "MMC with position tolerance should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :position
     :tolerance-zone 0.2
     :datum-refs '("A" "B" "C")
     :material-condition :mmc)))

;;; ISO Fit Edge Cases
;;; ============================================================================

(test iso-fit-h7-valid-range
  "ISO H7 fit within valid size ranges"
  (finishes
    (clad.gdt:validate-iso-fit "H7" 10.0))  ; 10mm - in H7 range
  (finishes
    (clad.gdt:validate-iso-fit "H7" 25.0))  ; 25mm - in H7 range
  (finishes
    (clad.gdt:validate-iso-fit "H7" 100.0))) ; 100mm - in H7 range

(test iso-fit-boundary-values
  "ISO fit at boundary values"
  (finishes
    (clad.gdt:validate-iso-fit "H7" 6.0))   ; Lower boundary
  (finishes
    (clad.gdt:validate-iso-fit "H7" 10.0))  ; Upper boundary of one range
  (finishes
    (clad.gdt:validate-iso-fit "H7" 10.001))) ; Just above boundary

(test iso-fit-unknown-class-error
  "Unknown ISO fit class should error"
  (signals clad.units:iso-fit-error
    (clad.gdt:validate-iso-fit "X99" 25.0)))

(test iso-fit-out-of-range-error
  "Size outside ISO table range should error"
  (signals clad.units:iso-fit-error
    (clad.gdt:validate-iso-fit "H7" 500.0))  ; 500mm - outside H7 table
  (signals clad.units:iso-fit-error
    (clad.gdt:validate-iso-fit "H7" 2.0)))   ; 2mm - below H7 minimum

(test iso-fit-case-insensitive
  "ISO fit class should be case-insensitive"
  (finishes
    (clad.gdt:validate-iso-fit "h7" 25.0))  ; lowercase
  (finishes
    (clad.gdt:validate-iso-fit "H7" 25.0))  ; uppercase
  (finishes
    (clad.gdt:validate-iso-fit :H7 25.0)))  ; symbol

;;; Profile Tolerance Edge Cases
;;; ============================================================================

(test profile-bilateral-true
  "Profile with bilateral=t should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :profile-surface
     :tolerance-zone 0.15
     :datum-refs '("A")
     :bilateral t)))

(test profile-bilateral-false
  "Profile with bilateral=nil (unilateral) should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :profile-surface
     :tolerance-zone 0.15
     :datum-refs '("A")
     :bilateral nil)))

(test profile-no-datum-refs-ok
  "Profile without datum refs should be valid (controls form)"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :profile-surface
     :tolerance-zone 0.15
     :datum-refs nil
     :bilateral t)))

;;; Combination Edge Cases
;;; ============================================================================

(test orientation-single-datum-ok
  "Orientation with single datum should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :perpendicularity
     :tolerance-zone 0.1
     :datum-ref "A")))

(test orientation-multiple-datums-ok
  "Orientation with multiple datums should be valid (via datum-refs)"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :perpendicularity
     :tolerance-zone 0.1
     :datum-refs '("A" "B"))))

(test location-single-datum-ok
  "Location with single datum should be valid (via datum-ref)"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :symmetry
     :tolerance-zone 0.05
     :datum-ref "A")))

(test runout-with-datum-ref-ok
  "Runout with datum-ref should be valid"
  (finishes
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :circular-runout
     :tolerance-zone 0.1
     :datum-ref "A")))

(test runout-without-datum-error
  "Runout without datum should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:validate-geometric-tolerance
     :gdt-type :circular-runout
     :tolerance-zone 0.1
     :datum-ref nil)))

;;; DSL Integration Edge Cases
;;; ============================================================================

(test dsl-form-with-datum-compile-error
  "DSL form tolerance with datum should fail at compile time"
  (signals clad.gdt:gdt-validation-error
    (eval '(clad.dsl:defpart invalid-form-datum ()
             "Invalid: form tolerance with datum"
             (:body (clad.core:make-box 100 100 10))
             (:datum "A" :on-face :direction :-z :extreme :min)
             (:flatness :on-face :direction :+z :extreme :max
                        :tolerance 0.05 :datum-ref "A")))))

(test dsl-orientation-without-datum-compile-error
  "DSL orientation without datum should fail at compile time"
  (signals clad.gdt:gdt-validation-error
    (eval '(clad.dsl:defpart invalid-orient-no-datum ()
             "Invalid: orientation without datum"
             (:body (clad.core:make-box 100 100 10))
             (:perpendicularity :on-face :direction :+z :extreme :max
                                :tolerance 0.1)))))

(test dsl-zero-tolerance-compile-error
  "DSL zero tolerance should fail at compile time"
  (signals clad.gdt:gdt-validation-error
    (eval '(clad.dsl:defpart invalid-zero-tolerance ()
             "Invalid: zero tolerance"
             (:body (clad.core:make-box 100 100 10))
             (:flatness :on-face :direction :+z :extreme :max
                        :tolerance 0.0)))))

(test dsl-duplicate-datums-compile-error
  "DSL with duplicate datums should fail at compile time"
  (signals clad.gdt:gdt-validation-error
    (eval '(clad.dsl:defpart invalid-duplicate-datums ()
             "Invalid: duplicate datums in reference frame"
             (:body (clad.core:make-box 100 100 10))
             (:datum "A" :on-face :direction :-z :extreme :min)
             (:datum "B" :on-face :direction :+x :extreme :max)
             (:position :on-face :direction :+z :extreme :max
                        :tolerance 0.2
                        :datum-refs ("A" "B" "A"))))))  ; Duplicate "A"

(test dsl-valid-complex-gdt
  "DSL with valid complex GD&T should compile"
  (finishes
    (eval '(clad.dsl:defpart valid-complex-gdt ()
             "Valid complex GD&T specification"
             (:body (clad.core:make-box 100 100 10))
             (:datum "A" :on-face :direction :-z :extreme :min)
             (:datum "B" :on-face :direction :+x :extreme :max)
             (:datum "C" :on-face :direction :+y :extreme :max)
             (:flatness :on-face :direction :-z :extreme :min :tolerance 0.05)
             (:perpendicularity :on-face :direction :+z :extreme :max
                                :tolerance 0.1 :datum-ref "A")
             (:position :on-face :direction :+z :extreme :max
                        :tolerance 0.2 :datum-refs ("A" "B" "C") :mmc t)
             (:profile-surface :on-face :direction :+x :extreme :max
                               :tolerance 0.15 :datum-refs ("A")
                               :bilateral t)
             (:circular-runout :on-face :type :cylindrical
                               :tolerance 0.1 :datum-ref "A")))))

;;; Tolerance Conflict Detection
;;; ============================================================================

(test conflicting-flatness-same-feature
  "Multiple flatness tolerances on same feature should error"
  (signals clad.gdt:gdt-validation-error
    (clad.gdt:check-tolerance-conflicts
     (list
      (clad.gdt:make-flatness-tolerance
       '(:on-face :direction :+z :extreme :max) 0.05)
      (clad.gdt:make-flatness-tolerance
       '(:on-face :direction :+z :extreme :max) 0.1)))))

(test different-tolerances-same-feature-ok
  "Different tolerance types on same feature should be OK"
  (finishes
    (clad.gdt:check-tolerance-conflicts
     (list
      (clad.gdt:make-flatness-tolerance
       '(:on-face :direction :+z :extreme :max) 0.05)
      (clad.gdt:make-perpendicularity-tolerance
       '(:on-face :direction :+z :extreme :max) 0.1 "A")))))

(test same-tolerance-different-features-ok
  "Same tolerance type on different features should be OK"
  (finishes
    (clad.gdt:check-tolerance-conflicts
     (list
      (clad.gdt:make-flatness-tolerance
       '(:on-face :direction :+z :extreme :max) 0.05)
      (clad.gdt:make-flatness-tolerance
       '(:on-face :direction :-z :extreme :min) 0.05)))))
