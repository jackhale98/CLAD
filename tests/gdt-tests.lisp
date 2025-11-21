;;;; tests/gdt-tests.lisp --- Tests for geometric tolerancing (Phase T3)

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite gdt-tests
    :description "Tests for GD&T geometric tolerancing (Phase T3)"
    :in clad-tests)

(in-suite gdt-tests)

;;; ============================================================================
;;; Form Tolerances - Flatness (Cycle 3.1)
;;; ============================================================================

(test flatness-tolerance-basic
  "Flatness tolerance on face"
  (let ((part (part-with-flatness)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (is (= 1 (length gdt-list)))
      (let ((flatness-tol (first gdt-list)))
        (is (typep flatness-tol 'clad.gdt:form-tolerance))
        (is (eq :flatness (clad.gdt:tolerance-gdt-type flatness-tol)))
        (is (clad.tests::approximately= 0.05
                                        (clad.gdt:tolerance-zone-value flatness-tol)
                                        0.001))))))

(clad.dsl:defpart part-with-flatness ()
  "Part with flatness tolerance on top face"
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Flatness tolerance on top face
  (:flatness :on-face :direction :+z :extreme :max
             :tolerance 0.05))  ; 0.05mm flatness zone

;;; ============================================================================
;;; Form Tolerances - Straightness
;;; ============================================================================

(test straightness-tolerance
  "Straightness tolerance on edge"
  (let ((part (part-with-straightness)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :straightness (clad.gdt:tolerance-gdt-type tol)))
        (is (clad.tests::approximately= 0.02
                                        (clad.gdt:tolerance-zone-value tol)
                                        0.001))))))

(clad.dsl:defpart part-with-straightness ()
  "Part with straightness tolerance"
  (:body (clad.core:make-box 200 50 10))

  ;; Straightness on edge
  (:straightness :on-edge :direction :+x :extreme :max
                 :tolerance 0.02))

;;; ============================================================================
;;; Form Tolerances - Circularity
;;; ============================================================================

(test circularity-tolerance
  "Circularity (roundness) tolerance"
  (let ((part (part-with-circularity)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :circularity (clad.gdt:tolerance-gdt-type tol)))))))

(clad.dsl:defpart part-with-circularity ()
  "Cylinder with circularity tolerance"
  (:body (clad.core:make-cylinder 25 100))

  ;; Circularity on cylindrical surface
  (:circularity :on-face :type :cylindrical
                :tolerance 0.01))

;;; ============================================================================
;;; Form Tolerances - Cylindricity
;;; ============================================================================

(test cylindricity-tolerance
  "Cylindricity tolerance"
  (let ((part (part-with-cylindricity)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :cylindricity (clad.gdt:tolerance-gdt-type tol)))
        (is (clad.tests::approximately= 0.015
                                        (clad.gdt:tolerance-zone-value tol)
                                        0.001))))))

(clad.dsl:defpart part-with-cylindricity ()
  "Cylinder with cylindricity tolerance"
  (:body (clad.core:make-cylinder 25 100))

  ;; Cylindricity (combines circularity, straightness, parallelism)
  (:cylindricity :on-face :type :cylindrical
                 :tolerance 0.015))

;;; ============================================================================
;;; Orientation Tolerances - Perpendicularity
;;; ============================================================================

(test perpendicularity-tolerance
  "Perpendicularity with datum reference"
  (let ((part (part-with-perpendicularity)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((perp-tol (first gdt-list)))
        (is (typep perp-tol 'clad.gdt:orientation-tolerance))
        (is (eq :perpendicularity (clad.gdt:tolerance-gdt-type perp-tol)))
        (is (equal '("A") (clad.gdt:tolerance-datum-refs perp-tol)))
        (is (clad.tests::approximately= 0.1
                                        (clad.gdt:tolerance-zone-value perp-tol)
                                        0.001))))))

(clad.dsl:defpart part-with-perpendicularity ()
  "Part with perpendicularity tolerance"
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Side face must be perpendicular to datum A within 0.1mm
  (:perpendicularity :on-face :direction :+x :extreme :max
                     :tolerance 0.1
                     :datum-ref "A"))

;;; ============================================================================
;;; Orientation Tolerances - Parallelism
;;; ============================================================================

(test parallelism-tolerance
  "Parallelism with datum reference"
  (let ((part (part-with-parallelism)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :parallelism (clad.gdt:tolerance-gdt-type tol)))
        (is (equal '("A") (clad.gdt:tolerance-datum-refs tol)))))))

(clad.dsl:defpart part-with-parallelism ()
  "Part with parallelism tolerance"
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Top face must be parallel to datum A within 0.05mm
  (:parallelism :on-face :direction :+z :extreme :max
                :tolerance 0.05
                :datum-ref "A"))

;;; ============================================================================
;;; Orientation Tolerances - Angularity
;;; ============================================================================

(test angularity-tolerance
  "Angularity with datum reference and angle"
  (let ((part (part-with-angularity)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :angularity (clad.gdt:tolerance-gdt-type tol)))
        (is (equal '("A") (clad.gdt:tolerance-datum-refs tol)))))))

(clad.dsl:defpart part-with-angularity ()
  "Part with angularity tolerance"
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Face at angle to datum A
  (:angularity :on-face :direction :+x :extreme :max
               :tolerance 0.08
               :datum-ref "A"
               :angle 45))  ; 45 degree nominal angle

;;; ============================================================================
;;; Location Tolerances - Position
;;; ============================================================================

(test position-tolerance-basic
  "Position tolerance with datum reference frame"
  (let ((part (part-with-position)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((pos-tol (first gdt-list)))
        (is (typep pos-tol 'clad.gdt:location-tolerance))
        (is (eq :position (clad.gdt:tolerance-gdt-type pos-tol)))
        (is (equal '("A" "B" "C") (clad.gdt:tolerance-datum-refs pos-tol)))
        (is (clad.tests::approximately= 0.2
                                        (clad.gdt:tolerance-zone-value pos-tol)
                                        0.001))))))

(clad.dsl:defpart part-with-position ()
  "Part with position tolerance"
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)
  (:datum "B" :on-face :direction :+x :extreme :max)
  (:datum "C" :on-face :direction :+y :extreme :max)

  ;; Position tolerance on hole
  (:position :on-face :type :cylindrical
             :tolerance 0.2
             :datum-refs ("A" "B" "C")))

(test position-tolerance-with-mmc
  "Position tolerance at Maximum Material Condition"
  (let ((part (part-with-position-mmc)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((pos-tol (first gdt-list)))
        (is (eq :position (clad.gdt:tolerance-gdt-type pos-tol)))
        (is (eq :mmc (clad.gdt:tolerance-material-condition pos-tol)))))))

(clad.dsl:defpart part-with-position-mmc ()
  "Part with position tolerance at MMC"
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)
  (:datum "B" :on-face :direction :+x :extreme :max)

  ;; Position at MMC provides bonus tolerance
  (:position :on-face :type :cylindrical
             :tolerance 0.2
             :datum-refs ("A" "B")
             :mmc t))

;;; ============================================================================
;;; Location Tolerances - Concentricity
;;; ============================================================================

(test concentricity-tolerance
  "Concentricity tolerance"
  (let ((part (part-with-concentricity)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :concentricity (clad.gdt:tolerance-gdt-type tol)))
        (is (equal '("A") (clad.gdt:tolerance-datum-refs tol)))))))

(clad.dsl:defpart part-with-concentricity ()
  "Cylinder with concentricity tolerance"
  (:body (clad.core:make-cylinder 25 100))

  (:datum "A" :on-face :type :cylindrical)

  ;; Center axis must be concentric to datum A
  (:concentricity :on-face :type :cylindrical
                  :tolerance 0.05
                  :datum-ref "A"))

;;; ============================================================================
;;; Location Tolerances - Symmetry
;;; ============================================================================

(test symmetry-tolerance
  "Symmetry tolerance"
  (let ((part (part-with-symmetry)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :symmetry (clad.gdt:tolerance-gdt-type tol)))))))

(clad.dsl:defpart part-with-symmetry ()
  "Part with symmetry tolerance"
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Feature must be symmetric about center plane
  (:symmetry :on-face :direction :+x :extreme :max
             :tolerance 0.1
             :datum-ref "A"))

;;; ============================================================================
;;; Multiple Geometric Tolerances
;;; ============================================================================

(test multiple-geometric-tolerances
  "Part can have multiple geometric tolerances"
  (let ((part (part-with-multiple-gdt)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      ;; Should have flatness + perpendicularity
      (is (= 2 (length gdt-list)))
      (is (find :flatness gdt-list :key #'clad.gdt:tolerance-gdt-type))
      (is (find :perpendicularity gdt-list :key #'clad.gdt:tolerance-gdt-type)))))

(clad.dsl:defpart part-with-multiple-gdt ()
  "Part with multiple geometric tolerances"
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  (:flatness :on-face :direction :-z :extreme :min
             :tolerance 0.05)

  (:perpendicularity :on-face :direction :+x :extreme :max
                     :tolerance 0.1
                     :datum-ref "A"))

;;; ============================================================================
;;; GD&T with Datums and Toleranced Dimensions
;;; ============================================================================

(test gdt-with-datums-and-dimensions
  "Part can have datums, dimensional tolerances, and geometric tolerances"
  (let ((part (comprehensive-toleranced-part)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datums (getf meta :datums))
           (gdt-list (getf meta :geometric-tolerances))
           (dim-tols (getf meta :has-tolerances)))
      (is (not (null datums)))
      (is (not (null gdt-list)))
      (is (not (null dim-tols))))))

(clad.dsl:defpart comprehensive-toleranced-part ()
  "Part with complete tolerancing"
  (:body (clad.core:make-cylinder
          (clad.units:dim 25 :mm :fit :H7)  ; Dimensional tolerance
          100))

  ;; Datum system
  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Geometric tolerance
  (:flatness :on-face :direction :-z :extreme :min
             :tolerance 0.05))

;;; ============================================================================
;;; GD&T Metadata Queries
;;; ============================================================================

(test find-geometric-tolerances-by-type
  "Find geometric tolerances by type"
  (let ((part (part-with-multiple-gdt)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (flatness-tols (clad.gdt:find-geometric-tolerances meta :flatness)))
      (is (= 1 (length flatness-tols)))
      (is (eq :flatness (clad.gdt:tolerance-gdt-type (first flatness-tols)))))))

(test list-all-geometric-tolerances
  "List all geometric tolerances"
  (let ((part (part-with-multiple-gdt)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (all-tols (clad.gdt:list-geometric-tolerances meta)))
      (is (= 2 (length all-tols))))))

;;; ============================================================================
;;; GD&T Persistence Through Operations
;;; ============================================================================

(test gdt-preserved-through-boolean-ops
  "Geometric tolerances preserved through boolean operations"
  (let* ((part1 (part-with-flatness))
         (part2 (clad.core:make-box 50 50 5))
         (part2-translated (clad.core:translate part2 0 0 10))
         (combined (clad.core:union-shapes
                    (clad.shapes:unwrap-shape part1)
                    part2-translated)))
    ;; GD&T should still be present
    (let* ((meta (clad.core:shape-metadata combined))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (is (= 1 (length gdt-list))))))

;;; ============================================================================
;;; Print Representation
;;; ============================================================================

(test geometric-tolerance-print-representation
  "Geometric tolerances have readable print representation"
  (let ((tol (clad.gdt:make-flatness-tolerance
              '(:on-face :direction :+z :extreme :max)
              0.05)))
    (let ((printed (format nil "~A" tol)))
      (is (search "FLATNESS" printed :test #'char-equal))
      (is (search "0.05" printed)))))

;;; ============================================================================
;;; Profile Tolerances
;;; ============================================================================

(test profile-surface-tolerance
  "Profile of a surface tolerance"
  (let ((part (part-with-profile-surface)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (typep tol 'clad.gdt:profile-tolerance))
        (is (eq :profile-surface (clad.gdt:tolerance-gdt-type tol)))
        (is (clad.tests::approximately= 0.1
                                        (clad.gdt:tolerance-zone-value tol)
                                        0.001))
        (is (clad.gdt:tolerance-bilateral-p tol))))))

(clad.dsl:defpart part-with-profile-surface ()
  "Part with profile of surface tolerance"
  (:body (clad.core:make-box 100 50 25))

  (:datum "A" :on-face :direction :-z :extreme :min)
  (:datum "B" :on-face :direction :+x :extreme :max)

  ;; Profile of surface with datum references
  (:profile-surface :on-face :direction :+y :extreme :max
                    :tolerance 0.1
                    :datum-refs ("A" "B")
                    :bilateral t))

(test profile-line-tolerance
  "Profile of a line tolerance"
  (let ((part (part-with-profile-line)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :profile-line (clad.gdt:tolerance-gdt-type tol)))))))

(clad.dsl:defpart part-with-profile-line ()
  "Part with profile of line tolerance"
  (:body (clad.core:make-box 100 50 25))

  ;; Profile of line without datum references
  (:profile-line :on-edge :direction :+x :extreme :max
                 :tolerance 0.05))

;;; ============================================================================
;;; Runout Tolerances
;;; ============================================================================

(test circular-runout-tolerance
  "Circular runout tolerance"
  (let ((part (part-with-circular-runout)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (typep tol 'clad.gdt:runout-tolerance))
        (is (eq :circular-runout (clad.gdt:tolerance-gdt-type tol)))
        (is (equal '("A") (clad.gdt:tolerance-datum-refs tol)))
        (is (clad.tests::approximately= 0.03
                                        (clad.gdt:tolerance-zone-value tol)
                                        0.001))))))

(clad.dsl:defpart part-with-circular-runout ()
  "Cylinder with circular runout tolerance"
  (:body (clad.core:make-cylinder 25 100))

  (:datum "A" :on-face :type :cylindrical)

  ;; Circular runout about datum axis A
  (:circular-runout :on-face :type :cylindrical
                    :tolerance 0.03
                    :datum-ref "A"))

(test total-runout-tolerance
  "Total runout tolerance"
  (let ((part (part-with-total-runout)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt-list (getf meta :geometric-tolerances)))
      (is (not (null gdt-list)))
      (let ((tol (first gdt-list)))
        (is (eq :total-runout (clad.gdt:tolerance-gdt-type tol)))
        (is (equal '("A") (clad.gdt:tolerance-datum-refs tol)))))))

(clad.dsl:defpart part-with-total-runout ()
  "Cylinder with total runout tolerance"
  (:body (clad.core:make-cylinder 25 100))

  (:datum "A" :on-face :type :cylindrical)

  ;; Total runout about datum axis A
  (:total-runout :on-face :type :cylindrical
                 :tolerance 0.05
                 :datum-ref "A"))

