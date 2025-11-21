;;;; tests/datum-tests.lisp --- Tests for datum system (Phase T2)

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite datum-tests
    :description "Tests for GD&T datum system (Phase T2)"
    :in clad-tests)

(in-suite datum-tests)

;;; ============================================================================
;;; Basic Datum Definition (Cycle 2.1)
;;; ============================================================================

(test datum-definition-basic
  "Define datum feature on face"
  (let ((part (test-part-with-datum)))
    ;; Verify datum stored in metadata
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datums (getf meta :datums)))
      (is (not (null datums)))
      (is (assoc "A" datums :test #'equal)))))

(clad.dsl:defpart test-part-with-datum ()
  "Simple box with datum A on bottom face"
  (:body (clad.core:make-box 100 100 10))

  ;; Define datum A on bottom face
  (:datum "A" :on-face :direction :-z :extreme :min))

(test datum-label-accessor
  "Datum label is accessible"
  (let ((part (test-part-with-datum)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-a (cdr (assoc "A" (getf meta :datums) :test #'equal))))
      (is (typep datum-a 'clad.gdt:datum-feature))
      (is (equal "A" (clad.gdt:datum-label datum-a))))))

(test datum-selector-storage
  "Datum selector specification is stored"
  (let ((part (test-part-with-datum)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-a (cdr (assoc "A" (getf meta :datums) :test #'equal))))
      (is (not (null (clad.gdt:datum-selector datum-a)))))))

;;; ============================================================================
;;; Datum Reference Frame (A-B-C)
;;; ============================================================================

(test datum-reference-frame
  "Define complete datum reference frame A-B-C"
  (let ((part (test-part-with-drf)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datums (getf meta :datums)))
      (is (= 3 (length datums)))
      (is (assoc "A" datums :test #'equal))
      (is (assoc "B" datums :test #'equal))
      (is (assoc "C" datums :test #'equal)))))

(clad.dsl:defpart test-part-with-drf ()
  "Part with complete datum reference frame A-B-C"
  (:body (clad.core:make-box 100 100 10))

  ;; Primary datum (usually largest/flattest face)
  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Secondary datum (perpendicular to primary)
  (:datum "B" :on-face :direction :+x :extreme :max)

  ;; Tertiary datum (perpendicular to primary and secondary)
  (:datum "C" :on-face :direction :+y :extreme :max))

(test datum-reference-frame-order
  "Datums are stored in order of definition"
  (let ((part (test-part-with-drf)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datums (getf meta :datums))
           (labels (mapcar #'car datums)))
      ;; Datums are stored in reverse order (consed)
      ;; So we reverse to get definition order
      (is (equal '("C" "B" "A") labels)))))

;;; ============================================================================
;;; Material Condition Modifiers
;;; ============================================================================

(test datum-with-material-condition-mmc
  "Datum with Maximum Material Condition (MMC)"
  (let ((part (test-datum-with-mmc)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-a (cdr (assoc "A" (getf meta :datums) :test #'equal))))
      ;; Datum A specified at MMC
      (is (eq :mmc (clad.gdt:datum-material-condition datum-a))))))

(clad.dsl:defpart test-datum-with-mmc ()
  "Part with datum at Maximum Material Condition"
  (:body (clad.core:make-box 100 100 10))

  ;; Datum at Maximum Material Condition
  (:datum "A" :on-face :direction :-z :extreme :min :mmc t))

(test datum-with-material-condition-lmc
  "Datum with Least Material Condition (LMC)"
  (let ((part (test-datum-with-lmc)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-a (cdr (assoc "A" (getf meta :datums) :test #'equal))))
      ;; Datum A specified at LMC
      (is (eq :lmc (clad.gdt:datum-material-condition datum-a))))))

(clad.dsl:defpart test-datum-with-lmc ()
  "Part with datum at Least Material Condition"
  (:body (clad.core:make-box 100 100 10))

  ;; Datum at Least Material Condition
  (:datum "A" :on-face :direction :-z :extreme :min :lmc t))

(test datum-default-material-condition-rfs
  "Datum defaults to Regardless of Feature Size (RFS)"
  (let ((part (test-part-with-datum)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-a (cdr (assoc "A" (getf meta :datums) :test #'equal))))
      ;; Default should be RFS
      (is (eq :rfs (clad.gdt:datum-material-condition datum-a))))))

;;; ============================================================================
;;; Multiple Datums on Same Part
;;; ============================================================================

(test multiple-datums-different-faces
  "Part can have multiple datums on different faces"
  (let ((part (multiple-datum-part)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datums (getf meta :datums)))
      ;; Should have 4 datums
      (is (= 4 (length datums)))
      (is (assoc "A" datums :test #'equal))
      (is (assoc "B" datums :test #'equal))
      (is (assoc "C" datums :test #'equal))
      (is (assoc "D" datums :test #'equal)))))

(clad.dsl:defpart multiple-datum-part ()
  "Part with datums on all four sides"
  (:body (clad.core:make-box 100 50 20))

  (:datum "A" :on-face :direction :-z :extreme :min)
  (:datum "B" :on-face :direction :+x :extreme :max)
  (:datum "C" :on-face :direction :+y :extreme :max)
  (:datum "D" :on-face :direction :-x :extreme :min))

;;; ============================================================================
;;; Datum Make Function
;;; ============================================================================

(test make-datum-direct
  "Directly create datum feature using make-datum"
  (let ((datum (clad.gdt:make-datum "A"
                                    '(:on-face :direction :-z :extreme :min)
                                    :material-condition :mmc)))
    (is (typep datum 'clad.gdt:datum-feature))
    (is (equal "A" (clad.gdt:datum-label datum)))
    (is (eq :mmc (clad.gdt:datum-material-condition datum)))
    (is (not (null (clad.gdt:datum-selector datum))))))

(test make-datum-lowercase-label
  "Datum labels are normalized to uppercase"
  (let ((datum (clad.gdt:make-datum "a"
                                    '(:on-face :direction :-z :extreme :min))))
    (is (equal "A" (clad.gdt:datum-label datum)))))

(test make-datum-default-rfs
  "make-datum defaults to RFS material condition"
  (let ((datum (clad.gdt:make-datum "A"
                                    '(:on-face :direction :-z :extreme :min))))
    (is (eq :rfs (clad.gdt:datum-material-condition datum)))))

;;; ============================================================================
;;; Datum Query Functions
;;; ============================================================================

(test find-datum-by-label
  "Find datum by label in shape metadata"
  (let ((part (test-part-with-drf)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-b (clad.gdt:find-datum meta "B")))
      (is (not (null datum-b)))
      (is (equal "B" (clad.gdt:datum-label datum-b))))))

(test find-datum-not-found
  "find-datum returns nil when datum doesn't exist"
  (let ((part (test-part-with-datum)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-z (clad.gdt:find-datum meta "Z")))
      (is (null datum-z)))))

(test list-datums-all
  "List all datums in a part"
  (let ((part (test-part-with-drf)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-list (clad.gdt:list-datums meta)))
      (is (= 3 (length datum-list)))
      ;; All should be datum-feature instances
      (is (every (lambda (d) (typep d 'clad.gdt:datum-feature)) datum-list)))))

(test list-datums-empty
  "list-datums returns empty list when no datums defined"
  (let ((part (clad.core:make-box 100 100 10)))
    (let* ((meta (clad.core:shape-metadata part))
           (datum-list (clad.gdt:list-datums meta)))
      (is (null datum-list)))))

;;; ============================================================================
;;; Datum Persistence Through Operations
;;; ============================================================================

(test datum-preserved-through-union
  "Datum metadata preserved through boolean union"
  (let* ((part1 (test-part-with-datum))
         (part2 (clad.core:make-box 50 50 5))
         (part2-translated (clad.core:translate part2 0 0 10))
         (combined (clad.core:union-shapes
                    (clad.shapes:unwrap-shape part1)
                    part2-translated)))
    ;; Datum should still be present
    (let* ((meta (clad.core:shape-metadata combined))
           (datums (getf meta :datums)))
      (is (not (null datums)))
      (is (assoc "A" datums :test #'equal)))))

(test datum-preserved-through-cut
  "Datum metadata preserved through boolean cut"
  (let* ((part (test-part-with-drf))
         (hole (clad.core:make-cylinder 5 15))
         (cut-part (clad.core:cut-shapes
                    (clad.shapes:unwrap-shape part)
                    hole)))
    ;; All datums should still be present
    (let* ((meta (clad.core:shape-metadata cut-part))
           (datums (getf meta :datums)))
      (is (= 3 (length datums))))))

;;; ============================================================================
;;; Edge Cases and Validation
;;; ============================================================================

(test datum-with-cylindrical-feature
  "Datum can reference cylindrical feature"
  (let ((part (test-datum-on-cylinder)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-a (cdr (assoc "A" (getf meta :datums) :test #'equal))))
      (is (not (null datum-a))))))

(clad.dsl:defpart test-datum-on-cylinder ()
  "Cylinder with datum on cylindrical face"
  (:body (clad.core:make-cylinder 25 100))

  ;; Datum on cylindrical surface
  (:datum "A" :on-face :type :cylindrical))

(test datum-material-condition-validation
  "Material condition must be :mmc, :lmc, or :rfs"
  (signals error
    (clad.gdt:make-datum "A"
                         '(:on-face :direction :-z :extreme :min)
                         :material-condition :invalid)))

;;; ============================================================================
;;; Integration with Toleranced Dimensions
;;; ============================================================================

(test datum-with-toleranced-dimension
  "Part can have both datums and toleranced dimensions"
  (let ((part (test-datum-and-tolerance)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datums (getf meta :datums))
           (tolerances (getf meta :has-tolerances)))
      ;; Should have both datums and tolerances
      (is (not (null datums)))
      (is (not (null tolerances))))))

(clad.dsl:defpart test-datum-and-tolerance ()
  "Part with both datums and toleranced dimensions"
  (:body (clad.core:make-cylinder
          (clad.units:dim 25 :mm :fit :H7)
          100))

  ;; Define datum on bottom face
  (:datum "A" :on-face :direction :-z :extreme :min))

;;; ============================================================================
;;; Documentation and Print Representation
;;; ============================================================================

(test datum-print-representation
  "Datum features have readable print representation"
  (let ((datum (clad.gdt:make-datum "A"
                                    '(:on-face :direction :-z :extreme :min)
                                    :material-condition :mmc)))
    (let ((printed (format nil "~A" datum)))
      ;; Should contain label
      (is (search "A" printed))
      ;; Should indicate it's a datum
      (is (search "DATUM" printed :test #'char-equal)))))
