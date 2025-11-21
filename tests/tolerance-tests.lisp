;;;; tests/tolerance-tests.lisp --- Tests for dimensional tolerancing (Phase T1)

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite tolerance-tests
    :description "Tests for dimensional tolerancing (Phase T1)"
    :in clad-tests)

(in-suite tolerance-tests)

;;; ============================================================================
;;; Bilateral Tolerances
;;; ============================================================================

(test bilateral-tolerance-basic
  "dim macro accepts bilateral tolerance"
  (let ((dimension (clad.units:dim 50 :mm :tol 0.1)))
    ;; Returns a toleranced-dimension object
    (is (typep dimension 'clad.units:toleranced-dimension))
    ;; Nominal value is 50mm
    (is (approx= 50.0 (clad.units:dimension-nominal dimension) 0.001))
    ;; Tolerance spec is bilateral
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (eq :bilateral (clad.units:tolerance-type tol)))
      (is (approx= 0.1 (clad.units:tolerance-upper tol) 0.001))
      (is (approx= -0.1 (clad.units:tolerance-lower tol) 0.001)))))

(test bilateral-tolerance-asymmetric
  "Bilateral tolerance with different +/- values"
  (let ((dimension (clad.units:dim 25 :mm :tol '(0.05 -0.02))))
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (eq :bilateral (clad.units:tolerance-type tol)))
      (is (approx= 0.05 (clad.units:tolerance-upper tol) 0.001))
      (is (approx= -0.02 (clad.units:tolerance-lower tol) 0.001)))))

(test bilateral-tolerance-unit-conversion
  "Tolerance converts with units"
  (let ((dimension (clad.units:dim 1 :in :tol 0.005)))  ; 0.005 inches = ±0.005"
    ;; Nominal converted to mm (25.4)
    (is (approx= 25.4 (clad.units:dimension-nominal dimension) 0.001))
    ;; Tolerance also converted to mm (0.127)
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (approx= 0.127 (clad.units:tolerance-upper tol) 0.001)))))

;;; ============================================================================
;;; Limit Tolerances
;;; ============================================================================

(test limit-tolerance-basic
  "Limit dimensioning specifies upper and lower bounds"
  (let ((dimension (clad.units:dim 50 :mm :upper 50.1 :lower 49.9)))
    (is (typep dimension 'clad.units:toleranced-dimension))
    ;; Nominal is midpoint
    (is (approx= 50.0 (clad.units:dimension-nominal dimension) 0.001))
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (eq :limit (clad.units:tolerance-type tol)))
      (is (approx= 50.1 (clad.units:tolerance-upper-limit tol) 0.001))
      (is (approx= 49.9 (clad.units:tolerance-lower-limit tol) 0.001)))))

;;; ============================================================================
;;; ISO Fit Tolerances
;;; ============================================================================

(test iso-fit-h7
  "ISO H7 fit tolerance"
  (let ((dimension (clad.units:dim 50 :mm :fit :H7)))
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (eq :fit (clad.units:tolerance-type tol)))
      (is (equal "H7" (clad.units:tolerance-fit-class tol)))
      ;; For 50mm diameter, H7 = +0.025/+0.000
      (is (approx= 0.025 (clad.units:tolerance-upper tol) 0.001))
      (is (approx= 0.000 (clad.units:tolerance-lower tol) 0.001)))))

(test iso-fit-g6
  "ISO g6 fit tolerance"
  (let ((dimension (clad.units:dim 50 :mm :fit :g6)))
    (let ((tol (clad.units:dimension-tolerance dimension)))
      ;; For 50mm diameter, g6 = -0.009/-0.025
      (is (approx= -0.009 (clad.units:tolerance-upper tol) 0.001))
      (is (approx= -0.025 (clad.units:tolerance-lower tol) 0.001)))))

(test iso-fit-invalid-size
  "ISO fit rejects invalid sizes"
  (signals clad.units:iso-fit-error
    (clad.units:dim 0.5 :mm :fit :H7)))  ; Too small for ISO table

;;; ============================================================================
;;; Tolerance Printing and Display
;;; ============================================================================

(test tolerance-print-format
  "Tolerances format correctly for display"
  (let ((tol1 (clad.units:dim 50 :mm :tol 0.1))
        (tol2 (clad.units:dim 25 :mm :tol '(0.05 -0.02)))
        (tol3 (clad.units:dim 50 :mm :upper 50.1 :lower 49.9)))

    ;; Symmetric: "50 ±0.1"
    (is (equal "50.000 ±0.100" (clad.units:format-tolerance tol1)))

    ;; Asymmetric: "25 +0.05/-0.02"
    (is (equal "25.000 +0.050/-0.020" (clad.units:format-tolerance tol2)))

    ;; Limit: "50.1/49.9"
    (is (equal "50.100/49.900" (clad.units:format-tolerance tol3)))))

;;; ============================================================================
;;; Tolerance Metadata Storage (Phase 1.2)
;;; ============================================================================

(test tolerance-metadata-storage
  "Tolerances stored in shape metadata"
  (let ((shaft (clad.core:make-cylinder
                 (clad.units:dim 25 :mm :fit :H7)
                 100)))
    ;; Metadata contains tolerance info
    (let ((meta (clad.core:shape-metadata shaft)))
      (is (getf meta :has-tolerances))
      (is (listp (getf meta :tolerance-features))))))

(test tolerance-metadata-preserved-through-operations
  "Tolerance metadata preserved through boolean ops"
  (let* ((shaft (clad.core:make-cylinder
                  (clad.units:dim 25 :mm :fit :H7)
                  100))
         (keyway (clad.core:make-box 5 3 30))
         (shaft-with-keyway (clad.core:cut-shapes shaft keyway)))
    ;; Tolerance metadata still present
    (let ((meta (clad.core:shape-metadata shaft-with-keyway)))
      (is (getf meta :has-tolerances)))))

;;; ============================================================================
;;; STEP Export with Tolerances (Phase 1.3)
;;; ============================================================================

(test step-export-with-tolerances
  "Export shape with tolerances to STEP file"
  (let ((shaft (clad.core:make-cylinder
                 (clad.units:dim 25 :mm :fit :H7)
                 100))
        (filename "/tmp/test-toleranced-shaft.stp"))
    ;; Export should succeed
    (is (clad.export:export-step shaft filename))
    ;; File should exist
    (is (probe-file filename))
    ;; Clean up
    (when (probe-file filename)
      (delete-file filename))))

(test step-export-preserves-tolerance-metadata
  "STEP export includes tolerance information in comments"
  (let ((shaft (clad.core:make-cylinder
                 (clad.units:dim 25 :mm :fit :H7)
                 100))
        (filename "/tmp/test-toleranced-shaft-meta.stp"))
    ;; Export
    (clad.export:export-step shaft filename)
    ;; Read file and check for tolerance comments
    (let ((content (with-open-file (stream filename)
                     (let ((str (make-string (file-length stream))))
                       (read-sequence str stream)
                       str))))
      ;; Should contain tolerance information in comments
      (is (search "Tolerance" content))
      (is (search "H7" content)))
    ;; Clean up
    (when (probe-file filename)
      (delete-file filename))))
