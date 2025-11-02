;;;; tests/units-tests.lisp --- Units system tests

(in-package :clad.tests)

(in-suite clad-tests)

;;; ============================================================================
;;; Helper for approximate equality
;;; ============================================================================

(defun approx= (a b &optional (tolerance 1.0d-6))
  "Test if two numbers are approximately equal within tolerance"
  (< (abs (- a b)) tolerance))

;;; ============================================================================
;;; Unit Conversion Tests
;;; ============================================================================

(test test-basic-conversions
  "Test basic unit conversions"
  ;; mm to in
  (is (approx= (convert-units 25.4 :mm :in) 1.0d0))
  (is (approx= (convert-units 1 :in :mm) 25.4d0))

  ;; cm to mm
  (is (approx= (convert-units 10 :cm :mm) 100.0d0))
  (is (approx= (convert-units 100 :mm :cm) 10.0d0))

  ;; m to mm
  (is (approx= (convert-units 1 :m :mm) 1000.0d0))
  (is (approx= (convert-units 1000 :mm :m) 1.0d0))

  ;; ft to in
  (is (approx= (convert-units 1 :ft :in) 12.0d0)))

(test test-dim-macro-compile-time
  "Test dim macro with compile-time conversion"
  ;; Explicit units - should be compile-time constant
  (let ((val-mm (dim 10 :mm))
        (val-in (dim 1 :in))
        (val-cm (dim 5 :cm)))
    (is (= val-mm 10.0d0))
    (is (= val-in 25.4d0))
    (is (= val-cm 50.0d0))))

(test test-dim-macro-runtime
  "Test dim macro with runtime context"
  ;; Using default units
  (let ((*default-units* :mm))
    (is (= (dim 10) 10.0d0)))

  (let ((*default-units* :in))
    (is (= (dim 1) 25.4d0))))

(test test-with-units-macro
  "Test with-units dynamic context"
  ;; Default is mm
  (let ((*default-units* :mm))
    (is (= (dim 10) 10.0d0))

    ;; Override with with-units
    (with-units :in
      (is (= (dim 1) 25.4d0))
      (is (= (dim 2) 50.8d0)))

    ;; Back to default after with-units
    (is (= (dim 10) 10.0d0))))

(test test-unit-inheritance
  "Test file-level unit inheritance"
  (let ((clad.units:*default-units* :mm)
        (clad.units:*file-units* nil))
    ;; No file units, uses default
    (is (= (dim 10) 10.0d0))

    ;; Set file units in nested let
    (let ((clad.units:*file-units* :in))
      (is (= (dim 1) 25.4d0))

      ;; Explicit unit overrides file units
      (is (= (dim 10 :mm) 10.0d0))

      ;; with-units overrides file units
      (with-units :cm
        (is (= (dim 5) 50.0d0))))

    ;; Back to original state - file units cleared
    (is (= (dim 10) 10.0d0))))

(test test-custom-units
  "Test defining custom units"
  (define-unit-conversion :test-unit 42.0d0)

  (is (= (convert-units 1 :test-unit :mm) 42.0d0))
  (is (= (convert-units 42 :mm :test-unit) 1.0d0))
  (is (= (dim 1 :test-unit) 42.0d0)))

(test test-effective-units
  "Test effective-units function"
  ;; Use let to ensure clean state for this test
  (let ((clad.units:*default-units* :mm)
        (clad.units:*file-units* nil))
    (is (eq (effective-units) :mm))

    (let ((clad.units:*file-units* :in))
      (is (eq (effective-units) :in)))

    ;; Back to original state
    (is (eq (effective-units) :mm))))

;;; ============================================================================
;;; Utility Function Tests
;;; ============================================================================

(test test-unit-predicates
  "Test unit validation predicates"
  (is (unit-p :mm))
  (is (unit-p :in))
  (is (unit-p :cm))
  (is (not (unit-p :invalid)))
  (is (not (unit-p "mm")))
  (is (not (unit-p 'mm))))

(test test-conversion-utilities
  "Test utility conversion functions"
  (is (approx= (mm->in 25.4) 1.0d0))
  (is (approx= (in->mm 1) 25.4d0))
  (is (approx= (mm->cm 100) 10.0d0))
  (is (approx= (cm->mm 10) 100.0d0))
  (is (approx= (mm->m 1000) 1.0d0))
  (is (approx= (m->mm 1) 1000.0d0)))
