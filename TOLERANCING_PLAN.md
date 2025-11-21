# CLAD Tolerancing & GD&T Implementation Plan
## Test-Driven Development Roadmap

**Date:** 2025-01-14
**Target:** Autonomous AI Agent Implementation
**Methodology:** Test-Driven Development (TDD)
**Priority:** HIGH - Enables production engineering workflows

---

## Executive Summary

This plan implements comprehensive tolerancing and GD&T (Geometric Dimensioning and Tolerancing) support for CLAD, enabling:
1. **Dimensional Tolerancing** - ±tolerances, limit dimensions, fits
2. **Datum System** - Define datum features (A, B, C) for measurement references
3. **Geometric Tolerancing** - Form, orientation, location, profile, runout controls
4. **STEP AP242 Export** - Export parts with complete PMI (Product Manufacturing Information)
5. **Tolerance Analysis** - Stack-up calculations and reporting

This makes CLAD suitable for **production engineering** and **model-based definition (MBD)** workflows.

---

## Current Infrastructure Analysis

### ✅ Existing Foundation

**1. Units System** (`src/units/`)
```lisp
;; Already implemented:
- Unit conversions (mm, in, cm, m, etc.)
- toleranced-dimension class with nominal/tolerance slots
- Source value tracking for traceability
```

**2. Tolerance Specification Classes** (`src/units/dimension.lisp`)
```lisp
;; Placeholder classes ready to enhance:
(defclass tolerance-spec ()
  ((type :initarg :type)        ; :bilateral, :unilateral, :fit
   (upper :initarg :upper)
   (lower :initarg :lower)))

;; Helper functions exist:
- bilateral-tolerance
- unilateral-tolerance
- symmetric-tolerance
```

**3. Shape Metadata System** (`src/core/primitives.lisp`)
```lisp
;; All shapes support metadata:
(defclass shape ()
  ((handle ...)
   (metadata nil :type list)))  ; ← Can store tolerance/GD&T data

;; Used throughout transformations, operations
```

**4. STEP Export** (`src/export/step.lisp`)
```lisp
;; Basic AP203 export exists
;; FFI layer: ffi-export-step
;; Need to extend for AP242 with PMI
```

**5. Assembly Metadata** (`src/assembly/`)
```lisp
;; Components already support metadata:
:metadata '(:part-number "BP-001"
            :material "6061-T6"
            :finish "Anodized")
```

### ⚠️ Gaps to Fill

1. **No DSL syntax for tolerances** - Need declarative tolerance specification
2. **No datum system** - Need to mark faces/features as datums
3. **No GD&T feature control frames** - Need perpendicularity, position, etc.
4. **No STEP PMI export** - Need AP242 exporter with tolerance entities
5. **No validation** - Need to verify tolerance correctness per ASME Y14.5

---

## Implementation Phases

### Phase 1: Enhanced Dimensional Tolerancing (Week 1-2)

**Goal:** Extend existing tolerance system with full DSL support and STEP export.

**Deliverables:**
- Enhanced `dim` macro with tolerance syntax
- Bilateral, unilateral, limit, and fit tolerances
- ISO fit table (H7/g6, etc.)
- Tolerance metadata stored in shape
- Basic STEP dimensional tolerance export
- Full test coverage

---

#### Cycle 1.1: Enhanced dim Macro with Tolerances (4-6 hours)

**Test File:** `tests/tolerance-tests.lisp` (NEW)

```lisp
(in-package :clad.tests)

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
    (is (approximately= 50.0 (clad.units:dimension-nominal dimension) 0.001))
    ;; Tolerance spec is bilateral
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (eq :bilateral (clad.units:tolerance-type tol)))
      (is (approximately= 0.1 (clad.units:tolerance-upper tol) 0.001))
      (is (approximately= -0.1 (clad.units:tolerance-lower tol) 0.001)))))

(test bilateral-tolerance-asymmetric
  "Bilateral tolerance with different +/- values"
  (let ((dimension (clad.units:dim 25 :mm :tol (+0.05 -0.02))))
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (eq :bilateral (clad.units:tolerance-type tol)))
      (is (approximately= 0.05 (clad.units:tolerance-upper tol) 0.001))
      (is (approximately= -0.02 (clad.units:tolerance-lower tol) 0.001)))))

(test bilateral-tolerance-unit-conversion
  "Tolerance converts with units"
  (let ((dimension (clad.units:dim 1 :in :tol 0.005)))  ; 0.005 inches = ±0.005"
    ;; Nominal converted to mm (25.4)
    (is (approximately= 25.4 (clad.units:dimension-nominal dimension) 0.001))
    ;; Tolerance also converted to mm (0.127)
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (approximately= 0.127 (clad.units:tolerance-upper tol) 0.001)))))

;;; ============================================================================
;;; Limit Tolerances
;;; ============================================================================

(test limit-tolerance-basic
  "Limit dimensioning specifies upper and lower bounds"
  (let ((dimension (clad.units:dim :limit :upper 50.1 :lower 49.9 :mm)))
    (is (typep dimension 'clad.units:toleranced-dimension))
    ;; Nominal is midpoint
    (is (approximately= 50.0 (clad.units:dimension-nominal dimension) 0.001))
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (is (eq :limit (clad.units:tolerance-type tol)))
      (is (approximately= 50.1 (clad.units:tolerance-upper-limit tol) 0.001))
      (is (approximately= 49.9 (clad.units:tolerance-lower-limit tol) 0.001)))))

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
      (is (approximately= 0.025 (clad.units:tolerance-upper tol) 0.001))
      (is (approximately= 0.000 (clad.units:tolerance-lower tol) 0.001)))))

(test iso-fit-g6
  "ISO g6 fit tolerance"
  (let ((dimension (clad.units:dim 50 :mm :fit :g6)))
    (let ((tol (clad.units:dimension-tolerance dimension)))
      ;; For 50mm diameter, g6 = -0.009/-0.025
      (is (approximately= -0.009 (clad.units:tolerance-upper tol) 0.001))
      (is (approximately= -0.025 (clad.units:tolerance-lower tol) 0.001)))))

(test iso-fit-invalid-size
  "ISO fit rejects invalid sizes"
  (signals clad.units:iso-fit-error
    (clad.units:dim 0.5 :mm :fit :H7)))  ; Too small for ISO table

;;; ============================================================================
;;; Integration with defpart
;;; ============================================================================

(test tolerance-in-defpart
  "Toleranced dimensions work in defpart DSL"
  (let ((part (test-toleranced-shaft)))
    (is (not (null part)))
    ;; Verify tolerance metadata attached to shape
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (tolerances (getf meta :tolerances)))
      (is (not (null tolerances)))
      (is (= 2 (length tolerances))))))  ; Diameter and length

(clad.dsl:defpart test-toleranced-shaft
    ((diameter (clad.units:dim 25 :mm :fit :H7))
     (length (clad.units:dim 100 :mm :tol 0.5)))
  "Shaft with ISO H7 fit on diameter and ±0.5mm length tolerance"

  (:body (clad.core:make-cylinder diameter length))

  ;; Attach tolerance metadata
  (:tolerance :feature :diameter
              :nominal diameter
              :type :cylindrical)

  (:tolerance :feature :length
              :nominal length
              :type :linear))

;;; ============================================================================
;;; Tolerance Printing and Display
;;; ============================================================================

(test tolerance-print-format
  "Tolerances format correctly for display"
  (let ((tol1 (clad.units:dim 50 :mm :tol 0.1))
        (tol2 (clad.units:dim 25 :mm :tol '(0.05 -0.02)))
        (tol3 (clad.units:dim :limit :upper 30.1 :lower 29.9 :mm)))

    ;; Symmetric: "50 ±0.1"
    (is (equal "50.000 ±0.100" (clad.units:format-tolerance tol1)))

    ;; Asymmetric: "25 +0.05/-0.02"
    (is (equal "25.000 +0.050/-0.020" (clad.units:format-tolerance tol2)))

    ;; Limit: "30.1/29.9"
    (is (equal "30.100/29.900" (clad.units:format-tolerance tol3)))))
```

**Implementation:**

```lisp
;;;; src/units/dimension.lisp - ENHANCE EXISTING

;;; ============================================================================
;;; Enhanced Tolerance Specification Classes
;;; ============================================================================

(defclass tolerance-spec ()
  ((type :initarg :type
         :accessor tolerance-type
         :documentation "Type: :bilateral, :limit, :fit"))
  (:documentation "Base class for tolerance specifications"))

(defclass bilateral-tolerance (tolerance-spec)
  ((upper :initarg :upper
          :accessor tolerance-upper
          :type double-float)
   (lower :initarg :lower
          :accessor tolerance-lower
          :type double-float))
  (:documentation "Bilateral tolerance: nominal +upper/-lower"))

(defclass limit-tolerance (tolerance-spec)
  ((upper-limit :initarg :upper-limit
                :accessor tolerance-upper-limit
                :type double-float)
   (lower-limit :initarg :lower-limit
                :accessor tolerance-lower-limit
                :type double-float))
  (:documentation "Limit dimensioning: upper/lower absolute limits"))

(defclass fit-tolerance (tolerance-spec)
  ((fit-class :initarg :fit-class
              :accessor tolerance-fit-class
              :type string
              :documentation "ISO fit class (e.g., 'H7', 'g6')")
   (upper :initarg :upper
          :accessor tolerance-upper
          :type double-float)
   (lower :initarg :lower
          :accessor tolerance-lower
          :type double-float))
  (:documentation "ISO fit tolerance"))

;;; ============================================================================
;;; Enhanced dim Macro
;;; ============================================================================

(defmacro dim (value-or-keyword &optional unit-or-keyword &rest args)
  "Enhanced dimensional value with units and tolerances.

  Syntax Options:
    1. Simple: (dim 10 :mm)
    2. Bilateral symmetric: (dim 50 :mm :tol 0.1)          ; Implies ±0.1
    3. Bilateral asymmetric: (dim 25 :mm :tol '(0.05 -0.02)) ; Explicit +0.05/-0.02
    4. Limit: (dim :limit :upper 50.1 :lower 49.9 :mm)
    5. ISO Fit: (dim 50 :mm :fit :H7)

  Returns:
    - If :tol or :fit specified: toleranced-dimension object
    - Otherwise: double-float nominal value in mm"

  ;; Parse arguments
  (cond
    ;; Limit dimensioning: (dim :limit :upper 50.1 :lower 49.9 :mm)
    ((eq value-or-keyword :limit)
     (expand-limit-tolerance unit-or-keyword args))

    ;; Regular dimension with possible tolerance
    (t
     (expand-regular-dimension value-or-keyword unit-or-keyword args))))

(defun expand-limit-tolerance (unit-or-keyword args)
  "Expand limit tolerance syntax"
  (let ((upper nil)
        (lower nil)
        (units nil))
    ;; Parse :upper/:lower/:mm
    (loop for (key value) on args by #'cddr
          do (case key
               (:upper (setf upper value))
               (:lower (setf lower value))
               (otherwise
                (when (keywordp key)
                  (setf units key)))))

    (unless (and upper lower)
      (error "Limit tolerance requires :upper and :lower values"))

    (let ((unit (or units (effective-units))))
      `(make-limit-toleranced-dimension
         :upper-limit (convert-units ,upper ,unit :mm)
         :lower-limit (convert-units ,lower ,unit :mm)
         :units ,unit))))

(defun expand-regular-dimension (value unit-or-keyword args)
  "Expand regular dimension with optional tolerance/fit"
  (let ((unit (if (keywordp unit-or-keyword)
                  unit-or-keyword
                  '(effective-units)))
        (tol-spec nil)
        (fit-spec nil))

    ;; Parse :tol or :fit from args
    (loop for (key value) on args by #'cddr
          do (case key
               (:tol (setf tol-spec value))
               (:fit (setf fit-spec value))
               (t (error "Unknown dim parameter: ~A" key))))

    (cond
      ;; With fit tolerance
      (fit-spec
       `(make-fit-toleranced-dimension
          :nominal (convert-units ,value ,unit :mm)
          :source-value ,value
          :source-units ,unit
          :fit-class ,fit-spec))

      ;; With explicit tolerance
      (tol-spec
       `(make-bilateral-toleranced-dimension
          :nominal (convert-units ,value ,unit :mm)
          :source-value ,value
          :source-units ,unit
          :tolerance-spec ,(expand-tolerance-spec tol-spec unit)))

      ;; No tolerance - return nominal value
      (t
       `(coerce (convert-units ,value ,unit :mm) 'double-float)))))

(defun expand-tolerance-spec (spec unit)
  "Expand tolerance specification.

  Accepts:
    - Single number: 0.1 → bilateral symmetric ±0.1
    - List of two: '(0.05 -0.02) → bilateral asymmetric +0.05/-0.02"
  (cond
    ;; Symmetric: single number implies ±
    ((numberp spec)
     `(make-instance 'bilateral-tolerance
                     :type :bilateral
                     :upper (convert-units ,spec ,unit :mm)
                     :lower (- (convert-units ,spec ,unit :mm))))

    ;; Asymmetric: list of (upper lower)
    ((and (listp spec) (= 2 (length spec)))
     (let ((upper (first spec))
           (lower (second spec)))
       `(make-instance 'bilateral-tolerance
                       :type :bilateral
                       :upper (convert-units ,upper ,unit :mm)
                       :lower (convert-units ,lower ,unit :mm))))

    (t (error "Invalid tolerance specification: ~A. Use a number (0.1) or list '(0.05 -0.02)" spec))))

;;; ============================================================================
;;; ISO Fit Tables (ISO 286-1)
;;; ============================================================================

(defvar *iso-fit-table* (make-hash-table :test 'equal)
  "ISO 286-1 fundamental tolerance values")

(defun define-iso-fit (fit-class size-min size-max upper lower)
  "Define ISO fit tolerance values"
  (push (list size-min size-max upper lower)
        (gethash fit-class *iso-fit-table*)))

;; Populate ISO H7 fit (hole basis - common)
;; Size ranges in mm, tolerances in mm
(define-iso-fit "H7" 3 6 0.010 0.000)
(define-iso-fit "H7" 6 10 0.015 0.000)
(define-iso-fit "H7" 10 18 0.018 0.000)
(define-iso-fit "H7" 18 30 0.021 0.000)
(define-iso-fit "H7" 30 50 0.025 0.000)
(define-iso-fit "H7" 50 80 0.030 0.000)
(define-iso-fit "H7" 80 120 0.035 0.000)
(define-iso-fit "H7" 120 180 0.040 0.000)
(define-iso-fit "H7" 180 250 0.046 0.000)
(define-iso-fit "H7" 250 315 0.052 0.000)

;; Populate ISO g6 fit (shaft basis - common)
(define-iso-fit "g6" 3 6 -0.006 -0.014)
(define-iso-fit "g6" 6 10 -0.009 -0.020)
(define-iso-fit "g6" 10 18 -0.011 -0.025)
(define-iso-fit "g6" 18 30 -0.013 -0.029)
(define-iso-fit "g6" 30 50 -0.009 -0.025)  ; Note: This is simplified
(define-iso-fit "g6" 50 80 -0.010 -0.029)
(define-iso-fit "g6" 80 120 -0.012 -0.034)

;; Add more fits as needed: h6, js7, k6, n6, p6, r6, etc.

(define-condition iso-fit-error (error)
  ((message :initarg :message :reader iso-fit-error-message))
  (:report (lambda (condition stream)
             (format stream "ISO Fit Error: ~A"
                     (iso-fit-error-message condition)))))

(defun lookup-iso-fit (fit-class nominal-size)
  "Lookup ISO fit tolerance for nominal size.

  Args:
    fit-class - String like 'H7', 'g6'
    nominal-size - Nominal dimension in mm

  Returns: (values upper lower) in mm"
  (let ((ranges (gethash (string-upcase fit-class) *iso-fit-table*)))
    (unless ranges
      (error 'iso-fit-error
             :message (format nil "Unknown fit class: ~A" fit-class)))

    ;; Find matching size range
    (loop for (size-min size-max upper lower) in ranges
          when (and (<= size-min nominal-size)
                    (< nominal-size size-max))
            do (return-from lookup-iso-fit (values upper lower)))

    ;; No matching range
    (error 'iso-fit-error
           :message (format nil "Nominal size ~A mm not in ISO table for ~A"
                            nominal-size fit-class))))

;;; ============================================================================
;;; Constructors for Toleranced Dimensions
;;; ============================================================================

(defun make-bilateral-toleranced-dimension (&key nominal source-value source-units tolerance-spec)
  "Create bilateral toleranced dimension"
  (make-instance 'toleranced-dimension
                 :nominal nominal
                 :source-value source-value
                 :source-units source-units
                 :tolerance tolerance-spec))

(defun make-limit-toleranced-dimension (&key upper-limit lower-limit units)
  "Create limit toleranced dimension"
  (let ((nominal (/ (+ upper-limit lower-limit) 2.0))
        (tol-spec (make-instance 'limit-tolerance
                                 :type :limit
                                 :upper-limit upper-limit
                                 :lower-limit lower-limit)))
    (make-instance 'toleranced-dimension
                   :nominal nominal
                   :source-value nominal
                   :source-units units
                   :tolerance tol-spec)))

(defun make-fit-toleranced-dimension (&key nominal source-value source-units fit-class)
  "Create ISO fit toleranced dimension"
  (multiple-value-bind (upper lower)
      (lookup-iso-fit (string fit-class) nominal)
    (let ((tol-spec (make-instance 'fit-tolerance
                                   :type :fit
                                   :fit-class (string fit-class)
                                   :upper upper
                                   :lower lower)))
      (make-instance 'toleranced-dimension
                     :nominal nominal
                     :source-value source-value
                     :source-units source-units
                     :tolerance tol-spec))))

;;; ============================================================================
;;; Formatting and Display
;;; ============================================================================

(defun format-tolerance (toleranced-dim &optional (stream nil))
  "Format toleranced dimension for display.

  Examples:
    50.000 ±0.100
    25.000 +0.050/-0.020
    30.100/29.900
    50.000 H7 (+0.025/+0.000)"
  (let ((nominal (dimension-nominal toleranced-dim))
        (tol (dimension-tolerance toleranced-dim)))
    (cond
      ;; Bilateral symmetric
      ((and (typep tol 'bilateral-tolerance)
            (approximately= (tolerance-upper tol)
                           (- (tolerance-lower tol))
                           0.0001))
       (format stream "~,3F ±~,3F" nominal (tolerance-upper tol)))

      ;; Bilateral asymmetric
      ((typep tol 'bilateral-tolerance)
       (format stream "~,3F +~,3F/~,3F"
               nominal
               (tolerance-upper tol)
               (tolerance-lower tol)))

      ;; Limit
      ((typep tol 'limit-tolerance)
       (format stream "~,3F/~,3F"
               (tolerance-upper-limit tol)
               (tolerance-lower-limit tol)))

      ;; ISO Fit
      ((typep tol 'fit-tolerance)
       (format stream "~,3F ~A (+~,3F/~,3F)"
               nominal
               (tolerance-fit-class tol)
               (tolerance-upper tol)
               (tolerance-lower tol)))

      (t
       (format stream "~,3F" nominal)))))
```

---

#### Cycle 1.2: Tolerance Metadata Attachment (3-4 hours)

**Goal:** Attach tolerance metadata to shapes for export.

**Tests:**

```lisp
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
```

**Implementation:**

```lisp
;;;; src/core/primitives.lisp - ENHANCE

(defun make-cylinder (radius height &key (center t) metadata)
  "Enhanced to handle toleranced dimensions"
  ;; Extract nominal value if toleranced-dimension
  (let ((nominal-radius (if (typep radius 'clad.units:toleranced-dimension)
                           (clad.units:dimension-nominal radius)
                           radius)))

    ;; Build tolerance metadata
    (let ((tol-meta (when (typep radius 'clad.units:toleranced-dimension)
                      (list :tolerances
                            (list (list :feature :diameter
                                       :dimension radius
                                       :type :cylindrical))))))

      ;; Merge with user metadata
      (let ((full-metadata (append metadata tol-meta)))
        (let ((handle (ffi-make-cylinder nominal-radius height :center center)))
          (make-shape handle :metadata full-metadata))))))

;; Similar for make-box, make-sphere, make-cone
```

---

### Phase 2: Datum System (Week 3-4)

**Goal:** Define datum features for GD&T references.

**Deliverables:**
- `:datum` DSL form to mark faces/features
- Datum reference frame (A, B, C)
- Material condition modifiers (MMC, LMC, RFS)
- Datum metadata storage
- Full test coverage

---

#### Cycle 2.1: Basic Datum Definition (4-6 hours)

**Tests:**

```lisp
(test datum-definition-basic
  "Define datum feature on face"
  (let ((part (test-part-with-datum)))
    ;; Verify datum stored in metadata
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datums (getf meta :datums)))
      (is (not (null datums)))
      (is (assoc "A" datums :test #'equal)))))

(clad.dsl:defpart test-part-with-datum ()
  (:body (clad.core:make-box 100 100 10))

  ;; Define datum A on bottom face
  (:datum "A" :on-face :direction :-z :extreme :min))

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
  (:body (clad.core:make-box 100 100 10))

  ;; Primary datum (usually largest/flattest face)
  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Secondary datum (perpendicular to primary)
  (:datum "B" :on-face :direction :+x :extreme :max)

  ;; Tertiary datum (perpendicular to primary and secondary)
  (:datum "C" :on-face :direction :+y :extreme :max))

(test datum-with-material-condition
  "Datum with material condition modifier"
  (let ((part (test-datum-with-mmc)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (datum-a (cdr (assoc "A" (getf meta :datums) :test #'equal))))
      ;; Datum A specified at MMC
      (is (eq :mmc (getf datum-a :material-condition))))))

(clad.dsl:defpart test-datum-with-mmc ()
  (:body (clad.core:make-box 100 100 10))

  ;; Datum at Maximum Material Condition
  (:datum "A" :on-face :direction :-z :extreme :min :mmc t))
```

**Implementation:**

```lisp
;;;; src/gdt/datums.lisp (NEW FILE)

(in-package :clad.gdt)

;;; ============================================================================
;;; Datum Feature Definition
;;; ============================================================================

(defclass datum-feature ()
  ((label :initarg :label
          :accessor datum-label
          :type string
          :documentation "Datum label (A, B, C, etc.)")
   (selector :initarg :selector
             :accessor datum-selector
             :documentation "Selector specification for datum feature")
   (material-condition :initarg :material-condition
                       :initform :rfs
                       :accessor datum-material-condition
                       :type (member :mmc :lmc :rfs)
                       :documentation "Material condition: MMC, LMC, or RFS"))
  (:documentation "Datum feature definition per ASME Y14.5"))

(defun make-datum (label selector &key (material-condition :rfs))
  "Create datum feature definition.

  Args:
    label - Datum label (A, B, C, etc.)
    selector - Selector spec to identify feature
    material-condition - :mmc, :lmc, or :rfs (default)

  Returns: datum-feature instance"
  (make-instance 'datum-feature
                 :label (string-upcase label)
                 :selector selector
                 :material-condition material-condition))

;;; ============================================================================
;;; DSL Integration
;;; ============================================================================

;;;; src/dsl/defpart.lisp - ADD NEW FORM

(defun expand-part-form-at-compile-time (form)
  (case (first form)
    ;; Existing cases: :body, :on-face, :on-edge, etc.

    (:datum
     (expand-datum-form-at-compile-time (rest form)))

    ...))

(defun expand-datum-form-at-compile-time (args)
  "Expand (:datum \"A\" :on-face :direction :-z :extreme :min :mmc t)

  Syntax:
    (:datum label selector-spec [:mmc | :lmc | :rfs])

  The datum is stored in shape metadata for later GD&T references."
  (let ((label (first args))
        (selector-spec '())
        (material-condition :rfs))

    ;; Parse selector and modifiers
    (loop for arg in (rest args)
          do (cond
               ((eq arg :mmc) (setf material-condition :mmc))
               ((eq arg :lmc) (setf material-condition :lmc))
               ((eq arg :rfs) (setf material-condition :rfs))
               (t (push arg selector-spec))))

    (setf selector-spec (nreverse selector-spec))

    ;; Generate code to store datum in metadata
    `(let* ((current-shape (get-result))
            (meta (clad.core:shape-metadata (clad.shapes:unwrap-shape current-shape)))
            (datums (getf meta :datums))
            (datum-def (clad.gdt:make-datum
                         ,label
                         ',selector-spec
                         :material-condition ,material-condition)))

       ;; Add datum to metadata
       (setf (getf meta :datums)
             (cons (cons ,label datum-def) datums))

       ;; Update shape metadata
       (setf (clad.core:shape-metadata (clad.shapes:unwrap-shape current-shape))
             meta))))
```

---

### Phase 3: Geometric Tolerancing (Week 5-8)

**Goal:** Implement GD&T feature control frames.

**Deliverables:**
- Form tolerances (flatness, straightness, circularity, cylindricity)
- Orientation (perpendicularity, parallelism, angularity)
- Location (position, concentricity, symmetry)
- Profile (surface profile, line profile)
- Runout (circular runout, total runout)
- Full test coverage

---

#### Cycle 3.1: Form Tolerances (6-8 hours)

**Tests:**

```lisp
(test flatness-tolerance
  "Flatness tolerance on face"
  (let ((part (part-with-flatness)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt (getf meta :geometric-tolerances))
           (flatness-tol (find-if (lambda (t) (eq (getf t :type) :flatness)) gdt)))
      (is (not (null flatness-tol)))
      (is (approximately= 0.05 (getf flatness-tol :tolerance-zone) 0.001)))))

(clad.dsl:defpart part-with-flatness ()
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Flatness tolerance on top face
  (:flatness :on-face :direction :+z :extreme :max
             :tolerance 0.05))  ; 0.05mm flatness zone

(test perpendicularity-tolerance
  "Perpendicularity with datum reference"
  (let ((part (part-with-perpendicularity)))
    (let* ((meta (clad.core:shape-metadata (clad.shapes:unwrap-shape part)))
           (gdt (getf meta :geometric-tolerances))
           (perp-tol (find-if (lambda (t) (eq (getf t :type) :perpendicularity)) gdt)))
      (is (not (null perp-tol)))
      (is (equal "A" (getf perp-tol :datum-ref)))
      (is (approximately= 0.1 (getf perp-tol :tolerance-zone) 0.001)))))

(clad.dsl:defpart part-with-perpendicularity ()
  (:body (clad.core:make-box 100 100 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Side face must be perpendicular to datum A within 0.1mm
  (:perpendicularity :on-face :direction :+x :extreme :max
                     :tolerance 0.1
                     :datum-ref "A"))
```

**Implementation:**

```lisp
;;;; src/gdt/geometric-tolerances.lisp (NEW FILE)

(in-package :clad.gdt)

;;; ============================================================================
;;; Geometric Tolerance Base Class
;;; ============================================================================

(defclass geometric-tolerance ()
  ((type :initarg :type
         :accessor tolerance-gdt-type
         :documentation "GD&T type: :flatness, :perpendicularity, :position, etc.")
   (tolerance-zone :initarg :tolerance-zone
                   :accessor tolerance-zone-value
                   :type double-float
                   :documentation "Tolerance zone size in mm")
   (feature-selector :initarg :feature-selector
                     :accessor tolerance-feature-selector
                     :documentation "Selector for controlled feature"))
  (:documentation "Base class for geometric tolerances"))

(defclass form-tolerance (geometric-tolerance)
  ()
  (:documentation "Form tolerance: flatness, straightness, circularity, cylindricity"))

(defclass orientation-tolerance (geometric-tolerance)
  ((datum-refs :initarg :datum-refs
               :accessor tolerance-datum-refs
               :type list
               :documentation "List of datum references")
   (material-conditions :initarg :material-conditions
                        :initform '()
                        :accessor tolerance-material-conditions
                        :type list
                        :documentation "Material condition modifiers for datum refs"))
  (:documentation "Orientation tolerance: perpendicularity, parallelism, angularity"))

(defclass location-tolerance (geometric-tolerance)
  ((datum-refs :initarg :datum-refs
               :accessor tolerance-datum-refs
               :type list)
   (material-condition :initarg :material-condition
                       :initform :rfs
                       :accessor tolerance-material-condition
                       :type (member :mmc :lmc :rfs))
   (material-conditions :initarg :material-conditions
                        :initform '()
                        :accessor tolerance-datum-material-conditions
                        :type list))
  (:documentation "Location tolerance: position, concentricity, symmetry"))

;;; ============================================================================
;;; Flatness
;;; ============================================================================

(defun make-flatness-tolerance (feature-selector tolerance-zone)
  "Create flatness tolerance.

  Flatness: Surface must lie within tolerance zone (two parallel planes)
  No datum reference (form control)"
  (make-instance 'form-tolerance
                 :type :flatness
                 :tolerance-zone tolerance-zone
                 :feature-selector feature-selector))

;;; ============================================================================
;;; Perpendicularity
;;; ============================================================================

(defun make-perpendicularity-tolerance (feature-selector tolerance-zone datum-ref
                                        &key (material-condition :rfs))
  "Create perpendicularity tolerance.

  Perpendicularity: Feature must be perpendicular to datum within tolerance zone"
  (make-instance 'orientation-tolerance
                 :type :perpendicularity
                 :tolerance-zone tolerance-zone
                 :feature-selector feature-selector
                 :datum-refs (list datum-ref)
                 :material-conditions (list material-condition)))

;;; ============================================================================
;;; Position
;;; ============================================================================

(defun make-position-tolerance (feature-selector tolerance-zone datum-refs
                                &key (material-condition :rfs)
                                     (datum-material-conditions '())
                                     (theoretical-position nil))
  "Create position tolerance.

  Position: Feature center, axis, or surface must be within tolerance zone
  from theoretically exact dimensions (basic dimensions)"
  (make-instance 'location-tolerance
                 :type :position
                 :tolerance-zone tolerance-zone
                 :feature-selector feature-selector
                 :datum-refs datum-refs
                 :material-condition material-condition
                 :material-conditions datum-material-conditions))

;;; ============================================================================
;;; DSL Integration
;;; ============================================================================

;;;; src/dsl/defpart.lisp - ADD GDT FORMS

(defun expand-part-form-at-compile-time (form)
  (case (first form)
    ;; Existing: :body, :on-face, :datum, etc.

    (:flatness
     (expand-flatness-form-at-compile-time (rest form)))

    (:perpendicularity
     (expand-perpendicularity-form-at-compile-time (rest form)))

    (:position
     (expand-position-form-at-compile-time (rest form)))

    ;; Add: :parallelism, :angularity, :circularity, :cylindricity,
    ;;      :concentricity, :symmetry, :profile-surface, :profile-line,
    ;;      :circular-runout, :total-runout

    ...))

(defun expand-flatness-form-at-compile-time (args)
  "Expand (:flatness :on-face selector :tolerance 0.05)

  Syntax:
    (:flatness :on-face selector-spec :tolerance zone-size)"
  (let ((selector-spec '())
        (tolerance-zone nil))

    ;; Parse arguments
    (loop for (key value) on args by #'cddr
          do (case key
               (:on-face (setf selector-spec value))
               (:tolerance (setf tolerance-zone value))
               (t (push value selector-spec))))

    `(let* ((current-shape (get-result))
            (meta (clad.core:shape-metadata (clad.shapes:unwrap-shape current-shape)))
            (gdt-list (getf meta :geometric-tolerances))
            (tolerance-obj (clad.gdt:make-flatness-tolerance
                             ',selector-spec
                             ,tolerance-zone)))

       ;; Add to GDT list in metadata
       (setf (getf meta :geometric-tolerances)
             (cons tolerance-obj gdt-list))

       ;; Update metadata
       (setf (clad.core:shape-metadata (clad.shapes:unwrap-shape current-shape))
             meta))))
```

---

### Phase 4: STEP AP242 Export with PMI (Week 9-12)

**Goal:** Export parts with complete Product Manufacturing Information.

**Deliverables:**
- STEP AP242 file writer
- Dimensional tolerance entities
- Datum entities
- Geometric tolerance entities
- Material condition modifiers
- Full test coverage

---

#### Cycle 4.1: STEP AP242 Base Exporter (8-12 hours)

**Tests:**

```lisp
(test step-ap242-dimensional-tolerance-export
  "Export dimension with tolerance to STEP AP242"
  (let ((shaft (clad.core:make-cylinder
                 (clad.units:dim 25 :mm :fit :H7)
                 100)))
    ;; Export to STEP AP242
    (clad.export:export-step-ap242 shaft "test-shaft.stp")

    ;; Verify file contains PMI entities
    (let ((step-content (read-file-to-string "test-shaft.stp")))
      (is (search "DIMENSIONAL_SIZE_WITH_TOLERANCE" step-content))
      (is (search "PLUS_MINUS_TOLERANCE" step-content)))))

(test step-ap242-datum-export
  "Export datum features to STEP AP242"
  (let ((part (part-with-datums)))
    (clad.export:export-step-ap242 part "test-datums.stp")

    (let ((step-content (read-file-to-string "test-datums.stp")))
      (is (search "DATUM_FEATURE" step-content))
      (is (search "DATUM_REFERENCE" step-content)))))

(test step-ap242-geometric-tolerance-export
  "Export GD&T to STEP AP242"
  (let ((part (part-with-position-tolerance)))
    (clad.export:export-step-ap242 part "test-gdt.stp")

    (let ((step-content (read-file-to-string "test-gdt.stp")))
      (is (search "GEOMETRIC_TOLERANCE" step-content))
      (is (search "POSITION_TOLERANCE" step-content))
      (is (search "TOLERANCE_ZONE" step-content)))))
```

**Implementation:**

```lisp
;;;; src/export/step-ap242.lisp (NEW FILE)

(in-package :clad.export)

;;; ============================================================================
;;; STEP AP242 Export with PMI
;;; ============================================================================

(defun export-step-ap242 (shape filename &key (units :mm))
  "Export shape to STEP AP242 with Product Manufacturing Information.

  AP242 includes:
  - Geometry (like AP203)
  - Dimensional tolerances
  - Geometric tolerances (GD&T)
  - Datum features
  - Material conditions
  - Surface finish
  - Notes and annotations

  Args:
    shape - Shape to export
    filename - Output STEP file path
    units - Unit system (:mm, :in, etc.)

  Returns: T on success"

  ;; Unwrap shape
  (let ((core-shape (if (typep shape 'clad.shapes:cad-shape)
                        (clad.shapes:unwrap-shape shape)
                        shape)))

    (unless (clad.core:valid-shape-p core-shape)
      (error "Invalid shape for export"))

    ;; Create STEP file
    (with-open-file (stream filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)

      ;; Write STEP header
      (write-step-header stream :format :ap242)

      ;; Write geometry section
      (write-step-geometry stream core-shape)

      ;; Write PMI section
      (let ((metadata (clad.core:shape-metadata core-shape)))
        (when metadata
          ;; Write dimensional tolerances
          (write-dimensional-tolerances stream metadata)

          ;; Write datum features
          (write-datum-features stream metadata)

          ;; Write geometric tolerances
          (write-geometric-tolerances stream metadata)))

      ;; Write STEP footer
      (write-step-footer stream))

    (format t "~&Exported STEP AP242: ~A~%" filename)
    t))

;;; ============================================================================
;;; STEP Entity Writers
;;; ============================================================================

(defun write-step-header (stream &key (format :ap242))
  "Write STEP file header"
  (format stream "ISO-10303-21;~%")
  (format stream "HEADER;~%")
  (format stream "FILE_DESCRIPTION(('AP242 - 3D Product Definition'), '1');~%")
  (format stream "FILE_NAME('~A','~A',('Claude CLAD'),(''),'','CLAD ~A','');~%"
          "generated"
          (format-iso-timestamp (get-universal-time))
          "0.1.0")
  (format stream "FILE_SCHEMA(('AP242_MANAGED_MODEL_BASED_3D_ENGINEERING_MIM_LF'));~%")
  (format stream "ENDSEC;~%")
  (format stream "DATA;~%"))

(defun write-dimensional-tolerances (stream metadata)
  "Write DIMENSIONAL_SIZE_WITH_TOLERANCE entities"
  (let ((tolerances (getf metadata :tolerances)))
    (loop for tol-feature in tolerances
          for id from 1000
          do (let ((dimension (getf tol-feature :dimension)))
               (when (typep dimension 'clad.units:toleranced-dimension)
                 (write-dimensional-tolerance-entity
                   stream id dimension))))))

(defun write-dimensional-tolerance-entity (stream id dimension)
  "Write a single dimensional tolerance entity"
  (let ((nominal (clad.units:dimension-nominal dimension))
        (tol (clad.units:dimension-tolerance dimension)))
    (cond
      ;; Bilateral tolerance
      ((typep tol 'clad.units:bilateral-tolerance)
       (format stream "#~A=DIMENSIONAL_SIZE_WITH_TOLERANCE();~%" id)
       (format stream "#~A=LENGTH_MEASURE_WITH_UNIT(~,3F,#~A);~%"
               (+ id 1) nominal (+ id 2))
       (format stream "#~A=PLUS_MINUS_TOLERANCE(~,3F,~,3F);~%"
               (+ id 3)
               (clad.units:tolerance-upper tol)
               (abs (clad.units:tolerance-lower tol))))

      ;; Limit tolerance
      ((typep tol 'clad.units:limit-tolerance)
       (format stream "#~A=DIMENSIONAL_SIZE_WITH_TOLERANCE();~%" id)
       (format stream "#~A=LIMITS_AND_FITS(~,3F,~,3F);~%"
               (+ id 1)
               (clad.units:tolerance-upper-limit tol)
               (clad.units:tolerance-lower-limit tol)))

      ;; ISO Fit
      ((typep tol 'clad.units:fit-tolerance)
       (format stream "#~A=DIMENSIONAL_SIZE_WITH_TOLERANCE();~%" id)
       (format stream "#~A=TOLERANCE_VALUE('~A');~%"
               (+ id 1)
               (clad.units:tolerance-fit-class tol))
       (format stream "#~A=PLUS_MINUS_TOLERANCE(~,3F,~,3F);~%"
               (+ id 2)
               (clad.units:tolerance-upper tol)
               (abs (clad.units:tolerance-lower tol)))))))

(defun write-datum-features (stream metadata)
  "Write DATUM_FEATURE entities"
  (let ((datums (getf metadata :datums)))
    (loop for (label . datum) in datums
          for id from 2000
          do (format stream "#~A=DATUM_FEATURE('~A');~%"
                     id label)
             (format stream "#~A=DATUM_REFERENCE('~A');~%"
                     (+ id 1) label))))

(defun write-geometric-tolerances (stream metadata)
  "Write GEOMETRIC_TOLERANCE entities"
  (let ((gdt-list (getf metadata :geometric-tolerances)))
    (loop for tol in gdt-list
          for id from 3000
          do (write-geometric-tolerance-entity stream id tol))))

(defun write-geometric-tolerance-entity (stream id tolerance)
  "Write a single geometric tolerance entity"
  (let ((type (clad.gdt:tolerance-gdt-type tolerance))
        (zone (clad.gdt:tolerance-zone-value tolerance)))
    (case type
      (:flatness
       (format stream "#~A=FLATNESS_TOLERANCE(~,3F);~%" id zone))

      (:perpendicularity
       (let ((datum-refs (clad.gdt:tolerance-datum-refs tolerance)))
         (format stream "#~A=PERPENDICULARITY_TOLERANCE(~,3F,#~A);~%"
                 id zone (+ id 100))))

      (:position
       (let ((datum-refs (clad.gdt:tolerance-datum-refs tolerance)))
         (format stream "#~A=POSITION_TOLERANCE(~,3F,(~{#~A~^,~}));~%"
                 id zone (loop for ref in datum-refs
                               for did from (+ id 100)
                               collect did)))))))

(defun write-step-footer (stream)
  "Write STEP file footer"
  (format stream "ENDSEC;~%")
  (format stream "END-ISO-10303-21;~%"))

(defun format-iso-timestamp (universal-time)
  "Format timestamp for STEP header"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))
```

---

### Phase 5: Tolerance Analysis (Week 13-14)

**Goal:** Analyze tolerance stack-ups and worst-case scenarios.

**Deliverables:**
- Tolerance stack-up calculator
- Worst-case and RSS analysis
- Assembly tolerance propagation
- Reporting and visualization
- Full test coverage

---

#### Cycle 5.1: Basic Stack-Up Analysis (6-8 hours)

**Tests:**

```lisp
(test worst-case-stack-up
  "Calculate worst-case tolerance stack-up"
  (let ((dim1 (clad.units:dim 50 :mm :tol 0.1))
        (dim2 (clad.units:dim 30 :mm :tol 0.05))
        (dim3 (clad.units:dim 20 :mm :tol 0.08)))

    ;; Total dimension = 50 + 30 + 20 = 100mm
    ;; Worst case tolerance = ±(0.1 + 0.05 + 0.08) = ±0.23mm
    (let ((stack (clad.tolerance:stack-up :worst-case dim1 dim2 dim3)))
      (is (approximately= 100.0 (clad.tolerance:stack-nominal stack) 0.001))
      (is (approximately= 0.23 (clad.tolerance:stack-total-tolerance stack) 0.001)))))

(test rss-stack-up
  "Calculate RSS (root sum square) tolerance stack-up"
  (let ((dim1 (clad.units:dim 50 :mm :tol 0.1))
        (dim2 (clad.units:dim 30 :mm :tol 0.05))
        (dim3 (clad.units:dim 20 :mm :tol 0.08)))

    ;; RSS tolerance = sqrt(0.1^2 + 0.05^2 + 0.08^2) = ±0.135mm
    (let ((stack (clad.tolerance:stack-up :rss dim1 dim2 dim3)))
      (is (approximately= 0.135 (clad.tolerance:stack-total-tolerance stack) 0.001)))))

(test assembly-tolerance-propagation
  "Tolerance propagates through assembly"
  (let ((shaft (part-with-toleranced-shaft))
        (housing (part-with-toleranced-bore)))
    (let ((assembly (clad.assembly:make-assembly))
          (clearance-analysis
            (clad.tolerance:analyze-fit shaft :diameter-feature
                                        housing :bore-feature)))
      ;; Min/max clearance based on tolerances
      (is (> (getf clearance-analysis :max-clearance) 0))
      (is (< (getf clearance-analysis :min-clearance)
             (getf clearance-analysis :max-clearance))))))
```

**Implementation:**

```lisp
;;;; src/tolerance/analysis.lisp (NEW FILE)

(in-package :clad.tolerance)

;;; ============================================================================
;;; Tolerance Stack-Up Analysis
;;; ============================================================================

(defun stack-up (method &rest dimensions)
  "Calculate tolerance stack-up using specified method.

  Methods:
    :worst-case - Arithmetic sum (conservative)
    :rss - Root sum square (statistical)
    :monte-carlo - Monte Carlo simulation

  Args:
    method - Analysis method
    dimensions - List of toleranced-dimension objects

  Returns: stack-up-result object with:
    - nominal: Total nominal dimension
    - total-tolerance: Combined tolerance
    - method: Method used"
  (case method
    (:worst-case
     (worst-case-stack-up dimensions))
    (:rss
     (rss-stack-up dimensions))
    (:monte-carlo
     (monte-carlo-stack-up dimensions))
    (t (error "Unknown stack-up method: ~A" method))))

(defun worst-case-stack-up (dimensions)
  "Worst-case (arithmetic) tolerance stack-up"
  (let ((total-nominal 0.0)
        (total-upper 0.0)
        (total-lower 0.0))
    (dolist (dim dimensions)
      (let ((nominal (clad.units:dimension-nominal dim))
            (tol (clad.units:dimension-tolerance dim)))
        (incf total-nominal nominal)
        (incf total-upper (clad.units:tolerance-upper tol))
        (incf total-lower (abs (clad.units:tolerance-lower tol)))))

    (make-instance 'stack-up-result
                   :nominal total-nominal
                   :upper-tolerance total-upper
                   :lower-tolerance total-lower
                   :method :worst-case)))

(defun rss-stack-up (dimensions)
  "Root Sum Square (RSS) tolerance stack-up"
  (let ((total-nominal 0.0)
        (sum-squares-upper 0.0)
        (sum-squares-lower 0.0))
    (dolist (dim dimensions)
      (let ((nominal (clad.units:dimension-nominal dim))
            (tol (clad.units:dimension-tolerance dim)))
        (incf total-nominal nominal)
        (incf sum-squares-upper
              (expt (clad.units:tolerance-upper tol) 2))
        (incf sum-squares-lower
              (expt (abs (clad.units:tolerance-lower tol)) 2))))

    (let ((rss-upper (sqrt sum-squares-upper))
          (rss-lower (sqrt sum-squares-lower)))
      (make-instance 'stack-up-result
                     :nominal total-nominal
                     :upper-tolerance rss-upper
                     :lower-tolerance rss-lower
                     :method :rss))))

(defclass stack-up-result ()
  ((nominal :initarg :nominal
            :accessor stack-nominal)
   (upper-tolerance :initarg :upper-tolerance
                    :accessor stack-upper-tolerance)
   (lower-tolerance :initarg :lower-tolerance
                    :accessor stack-lower-tolerance)
   (method :initarg :method
           :accessor stack-method))
  (:documentation "Result of tolerance stack-up analysis"))

(defmethod stack-total-tolerance ((result stack-up-result))
  "Get total tolerance (symmetric)"
  (max (stack-upper-tolerance result)
       (stack-lower-tolerance result)))

;;; ============================================================================
;;; Fit Analysis
;;; ============================================================================

(defun analyze-fit (shaft shaft-feature housing housing-feature)
  "Analyze clearance/interference fit between mating parts.

  Returns property list with:
    :max-clearance - Maximum clearance (loosest fit)
    :min-clearance - Minimum clearance (tightest fit)
    :fit-type - :clearance, :transition, or :interference"

  ;; Extract tolerance data from features
  (let* ((shaft-meta (clad.core:shape-metadata
                       (clad.shapes:unwrap-shape shaft)))
         (housing-meta (clad.core:shape-metadata
                         (clad.shapes:unwrap-shape housing)))
         (shaft-dim (get-feature-dimension shaft-meta shaft-feature))
         (housing-dim (get-feature-dimension housing-meta housing-feature)))

    ;; Calculate min/max shaft diameter
    (let* ((shaft-nom (clad.units:dimension-nominal shaft-dim))
           (shaft-tol (clad.units:dimension-tolerance shaft-dim))
           (shaft-max (+ shaft-nom (clad.units:tolerance-upper shaft-tol)))
           (shaft-min (+ shaft-nom (clad.units:tolerance-lower shaft-tol)))

           ;; Calculate min/max housing bore
           (housing-nom (clad.units:dimension-nominal housing-dim))
           (housing-tol (clad.units:dimension-tolerance housing-dim))
           (housing-max (+ housing-nom (clad.units:tolerance-upper housing-tol)))
           (housing-min (+ housing-nom (clad.units:tolerance-lower housing-tol)))

           ;; Calculate clearances
           (max-clearance (- housing-max shaft-min))
           (min-clearance (- housing-min shaft-max)))

      (list :max-clearance max-clearance
            :min-clearance min-clearance
            :fit-type (cond
                        ((minusp min-clearance) :interference)
                        ((minusp max-clearance) :impossible)  ; Error condition
                        ((< min-clearance 0.001) :transition)
                        (t :clearance))))))
```

---

## File Structure

### New Files to Create

```
src/gdt/
  datums.lisp                       # Datum feature definitions
  geometric-tolerances.lisp         # GD&T feature control frames
  validation.lisp                   # Validate GD&T per ASME Y14.5

src/tolerance/
  analysis.lisp                     # Stack-up and fit analysis
  reporting.lisp                    # Generate tolerance reports

src/export/
  step-ap242.lisp                   # STEP AP242 exporter with PMI
  pmi-entities.lisp                 # STEP PMI entity generators

tests/
  tolerance-tests.lisp              # Dimensional tolerance tests
  datum-tests.lisp                  # Datum system tests
  gdt-tests.lisp                    # Geometric tolerance tests
  step-pmi-tests.lisp               # STEP PMI export tests
  tolerance-analysis-tests.lisp     # Stack-up analysis tests

examples/
  08-toleranced-parts.lisp          # Examples with dimensional tolerances
  09-gdt-features.lisp              # Examples with GD&T
  10-tolerance-analysis.lisp        # Tolerance stack-up examples
```

### Files to Modify

```
src/units/
  dimension.lisp                    # Enhance tolerance-spec classes
  units.lisp                        # No changes needed

src/core/
  primitives.lisp                   # Enhanced to handle toleranced dimensions

src/packages.lisp                   # Add new exports

clad.asd                            # Add new modules

IMPLEMENTATION_PLAN.md              # Add reference to tolerancing plan
```

---

## Testing Strategy

### Test Organization

- **tolerance-tests.lisp** - ~40 tests
- **datum-tests.lisp** - ~20 tests
- **gdt-tests.lisp** - ~50 tests
- **step-pmi-tests.lisp** - ~30 tests
- **tolerance-analysis-tests.lisp** - ~25 tests

**Total:** ~165 new tests

### Test Categories

1. **Unit Tests** - Test individual tolerance classes
2. **Integration Tests** - Test tolerances in defpart DSL
3. **Export Tests** - Verify STEP AP242 output
4. **Analysis Tests** - Validate stack-up calculations
5. **Round-Trip Tests** - Export then import STEP files

### Coverage Goals

- **Line Coverage:** > 90%
- **Branch Coverage:** > 85%
- **Critical Paths:** 100%

---

## Success Criteria

### Phase 1 Complete When:

- [ ] All tolerance syntax tests pass (40+ tests)
- [ ] ISO fit table works for common fits
- [ ] Tolerances stored in shape metadata
- [ ] Documentation updated
- [ ] Examples demonstrate all tolerance types

### Phase 2 Complete When:

- [ ] Datum system tests pass (20+ tests)
- [ ] Datum reference frames (A-B-C) work
- [ ] Material conditions supported
- [ ] DSL syntax clean and intuitive

### Phase 3 Complete When:

- [ ] All GD&T tests pass (50+ tests)
- [ ] All 5 tolerance categories implemented
- [ ] Validation catches invalid GD&T
- [ ] Examples demonstrate real-world usage

### Phase 4 Complete When:

- [ ] STEP AP242 export tests pass (30+ tests)
- [ ] Exported files importable in SolidWorks/FreeCAD
- [ ] PMI visible in CAD viewers
- [ ] All tolerance types export correctly

### Phase 5 Complete When:

- [ ] Stack-up analysis tests pass (25+ tests)
- [ ] Worst-case and RSS methods work
- [ ] Assembly fit analysis functional
- [ ] Reports generate useful output

### Overall Project Complete When:

- [ ] All 5 phases complete
- [ ] All tests pass (165+ tests)
- [ ] Documentation comprehensive
- [ ] Real-world examples demonstrate value
- [ ] STEP files validated by third-party tools
- [ ] No regressions in existing functionality

---

## Timeline Estimate

**Total: 14 weeks (3.5 months)**

- Phase 1: 2 weeks
- Phase 2: 2 weeks
- Phase 3: 4 weeks (most complex)
- Phase 4: 4 weeks (STEP AP242 is involved)
- Phase 5: 2 weeks

**Note:** Can ship incrementally after each phase.

---

## Priority Recommendations

### Immediate Value (Phase 1)

Start here. Dimensional tolerancing provides immediate value for production parts.

### High Value (Phase 2 + 4.1)

Basic datums + dimensional tolerance export enables MBD workflow.

### Maximum Value (All Phases)

Complete GD&T + AP242 export makes CLAD production-ready.

---

## Future Extensions

**Not in this plan, but natural follow-ups:**

1. **Surface Finish**
   - Roughness (Ra, Rz)
   - Surface texture symbols
   - Lay direction

2. **Material Specifications**
   - Material callouts
   - Heat treatment specs
   - Finish specifications

3. **3D Annotations**
   - Notes and labels
   - Leader lines
   - Dimensions on views

4. **Tolerance Optimization**
   - Cost-tolerance analysis
   - Manufacturing process selection
   - Tolerance synthesis

5. **CMM Integration**
   - Inspection plan generation
   - CMM program export
   - Measurement result import

---

## Risk Mitigation

### Technical Risks

**Risk:** STEP AP242 complexity

**Mitigation:**
- Start with simple dimensional tolerances
- Test export with multiple CAD systems
- Consult STEP AP242 specification
- Use existing tools to validate output

**Risk:** ISO fit table completeness

**Mitigation:**
- Implement most common fits first (H7/g6, H7/h6)
- Allow user-defined fits
- Provide clear error messages
- Reference ISO 286-1 standard

**Risk:** GD&T validation complexity

**Mitigation:**
- Start with basic rules
- Add validation incrementally
- Provide warnings, not errors
- Reference ASME Y14.5 standard

---

## Autonomous Implementation Notes

**For AI Agent:**

Follow strict TDD:
1. **Write tests first** for each cycle
2. **Minimal implementation** to pass tests
3. **Refactor** for clarity
4. **Document** thoroughly
5. **Commit** after each cycle

**Test-Driven Cycle:**
```
1. RED: Write failing test
2. GREEN: Minimal implementation
3. REFACTOR: Clean up code
4. DOCUMENT: Add docstrings
5. COMMIT: Save progress
```

---

**End of Tolerancing Implementation Plan**

This plan enables CLAD to become a **production engineering tool** with full MBD (Model-Based Definition) capabilities. Each phase delivers independent value and can be shipped incrementally.
