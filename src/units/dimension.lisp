;;;; src/units/dimension.lisp --- Dimension macro and toleranced dimensions

(in-package :clad.units)

;;; ============================================================================
;;; Toleranced Dimension Class (for future tolerance analysis)
;;; ============================================================================

(defclass toleranced-dimension ()
  ((nominal :initarg :nominal
            :accessor dimension-nominal
            :type double-float
            :documentation "Nominal dimension in mm (OCCT native)")
   (source-value :initarg :source-value
                 :accessor dimension-source-value
                 :documentation "Original value as entered")
   (source-units :initarg :source-units
                 :accessor dimension-source-units
                 :documentation "Units entered in")
   (tolerance :initarg :tolerance
              :initform nil
              :accessor dimension-tolerance
              :documentation "Tolerance specification (for Phase 9)")
   (display-units :initarg :display-units
                  :initform nil
                  :accessor dimension-display-units
                  :documentation "Preferred display units"))
  (:documentation "Represents a dimension with optional tolerance information.

  The nominal value is always stored in mm (OCCT native units).
  Source value and units are preserved for traceability.
  Tolerance information will be used in Phase 9 for tolerance analysis."))

(defmethod print-object ((dim toleranced-dimension) stream)
  (print-unreadable-object (dim stream :type t)
    (format stream "~,3Fmm" (dimension-nominal dim))
    (when (dimension-source-units dim)
      (format stream " (from ~A~A)"
              (dimension-source-value dim)
              (unit-symbol (dimension-source-units dim))))))

(defun make-toleranced-dimension (&key nominal source-value source-units
                                       tolerance display-units)
  "Create a toleranced dimension instance"
  (make-instance 'toleranced-dimension
                 :nominal nominal
                 :source-value source-value
                 :source-units source-units
                 :tolerance tolerance
                 :display-units display-units))

;;; ============================================================================
;;; Dimension Macro
;;; ============================================================================

(defmacro dim (value &optional (unit nil unit-provided-p) &key tol fit upper lower grade)
  "Enhanced dimensional value with units and tolerances (Phase T1).

  Unit resolution order:
    1. Explicit unit provided: (dim 10 :in)
    2. Current dynamic context from with-units
    3. File-level *file-units*
    4. Global *default-units*

  Arguments:
    value - Numeric value
    unit  - Optional unit keyword (if omitted, uses context)
    tol   - Optional tolerance: number (symmetric) or list (asymmetric)
    fit   - Optional ISO fit specification (:H7, :g6, etc.)
    upper - Optional upper limit (for limit dimensioning)
    lower - Optional lower limit (for limit dimensioning)
    grade - Optional tolerance grade (reserved for future)

  Returns:
    - If tol/fit/upper/lower provided: toleranced-dimension object
    - Otherwise: double-float in mm (OCCT native units)

  Examples:
    (dim 10)                      ; Uses inherited units, returns value in mm
    (dim 10 :in)                  ; Explicit override to inches
    (dim 50 :mm :tol 0.1)         ; Bilateral symmetric ±0.1
    (dim 25 :mm :tol '(0.05 -0.02)); Bilateral asymmetric +0.05/-0.02
    (dim 50 :mm :upper 50.1 :lower 49.9); Limit dimensioning
    (dim 30 :mm :fit :H7)         ; ISO H7 fit"
  (declare (ignore grade))  ; Reserved for future

  (let ((source-unit (cond
                       (unit-provided-p unit)
                       (t '(effective-units)))))

    (cond
      ;; ISO fit tolerance
      (fit
       `(make-fit-toleranced-dimension
         :nominal (coerce (convert-units ,value ,source-unit :mm) 'double-float)
         :source-value ,value
         :source-units ,source-unit
         :fit-class ,fit))

      ;; Limit dimensioning
      ((and upper lower)
       `(make-limit-toleranced-dimension
         :upper-limit (coerce (convert-units ,upper ,source-unit :mm) 'double-float)
         :lower-limit (coerce (convert-units ,lower ,source-unit :mm) 'double-float)
         :source-value ,value
         :source-units ,source-unit))

      ;; Explicit tolerance
      (tol
       (if (numberp tol)
           ;; Symmetric bilateral
           `(make-bilateral-toleranced-dimension
             :nominal (coerce (convert-units ,value ,source-unit :mm) 'double-float)
             :source-value ,value
             :source-units ,source-unit
             :tolerance-value (coerce (convert-units ,tol ,source-unit :mm) 'double-float))
           ;; Asymmetric bilateral - tol is a list '(upper lower)
           ;; Extract the actual list if it's quoted
           (let ((tol-list (if (and (listp tol) (eq (first tol) 'quote))
                               (second tol)
                               tol)))
             `(make-bilateral-toleranced-dimension
               :nominal (coerce (convert-units ,value ,source-unit :mm) 'double-float)
               :source-value ,value
               :source-units ,source-unit
               :tolerance-value ',tol-list
               :tolerance-unit ,source-unit))))

      ;; No tolerance - return nominal value
      (t
       ;; If source unit is a compile-time constant, try compile-time conversion
       (if (keywordp source-unit)
           ;; Try compile-time conversion, fall back to runtime if unit unknown
           (handler-case
               (let ((converted-value (convert-units value source-unit :mm)))
                 `(coerce ,converted-value 'double-float))
             (error ()
               ;; Unit not known at compile time, defer to runtime
               `(coerce (convert-units ,value ,source-unit :mm) 'double-float)))

           ;; Runtime conversion
           `(coerce (convert-units ,value ,source-unit :mm) 'double-float))))))

;;; ============================================================================
;;; Enhanced Tolerance Specification Classes (Phase T1)
;;; ============================================================================

(defclass tolerance-spec ()
  ((type :initarg :type
         :accessor tolerance-type
         :documentation "Type: :bilateral, :limit, :fit"))
  (:documentation "Base class for tolerance specifications"))

(defclass bilateral-tolerance-spec (tolerance-spec)
  ((upper :initarg :upper
          :accessor tolerance-upper
          :type double-float)
   (lower :initarg :lower
          :accessor tolerance-lower
          :type double-float))
  (:documentation "Bilateral tolerance: nominal +upper/-lower"))

(defclass limit-tolerance-spec (tolerance-spec)
  ((upper-limit :initarg :upper-limit
                :accessor tolerance-upper-limit
                :type double-float)
   (lower-limit :initarg :lower-limit
                :accessor tolerance-lower-limit
                :type double-float))
  (:documentation "Limit dimensioning: upper/lower absolute limits"))

(defclass fit-tolerance-spec (tolerance-spec)
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
(define-iso-fit "G6" 3 6 -0.006 -0.014)
(define-iso-fit "G6" 6 10 -0.009 -0.020)
(define-iso-fit "G6" 10 18 -0.011 -0.025)
(define-iso-fit "G6" 18 30 -0.013 -0.029)
(define-iso-fit "G6" 30 50 -0.009 -0.025)
(define-iso-fit "G6" 50 80 -0.010 -0.029)
(define-iso-fit "G6" 80 120 -0.012 -0.034)

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
  (let ((ranges (gethash (string-upcase (string fit-class)) *iso-fit-table*)))
    (unless ranges
      (error 'iso-fit-error
             :message (format nil "Unknown fit class: ~A" fit-class)))

    ;; Find matching size range
    ;; ISO ranges are "over size-min up to and including size-max"
    (loop for (size-min size-max upper lower) in ranges
          when (and (> nominal-size size-min)
                    (<= nominal-size size-max))
            do (return-from lookup-iso-fit (values upper lower)))

    ;; No matching range
    (error 'iso-fit-error
           :message (format nil "Nominal size ~A mm not in ISO table for ~A"
                            nominal-size fit-class))))

;;; ============================================================================
;;; Tolerance Helper Functions
;;; ============================================================================

(defun bilateral-tolerance (value)
  "Create a bilateral (±) tolerance spec"
  (make-instance 'bilateral-tolerance-spec
                 :type :bilateral
                 :upper (coerce value 'double-float)
                 :lower (coerce (- value) 'double-float)))

(defun unilateral-tolerance (upper lower)
  "Create a unilateral tolerance spec"
  (make-instance 'bilateral-tolerance-spec
                 :type :bilateral
                 :upper (coerce upper 'double-float)
                 :lower (coerce lower 'double-float)))

(defun symmetric-tolerance (value)
  "Alias for bilateral-tolerance"
  (bilateral-tolerance value))

;;; ============================================================================
;;; Constructors for Toleranced Dimensions
;;; ============================================================================

(defun make-bilateral-toleranced-dimension (&key nominal source-value source-units
                                                  tolerance-value tolerance-unit)
  "Create bilateral toleranced dimension"
  (let ((tol-spec
          (if (numberp tolerance-value)
              ;; Symmetric
              (make-instance 'bilateral-tolerance-spec
                             :type :bilateral
                             :upper (coerce tolerance-value 'double-float)
                             :lower (coerce (- tolerance-value) 'double-float))
              ;; Asymmetric - tolerance-value is a list
              (make-instance 'bilateral-tolerance-spec
                             :type :bilateral
                             :upper (coerce (convert-units (first tolerance-value)
                                                          tolerance-unit :mm)
                                           'double-float)
                             :lower (coerce (convert-units (second tolerance-value)
                                                          tolerance-unit :mm)
                                           'double-float)))))
    (make-instance 'toleranced-dimension
                   :nominal nominal
                   :source-value source-value
                   :source-units source-units
                   :tolerance tol-spec)))

(defun make-limit-toleranced-dimension (&key upper-limit lower-limit source-value source-units)
  "Create limit toleranced dimension"
  (let ((nominal (/ (+ upper-limit lower-limit) 2.0))
        (tol-spec (make-instance 'limit-tolerance-spec
                                 :type :limit
                                 :upper-limit upper-limit
                                 :lower-limit lower-limit)))
    (make-instance 'toleranced-dimension
                   :nominal nominal
                   :source-value source-value
                   :source-units source-units
                   :tolerance tol-spec)))

(defun make-fit-toleranced-dimension (&key nominal source-value source-units fit-class)
  "Create ISO fit toleranced dimension"
  (multiple-value-bind (upper lower)
      (lookup-iso-fit (string fit-class) nominal)
    (let ((tol-spec (make-instance 'fit-tolerance-spec
                                   :type :fit
                                   :fit-class (string-upcase (string fit-class))
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
    50.100/49.900
    50.000 H7 (+0.025/+0.000)"
  (let ((nominal (dimension-nominal toleranced-dim))
        (tol (dimension-tolerance toleranced-dim)))
    (cond
      ;; Bilateral symmetric
      ((and (typep tol 'bilateral-tolerance-spec)
            (< (abs (+ (tolerance-upper tol) (tolerance-lower tol))) 0.0001))
       (format stream "~,3F ±~,3F" nominal (tolerance-upper tol)))

      ;; Bilateral asymmetric
      ((typep tol 'bilateral-tolerance-spec)
       (format stream "~,3F +~,3F/~,3F"
               nominal
               (tolerance-upper tol)
               (tolerance-lower tol)))

      ;; Limit
      ((typep tol 'limit-tolerance-spec)
       (format stream "~,3F/~,3F"
               (tolerance-upper-limit tol)
               (tolerance-lower-limit tol)))

      ;; ISO Fit
      ((typep tol 'fit-tolerance-spec)
       (format stream "~,3F ~A (+~,3F/~,3F)"
               nominal
               (tolerance-fit-class tol)
               (tolerance-upper tol)
               (tolerance-lower tol)))

      (t
       (format stream "~,3F" nominal)))))
