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

(defmacro dim (value &optional (unit nil unit-provided-p) &key tol fit grade)
  "Dimensional value with unit inheritance and optional tolerance.

  Unit resolution order:
    1. Explicit unit provided: (dim 10 :in)
    2. Current dynamic context from with-units
    3. File-level *file-units*
    4. Global *default-units*

  Arguments:
    value - Numeric value
    unit  - Optional unit keyword (if omitted, uses context)
    tol   - Optional tolerance specification (for Phase 9)
    fit   - Optional ISO fit specification (for Phase 9)
    grade - Optional tolerance grade (for Phase 9)

  Returns: double-float in mm (OCCT native units)

  Examples:
    (dim 10)              ; Uses inherited units, returns value in mm
    (dim 10 :in)          ; Explicit override to inches
    (dim 10 :mm :tol ±0.1); With tolerance (Phase 9)
    (dim 30 :mm :fit :H7) ; ISO fit (Phase 9)

  Note: Tolerance and fit parameters are reserved for Phase 9 and currently ignored."
  (declare (ignore tol fit grade))  ; Will be implemented in Phase 9

  (let ((source-unit (cond
                       (unit-provided-p unit)
                       (t '(effective-units)))))

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
        `(coerce (convert-units ,value ,source-unit :mm) 'double-float))))

;;; ============================================================================
;;; Tolerance Specifications (Placeholder for Phase 9)
;;; ============================================================================

(defclass tolerance-spec ()
  ((type :initarg :type
         :accessor tolerance-type
         :documentation "Type of tolerance: :bilateral, :unilateral, :fit")
   (upper :initarg :upper
          :accessor tolerance-upper
          :documentation "Upper tolerance limit")
   (lower :initarg :lower
          :accessor tolerance-lower
          :documentation "Lower tolerance limit"))
  (:documentation "Tolerance specification (Phase 9)"))

(defun make-tolerance-spec (&key type upper lower)
  "Create a tolerance specification (Phase 9)"
  (make-instance 'tolerance-spec
                 :type type
                 :upper upper
                 :lower lower))

(defun bilateral-tolerance (value)
  "Create a bilateral (±) tolerance spec (Phase 9)"
  (make-tolerance-spec :type :bilateral :upper value :lower (- value)))

(defun unilateral-tolerance (upper lower)
  "Create a unilateral tolerance spec (Phase 9)"
  (make-tolerance-spec :type :unilateral :upper upper :lower lower))

(defun symmetric-tolerance (value)
  "Alias for bilateral-tolerance (Phase 9)"
  (bilateral-tolerance value))
