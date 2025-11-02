;;;; src/units/units.lisp --- Unit conversion system

(in-package :clad.units)

;;; ============================================================================
;;; Unit Context Variables
;;; ============================================================================

(defvar *default-units* :mm
  "System-wide default units (typically :mm or :in).
  This is the fallback when no other unit context is specified.")

(defvar *file-units* nil
  "File-level unit override. If nil, uses *default-units*.
  Set this at the top of a file to establish file-wide units.")

;;; ============================================================================
;;; Unit Conversion Table
;;; ============================================================================

(defvar *unit-conversions*
  '((:mm . 1.0d0)           ; millimeters (OCCT native)
    (:cm . 10.0d0)          ; centimeters
    (:m . 1000.0d0)         ; meters
    (:in . 25.4d0)          ; inches
    (:ft . 304.8d0)         ; feet
    (:thou . 0.0254d0)      ; thousandths of an inch (mils)
    (:mil . 0.0254d0))      ; mils (same as thou)
  "Conversion factors from each unit to mm (OCCT native units).
  Each entry is (unit . factor-to-mm)")

;;; ============================================================================
;;; Unit Functions
;;; ============================================================================

(defun unit-p (unit)
  "Check if UNIT is a valid unit keyword"
  (and (keywordp unit)
       (assoc unit *unit-conversions*)))

(defun valid-unit-p (unit)
  "Alias for UNIT-P"
  (unit-p unit))

(defun effective-units ()
  "Get the currently effective units from the context stack.

  Resolution order:
    1. *file-units* (if non-nil)
    2. *default-units*

  Returns: keyword representing current units"
  (or *file-units* *default-units*))

(defun convert-units (value from-unit to-unit)
  "Convert VALUE from FROM-UNIT to TO-UNIT.

  Arguments:
    value     - Numeric value to convert
    from-unit - Source unit keyword (e.g., :in, :mm)
    to-unit   - Target unit keyword

  Returns: Converted value as double-float

  Example:
    (convert-units 1 :in :mm) => 25.4d0
    (convert-units 10 :cm :in) => 3.937...d0"
  (let ((from-factor (or (cdr (assoc from-unit *unit-conversions*))
                        (error "Unknown unit: ~A" from-unit)))
        (to-factor (or (cdr (assoc to-unit *unit-conversions*))
                      (error "Unknown unit: ~A" to-unit))))
    (coerce (* value (/ from-factor to-factor)) 'double-float)))

(defun define-unit-conversion (unit-keyword factor-to-mm)
  "Define a custom unit with conversion factor to mm.

  Arguments:
    unit-keyword  - Keyword symbol for the unit (e.g., :yard)
    factor-to-mm  - Conversion factor from this unit to mm

  Example:
    (define-unit-conversion :yard 914.4d0)
    (define-unit-conversion :mile 1609344.0d0)"
  (unless (keywordp unit-keyword)
    (error "Unit must be a keyword: ~A" unit-keyword))
  (unless (and (numberp factor-to-mm) (plusp factor-to-mm))
    (error "Conversion factor must be a positive number: ~A" factor-to-mm))

  (let ((existing (assoc unit-keyword *unit-conversions*)))
    (if existing
        (setf (cdr existing) (coerce factor-to-mm 'double-float))
        (push (cons unit-keyword (coerce factor-to-mm 'double-float))
              *unit-conversions*)))
  unit-keyword)

;;; ============================================================================
;;; Context Macro
;;; ============================================================================

(defmacro with-units (unit &body body)
  "Execute BODY with *file-units* bound to UNIT.

  This creates a dynamic unit context for the body forms.

  Arguments:
    unit - Unit keyword to use (:mm, :in, etc.)
    body - Forms to execute in this unit context

  Example:
    (with-units :in
      (make-box (dim 4) (dim 2) (dim 1)))  ; All dimensions in inches"
  `(let ((*file-units* ,unit))
     ,@body))

;;; ============================================================================
;;; Pre-defined Custom Units
;;; ============================================================================

;; Add some commonly used units
(define-unit-conversion :yard 914.4d0)
(define-unit-conversion :mile 1609344.0d0)
(define-unit-conversion :km 1000000.0d0)
(define-unit-conversion :um 0.001d0)        ; micrometers
(define-unit-conversion :nm 0.000001d0)     ; nanometers
