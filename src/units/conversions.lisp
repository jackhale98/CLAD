;;;; src/units/conversions.lisp --- Additional conversion utilities

(in-package :clad.units)

;;; ============================================================================
;;; Conversion Utilities
;;; ============================================================================

(defun mm->in (mm)
  "Convert millimeters to inches"
  (convert-units mm :mm :in))

(defun in->mm (inches)
  "Convert inches to millimeters"
  (convert-units inches :in :mm))

(defun mm->cm (mm)
  "Convert millimeters to centimeters"
  (convert-units mm :mm :cm))

(defun cm->mm (cm)
  "Convert centimeters to millimeters"
  (convert-units cm :cm :mm))

(defun mm->m (mm)
  "Convert millimeters to meters"
  (convert-units mm :mm :m))

(defun m->mm (m)
  "Convert meters to millimeters"
  (convert-units m :m :mm))

;;; ============================================================================
;;; Unit Display
;;; ============================================================================

(defun format-dimension (value unit &optional (stream t))
  "Format a dimensional value with units for display.

  Arguments:
    value  - Numeric value
    unit   - Unit keyword
    stream - Output stream (default: t for *standard-output*)

  Example:
    (format-dimension 25.4 :mm)  => \"25.4mm\"
    (format-dimension 1.0 :in)   => \"1.0in\""
  (format stream "~,3F~A" value (string-downcase (symbol-name unit))))

(defun unit-symbol (unit)
  "Get the display symbol for a unit.

  Returns: string representing the unit"
  (case unit
    (:mm "mm")
    (:cm "cm")
    (:m "m")
    (:in "in")
    (:ft "ft")
    (:thou "thou")
    (:mil "mil")
    (:yard "yd")
    (:mile "mi")
    (:km "km")
    (:um "μm")
    (:nm "nm")
    (t (string-downcase (symbol-name unit)))))
