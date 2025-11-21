;;;; src/gdt/validation.lisp --- GD&T validation (Priority 1)

(in-package :clad.gdt)

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition gdt-validation-error (error)
  ((message :initarg :message
            :reader gdt-validation-error-message))
  (:report (lambda (condition stream)
             (format stream "GD&T Validation Error: ~A"
                     (gdt-validation-error-message condition))))
  (:documentation "Signaled when GD&T specification violates ASME Y14.5 rules"))

;;; ============================================================================
;;; Tolerance Category Classification
;;; ============================================================================

(defparameter *form-tolerances*
  '(:flatness :straightness :circularity :cylindricity)
  "Form tolerances control shape without datum references")

(defparameter *orientation-tolerances*
  '(:perpendicularity :parallelism :angularity)
  "Orientation tolerances require datum references")

(defparameter *location-tolerances*
  '(:position :concentricity :symmetry)
  "Location tolerances require datum references")

(defparameter *profile-tolerances*
  '(:profile-surface :profile-line)
  "Profile tolerances control complex surfaces (may or may not reference datums)")

(defparameter *runout-tolerances*
  '(:circular-runout :total-runout)
  "Runout tolerances require datum axis reference")

(defun form-tolerance-p (gdt-type)
  "Check if tolerance type is a form tolerance"
  (member gdt-type *form-tolerances*))

(defun orientation-tolerance-p (gdt-type)
  "Check if tolerance type is an orientation tolerance"
  (member gdt-type *orientation-tolerances*))

(defun location-tolerance-p (gdt-type)
  "Check if tolerance type is a location tolerance"
  (member gdt-type *location-tolerances*))

(defun profile-tolerance-p (gdt-type)
  "Check if tolerance type is a profile tolerance"
  (member gdt-type *profile-tolerances*))

(defun runout-tolerance-p (gdt-type)
  "Check if tolerance type is a runout tolerance"
  (member gdt-type *runout-tolerances*))

;;; ============================================================================
;;; Validation Functions
;;; ============================================================================

(defun validate-geometric-tolerance (&key gdt-type datum-refs datum-ref
                                          tolerance-zone material-condition
                                          zone-type bilateral basic-angle)
  "Validate geometric tolerance per ASME Y14.5 rules.

  Validation checks:
    1. Form tolerances must NOT reference datums
    2. Orientation tolerances MUST reference datums
    3. Location tolerances MUST reference datums
    4. Runout tolerances MUST reference datum axis
    5. Tolerance zones must be positive
    6. Material conditions only apply to certain tolerance types
    7. Zone types must be appropriate for tolerance type

  Args:
    gdt-type - Tolerance type keyword (:flatness, :perpendicularity, etc.)
    datum-refs - List of datum references (for most tolerances)
    datum-ref - Single datum reference (for runout tolerances)
    tolerance-zone - Tolerance zone value (must be positive)
    material-condition - Material condition modifier (:mmc, :lmc, :rfs, nil)
    zone-type - Tolerance zone type (:diametrical, :cylindrical, :spherical, etc.)
    bilateral - For profile tolerances (t = bilateral, nil = unilateral)
    basic-angle - For angularity (optional)

  Signals:
    gdt-validation-error - If validation fails"
  (declare (ignore bilateral basic-angle))  ; Used for documentation, not validation

  ;; Check 1: Form tolerances must NOT reference datums
  (when (form-tolerance-p gdt-type)
    (when (or datum-refs datum-ref)
      (error 'gdt-validation-error
             :message (format nil "Form tolerance ~A must not reference datums per ASME Y14.5-2018 Section 7.~%~
                                   Form tolerances (flatness, straightness, circularity, cylindricity) ~
                                   control the shape of a feature without regard to datum references."
                              gdt-type))))

  ;; Check 2: Orientation tolerances MUST reference datums
  (when (orientation-tolerance-p gdt-type)
    ;; Accept either :datum-ref or :datum-refs
    (when (and (null datum-ref)
               (or (null datum-refs) (endp datum-refs)))
      (error 'gdt-validation-error
             :message (format nil "Orientation tolerance ~A requires datum reference per ASME Y14.5-2018 Section 8.~%~
                                   Orientation tolerances (perpendicularity, parallelism, angularity) ~
                                   control orientation relative to datum(s).~%~
                                   ~%Example: (:perpendicularity :on-face :direction :+z :tolerance 0.1 :datum-ref \"A\")"
                              gdt-type))))

  ;; Check 3: Location tolerances MUST reference datums
  (when (location-tolerance-p gdt-type)
    ;; Accept either :datum-ref or :datum-refs
    (when (and (null datum-ref)
               (or (null datum-refs) (endp datum-refs)))
      (error 'gdt-validation-error
             :message (format nil "Location tolerance ~A requires datum reference frame per ASME Y14.5-2018 Section 9.~%~
                                   Location tolerances (position, concentricity, symmetry) establish position relative to datums.~%~
                                   Typically uses 3-2-1 datum reference frame (A-B-C).~%~
                                   ~%Example: (:position :on-face :type :cylindrical :tolerance 0.2 :datum-refs (\"A\" \"B\" \"C\"))"
                              gdt-type))))

  ;; Check 4: Runout tolerances MUST reference datum axis
  (when (runout-tolerance-p gdt-type)
    (when (null datum-ref)
      (error 'gdt-validation-error
             :message (format nil "Runout tolerance ~A requires datum axis reference per ASME Y14.5-2018 Section 10.~%~
                                   Runout tolerances (circular-runout, total-runout) measure variation during rotation about datum axis.~%~
                                   ~%Example: (:circular-runout :on-face :type :cylindrical :tolerance 0.1 :datum-ref \"A\")"
                              gdt-type))))

  ;; Check 5: Tolerance zones must be positive
  (when tolerance-zone
    (unless (and (numberp tolerance-zone) (> tolerance-zone 0))
      (error 'gdt-validation-error
             :message (format nil "Tolerance zone must be positive number, got: ~A~%~
                                   Tolerance zones define the maximum allowable variation and cannot be negative or zero."
                              tolerance-zone))))

  ;; Check 6: Material conditions only apply to certain tolerance types
  ;; :rfs means "Regardless of Feature Size" (no modifier), only check :mmc/:lmc
  (when (and material-condition
             (member material-condition '(:mmc :lmc)))
    (when (form-tolerance-p gdt-type)
      (error 'gdt-validation-error
             :message (format nil "Material condition modifiers (MMC/LMC) do not apply to form tolerance ~A.~%~
                                   Material conditions only apply to tolerances of size (position, profile of surfaces with datums, etc.)."
                              gdt-type))))

  ;; Check 7: Zone types must be appropriate for tolerance type
  (when (and zone-type (form-tolerance-p gdt-type))
    (when (member zone-type '(:diametrical :cylindrical :spherical))
      (error 'gdt-validation-error
             :message (format nil "Form tolerance ~A does not use ~A tolerance zone type.~%~
                                   Form tolerances use planar tolerance zones."
                              gdt-type zone-type))))

  t)

(defun validate-datum-reference-frame (datum-refs)
  "Validate datum reference frame.

  Checks:
    1. No duplicate datums
    2. Valid datum reference structure

  Args:
    datum-refs - List of datum labels

  Signals:
    gdt-validation-error - If validation fails"
  (when datum-refs
    ;; Check for duplicates
    (let ((unique-datums (remove-duplicates datum-refs :test #'equal)))
      (when (/= (length unique-datums) (length datum-refs))
        (error 'gdt-validation-error
               :message (format nil "Duplicate datums in reference frame: ~A~%~
                                     Each datum should appear only once in a datum reference frame."
                                datum-refs)))))
  t)

(defun check-tolerance-conflicts (tolerances)
  "Check for conflicting tolerances on same features.

  Detects when multiple tolerances of the same type apply to the same feature,
  which may indicate an error in the specification.

  Args:
    tolerances - List of geometric-tolerance objects

  Signals:
    gdt-validation-error - If conflicts detected"
  (let ((tolerance-map (make-hash-table :test 'equal)))
    (loop for tol in tolerances
          for key = (list (tolerance-gdt-type tol)
                          (tolerance-feature-selector tol))
          do (when (gethash key tolerance-map)
               (error 'gdt-validation-error
                      :message (format nil "Conflicting tolerance: Multiple ~A tolerances on same feature~%~
                                            Selector: ~A~%~
                                            This may indicate a specification error. Review your GD&T callouts."
                                       (tolerance-gdt-type tol)
                                       (tolerance-feature-selector tol))))
             (setf (gethash key tolerance-map) tol)))
  t)

(defun validate-iso-fit (fit-class nominal-size)
  "Validate ISO fit specification.

  Args:
    fit-class - ISO fit class (e.g., 'H7', 'g6')
    nominal-size - Nominal dimension in mm

  Signals:
    clad.units:iso-fit-error - If fit class unknown or size out of range"
  ;; This delegates to the existing ISO fit lookup which already validates
  (clad.units:lookup-iso-fit fit-class nominal-size)
  t)
