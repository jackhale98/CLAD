;;;; src/gdt/geometric-tolerances.lisp --- Geometric tolerance definitions (Phase T3)

(in-package :clad.gdt)

;;; ============================================================================
;;; Geometric Tolerance Base Classes
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
  (:documentation "Base class for geometric tolerances per ASME Y14.5"))

(defmethod print-object ((tol geometric-tolerance) stream)
  (print-unreadable-object (tol stream :type t)
    (format stream "~A ~,3Fmm"
            (tolerance-gdt-type tol)
            (tolerance-zone-value tol))))

;;; ============================================================================
;;; Form Tolerances
;;; ============================================================================

(defclass form-tolerance (geometric-tolerance)
  ()
  (:documentation "Form tolerance: flatness, straightness, circularity, cylindricity.

Form tolerances control the shape of individual features without reference
to datums. They include:
- Flatness: Surface must lie within two parallel planes
- Straightness: Line element must lie within tolerance zone
- Circularity: Cross-section must lie within two concentric circles
- Cylindricity: Cylindrical surface must lie within two coaxial cylinders"))

;;; ============================================================================
;;; Orientation Tolerances
;;; ============================================================================

(defclass orientation-tolerance (geometric-tolerance)
  ((datum-refs :initarg :datum-refs
               :accessor tolerance-datum-refs
               :type list
               :documentation "List of datum references (e.g., '(\"A\" \"B\"))")
   (material-conditions :initarg :material-conditions
                        :initform '()
                        :accessor tolerance-material-conditions
                        :type list
                        :documentation "Material condition modifiers for datum refs")
   (angle :initarg :angle
          :initform nil
          :accessor tolerance-angle
          :documentation "Nominal angle for angularity (in degrees)"))
  (:documentation "Orientation tolerance: perpendicularity, parallelism, angularity.

Orientation tolerances control the orientation of a feature relative to one
or more datum references. They include:
- Perpendicularity: Feature must be 90° to datum
- Parallelism: Feature must be parallel to datum
- Angularity: Feature must be at specific angle to datum"))

(defmethod print-object ((tol orientation-tolerance) stream)
  (print-unreadable-object (tol stream :type t)
    (format stream "~A ~,3Fmm |~{~A~^-~}|"
            (tolerance-gdt-type tol)
            (tolerance-zone-value tol)
            (tolerance-datum-refs tol))))

;;; ============================================================================
;;; Location Tolerances
;;; ============================================================================

(defclass location-tolerance (geometric-tolerance)
  ((datum-refs :initarg :datum-refs
               :accessor tolerance-datum-refs
               :type list
               :documentation "List of datum references")
   (material-condition :initarg :material-condition
                       :initform :rfs
                       :accessor tolerance-material-condition
                       :type (member :mmc :lmc :rfs)
                       :documentation "Material condition for feature itself")
   (material-conditions :initarg :material-conditions
                        :initform '()
                        :accessor tolerance-material-conditions
                        :type list
                        :documentation "Material conditions for datum refs"))
  (:documentation "Location tolerance: position, concentricity, symmetry.

Location tolerances control the location of a feature relative to datums or
other features. They include:
- Position: Feature center/axis within tolerance zone from true position
- Concentricity: Median points of feature lie within cylinder about datum axis
- Symmetry: Median points of feature lie within tolerance zone about datum plane"))

(defmethod print-object ((tol location-tolerance) stream)
  (print-unreadable-object (tol stream :type t)
    (format stream "~A ~,3Fmm~A |~{~A~^-~}|"
            (tolerance-gdt-type tol)
            (tolerance-zone-value tol)
            (case (tolerance-material-condition tol)
              (:mmc " (M)")
              (:lmc " (L)")
              (t ""))
            (tolerance-datum-refs tol))))

;;; ============================================================================
;;; Profile Tolerances
;;; ============================================================================

(defclass profile-tolerance (geometric-tolerance)
  ((datum-refs :initarg :datum-refs
               :initform '()
               :accessor tolerance-datum-refs
               :type list
               :documentation "List of datum references (optional for profile)")
   (bilateral :initarg :bilateral
              :initform t
              :accessor tolerance-bilateral-p
              :type boolean
              :documentation "T for bilateral zone, NIL for unilateral"))
  (:documentation "Profile tolerance: surface profile, line profile.

Profile tolerances control the overall shape of complex surfaces. They include:
- Profile of a Surface: 3D tolerance zone around nominal surface
- Profile of a Line: 2D tolerance zone in cutting plane"))

;;; ============================================================================
;;; Runout Tolerances
;;; ============================================================================

(defclass runout-tolerance (geometric-tolerance)
  ((datum-refs :initarg :datum-refs
               :accessor tolerance-datum-refs
               :type list
               :documentation "List of datum references (axis)"))
  (:documentation "Runout tolerance: circular runout, total runout.

Runout tolerances control surface variation during rotation about a datum axis:
- Circular Runout: Variation at individual circular elements
- Total Runout: Composite surface variation over entire surface"))

;;; ============================================================================
;;; Form Tolerance Constructors
;;; ============================================================================

(defun make-flatness-tolerance (feature-selector tolerance-zone)
  "Create flatness tolerance.

  Flatness: Surface must lie within tolerance zone defined by two parallel
  planes separated by the tolerance value.

  Args:
    feature-selector - Selector specification for the surface
    tolerance-zone - Tolerance zone width in mm

  Returns: form-tolerance instance

  Example:
    (make-flatness-tolerance
      '(:on-face :direction :+z :extreme :max)
      0.05)  ; Surface must be flat within 0.05mm"
  (make-instance 'form-tolerance
                 :type :flatness
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector))

(defun make-straightness-tolerance (feature-selector tolerance-zone)
  "Create straightness tolerance.

  Straightness: Line element must lie within tolerance zone (cylinder for
  axis, parallel planes for surface line element).

  Args:
    feature-selector - Selector specification for the line/axis
    tolerance-zone - Tolerance zone width in mm

  Returns: form-tolerance instance"
  (make-instance 'form-tolerance
                 :type :straightness
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector))

(defun make-circularity-tolerance (feature-selector tolerance-zone)
  "Create circularity (roundness) tolerance.

  Circularity: Each circular cross-section must lie within two concentric
  circles separated by the tolerance value.

  Args:
    feature-selector - Selector specification for the surface
    tolerance-zone - Radial tolerance zone width in mm

  Returns: form-tolerance instance"
  (make-instance 'form-tolerance
                 :type :circularity
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector))

(defun make-cylindricity-tolerance (feature-selector tolerance-zone)
  "Create cylindricity tolerance.

  Cylindricity: Cylindrical surface must lie within two coaxial cylinders
  separated by the tolerance value. This is a composite control combining
  circularity, straightness, and parallelism.

  Args:
    feature-selector - Selector specification for cylindrical surface
    tolerance-zone - Radial tolerance zone width in mm

  Returns: form-tolerance instance"
  (make-instance 'form-tolerance
                 :type :cylindricity
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector))

;;; ============================================================================
;;; Orientation Tolerance Constructors
;;; ============================================================================

(defun make-perpendicularity-tolerance (feature-selector tolerance-zone datum-ref
                                        &key (material-condition :rfs))
  "Create perpendicularity tolerance.

  Perpendicularity: Feature must be 90° to the datum within the tolerance zone.

  Args:
    feature-selector - Selector specification for controlled feature
    tolerance-zone - Tolerance zone width in mm
    datum-ref - Datum label (e.g., \"A\")
    material-condition - Material condition for datum (:mmc, :lmc, :rfs)

  Returns: orientation-tolerance instance

  Example:
    (make-perpendicularity-tolerance
      '(:on-face :direction :+x :extreme :max)
      0.1
      \"A\")  ; Face must be perpendicular to datum A within 0.1mm"
  (make-instance 'orientation-tolerance
                 :type :perpendicularity
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (list datum-ref)
                 :material-conditions (list material-condition)))

(defun make-parallelism-tolerance (feature-selector tolerance-zone datum-ref
                                   &key (material-condition :rfs))
  "Create parallelism tolerance.

  Parallelism: Feature must be parallel to the datum within the tolerance zone.

  Args:
    feature-selector - Selector specification for controlled feature
    tolerance-zone - Tolerance zone width in mm
    datum-ref - Datum label (e.g., \"A\")
    material-condition - Material condition for datum

  Returns: orientation-tolerance instance"
  (make-instance 'orientation-tolerance
                 :type :parallelism
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (list datum-ref)
                 :material-conditions (list material-condition)))

(defun make-angularity-tolerance (feature-selector tolerance-zone datum-ref angle
                                  &key (material-condition :rfs))
  "Create angularity tolerance.

  Angularity: Feature must be at the specified angle to the datum within
  the tolerance zone.

  Args:
    feature-selector - Selector specification for controlled feature
    tolerance-zone - Tolerance zone width in mm
    datum-ref - Datum label (e.g., \"A\")
    angle - Nominal angle in degrees (basic dimension)
    material-condition - Material condition for datum

  Returns: orientation-tolerance instance"
  (make-instance 'orientation-tolerance
                 :type :angularity
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (list datum-ref)
                 :material-conditions (list material-condition)
                 :angle (coerce angle 'double-float)))

;;; ============================================================================
;;; Location Tolerance Constructors
;;; ============================================================================

(defun make-position-tolerance (feature-selector tolerance-zone datum-refs
                                &key (material-condition :rfs)
                                     (datum-material-conditions '()))
  "Create position tolerance.

  Position: Feature center, axis, or surface must be within tolerance zone
  from theoretically exact dimensions (basic dimensions).

  Args:
    feature-selector - Selector specification for controlled feature
    tolerance-zone - Diameter/width of tolerance zone in mm
    datum-refs - List of datum labels (e.g., '(\"A\" \"B\" \"C\"))
    material-condition - Material condition for feature (:mmc, :lmc, :rfs)
    datum-material-conditions - List of material conditions for datums

  Returns: location-tolerance instance

  Example:
    (make-position-tolerance
      '(:on-face :type :cylindrical)
      0.2
      '(\"A\" \"B\" \"C\")
      :material-condition :mmc)  ; Position at MMC provides bonus tolerance"
  (make-instance 'location-tolerance
                 :type :position
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs datum-refs
                 :material-condition material-condition
                 :material-conditions (or datum-material-conditions
                                         (make-list (length datum-refs)
                                                   :initial-element :rfs))))

(defun make-concentricity-tolerance (feature-selector tolerance-zone datum-ref
                                     &key (material-condition :rfs))
  "Create concentricity tolerance.

  Concentricity: All median points of diametrically opposed elements must
  lie within a cylindrical tolerance zone whose axis coincides with the datum axis.

  Args:
    feature-selector - Selector specification for controlled feature
    tolerance-zone - Diameter of tolerance zone in mm
    datum-ref - Datum label (axis)
    material-condition - Material condition for datum

  Returns: location-tolerance instance"
  (make-instance 'location-tolerance
                 :type :concentricity
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (list datum-ref)
                 :material-condition material-condition
                 :material-conditions (list :rfs)))

(defun make-symmetry-tolerance (feature-selector tolerance-zone datum-ref
                                &key (material-condition :rfs))
  "Create symmetry tolerance.

  Symmetry: All median points of opposed elements must lie within tolerance
  zone centered on datum plane.

  Args:
    feature-selector - Selector specification for controlled feature
    tolerance-zone - Width of tolerance zone in mm
    datum-ref - Datum label (plane)
    material-condition - Material condition for datum

  Returns: location-tolerance instance"
  (make-instance 'location-tolerance
                 :type :symmetry
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (list datum-ref)
                 :material-condition material-condition
                 :material-conditions (list :rfs)))

;;; ============================================================================
;;; Profile Tolerance Constructors
;;; ============================================================================

(defun make-profile-surface-tolerance (feature-selector tolerance-zone
                                       &key datum-refs (bilateral t))
  "Create profile of a surface tolerance.

  Profile of a Surface: 3D tolerance zone normal to the true profile surface.
  Can be used with or without datums.

  Args:
    feature-selector - Selector specification for surface
    tolerance-zone - Total width of tolerance zone in mm
    datum-refs - Optional list of datum labels
    bilateral - T for bilateral zone (default), NIL for unilateral

  Returns: profile-tolerance instance"
  (make-instance 'profile-tolerance
                 :type :profile-surface
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (or datum-refs '())
                 :bilateral bilateral))

(defun make-profile-line-tolerance (feature-selector tolerance-zone
                                    &key datum-refs (bilateral t))
  "Create profile of a line tolerance.

  Profile of a Line: 2D tolerance zone in cutting plane normal to true profile.

  Args:
    feature-selector - Selector specification for line/edge
    tolerance-zone - Total width of tolerance zone in mm
    datum-refs - Optional list of datum labels
    bilateral - T for bilateral zone (default), NIL for unilateral

  Returns: profile-tolerance instance"
  (make-instance 'profile-tolerance
                 :type :profile-line
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (or datum-refs '())
                 :bilateral bilateral))

;;; ============================================================================
;;; Runout Tolerance Constructors
;;; ============================================================================

(defun make-circular-runout-tolerance (feature-selector tolerance-zone datum-ref)
  "Create circular runout tolerance.

  Circular Runout: Full indicator movement (FIM) at any circular measuring
  position as the part is rotated 360° about datum axis.

  Args:
    feature-selector - Selector specification for surface
    tolerance-zone - FIM tolerance value in mm
    datum-ref - Datum label (axis)

  Returns: runout-tolerance instance"
  (make-instance 'runout-tolerance
                 :type :circular-runout
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (list datum-ref)))

(defun make-total-runout-tolerance (feature-selector tolerance-zone datum-ref)
  "Create total runout tolerance.

  Total Runout: Composite surface variation as the part is rotated 360°
  about datum axis with indicator traversing the surface.

  Args:
    feature-selector - Selector specification for surface
    tolerance-zone - FIM tolerance value in mm
    datum-ref - Datum label (axis)

  Returns: runout-tolerance instance"
  (make-instance 'runout-tolerance
                 :type :total-runout
                 :tolerance-zone (coerce tolerance-zone 'double-float)
                 :feature-selector feature-selector
                 :datum-refs (list datum-ref)))

;;; ============================================================================
;;; GD&T Metadata Utilities
;;; ============================================================================

(defun add-geometric-tolerance-to-metadata (metadata tolerance)
  "Add a geometric tolerance to shape metadata.

  Args:
    metadata - Existing metadata plist (may be NIL)
    tolerance - geometric-tolerance instance

  Returns: Updated metadata plist"
  (let ((gdt-list (getf metadata :geometric-tolerances)))
    (if (null metadata)
        (list :geometric-tolerances (list tolerance))
        (progn
          (setf (getf metadata :geometric-tolerances)
                (cons tolerance gdt-list))
          metadata))))

(defun find-geometric-tolerances (metadata type)
  "Find all geometric tolerances of a specific type.

  Args:
    metadata - Shape metadata plist
    type - GD&T type keyword (e.g., :flatness, :position)

  Returns: List of geometric-tolerance instances of the specified type"
  (when metadata
    (let ((gdt-list (getf metadata :geometric-tolerances)))
      (remove-if-not (lambda (tol)
                       (eq type (tolerance-gdt-type tol)))
                     gdt-list))))

(defun list-geometric-tolerances (metadata)
  "List all geometric tolerances in shape metadata.

  Args:
    metadata - Shape metadata plist

  Returns: List of all geometric-tolerance instances"
  (when metadata
    (getf metadata :geometric-tolerances)))
