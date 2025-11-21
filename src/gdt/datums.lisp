;;;; src/gdt/datums.lisp --- Datum feature definitions (Phase T2)

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
                       :documentation "Material condition: MMC, LMC, or RFS (default)"))
  (:documentation "Datum feature definition per ASME Y14.5.

A datum feature is a physical feature of a part (like a face, hole, or boss)
that is used to establish a datum (a theoretically exact reference) for
measuring and tolerancing other features.

Material Conditions:
  :MMC - Maximum Material Condition (most material present)
  :LMC - Least Material Condition (least material present)
  :RFS - Regardless of Feature Size (default, not dependent on feature size)

Examples:
  - Datum A on bottom mounting face (primary datum)
  - Datum B on side face, perpendicular to A (secondary datum)
  - Datum C on another side face, perpendicular to A and B (tertiary datum)

The combination of datums A, B, and C forms a datum reference frame (DRF)
that fully constrains the part in 3D space for measurement."))

(defmethod print-object ((datum datum-feature) stream)
  (print-unreadable-object (datum stream :type t)
    (format stream "~A ~A"
            (datum-label datum)
            (case (datum-material-condition datum)
              (:mmc "(MMC)")
              (:lmc "(LMC)")
              (:rfs "")))))

;;; ============================================================================
;;; Datum Constructor
;;; ============================================================================

(defun make-datum (label selector &key (material-condition :rfs))
  "Create datum feature definition.

  Args:
    label - Datum label (A, B, C, etc.) - will be normalized to uppercase
    selector - Selector spec to identify feature (e.g., '(:on-face :direction :-z))
    material-condition - :mmc, :lmc, or :rfs (default)

  Returns: datum-feature instance

  Examples:
    (make-datum \"A\" '(:on-face :direction :-z :extreme :min))
    (make-datum \"B\" '(:on-face :direction :+x :extreme :max) :material-condition :mmc)

  Material Condition Meanings:
    :RFS (default) - Regardless of Feature Size
      The datum applies regardless of the actual size of the feature within
      its tolerance zone. Most common for planar features.

    :MMC - Maximum Material Condition
      The datum is established when the feature is at its maximum material
      size (smallest hole, largest pin). Provides bonus tolerance.

    :LMC - Least Material Condition
      The datum is established when the feature is at its minimum material
      size (largest hole, smallest pin). Rare, used for specific cases."

  ;; Validate material condition
  (unless (member material-condition '(:mmc :lmc :rfs))
    (error "Material condition must be :mmc, :lmc, or :rfs, got ~A"
           material-condition))

  (make-instance 'datum-feature
                 :label (string-upcase (string label))
                 :selector selector
                 :material-condition material-condition))

;;; ============================================================================
;;; Datum Query Functions
;;; ============================================================================

(defun find-datum (metadata label)
  "Find datum by label in shape metadata.

  Args:
    metadata - Shape metadata plist
    label - Datum label to find (case-insensitive)

  Returns: datum-feature instance or NIL if not found

  Example:
    (let ((meta (clad.core:shape-metadata shape)))
      (find-datum meta \"A\"))"
  (when metadata
    (let ((datums (getf metadata :datums))
          (search-label (string-upcase (string label))))
      (cdr (assoc search-label datums :test #'equal)))))

(defun list-datums (metadata)
  "List all datums defined in shape metadata.

  Args:
    metadata - Shape metadata plist

  Returns: List of datum-feature instances (in definition order, reversed)

  Example:
    (let ((meta (clad.core:shape-metadata shape)))
      (list-datums meta))
    => (#<DATUM-FEATURE A> #<DATUM-FEATURE B> #<DATUM-FEATURE C>)"
  (when metadata
    (let ((datums (getf metadata :datums)))
      (mapcar #'cdr datums))))

;;; ============================================================================
;;; Material Condition Utilities
;;; ============================================================================

(defun material-condition-p (symbol)
  "Check if symbol is a valid material condition modifier.

  Args:
    symbol - Symbol to check

  Returns: T if symbol is :mmc, :lmc, or :rfs

  Example:
    (material-condition-p :mmc) => T
    (material-condition-p :xyz) => NIL"
  (member symbol '(:mmc :lmc :rfs)))

;;; ============================================================================
;;; Datum Metadata Utilities
;;; ============================================================================

(defun add-datum-to-metadata (metadata label selector &key (material-condition :rfs))
  "Add a datum to shape metadata.

  This is a low-level function used by the DSL. Users should typically
  use the (:datum ...) form in defpart instead.

  Args:
    metadata - Existing metadata plist (may be NIL)
    label - Datum label
    selector - Selector specification
    material-condition - Material condition modifier

  Returns: Updated metadata plist (may be a new list if metadata was NIL)

  Example:
    (setf meta (add-datum-to-metadata meta \"A\"
                                      '(:on-face :direction :-z :extreme :min)))"
  (let* ((datum (make-datum label selector :material-condition material-condition))
         (datums (getf metadata :datums))
         (new-datums (cons (cons (string-upcase (string label)) datum) datums)))
    ;; If metadata is NIL, create a new plist
    ;; Otherwise, update the existing one
    (if (null metadata)
        (list :datums new-datums)
        (progn
          (setf (getf metadata :datums) new-datums)
          metadata))))

;;; ============================================================================
;;; Datum Reference Frame (DRF) Utilities
;;; ============================================================================

(defun datum-reference-frame-p (metadata)
  "Check if metadata contains a complete datum reference frame (A-B-C).

  A complete DRF typically has three datums:
    - Primary datum (A) - constrains 3 DOF (usually a plane)
    - Secondary datum (B) - constrains 2 DOF (perpendicular to primary)
    - Tertiary datum (C) - constrains 1 DOF (perpendicular to primary & secondary)

  Args:
    metadata - Shape metadata plist

  Returns: T if metadata has datums A, B, and C

  Note: This is a simple check. It doesn't validate that the datums are
        actually perpendicular or form a valid reference frame."
  (when metadata
    (let ((datums (getf metadata :datums)))
      (and (assoc "A" datums :test #'equal)
           (assoc "B" datums :test #'equal)
           (assoc "C" datums :test #'equal)))))

(defun primary-datum (metadata)
  "Get primary datum (typically 'A') from metadata.

  Args:
    metadata - Shape metadata plist

  Returns: datum-feature instance or NIL"
  (find-datum metadata "A"))

(defun secondary-datum (metadata)
  "Get secondary datum (typically 'B') from metadata.

  Args:
    metadata - Shape metadata plist

  Returns: datum-feature instance or NIL"
  (find-datum metadata "B"))

(defun tertiary-datum (metadata)
  "Get tertiary datum (typically 'C') from metadata.

  Args:
    metadata - Shape metadata plist

  Returns: datum-feature instance or NIL"
  (find-datum metadata "C"))
