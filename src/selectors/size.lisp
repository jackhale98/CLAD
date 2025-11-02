;;;; src/selectors/size.lisp --- Size-based selectors (Phase 8)

(in-package :clad.selectors)

;;; ============================================================================
;;; Size Selector (Phase 8 Enhancement)
;;; ============================================================================

(defclass size-selector (base-selector)
  ((property :initarg :property
             :accessor selector-property
             :type keyword
             :documentation "Property to measure: :area, :length, :volume, :radius")
   (comparator :initarg :comparator
               :accessor selector-comparator
               :type keyword
               :documentation "Comparison operator: :>, :<, :=, :between")
   (value1 :initarg :value1
           :accessor selector-value1
           :type number
           :documentation "Primary comparison value")
   (value2 :initarg :value2
           :initform nil
           :accessor selector-value2
           :type (or null number)
           :documentation "Secondary value for :between comparator"))
  (:documentation "Selector for filtering shapes by size/dimension properties.

  This selector measures geometric properties (area, length, volume, radius)
  and filters shapes based on comparison operators.

  Properties:
    :area      - Surface area of faces (mm²)
    :length    - Length of edges (mm)
    :volume    - Volume of solids (mm³)
    :radius    - Radius of circular edges/faces (mm)

  Comparators:
    :>         - Greater than value1
    :<         - Less than value1
    :=         - Approximately equal to value1 (within tolerance)
    :between   - Between value1 and value2 (inclusive)

  Examples:
    ;; Select faces with area > 1000 mm²
    (make-instance 'size-selector :property :area :comparator :> :value1 1000)

    ;; Select edges with length between 50 and 100 mm
    (make-instance 'size-selector
                   :property :length
                   :comparator :between
                   :value1 50
                   :value2 100)

    ;; Select edges approximately 100mm long (within 0.1mm)
    (make-instance 'size-selector
                   :property :length
                   :comparator :=
                   :value1 100
                   :tolerance 0.1)"))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun get-shape-property (shape property)
  "Get the specified property value from a shape.

  Args:
    shape - CLOS shape object (cad-edge, cad-face, cad-solid, etc.)
    property - Property keyword (:area, :length, :volume, :radius)

  Returns: Numeric value of the property

  Signals: error if property not applicable to shape type"

  (ecase property
    (:area
     (clad.shapes:area shape))

    (:length
     (clad.shapes:shape-length shape))

    (:volume
     (clad.shapes:volume shape))

    (:radius
     ;; For circular edges/faces, we need to compute radius
     ;; This is a simplified implementation
     ;; A full implementation would query OCCT for actual radius
     (let ((geom-type (clad.shapes:geom-type shape)))
       (cond
         ((eq geom-type :circle)
          ;; For circular edges, radius = length / (2 * pi)
          (/ (clad.shapes:shape-length shape) (* 2 pi)))

         ((eq geom-type :cylinder)
          ;; For cylindrical faces, approximate from area and height
          ;; This is a stub - real implementation would use OCCT
          10.0d0)  ; Placeholder

         (t
          0.0d0))))))  ; Non-circular shapes have no meaningful radius

(defun compare-values (actual comparator value1 &optional value2 tolerance)
  "Compare actual value with comparison criteria.

  Args:
    actual - Measured value from shape
    comparator - Comparison keyword (:>, :<, :=, :between)
    value1 - Primary comparison value
    value2 - Secondary value for :between (optional)
    tolerance - Tolerance for :=  comparator (optional, default 1e-6)

  Returns: t if comparison passes, nil otherwise"

  (let ((tol (or tolerance 1e-6)))
    (ecase comparator
      (:>
       (> actual value1))

      (:<
       (< actual value1))

      (:=
       ;; Approximately equal within tolerance
       (<= (abs (- actual value1)) tol))

      (:between
       (unless value2
         (error "Size selector with :between comparator requires :value2"))
       ;; Inclusive range
       (and (>= actual value1)
            (<= actual value2))))))

;;; ============================================================================
;;; Implementation
;;; ============================================================================

(defmethod apply-selector ((selector size-selector) shape-list)
  "Select shapes matching the size criteria.

  Algorithm:
    1. Get property, comparator, and values from selector
    2. For each shape:
       a. Extract the property value (area, length, etc.)
       b. Compare using the specified comparator
       c. Include if comparison passes
    3. Return filtered list

  Gracefully handles shapes where property is not applicable
  (e.g., asking for volume of an edge returns 0)."

  (when (null shape-list)
    (return-from apply-selector nil))

  (let ((prop (selector-property selector))
        (comp (selector-comparator selector))
        (val1 (selector-value1 selector))
        (val2 (selector-value2 selector))
        (tol (selector-tolerance selector)))

    ;; Filter shapes by size criteria
    (remove-if-not
     (lambda (shape)
       (handler-case
           (let ((actual (get-shape-property shape prop)))
             (compare-values actual comp val1 val2 tol))
         ;; If we can't measure the property, exclude this shape
         (error () nil)))
     shape-list)))
