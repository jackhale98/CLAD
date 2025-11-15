;;;; src/selectors/position.lisp --- Position-based selectors

(in-package :clad.selectors)

;;; ============================================================================
;;; Position Selector - Select by coordinate value
;;; ============================================================================

(defclass position-selector (base-selector)
  ((axis :initarg :axis
         :accessor position-axis
         :type keyword
         :documentation "Axis to check: :x, :y, or :z")
   (value :initarg :value
          :accessor position-value
          :type double-float
          :documentation "Coordinate value to match")
   (tolerance :initarg :tolerance
              :initform 0.01d0
              :accessor position-tolerance
              :type double-float
              :documentation "Matching tolerance in mm"))
  (:documentation "Selector for entities at specific coordinate value.

  Examples:
    (:at-x 50.0 :tolerance 0.1) => Entities centered at X=50±0.1
    (:at-y -25.0)               => Entities centered at Y=-25±0.01
    (:at-z 0.0 :tolerance 1.0)  => Entities centered near Z=0"))

;;; ============================================================================
;;; Implementation
;;; ============================================================================

(defmethod apply-selector ((selector position-selector) shape-list)
  "Select shapes whose center is at specified coordinate (within tolerance).

  Algorithm:
    1. Get bounding box for each shape
    2. Calculate center point of bounding box
    3. Extract coordinate for specified axis
    4. Check if within tolerance of target value
    5. Return matching shapes"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let ((axis (position-axis selector))
        (value (position-value selector))
        (tolerance (position-tolerance selector)))

    (remove-if-not
      (lambda (shape)
        (handler-case
            (let* ((bbox (clad.shapes:bounding-box shape))
                   ;; Calculate center of bounding box
                   (center-x (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0))
                   (center-y (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0))
                   (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0))
                   ;; Get coordinate for specified axis
                   (coord (case axis
                            (:x center-x)
                            (:y center-y)
                            (:z center-z)
                            (t (error "Invalid axis: ~A" axis)))))
              ;; Check if within tolerance
              (< (abs (- coord value)) tolerance))
          ;; If error getting bbox, exclude shape
          (error () nil)))
      shape-list)))

;;; ============================================================================
;;; Range Selector - Select by coordinate range
;;; ============================================================================

(defclass range-selector (base-selector)
  ((axis :initarg :axis
         :accessor range-axis
         :type keyword
         :documentation "Axis to check: :x, :y, or :z")
   (min-value :initarg :min
              :accessor range-min
              :type double-float
              :documentation "Minimum coordinate value")
   (max-value :initarg :max
              :accessor range-max
              :type double-float
              :documentation "Maximum coordinate value"))
  (:documentation "Selector for entities within coordinate range.

  Examples:
    (:between-z 20.0 40.0) => Entities with center Z between 20 and 40
    (:between-x -50.0 0.0) => Entities with center X between -50 and 0
    (:between-y 10.0 30.0) => Entities with center Y between 10 and 30"))

;;; ============================================================================
;;; Implementation
;;; ============================================================================

(defmethod apply-selector ((selector range-selector) shape-list)
  "Select shapes whose center is within specified coordinate range.

  Algorithm:
    1. Get bounding box for each shape
    2. Calculate center point of bounding box
    3. Extract coordinate for specified axis
    4. Check if within min/max range (inclusive)
    5. Return matching shapes"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let ((axis (range-axis selector))
        (min-val (range-min selector))
        (max-val (range-max selector)))

    ;; Auto-swap if min > max
    (when (> min-val max-val)
      (rotatef min-val max-val))

    (remove-if-not
      (lambda (shape)
        (handler-case
            (let* ((bbox (clad.shapes:bounding-box shape))
                   ;; Calculate center of bounding box
                   (center-x (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0))
                   (center-y (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0))
                   (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0))
                   ;; Get coordinate for specified axis
                   (coord (case axis
                            (:x center-x)
                            (:y center-y)
                            (:z center-z)
                            (t (error "Invalid axis: ~A" axis)))))
              ;; Check if within range (inclusive)
              (and (>= coord min-val)
                   (<= coord max-val)))
          ;; If error getting bbox, exclude shape
          (error () nil)))
      shape-list)))

;;; ============================================================================
;;; Bounding Box Selector - Select by 3D region
;;; ============================================================================

(defclass bbox-selector (base-selector)
  ((min-corner :initarg :min
               :accessor bbox-min
               :type list
               :documentation "Minimum corner (x y z) of bounding box")
   (max-corner :initarg :max
               :accessor bbox-max
               :type list
               :documentation "Maximum corner (x y z) of bounding box"))
  (:documentation "Selector for entities within 3D bounding box region.

  Examples:
    (:within-box '(0 0 0) '(50 50 50))     => Entities in positive octant
    (:within-box '(-10 -10 -10) '(10 10 10)) => Entities near origin
    (:within-box '(20 0 0) '(100 50 50))   => Entities in specific region"))

;;; ============================================================================
;;; Implementation
;;; ============================================================================

(defmethod apply-selector ((selector bbox-selector) shape-list)
  "Select shapes whose center is within specified 3D bounding box.

  Algorithm:
    1. Get bounding box for each shape
    2. Calculate center point of bounding box
    3. Check if center is within specified region (all 3 axes)
    4. Return matching shapes"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let* ((min-corner (bbox-min selector))
         (max-corner (bbox-max selector))
         (min-x (coerce (nth 0 min-corner) 'double-float))
         (min-y (coerce (nth 1 min-corner) 'double-float))
         (min-z (coerce (nth 2 min-corner) 'double-float))
         (max-x (coerce (nth 0 max-corner) 'double-float))
         (max-y (coerce (nth 1 max-corner) 'double-float))
         (max-z (coerce (nth 2 max-corner) 'double-float)))

    (remove-if-not
      (lambda (shape)
        (handler-case
            (let* ((bbox (clad.shapes:bounding-box shape))
                   ;; Calculate center of bounding box
                   (center-x (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0))
                   (center-y (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0))
                   (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
              ;; Check if center is within all three ranges
              (and (>= center-x min-x) (<= center-x max-x)
                   (>= center-y min-y) (<= center-y max-y)
                   (>= center-z min-z) (<= center-z max-z)))
          ;; If error getting bbox, exclude shape
          (error () nil)))
      shape-list)))

;;; ============================================================================
;;; Proximity Selector - Select by distance from point
;;; ============================================================================

(defclass proximity-selector (base-selector)
  ((point :initarg :point
          :accessor proximity-point
          :type list
          :documentation "3D point (x y z) as list")
   (radius :initarg :radius
           :accessor proximity-radius
           :type double-float
           :documentation "Maximum distance from point in mm"))
  (:documentation "Selector for entities within radius of a 3D point.

  Examples:
    (:near-point '(0 0 0) :radius 50)         => Entities within 50mm of origin
    (:near-point '(100 50 25) :radius 10)     => Entities within 10mm of point
    (:near-point '(-20 -20 0) :radius 30)     => Entities near specific location"))

;;; ============================================================================
;;; Implementation
;;; ============================================================================

(defmethod apply-selector ((selector proximity-selector) shape-list)
  "Select shapes whose center is within specified radius of point.

  Algorithm:
    1. Get bounding box for each shape
    2. Calculate center point of bounding box
    3. Calculate Euclidean distance from center to target point
    4. Check if distance <= radius
    5. Return matching shapes"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let* ((point (proximity-point selector))
         (radius (proximity-radius selector))
         (px (coerce (nth 0 point) 'double-float))
         (py (coerce (nth 1 point) 'double-float))
         (pz (coerce (nth 2 point) 'double-float)))

    (remove-if-not
      (lambda (shape)
        (handler-case
            (let* ((bbox (clad.shapes:bounding-box shape))
                   ;; Calculate center of bounding box
                   (center-x (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0))
                   (center-y (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0))
                   (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0))
                   ;; Calculate Euclidean distance
                   (dx (- center-x px))
                   (dy (- center-y py))
                   (dz (- center-z pz))
                   (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
              ;; Check if within radius
              (<= distance radius))
          ;; If error getting bbox, exclude shape
          (error () nil)))
      shape-list)))
