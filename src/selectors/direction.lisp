;;;; src/selectors/direction.lisp --- Direction-based selectors

(in-package :clad.selectors)

;;; ============================================================================
;;; Direction Selector
;;; ============================================================================

(defclass direction-selector (base-selector)
  ((axis :initarg :axis
         :accessor selector-axis
         :type keyword
         :documentation "Axis keyword: :+x, :+y, :+z, :-x, :-y, :-z")
   (extreme :initarg :extreme
            :accessor selector-extreme
            :type (member :max :min)
            :documentation ":max for maximum projection, :min for minimum"))
  (:documentation "Selector for finding faces at extremes in a given direction.

  Examples:
    (:axis :+z :extreme :max) => Top face
    (:axis :+x :extreme :min) => Leftmost face"))

;;; ============================================================================
;;; Implementation
;;; ============================================================================

(defmethod apply-selector ((selector direction-selector) shape-list)
  "Select shapes at extreme positions along an axis.

  Algorithm:
    1. Convert axis keyword to unit vector
    2. Project center-of-mass of each shape onto axis
    3. Find maximum or minimum projection
    4. Return all shapes within tolerance of that extreme"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let* ((axis-kw (selector-axis selector))
         (tolerance (selector-tolerance selector))
         (extreme-type (selector-extreme selector)))

    ;; Get axis vector
    (multiple-value-bind (ax ay az)
        (axis-keyword-to-vector axis-kw)

      ;; Compute projections for all shapes
      ;; Use bounding box center as a proxy for center-of-mass (works for all shape types)
      (let ((projections
              (mapcar (lambda (shape)
                        (let* ((bbox (clad.shapes:bounding-box shape))
                               ;; Compute center of bounding box
                               (cx (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0))
                               (cy (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0))
                               (cz (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
                          (cons shape
                                (+ (* cx ax)
                                   (* cy ay)
                                   (* cz az)))))
                      shape-list)))

        ;; Find extreme value
        (let ((extreme-value
                (if (eq extreme-type :max)
                    (reduce #'max projections :key #'cdr)
                    (reduce #'min projections :key #'cdr))))

          ;; Filter shapes within tolerance of extreme
          (mapcar #'car
                  (remove-if-not
                   (lambda (shape-proj)
                     (< (abs (- (cdr shape-proj) extreme-value))
                        tolerance))
                   projections)))))))
