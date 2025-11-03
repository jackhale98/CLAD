;;;; constraints.lisp --- Constraint types for 2D sketches

(in-package #:clad.sketch.constraints)

;;; ==============================================================================
;;; Base Constraint Class
;;; ==============================================================================

(defclass constraint ()
  ((entities
    :initarg :entities
    :accessor constraint-entities
    :documentation "List of entities this constraint applies to")
   (type
    :initarg :type
    :accessor constraint-type
    :documentation "Type of constraint")
   (parameters
    :initarg :parameters
    :initform nil
    :accessor constraint-parameters
    :documentation "Constraint-specific parameters")
   (weight
    :initarg :weight
    :initform 1.0d0
    :accessor constraint-weight
    :documentation "Weight of this constraint in solver"))
  (:documentation "Base class for all geometric constraints"))

;;; Generic functions for constraint protocol
(defgeneric constraint-error (constraint)
  (:documentation "Compute error measure for constraint (0 = satisfied)"))

(defgeneric constraint-jacobian (constraint)
  (:documentation "Compute gradient of error with respect to parameters"))

(defgeneric apply-constraint (constraint)
  (:documentation "Apply constraint by modifying entity parameters"))

;;; ==============================================================================
;;; Fixed Constraint - Fix point at specific location
;;; ==============================================================================

(defclass fixed-constraint (constraint)
  ((target-x
    :initarg :target-x
    :accessor constraint-target-x
    :documentation "Target X coordinate")
   (target-y
    :initarg :target-y
    :accessor constraint-target-y
    :documentation "Target Y coordinate"))
  (:documentation "Fixes a point at a specific location"))

(defun make-fixed-constraint (point target-x target-y)
  "Create a fixed constraint to hold POINT at (TARGET-X, TARGET-Y)."
  (make-instance 'fixed-constraint
                 :entities (list point)
                 :type :fixed
                 :target-x (coerce target-x 'double-float)
                 :target-y (coerce target-y 'double-float)))

(defmethod constraint-error ((c fixed-constraint))
  "Error = squared distance from target position"
  (let* ((point (first (constraint-entities c)))
         (x (clad.sketch:point-x point))
         (y (clad.sketch:point-y point))
         (tx (constraint-target-x c))
         (ty (constraint-target-y c))
         (dx (- x tx))
         (dy (- y ty)))
    (+ (* dx dx) (* dy dy))))

(defmethod constraint-jacobian ((c fixed-constraint))
  "Gradient: [2*(x-tx), 2*(y-ty)]"
  (let* ((point (first (constraint-entities c)))
         (x (clad.sketch:point-x point))
         (y (clad.sketch:point-y point))
         (tx (constraint-target-x c))
         (ty (constraint-target-y c)))
    (list (* 2.0d0 (- x tx))
          (* 2.0d0 (- y ty)))))

;;; ==============================================================================
;;; Coincident Constraint - Two points at same location
;;; ==============================================================================

(defclass coincident-constraint (constraint)
  ()
  (:documentation "Makes two points coincident"))

(defun make-coincident-constraint (point1 point2)
  "Create a coincident constraint between POINT1 and POINT2."
  (make-instance 'coincident-constraint
                 :entities (list point1 point2)
                 :type :coincident))

(defmethod constraint-error ((c coincident-constraint))
  "Error = squared distance between points"
  (let* ((p1 (first (constraint-entities c)))
         (p2 (second (constraint-entities c)))
         (dx (- (clad.sketch:point-x p1) (clad.sketch:point-x p2)))
         (dy (- (clad.sketch:point-y p1) (clad.sketch:point-y p2))))
    (+ (* dx dx) (* dy dy))))

(defmethod constraint-jacobian ((c coincident-constraint))
  "Gradient with respect to both points"
  (let* ((p1 (first (constraint-entities c)))
         (p2 (second (constraint-entities c)))
         (dx (- (clad.sketch:point-x p1) (clad.sketch:point-x p2)))
         (dy (- (clad.sketch:point-y p1) (clad.sketch:point-y p2))))
    (list (* 2.0d0 dx)     ; ∂E/∂x1
          (* 2.0d0 dy)     ; ∂E/∂y1
          (* -2.0d0 dx)    ; ∂E/∂x2
          (* -2.0d0 dy)))) ; ∂E/∂y2

;;; ==============================================================================
;;; Distance Constraint - Maintain distance between two points
;;; ==============================================================================

(defclass distance-constraint (constraint)
  ((target-distance
    :initarg :target-distance
    :accessor constraint-target-distance
    :documentation "Target distance between points"))
  (:documentation "Maintains a specific distance between two points"))

(defun make-distance-constraint (point1 point2 distance)
  "Create a distance constraint between POINT1 and POINT2 with target DISTANCE."
  (make-instance 'distance-constraint
                 :entities (list point1 point2)
                 :type :distance
                 :target-distance (coerce distance 'double-float)))

(defmethod constraint-error ((c distance-constraint))
  "Error = squared difference between actual and target distance"
  (let* ((p1 (first (constraint-entities c)))
         (p2 (second (constraint-entities c)))
         (dx (- (clad.sketch:point-x p2) (clad.sketch:point-x p1)))
         (dy (- (clad.sketch:point-y p2) (clad.sketch:point-y p1)))
         (actual-dist (sqrt (+ (* dx dx) (* dy dy))))
         (target-dist (constraint-target-distance c))
         (error-dist (- actual-dist target-dist)))
    (* error-dist error-dist)))

(defmethod constraint-jacobian ((c distance-constraint))
  "Gradient of squared distance error"
  (let* ((p1 (first (constraint-entities c)))
         (p2 (second (constraint-entities c)))
         (dx (- (clad.sketch:point-x p2) (clad.sketch:point-x p1)))
         (dy (- (clad.sketch:point-y p2) (clad.sketch:point-y p1)))
         (dist (sqrt (+ (* dx dx) (* dy dy))))
         (target (constraint-target-distance c))
         (error (- dist target)))
    (if (< dist 1.0d-10)
        ;; Avoid division by zero
        (list 0.0d0 0.0d0 0.0d0 0.0d0)
        (let ((factor (/ (* 2.0d0 error) dist)))
          (list (* factor (- dx))  ; ∂E/∂x1
                (* factor (- dy))  ; ∂E/∂y1
                (* factor dx)      ; ∂E/∂x2
                (* factor dy)))))) ; ∂E/∂y2

;;; ==============================================================================
;;; Horizontal Constraint - Line must be horizontal
;;; ==============================================================================

(defclass horizontal-constraint (constraint)
  ()
  (:documentation "Constrains a line to be horizontal"))

(defun make-horizontal-constraint (line)
  "Create a horizontal constraint on LINE."
  (make-instance 'horizontal-constraint
                 :entities (list line)
                 :type :horizontal))

(defmethod constraint-error ((c horizontal-constraint))
  "Error = squared Y-difference between line endpoints"
  (let* ((line (first (constraint-entities c)))
         (start (clad.sketch:line-start line))
         (end (clad.sketch:line-end line))
         (dy (- (clad.sketch:point-y end) (clad.sketch:point-y start))))
    (* dy dy)))

(defmethod constraint-jacobian ((c horizontal-constraint))
  "Gradient for horizontal constraint"
  (let* ((line (first (constraint-entities c)))
         (start (clad.sketch:line-start line))
         (end (clad.sketch:line-end line))
         (dy (- (clad.sketch:point-y end) (clad.sketch:point-y start))))
    (list 0.0d0              ; ∂E/∂x_start (no effect on error)
          (* -2.0d0 dy)      ; ∂E/∂y_start
          0.0d0              ; ∂E/∂x_end (no effect on error)
          (* 2.0d0 dy))))    ; ∂E/∂y_end

;;; ==============================================================================
;;; Vertical Constraint - Line must be vertical
;;; ==============================================================================

(defclass vertical-constraint (constraint)
  ()
  (:documentation "Constrains a line to be vertical"))

(defun make-vertical-constraint (line)
  "Create a vertical constraint on LINE."
  (make-instance 'vertical-constraint
                 :entities (list line)
                 :type :vertical))

(defmethod constraint-error ((c vertical-constraint))
  "Error = squared X-difference between line endpoints"
  (let* ((line (first (constraint-entities c)))
         (start (clad.sketch:line-start line))
         (end (clad.sketch:line-end line))
         (dx (- (clad.sketch:point-x end) (clad.sketch:point-x start))))
    (* dx dx)))

(defmethod constraint-jacobian ((c vertical-constraint))
  "Gradient for vertical constraint"
  (let* ((line (first (constraint-entities c)))
         (start (clad.sketch:line-start line))
         (end (clad.sketch:line-end line))
         (dx (- (clad.sketch:point-x end) (clad.sketch:point-x start))))
    (list (* -2.0d0 dx)      ; ∂E/∂x_start
          0.0d0              ; ∂E/∂y_start (no effect on error)
          (* 2.0d0 dx)       ; ∂E/∂x_end
          0.0d0)))           ; ∂E/∂y_end (no effect on error)

;;; ==============================================================================
;;; Parallel Constraint - Two lines must be parallel
;;; ==============================================================================

(defclass parallel-constraint (constraint)
  ()
  (:documentation "Constrains two lines to be parallel"))

(defun make-parallel-constraint (line1 line2)
  "Create a parallel constraint between LINE1 and LINE2."
  (make-instance 'parallel-constraint
                 :entities (list line1 line2)
                 :type :parallel))

(defmethod constraint-error ((c parallel-constraint))
  "Error = squared cross product of direction vectors (0 when parallel)"
  (let* ((line1 (first (constraint-entities c)))
         (line2 (second (constraint-entities c)))
         (start1 (clad.sketch:line-start line1))
         (end1 (clad.sketch:line-end line1))
         (start2 (clad.sketch:line-start line2))
         (end2 (clad.sketch:line-end line2))
         ;; Direction vectors
         (dx1 (- (clad.sketch:point-x end1) (clad.sketch:point-x start1)))
         (dy1 (- (clad.sketch:point-y end1) (clad.sketch:point-y start1)))
         (dx2 (- (clad.sketch:point-x end2) (clad.sketch:point-x start2)))
         (dy2 (- (clad.sketch:point-y end2) (clad.sketch:point-y start2)))
         ;; Cross product in 2D: dx1*dy2 - dy1*dx2
         (cross (- (* dx1 dy2) (* dy1 dx2))))
    (* cross cross)))

(defmethod constraint-jacobian ((c parallel-constraint))
  "Gradient for parallel constraint"
  (let* ((line1 (first (constraint-entities c)))
         (line2 (second (constraint-entities c)))
         (start1 (clad.sketch:line-start line1))
         (end1 (clad.sketch:line-end line1))
         (start2 (clad.sketch:line-start line2))
         (end2 (clad.sketch:line-end line2))
         (dx1 (- (clad.sketch:point-x end1) (clad.sketch:point-x start1)))
         (dy1 (- (clad.sketch:point-y end1) (clad.sketch:point-y start1)))
         (dx2 (- (clad.sketch:point-x end2) (clad.sketch:point-x start2)))
         (dy2 (- (clad.sketch:point-y end2) (clad.sketch:point-y start2)))
         (cross (- (* dx1 dy2) (* dy1 dx2))))
    ;; ∂E/∂variables for both lines
    (list (* -2.0d0 cross dy2)   ; ∂E/∂x1_start
          (* 2.0d0 cross dx2)    ; ∂E/∂y1_start
          (* 2.0d0 cross dy2)    ; ∂E/∂x1_end
          (* -2.0d0 cross dx2)   ; ∂E/∂y1_end
          (* -2.0d0 cross dy1)   ; ∂E/∂x2_start
          (* 2.0d0 cross dx1)    ; ∂E/∂y2_start
          (* 2.0d0 cross dy1)    ; ∂E/∂x2_end
          (* -2.0d0 cross dx1))))  ; ∂E/∂y2_end

;;; ==============================================================================
;;; Perpendicular Constraint - Two lines must be perpendicular
;;; ==============================================================================

(defclass perpendicular-constraint (constraint)
  ()
  (:documentation "Constrains two lines to be perpendicular"))

(defun make-perpendicular-constraint (line1 line2)
  "Create a perpendicular constraint between LINE1 and LINE2."
  (make-instance 'perpendicular-constraint
                 :entities (list line1 line2)
                 :type :perpendicular))

(defmethod constraint-error ((c perpendicular-constraint))
  "Error = squared dot product of direction vectors (0 when perpendicular)"
  (let* ((line1 (first (constraint-entities c)))
         (line2 (second (constraint-entities c)))
         (start1 (clad.sketch:line-start line1))
         (end1 (clad.sketch:line-end line1))
         (start2 (clad.sketch:line-start line2))
         (end2 (clad.sketch:line-end line2))
         ;; Direction vectors
         (dx1 (- (clad.sketch:point-x end1) (clad.sketch:point-x start1)))
         (dy1 (- (clad.sketch:point-y end1) (clad.sketch:point-y start1)))
         (dx2 (- (clad.sketch:point-x end2) (clad.sketch:point-x start2)))
         (dy2 (- (clad.sketch:point-y end2) (clad.sketch:point-y start2)))
         ;; Dot product: dx1*dx2 + dy1*dy2
         (dot (+ (* dx1 dx2) (* dy1 dy2))))
    (* dot dot)))

(defmethod constraint-jacobian ((c perpendicular-constraint))
  "Gradient for perpendicular constraint"
  (let* ((line1 (first (constraint-entities c)))
         (line2 (second (constraint-entities c)))
         (start1 (clad.sketch:line-start line1))
         (end1 (clad.sketch:line-end line1))
         (start2 (clad.sketch:line-start line2))
         (end2 (clad.sketch:line-end line2))
         (dx1 (- (clad.sketch:point-x end1) (clad.sketch:point-x start1)))
         (dy1 (- (clad.sketch:point-y end1) (clad.sketch:point-y start1)))
         (dx2 (- (clad.sketch:point-x end2) (clad.sketch:point-x start2)))
         (dy2 (- (clad.sketch:point-y end2) (clad.sketch:point-y start2)))
         (dot (+ (* dx1 dx2) (* dy1 dy2))))
    ;; ∂E/∂variables for both lines
    (list (* -2.0d0 dot dx2)   ; ∂E/∂x1_start
          (* -2.0d0 dot dy2)   ; ∂E/∂y1_start
          (* 2.0d0 dot dx2)    ; ∂E/∂x1_end
          (* 2.0d0 dot dy2)    ; ∂E/∂y1_end
          (* -2.0d0 dot dx1)   ; ∂E/∂x2_start
          (* -2.0d0 dot dy1)   ; ∂E/∂y2_start
          (* 2.0d0 dot dx1)    ; ∂E/∂x2_end
          (* 2.0d0 dot dy1))))   ; ∂E/∂y2_end

;;; ==============================================================================
;;; Angle Constraint - Maintain specific angle between two lines
;;; ==============================================================================

(defclass angle-constraint (constraint)
  ((target-angle
    :initarg :target-angle
    :accessor constraint-target-angle
    :documentation "Target angle in radians"))
  (:documentation "Maintains a specific angle between two lines"))

(defun make-angle-constraint (line1 line2 angle)
  "Create an angle constraint between LINE1 and LINE2 with target ANGLE (in radians)."
  (make-instance 'angle-constraint
                 :entities (list line1 line2)
                 :type :angle
                 :target-angle (coerce angle 'double-float)))

(defmethod constraint-error ((c angle-constraint))
  "Error = squared difference between actual and target angle"
  (let* ((line1 (first (constraint-entities c)))
         (line2 (second (constraint-entities c)))
         (start1 (clad.sketch:line-start line1))
         (end1 (clad.sketch:line-end line1))
         (start2 (clad.sketch:line-start line2))
         (end2 (clad.sketch:line-end line2))
         ;; Direction vectors
         (dx1 (- (clad.sketch:point-x end1) (clad.sketch:point-x start1)))
         (dy1 (- (clad.sketch:point-y end1) (clad.sketch:point-y start1)))
         (dx2 (- (clad.sketch:point-x end2) (clad.sketch:point-x start2)))
         (dy2 (- (clad.sketch:point-y end2) (clad.sketch:point-y start2)))
         ;; Compute angle using atan2
         (angle1 (atan dy1 dx1))
         (angle2 (atan dy2 dx2))
         (actual-angle (abs (- angle2 angle1)))
         (target-angle (constraint-target-angle c))
         (error-angle (- actual-angle target-angle)))
    (* error-angle error-angle)))

(defmethod constraint-jacobian ((c angle-constraint))
  "Gradient for angle constraint"
  ;; Simplified placeholder - full implementation would require more complex derivatives
  (list 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))

;;; ==============================================================================
;;; Tangent Constraint - Line/circle tangency
;;; ==============================================================================

(defclass tangent-constraint (constraint)
  ()
  (:documentation "Constrains a line to be tangent to a circle or two circles to be tangent"))

(defun make-tangent-constraint (entity1 entity2)
  "Create a tangent constraint between ENTITY1 and ENTITY2."
  (make-instance 'tangent-constraint
                 :entities (list entity1 entity2)
                 :type :tangent))

(defmethod constraint-error ((c tangent-constraint))
  "Error = squared difference between distance and radius (for line-circle tangency)"
  (let* ((entity1 (first (constraint-entities c)))
         (entity2 (second (constraint-entities c))))
    (cond
      ;; Line-Circle tangency
      ((and (typep entity1 'clad.sketch:line-2d)
            (typep entity2 'clad.sketch:circle-2d))
       (let* ((line entity1)
              (circle entity2)
              (center (clad.sketch:circle-center circle))
              (radius (clad.sketch:circle-radius circle))
              (line-start (clad.sketch:line-start line))
              (line-end (clad.sketch:line-end line))
              ;; Line direction vector
              (dx (- (clad.sketch:point-x line-end) (clad.sketch:point-x line-start)))
              (dy (- (clad.sketch:point-y line-end) (clad.sketch:point-y line-start)))
              ;; Vector from line start to circle center
              (cx (- (clad.sketch:point-x center) (clad.sketch:point-x line-start)))
              (cy (- (clad.sketch:point-y center) (clad.sketch:point-y line-start)))
              ;; Line length
              (line-len (sqrt (+ (* dx dx) (* dy dy))))
              ;; Distance from center to line (perpendicular distance)
              (dist (if (< line-len 1.0d-10)
                        (sqrt (+ (* cx cx) (* cy cy)))
                        (abs (/ (- (* cx dy) (* cy dx)) line-len))))
              (error-dist (- dist radius)))
         (* error-dist error-dist)))

      ;; Circle-Circle tangency
      ((and (typep entity1 'clad.sketch:circle-2d)
            (typep entity2 'clad.sketch:circle-2d))
       (let* ((center1 (clad.sketch:circle-center entity1))
              (center2 (clad.sketch:circle-center entity2))
              (radius1 (clad.sketch:circle-radius entity1))
              (radius2 (clad.sketch:circle-radius entity2))
              (dx (- (clad.sketch:point-x center2) (clad.sketch:point-x center1)))
              (dy (- (clad.sketch:point-y center2) (clad.sketch:point-y center1)))
              (center-dist (sqrt (+ (* dx dx) (* dy dy))))
              ;; External tangency: distance = r1 + r2
              (target-dist (+ radius1 radius2))
              (error-dist (- center-dist target-dist)))
         (* error-dist error-dist)))

      ;; Unsupported combination
      (t 0.0d0))))

(defmethod constraint-jacobian ((c tangent-constraint))
  "Gradient for tangent constraint"
  ;; Simplified placeholder - full implementation would require case analysis
  (list 0.0d0 0.0d0 0.0d0 0.0d0))

;;; ==============================================================================
;;; Default implementations for apply-constraint
;;; ==============================================================================

(defmethod apply-constraint ((c constraint))
  "Default implementation - does nothing (solver will handle)"
  nil)
