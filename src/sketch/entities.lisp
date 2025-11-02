;;;; entities.lisp --- 2D sketch entities for constraint-based sketching

(in-package #:clad.sketch)

;;; ==============================================================================
;;; Base Sketch Entity Class
;;; ==============================================================================

(defclass sketch-entity ()
  ((name
    :initarg :name
    :initform nil
    :accessor entity-name
    :documentation "Optional name for the entity")
   (parameters
    :initarg :parameters
    :initform nil
    :accessor entity-parameters
    :documentation "List of parameter symbols used by this entity"))
  (:documentation "Base class for all 2D sketch entities"))

;;; ==============================================================================
;;; Point-2D Entity
;;; ==============================================================================

(defclass point-2d (sketch-entity)
  ((x
    :initarg :x
    :accessor point-x
    :documentation "X coordinate of the point")
   (y
    :initarg :y
    :accessor point-y
    :documentation "Y coordinate of the point")
   (fixed
    :initarg :fixed
    :initform nil
    :accessor point-fixed-p
    :documentation "Whether this point is fixed in space"))
  (:documentation "A 2D point entity with X and Y coordinates"))

(defun make-point-2d (x y &key name fixed)
  "Create a new 2D point at coordinates (X, Y).

Arguments:
  x - X coordinate (double-float)
  y - Y coordinate (double-float)
  name - Optional name for the point
  fixed - If T, point is fixed and won't move during solving

Returns: A new point-2d instance"
  (make-instance 'point-2d
                 :x (coerce x 'double-float)
                 :y (coerce y 'double-float)
                 :name name
                 :fixed fixed))

;;; ==============================================================================
;;; Line-2D Entity
;;; ==============================================================================

(defclass line-2d (sketch-entity)
  ((start-point
    :initarg :start
    :accessor line-start
    :documentation "Start point of the line")
   (end-point
    :initarg :end
    :accessor line-end
    :documentation "End point of the line"))
  (:documentation "A 2D line entity defined by two points"))

(defun make-line-2d (start-point end-point &key name)
  "Create a new 2D line from START-POINT to END-POINT.

Arguments:
  start-point - Starting point (point-2d instance)
  end-point - Ending point (point-2d instance)
  name - Optional name for the line

Returns: A new line-2d instance"
  (make-instance 'line-2d
                 :start start-point
                 :end end-point
                 :name name))

(defun line-length (line)
  "Calculate the Euclidean distance between the start and end points of LINE.

Arguments:
  line - A line-2d instance

Returns: Length as a double-float"
  (let* ((start (line-start line))
         (end (line-end line))
         (dx (- (point-x end) (point-x start)))
         (dy (- (point-y end) (point-y start))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;;; ==============================================================================
;;; Arc-2D Entity
;;; ==============================================================================

(defclass arc-2d (sketch-entity)
  ((center
    :initarg :center
    :accessor arc-center
    :documentation "Center point of the arc")
   (radius
    :initarg :radius
    :accessor arc-radius
    :documentation "Radius of the arc")
   (start-angle
    :initarg :start-angle
    :accessor arc-start-angle
    :documentation "Start angle in radians")
   (end-angle
    :initarg :end-angle
    :accessor arc-end-angle
    :documentation "End angle in radians"))
  (:documentation "A 2D arc entity defined by center, radius, and angle range"))

(defun make-arc-2d (center radius start-angle end-angle &key name)
  "Create a new 2D arc.

Arguments:
  center - Center point (point-2d instance)
  radius - Radius of the arc (double-float)
  start-angle - Starting angle in radians (double-float)
  end-angle - Ending angle in radians (double-float)
  name - Optional name for the arc

Returns: A new arc-2d instance"
  (make-instance 'arc-2d
                 :center center
                 :radius (coerce radius 'double-float)
                 :start-angle (coerce start-angle 'double-float)
                 :end-angle (coerce end-angle 'double-float)
                 :name name))

;;; ==============================================================================
;;; Circle-2D Entity
;;; ==============================================================================

(defclass circle-2d (sketch-entity)
  ((center
    :initarg :center
    :accessor circle-center
    :documentation "Center point of the circle")
   (radius
    :initarg :radius
    :accessor circle-radius
    :documentation "Radius of the circle"))
  (:documentation "A 2D circle entity"))

(defun make-circle-2d (center radius &key name)
  "Create a new 2D circle.

Arguments:
  center - Center point (point-2d instance)
  radius - Radius of the circle (double-float)
  name - Optional name for the circle

Returns: A new circle-2d instance"
  (make-instance 'circle-2d
                 :center center
                 :radius (coerce radius 'double-float)
                 :name name))

;;; ==============================================================================
;;; Spline-2D Entity
;;; ==============================================================================

(defclass spline-2d (sketch-entity)
  ((control-points
    :initarg :points
    :accessor spline-points
    :documentation "List of control points")
   (closed
    :initarg :closed
    :initform nil
    :accessor spline-closed-p
    :documentation "Whether the spline is closed"))
  (:documentation "A 2D spline entity"))

(defun make-spline-2d (points &key name closed)
  "Create a new 2D spline.

Arguments:
  points - List of control points (point-2d instances)
  name - Optional name for the spline
  closed - If T, the spline forms a closed loop

Returns: A new spline-2d instance"
  (make-instance 'spline-2d
                 :points points
                 :closed closed
                 :name name))

;;; ==============================================================================
;;; Sketch Container
;;; ==============================================================================

(defclass sketch ()
  ((entities
    :initform nil
    :accessor sketch-entities
    :documentation "List of all entities in this sketch")
   (constraints
    :initform nil
    :accessor sketch-constraints
    :documentation "List of all constraints in this sketch")
   (name
    :initarg :name
    :initform nil
    :accessor sketch-name
    :documentation "Optional name for the sketch"))
  (:documentation "A container for 2D sketch entities and constraints"))

(defun make-sketch (&key name)
  "Create a new empty sketch.

Arguments:
  name - Optional name for the sketch

Returns: A new sketch instance"
  (make-instance 'sketch :name name))

(defun add-entity (sketch entity)
  "Add an entity to the sketch.

Arguments:
  sketch - The sketch to add to
  entity - The entity to add

Returns: The sketch"
  (push entity (sketch-entities sketch))
  sketch)

(defun add-constraint (sketch constraint)
  "Add a constraint to the sketch.

Arguments:
  sketch - The sketch to add to
  constraint - The constraint to add

Returns: The sketch"
  (push constraint (sketch-constraints sketch))
  sketch)

(defun find-entity (sketch name)
  "Find an entity in the sketch by name.

Arguments:
  sketch - The sketch to search in
  name - The name of the entity to find

Returns: The entity if found, NIL otherwise"
  (find name (sketch-entities sketch)
        :key #'entity-name
        :test #'equal))

;;; ==============================================================================
;;; Parameters (placeholder for now)
;;; ==============================================================================

(defclass sketch-parameter ()
  ((name
    :initarg :name
    :accessor param-name
    :documentation "Name of the parameter")
   (value
    :initarg :value
    :accessor param-value
    :documentation "Value of the parameter")
   (fixed
    :initarg :fixed
    :initform nil
    :accessor param-fixed-p
    :documentation "Whether this parameter is fixed"))
  (:documentation "A parameter that can be constrained"))
