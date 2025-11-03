;;;; validation.lisp - Sketch constraint validation system
;;;;
;;;; This file provides functions to validate sketch constraint systems,
;;;; detecting overconstrained, underconstrained, and conflicting constraints.

(defpackage :clad.sketch.validation
  (:use #:cl #:clad.sketch #:clad.sketch.constraints)
  (:documentation "Validation functions for 2D sketch constraint systems")
  (:export
   #:validate-sketch
   #:degrees-of-freedom
   #:validation-report
   #:list-underconstrained-entities
   #:list-redundant-constraints
   #:suggest-missing-constraints))

(in-package :clad.sketch.validation)

;;;; Degree of Freedom Calculation
;;;;
;;;; Calculate the number of degrees of freedom (DOF) in a sketch.
;;;; DOF = (total entity DOF) - (fixed entity DOF) - (constraint DOF)

(defun count-entity-dof (entity)
  "Count the degrees of freedom for a single entity.
   Points have 2 DOF (x, y) unless fixed.
   Lines inherit DOF from their endpoints.
   Circles/arcs inherit DOF from their center point.
   Splines inherit DOF from their control points."
  (cond
    ;; Fixed points contribute 0 DOF
    ((and (typep entity 'point-2d) (point-fixed-p entity))
     0)
    ;; Unfixed points contribute 2 DOF
    ((typep entity 'point-2d)
     2)
    ;; Lines don't add DOF beyond their endpoints
    ((typep entity 'line-2d)
     0)
    ;; Circles/arcs don't add DOF beyond their center
    ((or (typep entity 'circle-2d) (typep entity 'arc-2d))
     0)
    ;; Splines don't add DOF beyond their control points
    ((typep entity 'spline-2d)
     0)
    ;; Unknown entity type
    (t 0)))

(defun count-constraint-dof (constraint)
  "Count the degrees of freedom removed by a constraint.
   Most constraints remove 1 DOF, but some may remove 2."
  (cond
    ;; Fixed constraint removes 2 DOF (locks X and Y)
    ((typep constraint 'clad.sketch.constraints:fixed-constraint)
     2)
    ;; Distance constraint removes 1 DOF
    ((typep constraint 'clad.sketch.constraints:distance-constraint)
     1)
    ;; Coincident constraint removes 2 DOF (makes two points share X and Y)
    ((typep constraint 'clad.sketch.constraints:coincident-constraint)
     2)
    ;; Horizontal/vertical constraints remove 1 DOF
    ((or (typep constraint 'clad.sketch.constraints:horizontal-constraint)
         (typep constraint 'clad.sketch.constraints:vertical-constraint))
     1)
    ;; Parallel/perpendicular constraints remove 1 DOF (angle)
    ((or (typep constraint 'clad.sketch.constraints:parallel-constraint)
         (typep constraint 'clad.sketch.constraints:perpendicular-constraint))
     1)
    ;; Angle constraint removes 1 DOF
    ((typep constraint 'clad.sketch.constraints:angle-constraint)
     1)
    ;; Tangent constraint removes 1 DOF
    ((typep constraint 'clad.sketch.constraints:tangent-constraint)
     1)
    ;; Unknown constraint type
    (t 0)))

(defun degrees-of-freedom (sketch)
  "Calculate the total degrees of freedom for a sketch.
   Returns the number of unconstrained DOF remaining."
  (let ((entities (sketch-entities sketch))
        (constraints (sketch-constraints sketch)))
    ;; Sum up entity DOF
    (let ((total-dof (reduce #'+ entities
                             :key #'count-entity-dof
                             :initial-value 0))
          ;; Sum up constraint DOF
          (constrained-dof (reduce #'+ constraints
                                   :key #'count-constraint-dof
                                   :initial-value 0)))
      ;; DOF = entity DOF - constraint DOF
      ;; Note: Can go negative if overconstrained
      (- total-dof constrained-dof))))

;;;; Constraint Conflict Detection
;;;;
;;;; Detect if constraints are in conflict by attempting to solve
;;;; and checking if the error remains high.

(defun detect-conflicts (sketch &key (error-threshold 1.0d0) (max-iterations 1000))
  "Attempt to solve the sketch and detect if constraints conflict.
   Returns T if conflicts are detected, NIL otherwise."
  (let ((constraints (sketch-constraints sketch)))
    (when (null constraints)
      (return-from detect-conflicts nil))

    ;; Try to solve the sketch
    (clad.sketch.solver:solve-sketch sketch :max-iterations max-iterations)

    ;; Check if any constraint has high error after solving
    (some (lambda (c)
            (> (clad.sketch.constraints:constraint-error c) error-threshold))
          constraints)))

;;;; Validation Functions
;;;;
;;;; Main validation logic that combines DOF analysis and conflict detection.

(defun validate-sketch (sketch)
  "Validate a sketch and return its constraint status.
   Returns one of:
   - :well-constrained - Sketch is fully constrained with no conflicts
   - :under-constrained - Sketch has positive DOF (entities can still move)
   - :over-constrained - Sketch has negative DOF (redundant constraints)
   - :conflicting - Constraints cannot be satisfied simultaneously"
  (let ((dof (degrees-of-freedom sketch))
        (has-conflicts (detect-conflicts sketch)))
    (cond
      ;; If conflicts detected, return conflicting status
      (has-conflicts
       :conflicting)
      ;; If DOF < 0, sketch is overconstrained
      ((< dof 0)
       :over-constrained)
      ;; If DOF > 0, sketch is underconstrained
      ((> dof 0)
       :under-constrained)
      ;; If DOF = 0 and no conflicts, sketch is well-constrained
      (t
       :well-constrained))))

(defun validation-report (sketch)
  "Generate a detailed validation report for a sketch.
   Returns a property list with:
   - :status - The validation status keyword
   - :dof - The degrees of freedom count
   - :entity-count - Number of entities
   - :constraint-count - Number of constraints
   - :has-conflicts - Boolean indicating if conflicts detected"
  (let ((dof (degrees-of-freedom sketch))
        (status (validate-sketch sketch))
        (entities (sketch-entities sketch))
        (constraints (sketch-constraints sketch))
        (has-conflicts (detect-conflicts sketch)))
    (list :status status
          :dof dof
          :entity-count (length entities)
          :constraint-count (length constraints)
          :has-conflicts has-conflicts)))

;;;; Validation Analysis Helpers
;;;;
;;;; Additional functions for analyzing constraint systems.

(defun list-underconstrained-entities (sketch)
  "Return a list of entities that are not fully constrained.
   This is useful for highlighting problem areas in the sketch."
  ;; TODO: Implement detailed analysis
  ;; For now, return nil as placeholder
  nil)

(defun list-redundant-constraints (sketch)
  "Return a list of constraints that are redundant.
   These constraints don't contribute to constraining the sketch."
  ;; TODO: Implement redundancy analysis
  ;; For now, return nil as placeholder
  nil)

(defun suggest-missing-constraints (sketch)
  "Suggest constraints that might be needed to fully constrain the sketch.
   Returns a list of suggested constraint types."
  ;; TODO: Implement constraint suggestion system
  ;; For now, return nil as placeholder
  nil)
