;;;; constraints.lisp --- Mate constraints for assemblies

(in-package #:clad.assembly.constraints)

;;; ==============================================================================
;;; Mate Constraint Class (Week 11-12)
;;; ==============================================================================

(defclass mate-constraint ()
  ((type :initarg :type
         :initform nil
         :accessor mate-type
         :documentation "Constraint type (e.g., :coincident, :concentric, :distance, etc.)")
   (component1 :initarg :component1
               :initform nil
               :accessor mate-component1
               :documentation "First component name (symbol)")
   (component2 :initarg :component2
               :initform nil
               :accessor mate-component2
               :documentation "Second component name (symbol)")
   (reference1 :initarg :reference1
               :initform nil
               :accessor mate-reference1
               :documentation "Reference on first component (symbol or selector list)")
   (reference2 :initarg :reference2
               :initform nil
               :accessor mate-reference2
               :documentation "Reference on second component (symbol or selector list)")
   (offset :initarg :offset
           :initform 0.0d0
           :accessor mate-offset
           :documentation "Offset distance or angle (mm or degrees)")
   (assembly :initarg :assembly
             :initform nil
             :accessor mate-assembly
             :documentation "Parent assembly containing this constraint"))
  (:documentation "Mate constraint between two components in an assembly.

A mate constraint defines a geometric relationship between two components:
- Coincident: Faces or planes flush/aligned (offset = 0)
- Concentric: Cylindrical/circular features share axis
- Distance: Faces or planes separated by fixed distance
- Angle: Planar features at fixed angle
- Parallel: Planar features parallel to each other
- Perpendicular: Planar features at 90 degrees
- Tangent: Curved surfaces tangent to each other
- Fixed: Component locked in place (ground)

Example:
  (add-mate asm :coincident
            :base-plate :face-top
            :housing :face-bottom)"))

;;; ==============================================================================
;;; Constraint Management Functions
;;; ==============================================================================

(defun add-mate (assembly type component1 ref1 component2 ref2 &rest args)
  "Add a mate constraint between two components.

  Args:
    assembly - Assembly to add constraint to
    type - Constraint type (:coincident, :concentric, :distance, etc.)
    component1 - Name of first component (symbol)
    ref1 - Reference on first component (symbol or list)
    component2 - Name of second component (symbol)
    ref2 - Reference on second component (symbol or list)
    args - Additional keyword arguments:
      :offset - Distance/angle offset (default 0.0)
      :angle - Angle value (synonym for :offset, used with :angle constraints)

  Returns: The created mate-constraint

  Example:
    (add-mate asm :distance
              :part1 :face-top
              :part2 :face-bottom
              :offset 10.0)"
  (let* ((offset (or (getf args :offset)
                     (getf args :angle)
                     0.0d0))
         (mate (make-instance 'mate-constraint
                              :type type
                              :component1 component1
                              :component2 component2
                              :reference1 ref1
                              :reference2 ref2
                              :offset (coerce offset 'double-float)
                              :assembly assembly)))
    ;; Add to assembly's constraints list
    (push mate (clad.assembly:assembly-constraints assembly))
    mate))

(defun list-constraints (assembly)
  "List all mate constraints in an assembly.

  Args:
    assembly - Assembly to query

  Returns: List of mate-constraint objects

  Example:
    (list-constraints asm) => (#<MATE-CONSTRAINT :COINCIDENT> ...)"
  (clad.assembly:assembly-constraints assembly))

(defun remove-constraint (assembly mate)
  "Remove a mate constraint from an assembly.

  Args:
    assembly - Assembly to remove from
    mate - Mate constraint to remove

  Returns: The assembly (for chaining)

  Example:
    (remove-constraint asm some-mate)"
  (setf (clad.assembly:assembly-constraints assembly)
        (remove mate (clad.assembly:assembly-constraints assembly)))
  assembly)

(defun clear-constraints (assembly)
  "Remove all mate constraints from an assembly.

  Args:
    assembly - Assembly to clear

  Returns: The assembly (for chaining)

  Example:
    (clear-constraints asm)"
  (setf (clad.assembly:assembly-constraints assembly) '())
  assembly)

;;; ==============================================================================
;;; Constraint Type Validation
;;; ==============================================================================

(defparameter *valid-constraint-types*
  '(:coincident :concentric :distance :angle :parallel :perpendicular :tangent :fixed)
  "List of valid mate constraint types.")

(defun valid-constraint-type-p (type)
  "Check if a constraint type is valid.

  Args:
    type - Constraint type symbol

  Returns: T if valid, nil otherwise

  Example:
    (valid-constraint-type-p :coincident) => T
    (valid-constraint-type-p :invalid) => NIL"
  (member type *valid-constraint-types*))

;;; ==============================================================================
;;; Constraint Error Calculation (Week 11-12 Stub)
;;; ==============================================================================

(defun mate-error (mate)
  "Calculate constraint error (how far from being satisfied).

  Args:
    mate - Mate constraint to evaluate

  Returns: Non-negative number (0.0 = satisfied)

  The error is calculated based on constraint type:
  - Distance/coincident: Linear distance error (mm)
  - Angle/parallel/perpendicular: Angular error (degrees)
  - Concentric: Axis alignment error (mm)
  - Tangent: Gap/overlap distance (mm)

  Note: This is a stub implementation for Week 11-12.
        Full implementation will come in Week 13-14 (Assembly Solver).

  Example:
    (mate-error some-mate) => 0.0d0"
  (declare (ignore mate))
  0.0d0)

;;; ==============================================================================
;;; Constraint Application (Week 11-12 Stub)
;;; ==============================================================================

(defun apply-mate (mate)
  "Apply a mate constraint to move components into alignment.

  Args:
    mate - Mate constraint to apply

  Returns: T if successfully applied, nil if failed

  This function computes the necessary transformation to satisfy
  the constraint and updates the component's position/rotation.

  Note: This is a stub implementation for Week 11-12.
        Full implementation will come in Week 13-14 (Assembly Solver).

  Example:
    (apply-mate some-mate) => T"
  (declare (ignore mate))
  t)

;;; ==============================================================================
;;; Constraint Compatibility Checking
;;; ==============================================================================

(defun mate-compatible-p (mate1 mate2)
  "Check if two constraints are compatible (no conflict).

  Args:
    mate1 - First mate constraint
    mate2 - Second mate constraint

  Returns: T if compatible, nil if conflicting

  Two constraints are potentially incompatible if they:
  - Constrain the same component pair
  - Have conflicting geometric requirements
  - Over-constrain the same degrees of freedom

  Note: This is a simplified check for Week 11-12.
        Full DOF analysis will come in Week 13-14.

  Example:
    (mate-compatible-p mate1 mate2) => T"
  ;; Simple check: if they involve the same component pair in same order
  ;; and are of incompatible types, return nil
  (let ((c1-1 (mate-component1 mate1))
        (c1-2 (mate-component2 mate1))
        (c2-1 (mate-component1 mate2))
        (c2-2 (mate-component2 mate2))
        (t1 (mate-type mate1))
        (t2 (mate-type mate2)))

    ;; If they don't share both components, they're compatible
    (unless (and (or (and (eq c1-1 c2-1) (eq c1-2 c2-2))
                     (and (eq c1-1 c2-2) (eq c1-2 c2-1))))
      (return-from mate-compatible-p t))

    ;; If they're the same constraint type on same components with same refs,
    ;; they might conflict (simplified check)
    (let ((r1-1 (mate-reference1 mate1))
          (r1-2 (mate-reference2 mate1))
          (r2-1 (mate-reference1 mate2))
          (r2-2 (mate-reference2 mate2)))

      ;; If types are the same and references are the same, potential conflict
      (if (and (eq t1 t2)
               (equal r1-1 r2-1)
               (equal r1-2 r2-2))
          nil  ; Conflict
          t)))) ; Compatible

;;; ==============================================================================
;;; Assembly DOF Analysis (Week 11-12 Stub)
;;; ==============================================================================

(defun assembly-dof (assembly)
  "Calculate degrees of freedom for an assembly.

  Args:
    assembly - Assembly to analyze

  Returns: Number of degrees of freedom (0 = fully constrained)

  DOF calculation considers:
  - Number of components (each has 6 DOF: 3 translation + 3 rotation)
  - Fixed components (0 DOF)
  - Mate constraints (each removes DOF based on type)

  Formula: DOF = (6 * free-components) - (constraints-DOF)

  Note: This is a stub implementation for Week 11-12.
        Full implementation will come in Week 13-14 (Assembly Solver).

  Example:
    (assembly-dof asm) => 0"
  (let* ((components (clad.assembly:list-components assembly))
         (constraints (list-constraints assembly))
         (free-components (count-if-not #'clad.assembly:component-fixed-p components))
         (total-dof (* 6 free-components)))

    ;; Subtract DOF removed by each constraint (simplified)
    (dolist (mate constraints)
      (case (mate-type mate)
        (:coincident (decf total-dof 3))  ; Removes 3 DOF (normal translation + 2 rotations)
        (:concentric (decf total-dof 2))  ; Removes 2 DOF (2 translations perpendicular to axis)
        (:distance (decf total-dof 1))    ; Removes 1 DOF (translation along normal)
        (:angle (decf total-dof 1))       ; Removes 1 DOF (rotation about axis)
        (:parallel (decf total-dof 2))    ; Removes 2 DOF (2 rotations)
        (:perpendicular (decf total-dof 2)) ; Removes 2 DOF (2 rotations)
        (:tangent (decf total-dof 1))     ; Removes 1 DOF (translation normal to surface)
        (:fixed (decf total-dof 6))))     ; Removes all 6 DOF

    (max 0 total-dof)))

(defun assembly-status (assembly)
  "Determine if assembly is under/over/well-constrained.

  Args:
    assembly - Assembly to analyze

  Returns: One of :under-constrained, :well-constrained, :over-constrained

  Status determination:
  - Under-constrained: DOF > 0 (components can move)
  - Well-constrained: DOF = 0 (fully defined)
  - Over-constrained: Conflicting constraints detected

  Example:
    (assembly-status asm) => :WELL-CONSTRAINED"
  (let ((dof (assembly-dof assembly))
        (constraints (list-constraints assembly)))

    (cond
      ;; Check for over-constraint (conflicting mates)
      ((and (>= (length constraints) 2)
            (loop for (m1 . rest) on constraints
                  thereis (loop for m2 in rest
                                thereis (not (mate-compatible-p m1 m2)))))
       :over-constrained)

      ;; Well-constrained
      ((zerop dof)
       :well-constrained)

      ;; Under-constrained
      (t
       :under-constrained))))
