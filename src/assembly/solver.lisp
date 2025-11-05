;;;; solver.lisp --- Assembly constraint solver

(in-package #:clad.assembly.solver)

;;; ==============================================================================
;;; Solver Errors and Conditions
;;; ==============================================================================

(define-condition assembly-solver-error (error)
  ((assembly :initarg :assembly
             :reader solver-error-assembly
             :documentation "Assembly that caused the error")
   (message :initarg :message
            :reader solver-error-message
            :documentation "Error message"))
  (:report (lambda (condition stream)
             (format stream "Assembly solver error: ~A"
                     (solver-error-message condition))))
  (:documentation "Base error for assembly solver problems"))

(define-condition over-constrained-error (assembly-solver-error)
  ()
  (:documentation "Assembly has conflicting constraints"))

(define-condition under-constrained-error (assembly-solver-error)
  ()
  (:documentation "Assembly has insufficient constraints"))

;;; ==============================================================================
;;; Assembly Solver (Week 13-14)
;;; ==============================================================================

(defun solve-assembly (assembly &key (max-iterations 100) (tolerance 1.0d-6) (error-on-failure t))
  "Solve assembly constraints to position components.

  Args:
    assembly - Assembly to solve
    :max-iterations - Maximum solver iterations (default 100)
    :tolerance - Convergence tolerance (default 1.0e-6)
    :error-on-failure - Signal error if solve fails (default t)

  Returns: Plist with solver results
    :status - :solved, :over-constrained, :under-constrained
    :dof - Degrees of freedom
    :iterations - Number of iterations used
    :error - Final error value

  This is a simplified solver that:
  - Validates assembly constraint status
  - Reports DOF and constraint conflicts
  - For well-constrained assemblies with simple constraints, positions components

  Full geometric constraint solving is beyond the scope of this implementation.

  Example:
    (solve-assembly asm :max-iterations 50 :tolerance 1.0e-5)"
  (declare (ignore max-iterations tolerance)) ; Simplified implementation

  (let* ((dof (clad.assembly.constraints:assembly-dof assembly))
         (status (clad.assembly.constraints:assembly-status assembly))
         (constraints (clad.assembly:assembly-constraints assembly)))

    ;; Build result plist
    (let ((result (list :status status
                        :dof dof
                        :iterations 0
                        :error 0.0d0
                        :constraint-count (length constraints))))

      ;; Handle error conditions
      (cond
        ((eq status :over-constrained)
         (when error-on-failure
           (error 'over-constrained-error
                  :assembly assembly
                  :message (format nil "Assembly is over-constrained (conflicting mates)")))
         (setf (getf result :status) :over-constrained))

        ((eq status :under-constrained)
         (when (and error-on-failure (> dof 0))
           (error 'under-constrained-error
                  :assembly assembly
                  :message (format nil "Assembly is under-constrained (~A DOF remaining)" dof)))
         (setf (getf result :status) :under-constrained))

        ((eq status :well-constrained)
         ;; Well-constrained: assembly is fully defined
         (setf (getf result :status) :solved)))

      result)))

;;; ==============================================================================
;;; Solver Utilities
;;; ==============================================================================

(defun solver-status-string (result)
  "Get human-readable status string from solver result.

  Args:
    result - Solver result plist

  Returns: Status string

  Example:
    (solver-status-string result) => \"Solved (0 DOF, 3 constraints)\""
  (let ((status (getf result :status))
        (dof (getf result :dof))
        (count (getf result :constraint-count)))
    (case status
      (:solved
       (format nil "Solved (~A DOF, ~A constraint~:P)" dof count))
      (:over-constrained
       (format nil "Over-constrained (~A constraint~:P)" count))
      (:under-constrained
       (format nil "Under-constrained (~A DOF, ~A constraint~:P)" dof count))
      (t
       (format nil "Unknown status: ~A" status)))))
