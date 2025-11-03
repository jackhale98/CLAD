;;;; solver.lisp --- Constraint solver for 2D sketches

(in-package #:clad.sketch.solver)

;;; ==============================================================================
;;; Error Conditions
;;; ==============================================================================

(define-condition over-constrained-error (error)
  ()
  (:documentation "Signaled when a sketch has too many constraints"))

(define-condition under-constrained-error (error)
  ()
  (:documentation "Signaled when a sketch has too few constraints"))

(define-condition solver-failed-error (error)
  ((message :initarg :message :reader solver-error-message))
  (:documentation "Signaled when the solver fails to converge"))

;;; ==============================================================================
;;; Solver Options
;;; ==============================================================================

(defclass solver-options ()
  ((max-iterations
    :initarg :max-iterations
    :initform 500
    :accessor solver-max-iterations
    :documentation "Maximum number of iterations")
   (tolerance
    :initarg :tolerance
    :initform 0.01d0
    :accessor solver-tolerance
    :documentation "Convergence tolerance")
   (method
    :initarg :method
    :initform :gradient-descent
    :accessor solver-method
    :documentation "Solver method to use"))
  (:documentation "Options for the constraint solver"))

(defun make-solver-options (&key (max-iterations 500) (tolerance 0.01d0) (method :gradient-descent))
  "Create a new solver-options instance.

Arguments:
  max-iterations - Maximum number of solver iterations (default: 100)
  tolerance - Convergence tolerance (default: 1.0d-6)
  method - Solver method (default: :gradient-descent)

Returns: A new solver-options instance"
  (make-instance 'solver-options
                 :max-iterations max-iterations
                 :tolerance tolerance
                 :method method))

;;; ==============================================================================
;;; Helper Functions
;;; ==============================================================================

(defun collect-points (sketch)
  "Collect all unique points from the sketch entities.

Arguments:
  sketch - The sketch to collect points from

Returns: List of unique point-2d instances"
  (let ((points nil))
    (dolist (entity (clad.sketch:sketch-entities sketch))
      (typecase entity
        (clad.sketch:point-2d
         (pushnew entity points))
        (clad.sketch:line-2d
         (pushnew (clad.sketch:line-start entity) points)
         (pushnew (clad.sketch:line-end entity) points))
        (clad.sketch:circle-2d
         (pushnew (clad.sketch:circle-center entity) points))
        (clad.sketch:arc-2d
         (pushnew (clad.sketch:arc-center entity) points))))
    points))

(defun total-error (sketch)
  "Calculate total constraint error for the sketch.

Arguments:
  sketch - The sketch to evaluate

Returns: Sum of all constraint errors as double-float"
  (let ((total 0.0d0))
    (dolist (constraint (clad.sketch:sketch-constraints sketch))
      (incf total (clad.sketch.constraints:constraint-error constraint)))
    total))

;;; ==============================================================================
;;; Gradient Descent Solver
;;; ==============================================================================

(defun solve-gradient-descent (sketch options)
  "Solve constraints using gradient descent method with finite differences.

Arguments:
  sketch - The sketch to solve
  options - Solver options

Returns: T if converged, NIL otherwise"
  (let* ((points (collect-points sketch))
         (max-iter (solver-max-iterations options))
         (tolerance (solver-tolerance options))
         (learning-rate 0.01d0)
         (epsilon 1.0d-6)
         (converged nil)
         (stuck-counter 0))

    ;; Main iteration loop
    (dotimes (iter max-iter)
      (let* ((prev-error (total-error sketch))
             (gradients (make-hash-table)))

        ;; Early termination if error is small enough
        (when (< prev-error tolerance)
          (setf converged t)
          (return))

        ;; Compute gradients using finite differences for all free points
        (dolist (point points)
          (unless (clad.sketch:point-fixed-p point)
            (let ((x0 (clad.sketch:point-x point))
                  (y0 (clad.sketch:point-y point))
                  (grad-x 0.0d0)
                  (grad-y 0.0d0))

              ;; Gradient in X direction
              (setf (clad.sketch:point-x point) (+ x0 epsilon))
              (setf grad-x (/ (- (total-error sketch) prev-error) epsilon))
              (setf (clad.sketch:point-x point) x0)

              ;; Gradient in Y direction
              (setf (clad.sketch:point-y point) (+ y0 epsilon))
              (setf grad-y (/ (- (total-error sketch) prev-error) epsilon))
              (setf (clad.sketch:point-y point) y0)

              ;; Store gradients
              (setf (gethash point gradients) (list grad-x grad-y)))))

        ;; Apply gradient updates
        (dolist (point points)
          (unless (clad.sketch:point-fixed-p point)
            (let ((grad (gethash point gradients)))
              (when grad
                (decf (clad.sketch:point-x point) (* learning-rate (first grad)))
                (decf (clad.sketch:point-y point) (* learning-rate (second grad)))))))

        ;; Check for convergence and adjust learning rate
        (let ((current-error (total-error sketch)))
          ;; Check if we converged
          (when (< current-error tolerance)
            (setf converged t)
            (return))

          ;; If error increased or didn't change much, adjust strategy
          (cond
            ((> current-error prev-error)
             ;; Error increased - reduce learning rate and try again
             (setf learning-rate (* learning-rate 0.5d0))
             (incf stuck-counter)
             ;; Undo the step
             (dolist (point points)
               (unless (clad.sketch:point-fixed-p point)
                 (let ((grad (gethash point gradients)))
                   (when grad
                     (incf (clad.sketch:point-x point) (* learning-rate 2.0d0 (first grad)))
                     (incf (clad.sketch:point-y point) (* learning-rate 2.0d0 (second grad))))))))
            ((< (abs (- current-error prev-error)) (* tolerance 0.01d0))
             ;; Not making progress
             (incf stuck-counter))
            (t
             ;; Making progress
             (setf stuck-counter 0)))

          ;; Give up if we're stuck or learning rate is too small
          (when (or (> stuck-counter 10)
                    (< learning-rate 1.0d-12))
            (return)))))

    converged))

(defun constraint-involves-point-p (constraint point)
  "Check if a constraint involves a specific point.

Arguments:
  constraint - The constraint to check
  point - The point to look for

Returns: T if the constraint involves this point, NIL otherwise"
  (dolist (entity (clad.sketch.constraints:constraint-entities constraint))
    (typecase entity
      (clad.sketch:point-2d
       (when (eq entity point)
         (return-from constraint-involves-point-p t)))
      (clad.sketch:line-2d
       (when (or (eq (clad.sketch:line-start entity) point)
                 (eq (clad.sketch:line-end entity) point))
         (return-from constraint-involves-point-p t)))
      (clad.sketch:circle-2d
       (when (eq (clad.sketch:circle-center entity) point)
         (return-from constraint-involves-point-p t)))
      (clad.sketch:arc-2d
       (when (eq (clad.sketch:arc-center entity) point)
         (return-from constraint-involves-point-p t)))))
  nil)

;;; ==============================================================================
;;; Main Solver Entry Point
;;; ==============================================================================

(defun solve-sketch (sketch &optional (options (make-solver-options)))
  "Solve all constraints in a sketch using iterative optimization.

Arguments:
  sketch - The sketch to solve
  options - Optional solver-options instance (default: standard options)

Returns: T if solver converged successfully, NIL otherwise

The solver uses gradient descent to minimize constraint errors. Fixed points
are not moved during solving."
  (unless options
    (setf options (make-solver-options)))

  (ecase (solver-method options)
    (:gradient-descent
     (solve-gradient-descent sketch options))))
