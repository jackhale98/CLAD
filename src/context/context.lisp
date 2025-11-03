;;;; src/context/context.lisp --- Stateful modeling context API

(in-package :clad.context)

;;; ============================================================================
;;; Modeling Context Class
;;; ============================================================================

(defclass modeling-context ()
  ((current-shape :initform nil
                  :accessor current-shape
                  :documentation "The current shape being built")
   (selection-stack :initform '()
                   :accessor selection-stack
                   :documentation "Stack of selected sub-shapes (faces, edges)")
   (workplane-stack :initform '()
                    :accessor workplane-stack
                    :documentation "Stack of workplanes for coordinate transforms")
   (current-workplane :initform (xy-plane)
                     :accessor current-workplane
                     :documentation "Current active workplane"))
  (:documentation "Stateful context for sequential CAD modeling operations.

The modeling context maintains:
- current-shape: The shape being built (initially nil)
- selection-stack: Stack of selected faces/edges for operations
- workplane-stack: Stack of workplanes for push/pop operations
- current-workplane: Active workplane for coordinate transforms

Example usage:
  (with-context ()
    (add (make-box 100 100 100))
    (select-faces :direction :+z :extreme :max)
    (cut (make-cylinder 25 200)))"))

;;; ============================================================================
;;; Dynamic Variable for Current Context
;;; ============================================================================

(defvar *context* nil
  "Special variable holding the current modeling context")

;;; ============================================================================
;;; Constructor
;;; ============================================================================

(defun make-context ()
  "Create a new modeling context.

  Returns: New modeling-context instance

  The context starts with:
  - No current shape (nil)
  - Empty selection stack
  - Default XY plane as current workplane

  Example:
    (let ((ctx (make-context)))
      (setf (current-shape ctx) (make-box 100 100 100))
      ...)"
  (make-instance 'modeling-context))

;;; ============================================================================
;;; with-context Macro
;;; ============================================================================

(defmacro with-context ((&key initial-shape) &body body)
  "Execute body with a new modeling context.

  Args:
    :initial-shape - Optional initial shape for the context

  The macro binds *context* to a new modeling context and evaluates
  the body forms. Operations within the body can reference *context*
  implicitly.

  Example:
    (with-context ()
      (add (make-box 100 100 100))
      (add (make-cylinder 50 200))
      (get-result))"

  `(let ((*context* (make-context)))
     ,@(when initial-shape
         `((setf (current-shape *context*) ,initial-shape)))
     ,@body))

;;; ============================================================================
;;; State Query Functions (work on *context*)
;;; ============================================================================

(defun current-shape (&optional (ctx *context*))
  "Get the current shape from the context.

  Args:
    ctx - Context (defaults to *context*)

  Returns: Current shape or nil"
  (slot-value ctx 'current-shape))

(defun (setf current-shape) (value &optional (ctx *context*))
  "Set the current shape in the context.

  Args:
    value - New shape value
    ctx - Context (defaults to *context*)

  Returns: The new value"
  (setf (slot-value ctx 'current-shape) value))

(defun current-selection (&optional (ctx *context*))
  "Get the current selection from the context.

  Args:
    ctx - Context (defaults to *context*)

  Returns: List of currently selected sub-shapes"
  (first (selection-stack ctx)))

(defun current-workplane (&optional (ctx *context*))
  "Get the current workplane from the context.

  Args:
    ctx - Context (defaults to *context*)

  Returns: Current workplane"
  (slot-value ctx 'current-workplane))

;;; ============================================================================
;;; Shape Operations
;;; ============================================================================

(defun add (new-shape &optional (ctx *context*))
  "Add a shape to the context.

  Args:
    new-shape - Shape to add (core shape from clad.core)
    ctx - Context (defaults to *context*)

  Returns: The context

  If no current shape exists, sets new-shape as current.
  If a current shape exists, unions new-shape with it.

  Example:
    (with-context ()
      (add (make-box 100 100 100))
      (add (make-cylinder 50 200)))  ; unions with box"

  (let ((current (current-shape ctx)))
    (if (null current)
        ;; First shape - just set it
        (setf (current-shape ctx)
              (clad.shapes:wrap-shape new-shape 'clad.shapes:cad-solid))
        ;; Union with existing shape
        (let* ((unwrapped-current (clad.shapes:unwrap-shape current))
               (unioned (union-shapes unwrapped-current new-shape)))
          (setf (current-shape ctx)
                (clad.shapes:wrap-shape unioned 'clad.shapes:cad-solid)))))
  ctx)

(defun union-op (shape-to-union &optional (ctx *context*))
  "Union a shape with the current shape (alias for add).

  Args:
    shape-to-union - Shape to union
    ctx - Context (defaults to *context*)

  Returns: The context"
  (add shape-to-union ctx))

(defun cut-op (shape-to-cut &optional (ctx *context*))
  "Cut (subtract) a shape from the current shape.

  Args:
    shape-to-cut - Shape to subtract
    ctx - Context (defaults to *context*)

  Returns: The context

  Example:
    (with-context ()
      (add (make-box 100 100 100))
      (cut-op (make-cylinder 25 200)))  ; cuts hole through box"

  (let ((current (current-shape ctx)))
    (unless current
      (error "Cannot cut from empty context"))

    (let* ((unwrapped-current (clad.shapes:unwrap-shape current))
           (cut-result (cut-shapes unwrapped-current shape-to-cut)))
      (setf (current-shape ctx)
            (clad.shapes:wrap-shape cut-result 'clad.shapes:cad-solid))))
  ctx)

(defun intersect-op (shape-to-intersect &optional (ctx *context*))
  "Intersect the current shape with another shape.

  Args:
    shape-to-intersect - Shape to intersect with
    ctx - Context (defaults to *context*)

  Returns: The context"

  (let ((current (current-shape ctx)))
    (unless current
      (error "Cannot intersect with empty context"))

    (let* ((unwrapped-current (clad.shapes:unwrap-shape current))
           (intersect-result (intersect-shapes unwrapped-current shape-to-intersect)))
      (setf (current-shape ctx)
            (clad.shapes:wrap-shape intersect-result 'clad.shapes:cad-solid))))
  ctx)

;;; ============================================================================
;;; Result Extraction
;;; ============================================================================

(defun get-result (&optional (ctx *context*))
  "Extract the final result shape from the context.

  Args:
    ctx - Context (defaults to *context*)

  Returns: The current shape (unwrapped core shape)

  Example:
    (with-context ()
      (add (make-box 100 100 100))
      (get-result))  => shape"

  (let ((wrapped-shape (current-shape ctx)))
    (if wrapped-shape
        (clad.shapes:unwrap-shape wrapped-shape)
        nil)))

;;; ============================================================================
;;; Selection Operations
;;; ============================================================================

(defun select-faces (selector-spec &rest args)
  "Select faces from the current shape and push onto selection stack.

  Args:
    selector-spec - Selector specification (see clad.selectors:select)
    args - Additional arguments for the selector

  Returns: The context

  Example:
    (select-faces :direction :+z :extreme :max)
    => Selects top face"

  (let* ((ctx *context*)
         (current (current-shape ctx)))
    (unless current
      (error "Cannot select from empty context"))

    ;; Get faces from current shape
    (let* ((all-faces (faces current))
           ;; Apply selector - args already contains keyword pairs
           (selected (apply #'select all-faces selector-spec args)))

      ;; Push selection onto stack
      (push selected (selection-stack ctx))
      ctx)))

(defun select-edges (selector-spec &rest args)
  "Select edges from the current shape and push onto selection stack.

  Args:
    selector-spec - Selector specification
    args - Additional arguments for the selector

  Returns: The context"

  (let* ((ctx *context*)
         (current (current-shape ctx)))
    (unless current
      (error "Cannot select from empty context"))

    (let* ((all-edges (edges current))
           (selected (apply #'select all-edges selector-spec args)))
      (push selected (selection-stack ctx))
      ctx)))

(defun push-selection (selection &optional (ctx *context*))
  "Push a selection onto the selection stack.

  Args:
    selection - List of selected shapes
    ctx - Context (defaults to *context*)

  Returns: The context"
  (push selection (selection-stack ctx))
  ctx)

(defun pop-selection (&optional (ctx *context*))
  "Pop a selection from the selection stack.

  Args:
    ctx - Context (defaults to *context*)

  Returns: The popped selection"
  (pop (selection-stack ctx)))

;;; ============================================================================
;;; Workplane Management
;;; ============================================================================

(defun set-workplane (workplane &optional (ctx *context*))
  "Set the current workplane.

  Args:
    workplane - New workplane
    ctx - Context (defaults to *context*)

  Returns: The context"
  (setf (slot-value ctx 'current-workplane) workplane)
  ctx)

(defun push-workplane (workplane &optional (ctx *context*))
  "Push current workplane onto stack and set new workplane.

  Args:
    workplane - New workplane
    ctx - Context (defaults to *context*)

  Returns: The context"
  ;; Push current workplane onto stack
  (push (current-workplane ctx) (workplane-stack ctx))
  ;; Set new workplane
  (setf (slot-value ctx 'current-workplane) workplane)
  ctx)

(defun pop-workplane (&optional (ctx *context*))
  "Pop a workplane from the stack and make it current.

  Args:
    ctx - Context (defaults to *context*)

  Returns: The context"
  (let ((popped (pop (workplane-stack ctx))))
    (when popped
      (setf (slot-value ctx 'current-workplane) popped))
    ctx))

;;; ============================================================================
;;; Fillet Operations (Phase 8)
;;; ============================================================================

(defun fillet-selected (radius &optional (ctx *context*))
  "Apply fillet to currently selected edges.

  Args:
    radius - Fillet radius in mm
    ctx - Context (defaults to *context*)

  Returns: The context

  Example:
    (with-context ()
      (add (make-box 100 100 100))
      (select-edges :parallel :z)
      (fillet-selected 5.0))

  This function applies constant-radius fillets to all currently
  selected edges in the context. The edges must belong to the
  current shape."

  (let ((current (current-shape ctx))
        (selection (current-selection ctx)))

    (unless current
      (error "Cannot fillet without a current shape"))

    (unless selection
      (error "Cannot fillet without selected edges"))

    ;; Unwrap shapes, apply fillet, rewrap
    (let* ((unwrapped-current (clad.shapes:unwrap-shape current))
           (unwrapped-edges (mapcar #'clad.shapes:unwrap-shape selection))
           (filleted (clad.core:fillet unwrapped-current unwrapped-edges radius)))
      (setf (current-shape ctx)
            (clad.shapes:wrap-shape filleted 'clad.shapes:cad-solid))))

  ;; Pop selection after applying fillet
  (pop-selection ctx)
  ctx)

;;; ============================================================================
;;; Chamfer Operations (Phase 8)
;;; ============================================================================

(defun chamfer-selected (distance &optional (ctx *context*))
  "Apply chamfer to currently selected edges.

  Args:
    distance - Chamfer distance in mm (symmetric chamfer)
    ctx - Context (defaults to *context*)

  Returns: The context

  Example:
    (with-context ()
      (add (make-box 100 100 100))
      (select-edges :parallel :z)
      (chamfer-selected 2.0))

  This function applies symmetric chamfers to all currently
  selected edges in the context. The edges must belong to the
  current shape."

  (let ((current (current-shape ctx))
        (selection (current-selection ctx)))

    (unless current
      (error "Cannot chamfer without a current shape"))

    (unless selection
      (error "Cannot chamfer without selected edges"))

    ;; Unwrap shapes, apply chamfer, rewrap
    (let* ((unwrapped-current (clad.shapes:unwrap-shape current))
           (unwrapped-edges (mapcar #'clad.shapes:unwrap-shape selection))
           (chamfered (clad.core:chamfer unwrapped-current unwrapped-edges distance)))
      (setf (current-shape ctx)
            (clad.shapes:wrap-shape chamfered 'clad.shapes:cad-solid))))

  ;; Pop selection after applying chamfer
  (pop-selection ctx)
  ctx)
