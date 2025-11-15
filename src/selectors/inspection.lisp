;;;; src/selectors/inspection.lisp --- Selector Debugging and Inspection Tools

(in-package :clad.selectors)

;;; ============================================================================
;;; Inspection Function - REPL Tool
;;; ============================================================================

(defun inspect-selection (shape-list selector-spec &rest args)
  "Inspect what a selector would select, returning detailed report.

  This function applies a selector to a shape list and returns comprehensive
  information about the selection for debugging and understanding purposes.

  Arguments:
    shape-list - List of shapes (faces, edges, etc.) to select from
    selector-spec - Selector type keyword (:direction, :type, :at-z, etc.)
    args - Additional selector arguments

  Returns: Property list with:
    :count - Number of entities selected (integer)
    :shapes - List of selected shape objects
    :types - Geometry types of each entity (list of keywords)
    :centers - Center points of each entity (list of (x y z) lists)
    :descriptions - Human-readable descriptions (list of strings)

  Examples:
    ;; Inspect planar faces
    (inspect-selection faces :type :plane)
    => (:count 6 :shapes (...) :types (:plane :plane ...) ...)

    ;; Inspect with position selector
    (inspect-selection faces :at-z 50.0 :tolerance 0.1)
    => (:count 1 :shapes (...) :types (:plane) ...)

    ;; Inspect with combinators
    (inspect-selection faces :and :type :plane :direction :+z)
    => (:count 1 :shapes (...) :types (:plane) ...)

  Usage in REPL:
    CL-USER> (let ((box (make-box 100 100 50)))
               (print-selection-report
                 (inspect-selection (faces box) :type :plane)))
    Selection Report:
      Count: 6
      Entities:
        1. PLANE at (50.00, 0.00, 0.00) ...
        2. PLANE at (-50.00, 0.00, 0.00) ...
        ..."

  (when (null shape-list)
    (return-from inspect-selection
      (list :count 0
            :shapes nil
            :types nil
            :centers nil
            :descriptions nil)))

  ;; Apply selector to get selection
  (let ((selected (apply #'select shape-list selector-spec args)))

    ;; Build report
    (list :count (length selected)
          :shapes selected
          :types (mapcar #'entity-type selected)
          :centers (mapcar #'entity-center selected)
          :descriptions (mapcar #'describe-entity selected))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun entity-type (entity)
  "Get geometry type of entity as keyword.

  Returns:
    :plane, :cylinder, :sphere for faces
    :line, :circle, :arc, :spline for edges
    :unknown for other types"

  (handler-case
      (clad.shapes:geom-type entity)
    (error () :unknown)))

(defun entity-center (entity)
  "Get center point of entity's bounding box as (x y z) list.

  Returns: List of three double-floats (x y z)"

  (handler-case
      (let ((bbox (clad.shapes:bounding-box entity)))
        (list (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0)
              (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0)
              (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
    (error () (list 0.0d0 0.0d0 0.0d0))))

(defun describe-entity (entity)
  "Generate human-readable description of entity.

  Returns: String like 'PLANE at (50.00, 0.00, 25.00)'"

  (let* ((type (entity-type entity))
         (center (entity-center entity)))
    (format nil "~A at (~,2F, ~,2F, ~,2F)"
            type
            (first center)
            (second center)
            (third center))))

;;; ============================================================================
;;; Pretty Printing
;;; ============================================================================

(defun print-selection-report (report &optional (stream *standard-output*))
  "Pretty-print inspection report to stream.

  Arguments:
    report - Property list returned by inspect-selection
    stream - Output stream (defaults to *standard-output*)

  Example output:
    Selection Report:
      Count: 6
      Entities:
        1. PLANE at (50.00, 0.00, 0.00)
        2. PLANE at (-50.00, 0.00, 0.00)
        3. PLANE at (0.00, 50.00, 0.00)
        ..."

  (format stream "~%Selection Report:~%")
  (format stream "  Count: ~A~%" (getf report :count))

  (when (> (getf report :count) 0)
    (format stream "  Entities:~%")
    (loop for desc in (getf report :descriptions)
          for i from 1
          do (format stream "    ~A. ~A~%" i desc)))

  (when (zerop (getf report :count))
    (format stream "  (No entities selected)~%"))

  report)

;;; ============================================================================
;;; Debug Selection - Quick Inspection
;;; ============================================================================

(defun debug-selection (shape-list selector-spec &rest args)
  "Debug a selection by printing report and returning selected shapes.

  This is a convenience function for quick debugging in the REPL or during
  part builds. It combines inspect-selection and print-selection-report,
  then returns the selected shapes for further use.

  Arguments:
    shape-list - List of shapes to select from
    selector-spec - Selector type keyword
    args - Additional selector arguments, may include :message

  Keyword Arguments:
    :message - Optional message to print before report (string)

  Returns: List of selected shapes

  Examples:
    ;; Quick debug in REPL
    (debug-selection faces :type :plane)
    ; DEBUG: Selection Report
    ;   Count: 6
    ;   ...

    ;; With custom message
    (debug-selection faces :at-z 50.0 :message \"Top faces\")
    ; DEBUG: Top faces
    ; DEBUG: Selection Report
    ;   Count: 1
    ;   ...

  Usage in defpart (Phase 3.2 DSL integration):
    (:body (make-box 100 100 50))
    (:debug-selection :on-face :type :plane)
    (:on-face :type :plane
      (:fillet 2.0d0))"

  ;; Extract optional :message argument from args
  ;; args contains selector arguments plus possibly :message
  (let* ((message-pos (position :message args))
         (message (when message-pos (nth (1+ message-pos) args)))
         (selector-args (if message-pos
                           ;; Remove :message and its value from args
                           (append (subseq args 0 message-pos)
                                  (when (< (+ message-pos 2) (length args))
                                    (subseq args (+ message-pos 2))))
                           ;; No message, use all args
                           args)))

    ;; Print debug header
    (when message
      (format t "~%DEBUG: ~A~%" message))
    (format t "~%DEBUG: ")

    ;; Get report and print it
    (let ((report (apply #'inspect-selection shape-list selector-spec selector-args)))
      (print-selection-report report *standard-output*)

      ;; Return the selected shapes for chaining
      (getf report :shapes))))

;;; ============================================================================
;;; Viewer Highlighting - Visual Debugging (Phase 3.3)
;;; ============================================================================

(defvar *debug-highlights* nil
  "Global storage for debug highlights.
  Each entry is a plist with :entities (list of shapes) and :color (keyword).")

(defun add-highlight (entities color)
  "Add entities to the highlight list for visual debugging.

  This function stores highlighting information that can be used by the viewer
  to display selected entities with colored wireframes or overlays.

  Arguments:
    entities - List of shape objects (faces, edges, etc.) to highlight
    color - Color keyword (:red, :green, :blue, :yellow, :cyan, :magenta, etc.)

  Returns: The highlight entry that was added

  Examples:
    ;; Highlight all planar faces in red
    (add-highlight (select faces :type :plane) :red)

    ;; Highlight top face in green
    (add-highlight (select faces :direction :+z :extreme :max) :green)

    ;; Highlight edges in blue
    (add-highlight (select edges :type :line) :blue)

  Usage in defpart (future DSL integration):
    (:on-face :type :plane
      (:debug-highlight :color :red)
      (:fillet 2.0d0))

  Note: Highlights persist until cleared with (clear-highlights).
        Use (get-highlights) to retrieve all stored highlights."

  (when (null entities)
    (return-from add-highlight nil))

  (let ((highlight (list :entities entities :color color)))
    (push highlight *debug-highlights*)
    highlight))

(defun get-highlights ()
  "Get all stored debug highlights.

  Returns: List of highlight plists, each containing:
           :entities - List of shape objects
           :color - Color keyword

  Example:
    (get-highlights)
    => ((:entities (...) :color :red)
        (:entities (...) :color :blue))"

  *debug-highlights*)

(defun clear-highlights ()
  "Clear all stored debug highlights.

  This should be called at the start of a new modeling session or
  when you want to reset the visual debugging state.

  Returns: NIL"

  (setf *debug-highlights* nil))

;;; ============================================================================
;;; Export
;;; ============================================================================

;; These functions are already exported via the package definition
;; in src/packages.lisp if they're public API
