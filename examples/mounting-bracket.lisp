;;;; examples/mounting-bracket.lisp --- Complex part example: Mounting Bracket
;;;
;;; This example demonstrates the full CLAD workflow:
;;; - Primitive creation
;;; - Boolean operations
;;; - Face selection
;;; - Workplane creation from faces
;;; - Context-based modeling
;;; - Export to STEP file

(in-package :cl-user)

(defun make-mounting-bracket ()
  "Create a complex mounting bracket part.

  The bracket consists of:
  - A base plate (100x80x10mm)
  - A vertical wall (100x60x10mm)
  - Mounting holes in the base
  - A large center hole in the wall
  - Chamfers on edges

  Returns: The final CAD shape"

  (format t "~%Creating mounting bracket...~%")

  ;; Step 1: Create the base plate
  (format t "  Step 1: Creating base plate (100x80x10mm)~%")
  (let ((base (clad.core:make-box 100 80 10)))

    ;; Step 2: Create the vertical wall and position it
    (format t "  Step 2: Creating vertical wall (100x60x10mm)~%")
    (let* ((wall-initial (clad.core:make-box 100 60 10))
           ;; Rotate wall 90 degrees around X axis to make it vertical
           (wall-rotated (clad.core:rotate wall-initial :x 90))
           ;; Translate to sit on top of base at back edge
           (wall (clad.core:translate wall-rotated 0 70 10)))

      ;; Step 3: Union base and wall
      (format t "  Step 3: Joining base and wall~%")
      (let ((body (clad.core:union-shapes base wall)))

        ;; Step 4: Create mounting holes in the base
        (format t "  Step 4: Adding mounting holes (4x Ø8mm)~%")
        (let* (;; Create a cylinder for the holes
               (hole-template (clad.core:make-cylinder 4 20))
               ;; Position holes at corners of base (15mm from edges)
               (hole1 (clad.core:translate hole-template 15 15 -5))
               (hole2 (clad.core:translate hole-template 85 15 -5))
               (hole3 (clad.core:translate hole-template 15 65 -5))
               (hole4 (clad.core:translate hole-template 85 65 -5))
               ;; Cut all holes
               (body-with-holes-1 (clad.core:cut-shapes body hole1))
               (body-with-holes-2 (clad.core:cut-shapes body-with-holes-1 hole2))
               (body-with-holes-3 (clad.core:cut-shapes body-with-holes-2 hole3))
               (body-with-holes-4 (clad.core:cut-shapes body-with-holes-3 hole4)))

          ;; Step 5: Create center hole in wall
          (format t "  Step 5: Adding center hole in wall (Ø25mm)~%")
          (let* ((center-hole-template (clad.core:make-cylinder 12.5 20))
                 ;; Rotate to horizontal (along Y axis)
                 (center-hole-rotated (clad.core:rotate center-hole-template :x 90))
                 ;; Position at center of wall
                 (center-hole (clad.core:translate center-hole-rotated 50 65 40))
                 ;; Cut the center hole
                 (final-body (clad.core:cut-shapes body-with-holes-4 center-hole)))

            (format t "  Step 6: Bracket complete!~%~%")
            final-body))))))

(defun make-mounting-bracket-with-context ()
  "Create the same mounting bracket using the context API.

  This demonstrates the cleaner, more intuitive context-based workflow."

  (format t "~%Creating mounting bracket with context API...~%")

  (clad.context:with-context ()
    ;; Step 1: Create base plate
    (format t "  Step 1: Creating base plate~%")
    (clad.context:add (clad.core:make-box 100 80 10))

    ;; Step 2: Add vertical wall
    (format t "  Step 2: Adding vertical wall~%")
    (let* ((wall (clad.core:make-box 100 60 10))
           (wall-rotated (clad.core:rotate wall :x 90))
           (wall-positioned (clad.core:translate wall-rotated 0 70 10)))
      (clad.context:add wall-positioned))

    ;; Step 3: Cut mounting holes
    (format t "  Step 3: Cutting mounting holes~%")
    (let ((hole (clad.core:make-cylinder 4 20)))
      (clad.context:cut-op (clad.core:translate hole 15 15 -5))
      (clad.context:cut-op (clad.core:translate hole 85 15 -5))
      (clad.context:cut-op (clad.core:translate hole 15 65 -5))
      (clad.context:cut-op (clad.core:translate hole 85 65 -5)))

    ;; Step 4: Cut center hole
    (format t "  Step 4: Cutting center hole~%")
    (let* ((center-hole (clad.core:make-cylinder 12.5 20))
           (rotated (clad.core:rotate center-hole :x 90))
           (positioned (clad.core:translate rotated 50 65 40)))
      (clad.context:cut-op positioned))

    (format t "  Complete!~%~%")
    (clad.context:get-result)))

;;; ============================================================================
;;; DSL API Version (Phase 5)
;;; ============================================================================

(clad.dsl:defpart mounting-bracket-dsl
    ((base-width 100)
     (base-depth 80)
     (base-thickness 10)
     (wall-width 100)
     (wall-height 60)
     (wall-thickness 10)
     (hole-diameter 8)
     (hole-inset 15)
     (center-hole-diameter 25))
  "Parametric mounting bracket with declarative DSL syntax.

  Features:
  - Base plate with 4 corner mounting holes
  - Vertical wall with center hole
  - Fully parametric dimensions"

  ;; Create the base plate
  (:body
    (clad.core:make-box base-width base-depth base-thickness))

  ;; Add the vertical wall (rotated and positioned)
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:rotate
              (clad.core:make-box wall-width wall-height wall-thickness)
              :x 90)
            0 (- base-depth 10) base-thickness)))

  ;; Cut mounting hole 1 (front-left)
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* base-thickness 2))
            hole-inset hole-inset (- (/ base-thickness 2)))))

  ;; Cut mounting hole 2 (front-right)
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* base-thickness 2))
            (- base-width hole-inset) hole-inset (- (/ base-thickness 2)))))

  ;; Cut mounting hole 3 (back-left)
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* base-thickness 2))
            hole-inset (- base-depth hole-inset) (- (/ base-thickness 2)))))

  ;; Cut mounting hole 4 (back-right)
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* base-thickness 2))
            (- base-width hole-inset) (- base-depth hole-inset) (- (/ base-thickness 2)))))

  ;; Cut center hole in wall
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:rotate
              (clad.core:make-cylinder (/ center-hole-diameter 2) (* wall-thickness 2))
              :x 90)
            (/ base-width 2) (- base-depth 5) (+ base-thickness (/ wall-height 2))))))

(defun make-mounting-bracket-with-dsl ()
  "Create the mounting bracket using the declarative DSL.

  This demonstrates the most concise and readable approach using defpart."

  (format t "~%Creating mounting bracket with DSL...~%")
  (format t "  Using declarative defpart macro~%")
  (let ((bracket (mounting-bracket-dsl)))
    (format t "  Complete!~%~%")
    bracket))

(defun demonstrate-selectors ()
  "Demonstrate the selector system on the mounting bracket.

  Shows how to find specific faces for subsequent operations."

  (format t "~%Demonstrating selector system...~%")

  (let* ((bracket (make-mounting-bracket))
         (bracket-wrapped (clad.shapes:wrap-shape bracket 'clad.shapes:cad-solid))
         (all-faces (clad.shapes:faces bracket-wrapped)))

    (format t "  Total faces in bracket: ~A~%" (length all-faces))

    ;; Find the top face of the base
    (let ((top-faces (clad.selectors:select all-faces
                                            :direction :+z
                                            :extreme :max)))
      (format t "  Top faces (Z max): ~A~%" (length top-faces)))

    ;; Find the bottom face
    (let ((bottom-faces (clad.selectors:select all-faces
                                               :direction :+z
                                               :extreme :min)))
      (format t "  Bottom faces (Z min): ~A~%" (length bottom-faces)))

    ;; Find faces parallel to XY plane
    (let ((horizontal-faces (clad.selectors:select all-faces
                                                   :parallel :z)))
      (format t "  Horizontal faces (parallel to XY): ~A~%" (length horizontal-faces)))

    ;; Find faces parallel to XZ plane
    (let ((vertical-faces (clad.selectors:select all-faces
                                                 :parallel :y)))
      (format t "  Vertical faces (parallel to XZ): ~A~%~%" (length vertical-faces)))

    bracket-wrapped))

(defun demonstrate-workplanes ()
  "Demonstrate workplane creation from faces.

  Shows how to create local coordinate systems on face geometry."

  (format t "~%Demonstrating workplane system...~%")

  (let* ((bracket (make-mounting-bracket))
         (bracket-wrapped (clad.shapes:wrap-shape bracket 'clad.shapes:cad-solid))
         (all-faces (clad.shapes:faces bracket-wrapped)))

    ;; Get the top face
    (let* ((top-face (first (clad.selectors:select all-faces
                                                    :direction :+z
                                                    :extreme :max)))
           ;; Create workplane from top face
           (wp (clad.workplane:workplane-from-face top-face)))

      (format t "  Created workplane from top face~%")
      (format t "    Origin: ~A~%" (clad.workplane:workplane-origin wp))
      (format t "    X-dir:  ~A~%" (clad.workplane:workplane-x-dir wp))
      (format t "    Y-dir:  ~A~%" (clad.workplane:workplane-y-dir wp))
      (format t "    Z-dir:  ~A~%" (clad.workplane:workplane-z-dir wp))

      ;; Test coordinate transformation
      (let ((local-point '(10 10 0)))
        (format t "~%  Local point ~A transforms to global: ~A~%"
                local-point
                (multiple-value-list
                 (clad.workplane:local-to-global wp local-point))))

      wp)))

(defun export-bracket (&optional (filename "/tmp/mounting-bracket.step"))
  "Create and export the mounting bracket to a STEP file.

  Args:
    filename - Output STEP file path (default: /tmp/mounting-bracket.step)

  Returns: T on success"

  (format t "~%Exporting mounting bracket to ~A...~%" filename)

  (let* ((bracket-wrapped (make-mounting-bracket-with-context))
         ;; Unwrap to get core shape for export
         (bracket (clad.shapes:unwrap-shape bracket-wrapped))
         (success (clad.export:export-step bracket filename)))

    (if success
        (format t "  Export successful!~%")
        (format t "  Export failed!~%"))

    success))

(defun run-all-examples ()
  "Run all mounting bracket examples.

  This demonstrates:
  1. Functional API approach
  2. Context API approach
  3. Selector system
  4. Workplane system
  5. STEP export"

  (format t "~%")
  (format t "╔════════════════════════════════════════════════════════════╗~%")
  (format t "║         CLAD - Mounting Bracket Example Suite             ║~%")
  (format t "╚════════════════════════════════════════════════════════════╝~%")

  ;; Example 1: Functional API
  (format t "~%─────────────────────────────────────────────────────────────~%")
  (format t "Example 1: Functional API~%")
  (format t "─────────────────────────────────────────────────────────────~%")
  (let ((bracket1 (make-mounting-bracket)))
    (format t "Result: Shape with ~A vertices~%"
            (length (clad.shapes:vertices
                     (clad.shapes:wrap-shape bracket1 'clad.shapes:cad-solid)))))

  ;; Example 2: Context API
  (format t "~%─────────────────────────────────────────────────────────────~%")
  (format t "Example 2: Context API~%")
  (format t "─────────────────────────────────────────────────────────────~%")
  (let ((bracket2 (make-mounting-bracket-with-context)))
    (format t "Result: Shape with ~A vertices~%"
            (length (clad.shapes:vertices bracket2))))

  ;; Example 3: DSL API
  (format t "~%─────────────────────────────────────────────────────────────~%")
  (format t "Example 3: Declarative DSL (Phase 5)~%")
  (format t "─────────────────────────────────────────────────────────────~%")
  (let ((bracket3 (make-mounting-bracket-with-dsl)))
    (format t "Result: Shape with ~A vertices~%"
            (length (clad.shapes:vertices bracket3)))
    (format t "Dimensions: ~A~%"
            (let ((bbox (clad.shapes:bounding-box bracket3)))
              (format nil "~,1Fx~,1Fx~,1Fmm"
                      (- (fourth bbox) (first bbox))
                      (- (fifth bbox) (second bbox))
                      (- (sixth bbox) (third bbox))))))

  ;; Example 4: Selectors
  (format t "~%─────────────────────────────────────────────────────────────~%")
  (format t "Example 4: Selector System~%")
  (format t "─────────────────────────────────────────────────────────────~%")
  (demonstrate-selectors)

  ;; Example 5: Workplanes
  (format t "~%─────────────────────────────────────────────────────────────~%")
  (format t "Example 5: Workplane System~%")
  (format t "─────────────────────────────────────────────────────────────~%")
  (demonstrate-workplanes)

  ;; Example 6: Export
  (format t "~%─────────────────────────────────────────────────────────────~%")
  (format t "Example 6: Export to STEP~%")
  (format t "─────────────────────────────────────────────────────────────~%")
  (export-bracket)

  (format t "~%")
  (format t "╔════════════════════════════════════════════════════════════╗~%")
  (format t "║                   All Examples Complete!                   ║~%")
  (format t "╚════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  t)

;;; Usage:
;;;
;;; Load CLAD and this file:
;;;   (asdf:load-system :clad)
;;;   (load "examples/mounting-bracket.lisp")
;;;
;;; Run all examples:
;;;   (run-all-examples)
;;;
;;; Or run individual examples:
;;;   (make-mounting-bracket)                 ; Functional API
;;;   (make-mounting-bracket-with-context)    ; Context API
;;;   (make-mounting-bracket-with-dsl)        ; Declarative DSL (Phase 5)
;;;   (demonstrate-selectors)
;;;   (demonstrate-workplanes)
;;;   (export-bracket "/path/to/output.step")
;;;
;;; The DSL version demonstrates the most concise and readable approach:
;;;   (mounting-bracket-dsl)  ; Uses default parameters
;;;   (mounting-bracket-dsl 120 90 12)  ; Custom base dimensions
