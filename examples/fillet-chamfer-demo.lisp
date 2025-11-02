;;;; examples/fillet-chamfer-demo.lisp --- Fillet & Chamfer Demo
;;;
;;; QUICKSTART:
;;;
;;; 1. Load this file: (load "examples/fillet-chamfer-demo.lisp")
;;; 2. View in browser: (quick-view)
;;;
;;; This demonstrates Phase 8 advanced features:
;;; - Fillet operations (rounded edges)
;;; - Chamfer operations (beveled edges)
;;; - Edge selection

(require :asdf)
(asdf:load-system :clad)

(in-package :cl-user)

;;;; ============================================================================
;;;; Simple Part with Fillets and Chamfers
;;;; ============================================================================

(defun make-filleted-chamfered-block ()
  "Create a block demonstrating both fillets and chamfers.

  This creates:
  - An 80x80x30mm block with center hole
  - Filleted vertical edges (6mm radius - rounded corners)
  - Chamfered top edges (3mm - beveled top)"

  (format t "~%Creating block with fillets and chamfers...~%")

  (clad.context:with-context ()
    ;; Step 1: Create base box
    (format t "  Step 1: Creating 80x80x30mm box~%")
    (clad.context:add (clad.core:make-box 80 80 30))

    ;; Step 2: Cut center hole
    (format t "  Step 2: Cutting center hole (Ø25mm)~%")
    (clad.context:cut-op (clad.core:translate
                           (clad.core:make-cylinder 25 60)
                           40 40 -15))

    ;; Step 3: Fillet vertical edges (creates rounded corners)
    (format t "  Step 3: Filleting vertical edges (R=6mm)~%")
    (clad.context:select-edges :type :line)
    (clad.context:select-edges :parallel :z)
    (clad.context:fillet-selected 12.0d0)

    ;; Step 4: Chamfer top edges (creates beveled top)
    (format t "  Step 4: Chamfering top edges (3mm)~%")
    ;; Get top face
    (clad.context:select-faces :direction :+z :extreme :max)
    (let* ((top-faces (clad.context:current-selection))
           (top-face (first top-faces))
           (top-edges (clad.shapes:edges top-face)))
      (clad.context:pop-selection)
      (clad.context:push-selection top-edges))
    (clad.context:chamfer-selected 3.0d0)

    (format t "  Complete!~%~%")
    (clad.context:get-result)))

;;;; ============================================================================
;;;; Viewer Function
;;;; ============================================================================

(defun quick-view ()
  "Create and display the fillet/chamfer demo in the browser."
  (format t "~%")
  (format t "╔═══════════════════════════════════════════════════════════╗~%")
  (format t "║  CLAD Phase 8: Fillet & Chamfer Demo                     ║~%")
  (format t "╚═══════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  (let ((part (make-filleted-chamfered-block)))
    (format t "Opening browser...~%")
    (format t "~%")
    (clad:view part :name "filleted-block")
    (format t "~%")
    (format t "═══════════════════════════════════════════════════════════~%")
    (format t "~%")
    (format t "WHAT TO LOOK FOR:~%")
    (format t "~%")
    (format t "  • Rounded vertical corners (6mm fillets)~%")
    (format t "    - Smooth curves where edges meet~%")
    (format t "    - Especially visible at the 4 outer corners~%")
    (format t "~%")
    (format t "  • Beveled top edges (3mm chamfers)~%")
    (format t "    - Flat 45° bevels on all top edges~%")
    (format t "    - Both outer perimeter and hole edge~%")
    (format t "~%")
    (format t "  • Center through-hole~%")
    (format t "~%")
    (format t "TIP: Rotate the view to see the 3D features!~%")
    (format t "~%")
    (format t "View at: http://localhost:8080/?model=/models/filleted-block.glb~%")
    (format t "~%")
    part))

;;;; ============================================================================
;;;; Additional Examples
;;;; ============================================================================

(defun make-filleted-only ()
  "Create a simple block with only fillets (no chamfers)."
  (format t "~%Creating filleted block (no chamfers)...~%")

  (clad.context:with-context ()
    (format t "  Creating 60x60x20mm box~%")
    (clad.context:add (clad.core:make-box 60 60 20))

    (format t "  Filleting all vertical edges (R=5mm)~%")
    (clad.context:select-edges :parallel :z)
    (clad.context:fillet-selected 5.0d0)

    (format t "  Complete!~%~%")
    (clad.context:get-result)))

(defun make-chamfered-only ()
  "Create a simple block with only chamfers (no fillets)."
  (format t "~%Creating chamfered block (no fillets)...~%")

  (clad.context:with-context ()
    (format t "  Creating 60x60x20mm box~%")
    (clad.context:add (clad.core:make-box 60 60 20))

    (format t "  Chamfering top edges (3mm)~%")
    (clad.context:select-faces :direction :+z :extreme :max)
    (let* ((top-face (first (clad.context:current-selection)))
           (top-edges (clad.shapes:edges top-face)))
      (clad.context:pop-selection)
      (clad.context:push-selection top-edges))
    (clad.context:chamfer-selected 3.0d0)

    (format t "  Complete!~%~%")
    (clad.context:get-result)))

(defun view-filleted ()
  "View block with only fillets."
  (let ((part (make-filleted-only)))
    (clad:view part :name "filleted-only")
    (format t "~%View at: http://localhost:8080/?model=/models/filleted-only.glb~%")
    part))

(defun view-chamfered ()
  "View block with only chamfers."
  (let ((part (make-chamfered-only)))
    (clad:view part :name "chamfered-only")
    (format t "~%View at: http://localhost:8080/?model=/models/chamfered-only.glb~%")
    part))

;;;; ============================================================================
;;;; Export Function
;;;; ============================================================================

(defun export-demos (&optional (base-path "/tmp"))
  "Export all demo parts to STEP files."
  (format t "~%Exporting demos to STEP files...~%")

  ;; Main demo
  (let* ((part1 (make-filleted-chamfered-block))
         (unwrapped1 (clad.shapes:unwrap-shape part1))
         (file1 (format nil "~A/filleted-chamfered-block.step" base-path)))
    (clad.export:export-step unwrapped1 file1)
    (format t "  Exported: ~A~%" file1))

  ;; Filleted only
  (let* ((part2 (make-filleted-only))
         (unwrapped2 (clad.shapes:unwrap-shape part2))
         (file2 (format nil "~A/filleted-block.step" base-path)))
    (clad.export:export-step unwrapped2 file2)
    (format t "  Exported: ~A~%" file2))

  ;; Chamfered only
  (let* ((part3 (make-chamfered-only))
         (unwrapped3 (clad.shapes:unwrap-shape part3))
         (file3 (format nil "~A/chamfered-block.step" base-path)))
    (clad.export:export-step unwrapped3 file3)
    (format t "  Exported: ~A~%" file3))

  (format t "~%All demos exported successfully!~%")
  (format t "Open in FreeCAD, Fusion 360, or other CAD software.~%")
  t)

;;;; ============================================================================
;;;; Auto-Load Message
;;;; ============================================================================

(format t "~%")
(format t "╔═══════════════════════════════════════════════════════════╗~%")
(format t "║  Fillet & Chamfer Demo Loaded!                            ║~%")
(format t "╚═══════════════════════════════════════════════════════════╝~%")
(format t "~%")
(format t "QUICKSTART:~%")
(format t "~%")
(format t "  (quick-view)           - View main demo in browser~%")
(format t "~%")
(format t "ADDITIONAL EXAMPLES:~%")
(format t "~%")
(format t "  (view-filleted)        - Block with only fillets~%")
(format t "  (view-chamfered)       - Block with only chamfers~%")
(format t "  (export-demos)         - Export all to /tmp/*.step~%")
(format t "~%")

;;; Usage:
;;;
;;; Load and view:
;;;   (load "examples/fillet-chamfer-demo.lisp")
;;;   (quick-view)
;;;
;;; Alternative examples:
;;;   (view-filleted)
;;;   (view-chamfered)
;;;
;;; Export to STEP:
;;;   (export-demos)
