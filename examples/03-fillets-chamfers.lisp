;;;; examples/03-fillets-chamfers.lisp --- Fillet and Chamfer Examples
;;;;
;;;; This file demonstrates edge rounding and chamfering in CLAD.
;;;; Run with: (load "examples/03-fillets-chamfers.lisp")

(asdf:load-system :clad)
(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Simple Fillet using Context API
;;; ============================================================================

(defun demo-simple-fillet ()
  (format t "~%Example 1: Simple Fillet~%")
  (format t "========================~%~%")
  (format t "Creating a filleted box using context API~%")
  (format t "Selecting and filleting vertical edges~%")

  (clad.context:with-context ()
    ;; Create a box
    (clad.context:add (clad.core:make-box 60 40 30))

    ;; Select all vertical edges (parallel to Z axis)
    (clad.context:select-edges :parallel :z)

    ;; Apply 5mm fillet to selected edges
    (clad.context:fillet-selected 5.0d0)

    ;; Get the result
    (clad.context:get-result)))

;;; ============================================================================
;;; Example 2: Selective Filleting
;;; ============================================================================

(defun demo-selective-fillet ()
  (format t "~%Example 2: Selective Fillet~%")
  (format t "===========================~%~%")
  (format t "Filleting only top edges of a box~%")
  (format t "Using combined selectors: direction AND parallel~%")

  (clad.context:with-context ()
    ;; Create base
    (clad.context:add (clad.core:make-box 80 80 20))

    ;; Select only the top horizontal edges
    ;; (edges on top face that are parallel to XY plane)
    (clad.context:select-edges :type :line)

    ;; Apply fillet
    (clad.context:fillet-selected 3.0d0)

    (clad.context:get-result)))

;;; ============================================================================
;;; Example 3: Chamfer Example
;;; ============================================================================

(defun demo-simple-chamfer ()
  (format t "~%Example 3: Simple Chamfer~%")
  (format t "=========================~%~%")
  (format t "Creating a chamfered box~%")
  (format t "Chamfering all vertical edges~%")

  (clad.context:with-context ()
    ;; Create box
    (clad.context:add (clad.core:make-box 50 50 40))

    ;; Select vertical edges
    (clad.context:select-edges :parallel :z)

    ;; Apply 2mm chamfer
    (clad.context:chamfer-selected 2.0d0)

    (clad.context:get-result)))

;;; ============================================================================
;;; Example 4: Complex Part with Mixed Fillets/Chamfers
;;; ============================================================================

(defun demo-mixed-operations ()
  (format t "~%Example 4: Mixed Fillets and Chamfers~%")
  (format t "======================================~%~%")
  (format t "Creating a part with both fillets and chamfers~%")

  (clad.context:with-context ()
    ;; Create base with boss
    (clad.context:add (clad.core:make-box 100 100 15))
    (clad.context:add (clad.core:translate
                        (clad.core:make-cylinder 30 25)
                        50 50 15))

    ;; Fillet the vertical edges of the base
    (clad.context:select-edges :parallel :z)
    (clad.context:fillet-selected 4.0d0)

    ;; Note: After fillet, we can chamfer other edges
    ;; Select top circular edges and chamfer them
    (clad.context:select-edges :type :circle)
    (clad.context:chamfer-selected 2.0d0)

    (clad.context:get-result)))

;;; ============================================================================
;;; Example 5: Practical Part - Filleted Mounting Bracket
;;; ============================================================================

(defun demo-filleted-bracket ()
  (format t "~%Example 5: Filleted Mounting Bracket~%")
  (format t "=====================================~%~%")
  (format t "A practical example with strategic filleting~%")
  (format t "Fillets reduce stress concentration~%")

  (clad.context:with-context ()
    ;; Base plate
    (clad.context:add (clad.core:make-box 80 60 8))

    ;; Vertical bracket
    (clad.context:add (clad.core:translate
                        (clad.core:make-box 60 8 40)
                        10 26 8))

    ;; Mounting holes in base
    (clad.context:add (clad.core:make-box 80 60 8))  ; Re-select base
    (clad.context:select-faces :direction :+z :extreme :max)
    (clad.context:cut-op (clad.core:translate
                           (clad.core:make-cylinder 4 12)
                           15 15 0))
    (clad.context:cut-op (clad.core:translate
                           (clad.core:make-cylinder 4 12)
                           65 15 0))
    (clad.context:cut-op (clad.core:translate
                           (clad.core:make-cylinder 4 12)
                           15 45 0))
    (clad.context:cut-op (clad.core:translate
                           (clad.core:make-cylinder 4 12)
                           65 45 0))

    ;; Fillet the vertical edge between base and bracket (stress relief)
    (clad.context:select-edges :parallel :x)
    (clad.context:fillet-selected 6.0d0)

    (clad.context:get-result)))


;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(defun run-fillet-chamfer-demos ()
  "Run all fillet and chamfer demonstrations"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                   CLAD Fillet & Chamfer Examples~%")
  (format t "================================================================================~%")

  (let ((part1 (demo-simple-fillet))
        (part2 (demo-selective-fillet))
        (part3 (demo-simple-chamfer))
        (part4 (demo-mixed-operations))
        (part5 (demo-filleted-bracket)))

    (format t "~%~%")
    (format t "================================================================================~%")
    (format t "All fillet and chamfer examples completed!~%")
    (format t "~%To visualize (parts are in variables part1-5):~%")
    (format t "  (clad:view part1 :name \"simple-fillet\")~%")
    (format t "  (clad:view part2 :name \"selective-fillet\")~%")
    (format t "  (clad:view part3 :name \"simple-chamfer\")~%")
    (format t "  (clad:view part4 :name \"mixed-ops\")~%")
    (format t "  (clad:view part5 :name \"filleted-bracket\")~%")
    (format t "~%Or run individual demos:~%")
    (format t "  (demo-simple-fillet)~%")
    (format t "  (demo-simple-chamfer)~%")
    (format t "================================================================================~%")
    (format t "~%")

    (values part1 part2 part3 part4 part5)))

;; Auto-run when loaded
(run-fillet-chamfer-demos)
