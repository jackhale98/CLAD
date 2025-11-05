;;;; examples/03-fillets-chamfers.lisp --- Fillet and Chamfer Examples
;;;;
;;;; This file demonstrates edge rounding and chamfering using defpart DSL.
;;;; Updated for centered primitives!
;;;; Run with: (load "examples/03-fillets-chamfers.lisp")

(asdf:load-system :clad)
(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Simple Fillet using defpart DSL
;;; ============================================================================

(clad.dsl:defpart simple-filleted-box
    ((width 60) (height 40) (depth 30) (fillet-radius 5.0d0))
  "A box with filleted vertical edges"
  (:body (clad.core:make-box width height depth))
  (:on-edge :parallel :z
    (:fillet fillet-radius)))

(defun demo-simple-fillet ()
  (format t "~%Example 1: Simple Fillet~%")
  (format t "========================~%~%")
  (format t "Creating a filleted box using defpart DSL~%")
  (format t "Primitives are now centered - simpler positioning!~%")
  (format t "Selecting and filleting vertical edges with :on-edge~%")

  (simple-filleted-box))

;;; ============================================================================
;;; Example 2: Selective Filleting
;;; ============================================================================

(clad.dsl:defpart selective-filleted-plate
    ((size 80) (thickness 20) (fillet-radius 3.0d0))
  "Plate with filleted line-type edges"
  (:body (clad.core:make-box size size thickness))
  (:on-edge :type :line
    (:fillet fillet-radius)))

(defun demo-selective-fillet ()
  (format t "~%Example 2: Selective Fillet~%")
  (format t "===========================~%~%")
  (format t "Filleting only certain edges~%")
  (format t "Using type-based selectors in defpart~%")

  (selective-filleted-plate))

;;; ============================================================================
;;; Example 3: Chamfer Example
;;; ============================================================================

(clad.dsl:defpart simple-chamfered-box
    ((size 50) (depth 40) (chamfer-distance 2.0d0))
  "A box with chamfered vertical edges"
  (:body (clad.core:make-box size size depth))
  (:on-edge :parallel :z
    (:chamfer chamfer-distance)))

(defun demo-simple-chamfer ()
  (format t "~%Example 3: Simple Chamfer~%")
  (format t "=========================~%~%")
  (format t "Creating a chamfered box using defpart~%")
  (format t "Chamfering all vertical edges with :on-edge~%")

  (simple-chamfered-box))

;;; ============================================================================
;;; Example 4: Complex Part with Boss and Fillets
;;; ============================================================================

(clad.dsl:defpart part-with-boss-and-fillets
    ((size 100) (thickness 15) (boss-diameter 30) (boss-height 25) (fillet-radius 4.0d0))
  "Part with boss using centered primitives and filleted edges"
  (:body (clad.core:make-box size size thickness))
  ;; Add boss at center (cylinders are centered!)
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder (/ boss-diameter 2) boss-height)
            0 0 thickness)))
  ;; Fillet the vertical edges
  (:on-edge :parallel :z
    (:fillet fillet-radius)))

(defun demo-mixed-operations ()
  (format t "~%Example 4: Part with Boss and Fillets~%")
  (format t "======================================~%~%")
  (format t "Creating a part with boss using centered primitives~%")
  (format t "Boss positioning is much simpler with centering!~%")

  (part-with-boss-and-fillets))

;;; ============================================================================
;;; Example 5: Practical Part - Filleted Mounting Bracket
;;; ============================================================================

(clad.dsl:defpart filleted-bracket
    ((length 80) (width 60) (thickness 8) (fillet-radius 2.0d0))
  "A simplified mounting bracket with strategic filleting"
  ;; Base plate (centered on XY)
  (:body (clad.core:make-box length width thickness))

  ;; Fillet vertical edges (simpler geometry to avoid crashes)
  (:on-edge :parallel :z
    (:fillet fillet-radius)))

(defun demo-filleted-bracket ()
  (format t "~%Example 5: Filleted Mounting Bracket~%")
  (format t "=====================================~%~%")
  (format t "A practical example with strategic filleting~%")
  (format t "Centered primitives make this design much cleaner!~%")
  (format t "Fillets reduce stress concentration~%")

  (filleted-bracket))


;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(defun run-fillet-chamfer-demos ()
  "Run all fillet and chamfer demonstrations"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                   CLAD Fillet & Chamfer Examples~%")
  (format t "                  (Updated for Centered Primitives & defpart DSL!)~%")
  (format t "================================================================================~%")

  (let ((part1 (demo-simple-fillet))
        (part2 (demo-selective-fillet))
        (part3 (demo-simple-chamfer))
        (part4 (demo-mixed-operations))
        (part5 (demo-filleted-bracket)))

    (format t "~%~%")
    (format t "================================================================================~%")
    (format t "All fillet and chamfer examples completed!~%")
    (format t "~%Notice how defpart DSL makes edge operations simple:~%")
    (format t "  - :on-edge selector with :parallel, :type options~%")
    (format t "  - :fillet and :chamfer operations on selected edges~%")
    (format t "  - Centered primitives simplify positioning!~%")
    (format t "~%To visualize (parts are in variables part1-5):~%")
    (format t "  (clad:view part1 :name \\\"simple-fillet\\\")~%")
    (format t "  (clad:view part2 :name \\\"selective-fillet\\\")~%")
    (format t "  (clad:view part3 :name \\\"simple-chamfer\\\")~%")
    (format t "  (clad:view part4 :name \\\"mixed-ops\\\")~%")
    (format t "  (clad:view part5 :name \\\"filleted-bracket\\\")~%")
    (format t "~%Or call part functions directly with custom parameters:~%")
    (format t "  (simple-filleted-box :fillet-radius 10.0d0)~%")
    (format t "  (simple-chamfered-box :chamfer-distance 5.0d0)~%")
    (format t "================================================================================~%")
    (format t "~%")

    (values part1 part2 part3 part4 part5)))

;; Auto-run when loaded
(run-fillet-chamfer-demos)
