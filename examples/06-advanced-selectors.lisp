;;;; examples/06-advanced-selectors.lisp --- Advanced Selector System Examples
;;;;
;;;; This file demonstrates the full power of CLAD's selector system.
;;;; Run with: (load "examples/06-advanced-selectors.lisp")

(asdf:load-system :clad)
(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Direction and Extreme Selectors
;;; ============================================================================

(defun demo-direction-extreme ()
  (format t "~%Example 1: Direction and Extreme Selectors~%")
  (format t "===========================================~%~%")
  (format t "Selecting faces by direction and position~%")

  (clad.context:with-context ()
    ;; Create a tall box
    (clad.context:add (clad.core:make-box 40 40 80))

    ;; Select the TOP face (faces pointing +Z, furthest in +Z)
    (clad.context:select-faces :direction :+z :extreme :max)
    (format t "Selected top face (+Z, max)~%")

    ;; Add a boss on top
    (clad.context:add (clad.core:translate
                        (clad.core:make-cylinder 15 10)
                        20 20 80))

    ;; Select BOTTOM face
    (clad.context:select-faces :direction :-z :extreme :min)
    (format t "Selected bottom face (-Z, min)~%")

    ;; Add feet on bottom
    (clad.context:add (clad.core:translate
                        (clad.core:make-cylinder 8 5)
                        10 10 -5))
    (clad.context:add (clad.core:translate
                        (clad.core:make-cylinder 8 5)
                        30 30 -5))

    (format t "Created box with top boss and bottom feet~%")
    (clad.context:get-result)))

;;; ============================================================================
;;; Example 2: Type-Based Selection
;;; ============================================================================

(defun demo-type-selection ()
  (format t "~%Example 2: Type-Based Selection~%")
  (format t "================================~%~%")
  (format t "Selecting edges by type (line vs circle)~%")

  (clad.context:with-context ()
    ;; Create base with cylindrical boss
    (clad.context:add (clad.core:make-box 60 60 10))
    (clad.context:add (clad.core:translate
                        (clad.core:make-cylinder 20 15)
                        30 30 10))

    ;; Select all LINE edges (straight edges)
    (clad.context:select-edges :type :line)
    (format t "Selected all straight (LINE) edges~%")
    (clad.context:fillet-selected 2.0d0)
    (format t "Applied 2mm fillet to straight edges~%")

    (clad.context:get-result)))

;;; ============================================================================
;;; Example 3: Parallel Edge Selection
;;; ============================================================================

(defun demo-parallel-selection ()
  (format t "~%Example 3: Parallel Edge Selection~%")
  (format t "====================================~%~%")
  (format t "Selecting edges parallel to specific axes~%")

  (clad.context:with-context ()
    ;; Create rectangular block
    (clad.context:add (clad.core:make-box 80 50 30))

    ;; Select edges parallel to X-axis
    (clad.context:select-edges :parallel :x)
    (format t "Selected edges parallel to X-axis~%")
    (clad.context:fillet-selected 3.0d0)
    (format t "Applied 3mm fillet to X-parallel edges~%")

    ;; Select edges parallel to Y-axis
    (clad.context:select-edges :parallel :y)
    (format t "Selected edges parallel to Y-axis~%")
    (clad.context:fillet-selected 4.0d0)
    (format t "Applied 4mm fillet to Y-parallel edges~%")

    (format t "Demonstrated parallel selection on X, Y axes~%")

    (clad.context:get-result)))

;;; ============================================================================
;;; Example 4: Combined Selectors (AND logic)
;;; ============================================================================

(defun demo-combined-selectors ()
  (format t "~%Example 4: Combined Selectors~%")
  (format t "==============================~%~%")
  (format t "Using multiple selector criteria together~%")

  (clad.context:with-context ()
    ;; Create part with multiple features
    (clad.context:add (clad.core:make-box 80 80 40))

    ;; Select edges that are BOTH parallel to Z
    (clad.context:select-edges :parallel :z)
    (format t "Selected edges: parallel to Z~%")
    (clad.context:fillet-selected 4.0d0)
    (format t "Applied 4mm fillet~%")

    ;; Select edges that are LINE type AND parallel to X
    (clad.context:select-edges :type :line :parallel :x)
    (format t "Selected edges: LINE type AND parallel to X~%")
    (clad.context:chamfer-selected 2.0d0)
    (format t "Applied 2mm chamfer~%")

    ;; Select faces pointing UP (+Z) at maximum height
    (clad.context:select-faces :direction :+z :extreme :max)
    (format t "Selected top face: direction +Z, extreme max~%")
    (clad.context:add (clad.core:translate
                        (clad.core:make-cylinder 20 15)
                        40 40 40))
    (format t "Added boss on top~%")

    (clad.context:get-result)))

;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(defun run-selector-demos ()
  "Run all advanced selector demonstrations"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "              CLAD Advanced Selector System Examples~%")
  (format t "================================================================================~%")

  (let ((part1 (demo-direction-extreme))
        (part2 (demo-type-selection))
        (part3 (demo-parallel-selection))
        (part4 (demo-combined-selectors)))

    (format t "~%~%")
    (format t "================================================================================~%")
    (format t "All advanced selector examples completed!~%")
    (format t "~%Selector capabilities demonstrated:~%")
    (format t "  - Direction-based selection (:direction :+x/+y/+z/-x/-y/-z)~%")
    (format t "  - Extreme position (:extreme :min/:max)~%")
    (format t "  - Type-based (:type :line/:circle/:spline)~%")
    (format t "  - Parallel to axis (:parallel :x/:y/:z)~%")
    (format t "  - Combined selectors (AND logic)~%")
    (format t "~%Real-world applications:~%")
    (format t "  - Strategic filleting for stress relief~%")
    (format t "  - Selective edge finishing for manufacturing~%")
    (format t "  - Precise feature placement using direction selectors~%")
    (format t "================================================================================~%")
    (format t "~%")

    (values part1 part2 part3 part4)))

;; Auto-run when loaded
(run-selector-demos)
