;;;; examples/02-patterns.lisp --- Pattern Examples
;;;;
;;;; This file demonstrates linear and circular patterns in CLAD DSL.
;;;; Run with: (load "examples/02-patterns.lisp")

(require :asdf)
(asdf:load-system :clad)
(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Linear Pattern - Bolt Holes
;;; ============================================================================

(clad.dsl:defpart flange-linear
    ((length 200) (width 60) (thickness 10) (hole-count 5))
  "Flange with linearly patterned bolt holes"
  (:body (clad.core:make-box length width thickness))

  ;; Create linear pattern of holes along the length
  ;; With centered primitives, coordinates are relative to centered box
  (:on-face :direction :+z :extreme :max
    (:linear-pattern
        :count hole-count
        :spacing (/ (- length 40) (1- hole-count))
        :direction-x 1
        :direction-y 0
        :start-x (- (/ length 2) 20)
        :start-y 0
      (:cut (clad.core:make-cylinder 4 (* thickness 1.5))))))

(defun demo-linear-pattern ()
  (format t "~%Example 1: Linear Pattern~%")
  (format t "=========================~%~%")
  (format t "Flange: 200x60x10mm with 5 bolt holes~%")
  (format t "Holes are evenly spaced along the length~%")
  (format t "Pattern parameters:~%")
  (format t "  count: 5 holes~%")
  (format t "  spacing: calculated for even distribution~%")
  (format t "  direction: along X-axis~%")

  (flange-linear))

;;; ============================================================================
;;; Example 2: Circular Pattern - Mounting Holes
;;; ============================================================================

(clad.dsl:defpart mounting-plate-circular
    ((diameter 120) (thickness 8) (hole-count 8) (hole-diameter 6))
  "Circular plate with radially patterned mounting holes"
  (:body (clad.core:make-cylinder (/ diameter 2) thickness))

  ;; Center hole (cylinder is now centered at origin)
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder 10 (* thickness 1.5))
            0 0 (- (/ thickness 4)))))

  ;; Circular pattern of mounting holes (centered at origin)
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count hole-count
        :radius (/ diameter 3)
        :center-x 0
        :center-y 0
      (:cut (clad.core:make-cylinder (/ hole-diameter 2) (* thickness 1.5))))))

(defun demo-circular-pattern ()
  (format t "~%Example 2: Circular Pattern~%")
  (format t "===========================~%~%")
  (format t "Circular plate: 120mm diameter, 8mm thick~%")
  (format t "8 mounting holes in a circular pattern~%")
  (format t "Pattern parameters:~%")
  (format t "  count: 8 holes~%")
  (format t "  radius: diameter/3 (40mm)~%")
  (format t "  angle range: full circle (0-360°)~%")

  (mounting-plate-circular))

;;; ============================================================================
;;; Example 3: Partial Circular Pattern
;;; ============================================================================

(clad.dsl:defpart arc-bracket
    ((radius 80) (thickness 10))
  "Bracket with partial circular pattern"
  (:body (clad.core:make-box (* radius 2) (* radius 2) thickness))

  ;; Partial circular pattern (90 degrees)
  ;; Box is centered, so pattern center is at origin
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count 4
        :radius radius
        :center-x 0
        :center-y 0
        :angle-start 0
        :angle-end 90
      (:add (clad.core:translate (clad.core:make-cylinder 8 20)
                                 0 0 thickness)))))

(defun demo-partial-circular ()
  (format t "~%Example 3: Partial Circular Pattern~%")
  (format t "====================================~%~%")
  (format t "Demonstrates partial arc patterns~%")
  (format t "4 posts arranged in a 90° arc~%")
  (format t "Pattern parameters:~%")
  (format t "  angle-start: 0°~%")
  (format t "  angle-end: 90°~%")
  (format t "  count: 4 posts~%")

  (arc-bracket))

;;; ============================================================================
;;; Example 4: Combined Patterns - Mounting Plate
;;; ============================================================================

(clad.dsl:defpart complex-mounting-plate
    ((size 100) (thickness 10))
  "Plate with both linear and circular patterns"
  (:body (clad.core:make-box size size thickness))

  ;; Corner holes (linear pattern in X)
  ;; Box is centered, so adjust start positions
  (:on-face :direction :+z :extreme :max
    (:linear-pattern
        :count 2
        :spacing (- size 20)
        :direction-x 1
        :direction-y 0
        :start-x (- (/ size 2) 10)
        :start-y (- (/ size 2) 10)
      (:cut (clad.core:make-cylinder 3 (* thickness 1.5)))))

  ;; Corner holes (linear pattern in Y)
  (:on-face :direction :+z :extreme :max
    (:linear-pattern
        :count 2
        :spacing (- size 20)
        :direction-x 0
        :direction-y 1
        :start-x (- (/ size 2) 10)
        :start-y (- (/ size 2) 10)
      (:cut (clad.core:make-cylinder 3 (* thickness 1.5)))))

  ;; Center circular pattern (centered at origin)
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count 6
        :radius 30
        :center-x 0
        :center-y 0
      (:cut (clad.core:make-cylinder 2.5 (* thickness 1.5))))))

(defun demo-combined-patterns ()
  (format t "~%Example 4: Combined Patterns~%")
  (format t "=============================~%~%")
  (format t "Demonstrates using multiple pattern types:~%")
  (format t "  - 2 linear patterns for corner holes (4 total)~%")
  (format t "  - 1 circular pattern for center holes (6 total)~%")
  (format t "Total: 10 holes in one part~%")

  (complex-mounting-plate))

;;; ============================================================================
;;; Example 5: 2D Grid Pattern (Separate Linear Patterns)
;;; ============================================================================

(clad.dsl:defpart grid-plate
    ((size 100) (grid-count 4))
  "Plate with 2D grid of holes using multiple linear patterns"
  (:body (clad.core:make-box size size 8))

  ;; Linear pattern for row 1
  ;; Box is centered, so adjust all start positions
  (:on-face :direction :+z :extreme :max
    (:linear-pattern
        :count grid-count
        :spacing (/ (- size 20) (1- grid-count))
        :direction-x 1
        :direction-y 0
        :start-x (- (/ size 2) 10)
        :start-y (- (/ size 2) 10)
      (:cut (clad.core:make-cylinder 2 12))))

  ;; Linear pattern for row 2
  (:on-face :direction :+z :extreme :max
    (:linear-pattern
        :count grid-count
        :spacing (/ (- size 20) (1- grid-count))
        :direction-x 1
        :direction-y 0
        :start-x (- (/ size 2) 10)
        :start-y (+ (- (/ size 2) 10) (/ (- size 20) (1- grid-count)))
      (:cut (clad.core:make-cylinder 2 12))))

  ;; Linear pattern for row 3
  (:on-face :direction :+z :extreme :max
    (:linear-pattern
        :count grid-count
        :spacing (/ (- size 20) (1- grid-count))
        :direction-x 1
        :direction-y 0
        :start-x (- (/ size 2) 10)
        :start-y (+ (- (/ size 2) 10) (* 2 (/ (- size 20) (1- grid-count))))
      (:cut (clad.core:make-cylinder 2 12))))

  ;; Linear pattern for row 4
  (:on-face :direction :+z :extreme :max
    (:linear-pattern
        :count grid-count
        :spacing (/ (- size 20) (1- grid-count))
        :direction-x 1
        :direction-y 0
        :start-x (- (/ size 2) 10)
        :start-y (+ (- (/ size 2) 10) (* 3 (/ (- size 20) (1- grid-count))))
      (:cut (clad.core:make-cylinder 2 12)))))

(defun demo-grid-pattern ()
  (format t "~%Example 5: 2D Grid Pattern~%")
  (format t "===========================~%~%")
  (format t "Demonstrates multiple linear patterns for grid layout~%")
  (format t "Creates a 4x4 grid of holes (16 total)~%")
  (format t "Uses 4 separate linear patterns (one per row)~%")

  (grid-plate))

;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(defun run-pattern-demos ()
  "Run all pattern demonstrations"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                      CLAD Pattern Examples~%")
  (format t "================================================================================~%")

  (demo-linear-pattern)
  (demo-circular-pattern)
  (demo-partial-circular)
  (demo-combined-patterns)
  (demo-grid-pattern)

  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "All pattern examples completed!~%")
  (format t "~%To visualize:~%")
  (format t "  (clad:view (flange-linear) :name \"flange\")~%")
  (format t "  (clad:view (mounting-plate-circular) :name \"circular-plate\")~%")
  (format t "  (clad:view (arc-bracket) :name \"arc-bracket\")~%")
  (format t "  (clad:view (complex-mounting-plate) :name \"complex-plate\")~%")
  (format t "  (clad:view (grid-plate) :name \"grid\")~%")
  (format t "================================================================================~%")
  (format t "~%"))

;; Auto-run when loaded
(run-pattern-demos)

;;; ============================================================================
;;; Interactive Viewer Integration
;;; ============================================================================

(defun view-all-patterns ()
  "Start the viewer and display all pattern examples in the browser.

  This function demonstrates:
  - Linear patterns for evenly-spaced features
  - Circular patterns for radial arrangements
  - Partial arc patterns
  - Combined patterns (linear + circular)
  - 2D grid patterns using multiple linear patterns"

  (format t "~%~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║              Pattern Examples - Viewer Integration            ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  ;; Start viewer
  (format t "Starting CLAD web viewer...~%")
  (clad:start-viewer)
  (format t "~%")

  ;; View each pattern example
  (format t "Viewing Linear Pattern (Flange with bolt holes)~%")
  (clad:view (flange-linear) :name "02-linear-flange")
  (sleep 0.5)

  (format t "Viewing Circular Pattern (Mounting plate)~%")
  (clad:view (mounting-plate-circular) :name "02-circular-plate")
  (sleep 0.5)

  (format t "Viewing Partial Circular Pattern (Arc bracket)~%")
  (clad:view (arc-bracket) :name "02-arc-bracket")
  (sleep 0.5)

  (format t "Viewing Combined Patterns~%")
  (clad:view (complex-mounting-plate) :name "02-complex-plate")
  (sleep 0.5)

  (format t "Viewing 2D Grid Pattern~%")
  (clad:view (grid-plate) :name "02-grid-plate")

  (format t "~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║  All pattern examples loaded! http://localhost:8080           ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%"))

;; Individual viewer functions for each pattern example
(defun view-linear-pattern ()
  "View linear pattern example (flange with evenly-spaced holes)"
  (clad:view (flange-linear) :name "linear-flange"))

(defun view-circular-pattern ()
  "View circular pattern example (mounting plate with radial holes)"
  (clad:view (mounting-plate-circular) :name "circular-plate"))

(defun view-arc-pattern ()
  "View partial circular pattern example (90-degree arc)"
  (clad:view (arc-bracket) :name "arc-bracket"))

(defun view-combined-patterns ()
  "View combined pattern example (linear + circular)"
  (clad:view (complex-mounting-plate) :name "complex-plate"))

(defun view-grid-pattern ()
  "View 2D grid pattern example (4x4 hole array)"
  (clad:view (grid-plate) :name "grid-plate"))

;;; ============================================================================
;;; Quick Start Instructions
;;; ============================================================================

(format t "~%~%")
(format t "╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                     QUICK START GUIDE                          ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
(format t "~%")
(format t "To view all pattern examples in your browser:~%")
(format t "  (view-all-patterns)~%")
(format t "~%")
(format t "To view individual examples:~%")
(format t "  (view-linear-pattern)      ; Evenly-spaced holes~%")
(format t "  (view-circular-pattern)    ; Radial bolt pattern~%")
(format t "  (view-arc-pattern)         ; 90-degree arc~%")
(format t "  (view-combined-patterns)   ; Multiple pattern types~%")
(format t "  (view-grid-pattern)        ; 2D hole array~%")
(format t "~%")
(format t "Create custom patterns:~%")
(format t "  (clad:view (flange-linear 300 80 12 8) :name \"large-flange\")~%")
(format t "  (clad:view (mounting-plate-circular 150 10 12 8) :name \"big-plate\")~%")
(format t "~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
(format t "~%")
