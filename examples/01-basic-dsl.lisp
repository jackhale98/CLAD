;;;; examples/01-basic-dsl.lisp --- Basic DSL Usage Examples
;;;;
;;;; This file demonstrates the fundamental DSL constructs in CLAD.
;;;; Run with: (load "examples/01-basic-dsl.lisp")

(asdf:load-system :clad)
(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Simple Box with Parameters
;;; ============================================================================

(clad.dsl:defpart simple-box
    ((width 100) (height 50) (depth 30))
  "A simple parametric box"
  (:body (clad.core:make-box width height depth)))

;; Create instances with different parameters
(defun demo-simple-box ()
  (format t "~%Example 1: Simple Parametric Box~%")
  (format t "================================~%~%")

  (let ((box1 (simple-box))           ; Default: 100x50x30
        (box2 (simple-box 200))       ; 200x50x30
        (box3 (simple-box 150 75 40))) ; 150x75x40

    (format t "Created 3 boxes with different dimensions~%")
    (format t "Box 1: 100x50x30 (default)~%")
    (format t "Box 2: 200x50x30 (width changed)~%")
    (format t "Box 3: 150x75x40 (all changed)~%")

    ;; Return the default box for visualization
    box1))

;;; ============================================================================
;;; Example 2: Box with Hole (Boolean Operations)
;;; ============================================================================

(clad.dsl:defpart box-with-hole
    ((size 100) (hole-diameter 20))
  "Box with a centered through-hole"
  (:body (clad.core:make-box size size size))
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* size 1.2))
            0 0 (- (/ size 10))))))

(defun demo-box-with-hole ()
  (format t "~%Example 2: Box with Hole~%")
  (format t "========================~%~%")
  (format t "Creating a 100mm cube with a 20mm diameter hole~%")
  (format t "The :cut operation subtracts the cylinder from the box~%")

  (box-with-hole))

;;; ============================================================================
;;; Example 3: Multiple Operations (Add and Cut)
;;; ============================================================================

(clad.dsl:defpart base-with-boss
    ((base-size 80) (boss-height 30) (boss-diameter 40))
  "Base plate with raised boss and through hole"
  ;; Start with base plate
  (:body (clad.core:make-box base-size base-size 10))

  ;; Add boss on top face (centered primitives make this simpler)
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder (/ boss-diameter 2) boss-height)
            0 0 10)))

  ;; Cut hole through boss and base (also centered)
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder 8 (+ boss-height 15))
            0 0 8))))

(defun demo-base-with-boss ()
  (format t "~%Example 3: Base with Boss~%")
  (format t "=========================~%~%")
  (format t "Demonstrates sequential operations:~%")
  (format t "  1. Create base plate (80x80x10mm)~%")
  (format t "  2. Add cylindrical boss (40mm diameter, 30mm high)~%")
  (format t "  3. Cut hole through both (16mm diameter)~%")

  (base-with-boss))

;;; ============================================================================
;;; Example 4: Using Different Face Selectors
;;; ============================================================================

(clad.dsl:defpart bracket-base
    ((size 60))
  "Bracket base demonstrating face selection"
  (:body (clad.core:make-box size size 8))

  ;; Add mounting tabs on all four sides
  ;; With centered primitives, tabs are positioned from the centered base
  (:on-face :direction :+x :extreme :max
    (:add (clad.core:translate (clad.core:make-box 5 20 15)
                               (/ size 2) -10 8)))

  (:on-face :direction :-x :extreme :min
    (:add (clad.core:translate (clad.core:make-box 5 20 15)
                               (- (/ size 2) 2.5) -10 8)))

  (:on-face :direction :+y :extreme :max
    (:add (clad.core:translate (clad.core:make-box 20 5 15)
                               -10 (/ size 2) 8)))

  (:on-face :direction :-y :extreme :min
    (:add (clad.core:translate (clad.core:make-box 20 5 15)
                               -10 (- (/ size 2) 2.5) 8))))

(defun demo-bracket-base ()
  (format t "~%Example 4: Face Selection~%")
  (format t "=========================~%~%")
  (format t "Demonstrates selecting faces by direction:~%")
  (format t "  :+x :extreme :max  - rightmost face~%")
  (format t "  :-x :extreme :min  - leftmost face~%")
  (format t "  :+y :extreme :max  - front face~%")
  (format t "  :-y :extreme :min  - back face~%")

  (bracket-base))

;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(defun run-basic-demos ()
  "Run all basic DSL demonstrations"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                      CLAD Basic DSL Examples~%")
  (format t "================================================================================~%")

  (demo-simple-box)
  (demo-box-with-hole)
  (demo-base-with-boss)
  (demo-bracket-base)

  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "All basic examples completed!~%")
  (format t "~%To visualize a part:~%")
  (format t "  (clad:view (simple-box) :name \"simple-box\")~%")
  (format t "  (clad:view (box-with-hole) :name \"box-with-hole\")~%")
  (format t "  (clad:view (base-with-boss) :name \"base-with-boss\")~%")
  (format t "  (clad:view (bracket-base) :name \"bracket-base\")~%")
  (format t "================================================================================~%")
  (format t "~%"))

;; Auto-run when loaded
(run-basic-demos)
