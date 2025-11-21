;;;; examples/06-sketches.lisp --- Sketch-based Modeling Examples
;;;;
;;;; This file demonstrates 2D constraint-based sketching with extrusion.
;;;; Run with: (load "examples/06-sketches.lisp")

(require :asdf)
(asdf:load-system :clad)
(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Simple Constrained Rectangle Sketch
;;; ============================================================================

(defun demo-simple-rectangle-sketch ()
  (format t "~%Example 1: Simple Rectangle Sketch~%")
  (format t "===================================~%~%")
  (format t "Creating a constrained rectangle and extruding it~%")

  ;; Create sketch plane on XY plane
  (let* ((plane (clad.sketch:make-sketch-plane :origin '(0 0 0) :normal '(0 0 1)))
         (sketch (clad.sketch:make-sketch :name "Rectangle" :plane plane))

         ;; Create 4 corner points
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 50.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 50.0d0 30.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 0.0d0 30.0d0 :name "P4"))

         ;; Create 4 lines
         (l1 (clad.sketch:make-line-2d p1 p2 :name "Bottom"))
         (l2 (clad.sketch:make-line-2d p2 p3 :name "Right"))
         (l3 (clad.sketch:make-line-2d p3 p4 :name "Top"))
         (l4 (clad.sketch:make-line-2d p4 p1 :name "Left")))

    ;; Add entities to sketch
    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch p3)
    (clad.sketch:add-entity sketch p4)
    (clad.sketch:add-entity sketch l1)
    (clad.sketch:add-entity sketch l2)
    (clad.sketch:add-entity sketch l3)
    (clad.sketch:add-entity sketch l4)

    ;; Add constraints
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint p1 p2 50.0d0))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint p2 p3 30.0d0))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-perpendicular-constraint l1 l2))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-parallel-constraint l1 l3))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-parallel-constraint l2 l4))

    (format t "Created constrained rectangle sketch:~%")
    (format t "  Width: 50mm, Height: 30mm~%")
    (format t "  Constraints: distance, perpendicular, parallel~%")
    (format t "  Sketch status: ~A~%"
            (clad.sketch.validation:validate-sketch sketch))

    ;; Extrude the sketch
    (let ((profile (clad.sketch:sketch-to-wire sketch))
          (extruded (clad.core:extrude
                      (clad.sketch:sketch-to-wire sketch)
                      20.0d0)))
      (format t "  Extruded to 20mm height~%")
      extruded)))

;;; ============================================================================
;;; Example 2: Circular Sketch with Constraints
;;; ============================================================================

(defun demo-circle-sketch ()
  (format t "~%Example 2: Circular Sketch~%")
  (format t "===========================~%~%")
  (format t "Creating a constrained circle sketch~%")

  (let* ((plane (clad.sketch:make-sketch-plane :origin '(0 0 0) :normal '(0 0 1)))
         (sketch (clad.sketch:make-sketch :name "Circle" :plane plane))

         ;; Center point and point on circumference
         (center (clad.sketch:make-point-2d 25.0d0 25.0d0 :name "Center" :fixed t))
         (rim (clad.sketch:make-point-2d 40.0d0 25.0d0 :name "Rim"))

         ;; Create circle
         (circle (clad.sketch:make-circle-2d center 15.0d0 :name "MainCircle")))

    (clad.sketch:add-entity sketch center)
    (clad.sketch:add-entity sketch rim)
    (clad.sketch:add-entity sketch circle)

    ;; Constrain the radius
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint center rim 15.0d0))

    (format t "Created circle sketch:~%")
    (format t "  Center: (25, 25), Radius: 15mm~%")
    (format t "  Sketch status: ~A~%"
            (clad.sketch.validation:validate-sketch sketch))

    ;; Extrude
    (let ((profile (clad.sketch:sketch-to-wire sketch)))
      (format t "  Extruded to create cylinder~%")
      (clad.core:extrude profile 50.0d0))))

;;; ============================================================================
;;; Example 3: L-Bracket Profile Sketch
;;; ============================================================================

(defun demo-l-bracket-sketch ()
  (format t "~%Example 3: L-Bracket Profile~%")
  (format t "=============================~%~%")
  (format t "Creating an L-shaped profile using sketch constraints~%")

  (let* ((plane (clad.sketch:make-sketch-plane :origin '(0 0 0) :normal '(0 0 1)))
         (sketch (clad.sketch:make-sketch :name "L-Bracket" :plane plane))

         ;; Create points for L-shape (clockwise from origin)
         (p1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
         (p2 (clad.sketch:make-point-2d 60.0d0 0.0d0 :name "P2"))
         (p3 (clad.sketch:make-point-2d 60.0d0 10.0d0 :name "P3"))
         (p4 (clad.sketch:make-point-2d 10.0d0 10.0d0 :name "P4"))
         (p5 (clad.sketch:make-point-2d 10.0d0 50.0d0 :name "P5"))
         (p6 (clad.sketch:make-point-2d 0.0d0 50.0d0 :name "P6"))

         ;; Create lines
         (l1 (clad.sketch:make-line-2d p1 p2))
         (l2 (clad.sketch:make-line-2d p2 p3))
         (l3 (clad.sketch:make-line-2d p3 p4))
         (l4 (clad.sketch:make-line-2d p4 p5))
         (l5 (clad.sketch:make-line-2d p5 p6))
         (l6 (clad.sketch:make-line-2d p6 p1)))

    ;; Add all entities
    (dolist (pt (list p1 p2 p3 p4 p5 p6))
      (clad.sketch:add-entity sketch pt))
    (dolist (ln (list l1 l2 l3 l4 l5 l6))
      (clad.sketch:add-entity sketch ln))

    ;; Add dimensional constraints
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint p1 p2 60.0d0))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint p1 p6 50.0d0))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint p2 p3 10.0d0))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint p3 p4 50.0d0))

    ;; Add geometric constraints
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-perpendicular-constraint l1 l2))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-perpendicular-constraint l2 l3))

    (format t "Created L-bracket profile:~%")
    (format t "  Base: 60x10mm~%")
    (format t "  Vertical: 10x50mm~%")
    (format t "  Total constraints: ~A~%"
            (length (clad.sketch:sketch-constraints sketch)))

    ;; Extrude
    (let ((profile (clad.sketch:sketch-to-wire sketch)))
      (format t "  Extruded to 8mm thickness~%")
      (clad.core:extrude profile 8.0d0))))

;;; ============================================================================
;;; Example 4: Sketch with Multiple Profiles (Holes)
;;; ============================================================================

(defun demo-plate-with-holes-sketch ()
  (format t "~%Example 4: Plate with Holes~%")
  (format t "============================~%~%")
  (format t "Sketch with outer profile and inner hole profiles~%")

  (let* ((plane (clad.sketch:make-sketch-plane :origin '(0 0 0) :normal '(0 0 1)))
         (sketch (clad.sketch:make-sketch :name "Plate" :plane plane))

         ;; Outer rectangle
         (o1 (clad.sketch:make-point-2d 0.0d0 0.0d0 :fixed t))
         (o2 (clad.sketch:make-point-2d 100.0d0 0.0d0))
         (o3 (clad.sketch:make-point-2d 100.0d0 80.0d0))
         (o4 (clad.sketch:make-point-2d 0.0d0 80.0d0))

         ;; Hole 1 center
         (h1-center (clad.sketch:make-point-2d 25.0d0 40.0d0))
         (hole1 (clad.sketch:make-circle-2d h1-center 10.0d0))

         ;; Hole 2 center
         (h2-center (clad.sketch:make-point-2d 75.0d0 40.0d0))
         (hole2 (clad.sketch:make-circle-2d h2-center 10.0d0)))

    ;; Add outer rectangle
    (clad.sketch:add-entity sketch o1)
    (clad.sketch:add-entity sketch o2)
    (clad.sketch:add-entity sketch o3)
    (clad.sketch:add-entity sketch o4)
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d o1 o2))
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d o2 o3))
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d o3 o4))
    (clad.sketch:add-entity sketch (clad.sketch:make-line-2d o4 o1))

    ;; Add holes
    (clad.sketch:add-entity sketch h1-center)
    (clad.sketch:add-entity sketch hole1)
    (clad.sketch:add-entity sketch h2-center)
    (clad.sketch:add-entity sketch hole2)

    ;; Constrain dimensions
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint o1 o2 100.0d0))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint o2 o3 80.0d0))

    (format t "Created plate with 2 holes:~%")
    (format t "  Outer: 100x80mm~%")
    (format t "  Holes: 2x 20mm diameter~%")

    ;; Note: This would typically create a face with holes, but for simplicity
    ;; we'll just extrude the outer profile and cut holes separately
    (let* ((outer-wire (clad.core:make-polygon
                         (list '(0 0 0) '(100 0 0) '(100 80 0) '(0 80 0))))
           (base (clad.core:extrude outer-wire 10.0d0))
           (hole1-cyl (clad.core:translate
                        (clad.core:make-cylinder 10.0d0 15.0d0)
                        25 40 -2))
           (hole2-cyl (clad.core:translate
                        (clad.core:make-cylinder 10.0d0 15.0d0)
                        75 40 -2)))
      (format t "  Extruded and cut holes~%")
      (clad.core:cut-shapes (clad.core:cut-shapes base hole1-cyl) hole2-cyl))))

;;; ============================================================================
;;; Example 5: Parametric Sketch
;;; ============================================================================

(defun demo-parametric-sketch (width height fillet-r)
  (format t "~%Example 5: Parametric Rounded Rectangle~%")
  (format t "========================================~%~%")
  (format t "Creating parametric sketch with parameters:~%")
  (format t "  Width: ~Amm, Height: ~Amm, Fillet: ~Amm~%" width height fillet-r)

  (let* ((plane (clad.sketch:make-sketch-plane :origin '(0 0 0) :normal '(0 0 1)))
         (sketch (clad.sketch:make-sketch :name "Parametric" :plane plane))

         ;; Create corner points with fillet offsets
         (p1 (clad.sketch:make-point-2d
               (coerce fillet-r 'double-float)
               0.0d0 :fixed t))
         (p2 (clad.sketch:make-point-2d
               (coerce (- width fillet-r) 'double-float)
               0.0d0))
         (p3 (clad.sketch:make-point-2d
               (coerce width 'double-float)
               (coerce height 'double-float)))
         (p4 (clad.sketch:make-point-2d
               0.0d0
               (coerce (- height fillet-r) 'double-float))))

    (clad.sketch:add-entity sketch p1)
    (clad.sketch:add-entity sketch p2)
    (clad.sketch:add-entity sketch p3)
    (clad.sketch:add-entity sketch p4)

    ;; Add dimensional constraints
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint
        p1 p2 (coerce (- width (* 2 fillet-r)) 'double-float)))
    (clad.sketch:add-constraint sketch
      (clad.sketch.constraints:make-distance-constraint
        p2 p3 (coerce height 'double-float)))

    (format t "Sketch created with parametric dimensions~%")

    ;; For simplicity, create a simple box (full rounded rectangle would need arcs)
    (clad.core:make-box width height 10)))

;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(defun run-sketch-demos ()
  "Run all sketch-based modeling demonstrations"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                  CLAD Sketch-Based Modeling Examples~%")
  (format t "================================================================================~%")

  (let ((part1 (demo-simple-rectangle-sketch))
        (part2 (demo-circle-sketch))
        (part3 (demo-l-bracket-sketch))
        (part4 (demo-plate-with-holes-sketch))
        (part5 (demo-parametric-sketch 80 60 5)))

    (format t "~%~%")
    (format t "================================================================================~%")
    (format t "All sketch examples completed!~%")
    (format t "~%These examples demonstrate:~%")
    (format t "  - Creating 2D constraint-based sketches~%")
    (format t "  - Dimensional constraints (distance, radius)~%")
    (format t "  - Geometric constraints (perpendicular, parallel)~%")
    (format t "  - Sketch validation (under/well/over-constrained)~%")
    (format t "  - Extruding sketches to 3D solids~%")
    (format t "  - Complex profiles (L-brackets, plates with holes)~%")
    (format t "  - Parametric sketches~%")
    (format t "================================================================================~%")
    (format t "~%")

    (values part1 part2 part3 part4 part5)))

;; Auto-run when loaded
(run-sketch-demos)

;;; ============================================================================
;;; Interactive Viewer Integration
;;; ============================================================================

(defun view-all-sketches ()
  "Start the viewer and display all sketch-based modeling examples in the browser.

  This function demonstrates:
  - 2D constraint-based sketching
  - Dimensional and geometric constraints
  - Sketch validation (checking constraint status)
  - Extruding sketches to 3D solids
  - Complex profiles and parametric sketches"

  (format t "~%~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║       Sketch-Based Modeling - Viewer Integration              ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  ;; Start viewer
  (format t "Starting CLAD web viewer...~%")
  (clad:start-viewer)
  (format t "~%")

  ;; View each sketch example
  (format t "Viewing Simple Rectangle Sketch~%")
  (clad:view (demo-simple-rectangle-sketch) :name "sketch-rectangle")
  (sleep 0.5)

  (format t "Viewing Circular Sketch~%")
  (clad:view (demo-circle-sketch) :name "sketch-circle")
  (sleep 0.5)

  (format t "Viewing L-Bracket Profile~%")
  (clad:view (demo-l-bracket-sketch) :name "sketch-l-bracket")
  (sleep 0.5)

  (format t "Viewing Plate with Holes~%")
  (clad:view (demo-plate-with-holes-sketch) :name "sketch-plate-holes")
  (sleep 0.5)

  (format t "Viewing Parametric Rounded Rectangle~%")
  (clad:view (demo-parametric-sketch 80 60 5) :name "sketch-parametric")

  (format t "~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║  All sketch examples loaded! http://localhost:8080            ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%"))

;; Individual viewer functions for each sketch example
(defun view-rectangle-sketch ()
  "View simple constrained rectangle sketch"
  (clad:view (demo-simple-rectangle-sketch) :name "rectangle-sketch"))

(defun view-circle-sketch ()
  "View constrained circle sketch"
  (clad:view (demo-circle-sketch) :name "circle-sketch"))

(defun view-l-bracket-sketch ()
  "View L-bracket profile sketch"
  (clad:view (demo-l-bracket-sketch) :name "l-bracket-sketch"))

(defun view-plate-holes-sketch ()
  "View plate with holes sketch"
  (clad:view (demo-plate-with-holes-sketch) :name "plate-holes-sketch"))

(defun view-parametric-sketch ()
  "View parametric rounded rectangle sketch"
  (clad:view (demo-parametric-sketch 80 60 5) :name "parametric-sketch"))

;;; ============================================================================
;;; Quick Start Instructions
;;; ============================================================================

(format t "~%~%")
(format t "╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                     QUICK START GUIDE                          ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
(format t "~%")
(format t "To view all sketch-based modeling examples in your browser:~%")
(format t "  (view-all-sketches)~%")
(format t "~%")
(format t "To view individual examples:~%")
(format t "  (view-rectangle-sketch)      ; Constrained rectangle~%")
(format t "  (view-circle-sketch)         ; Constrained circle~%")
(format t "  (view-l-bracket-sketch)      ; L-shaped profile~%")
(format t "  (view-plate-holes-sketch)    ; Plate with 2 holes~%")
(format t "  (view-parametric-sketch)     ; Parametric design~%")
(format t "~%")
(format t "Create custom parametric sketches:~%")
(format t "  (clad:view (demo-parametric-sketch 100 75 8) :name \"custom\")~%")
(format t "~%")
(format t "Sketch-based modeling features:~%")
(format t "  - 2D constraint-based sketching~%")
(format t "  - Dimensional constraints (distance, radius)~%")
(format t "  - Geometric constraints (perpendicular, parallel)~%")
(format t "  - Sketch validation (constraint status checking)~%")
(format t "  - Extrusion to 3D solids~%")
(format t "~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
(format t "~%")
