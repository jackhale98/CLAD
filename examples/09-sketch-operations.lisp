;;;; examples/09-sketch-operations.lisp --- Comprehensive Sketch Examples
;;;;
;;;; This file demonstrates CLAD's 2D sketch system with constraint solving,
;;;; and conversion to 3D solids via extrusion and revolution.

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :clad)

(defpackage #:sketch-examples
  (:use #:cl))

(in-package #:sketch-examples)

(format t "~%")
(format t "============================================================~%")
(format t "          CLAD Sketch System Examples~%")
(format t "============================================================~%~%")

;;; ============================================================================
;;; Example 1: Simple Circle Extrusion (Cylinder)
;;; ============================================================================

(format t "Example 1: Extruding a Circle to Create a Cylinder~%")
(format t "~50,,,'-A~%" "-")

(let* ((sketch (clad.sketch:make-sketch :name "circle-profile"))
       (center (clad.sketch:make-point-2d 0 0))
       (circle (clad.sketch:make-circle-2d center 20.0)))
  
  ;; Add circle to sketch
  (clad.sketch:add-entity sketch circle)
  
  ;; Extrude to create cylinder
  (let ((cylinder (clad.sketch:extrude-sketch sketch 50.0)))
    (format t "  Created cylinder from circle profile~%")
    (format t "  - Profile radius: 20mm~%")
    (format t "  - Extrusion height: 50mm~%")
    
    ;; Export to STEP
    (clad.ffi:ffi-export-step cylinder "/tmp/sketch-cylinder.step")
    (format t "  Exported to: /tmp/sketch-cylinder.step~%~%")))

;;; ============================================================================
;;; Example 2: Rectangle Extrusion (Box)
;;; ============================================================================

(format t "Example 2: Extruding a Rectangle to Create a Box~%")
(format t "~50,,,'-A~%" "-")

(let* ((sketch (clad.sketch:make-sketch :name "rectangle-profile"))
       ;; Define corners
       (p1 (clad.sketch:make-point-2d 0 0))
       (p2 (clad.sketch:make-point-2d 100 0))
       (p3 (clad.sketch:make-point-2d 100 60))
       (p4 (clad.sketch:make-point-2d 0 60)))
  
  ;; Add lines forming rectangle
  (clad.sketch:add-entity sketch (clad.sketch:make-line-2d p1 p2))
  (clad.sketch:add-entity sketch (clad.sketch:make-line-2d p2 p3))
  (clad.sketch:add-entity sketch (clad.sketch:make-line-2d p3 p4))
  (clad.sketch:add-entity sketch (clad.sketch:make-line-2d p4 p1))
  
  ;; Extrude to create box
  (let ((box (clad.sketch:extrude-sketch sketch 25.0)))
    (format t "  Created box from rectangle profile~%")
    (format t "  - Profile: 100mm x 60mm~%")
    (format t "  - Extrusion height: 25mm~%")
    
    (clad.ffi:ffi-export-step box "/tmp/sketch-box.step")
    (format t "  Exported to: /tmp/sketch-box.step~%~%")))

;;; ============================================================================
;;; Example 3: Circle Revolution (Torus)
;;; ============================================================================

(format t "Example 3: Revolving a Circle to Create a Torus~%")
(format t "~50,,,'-A~%" "-")

(let* ((sketch (clad.sketch:make-sketch :name "torus-profile"))
       ;; Circle offset from axis
       (center (clad.sketch:make-point-2d 40 0))
       (circle (clad.sketch:make-circle-2d center 10.0)))
  
  (clad.sketch:add-entity sketch circle)
  
  ;; Revolve around Y axis (full rotation)
  (let ((torus (clad.sketch:revolve-sketch sketch)))
    (format t "  Created torus from circle profile~%")
    (format t "  - Minor radius: 10mm~%")
    (format t "  - Major radius: 40mm~%")
    (format t "  - Revolution: 360 degrees around Y axis~%")
    
    (clad.ffi:ffi-export-step torus "/tmp/sketch-torus.step")
    (format t "  Exported to: /tmp/sketch-torus.step~%~%")))

;;; ============================================================================
;;; Example 4: Partial Revolution (Quarter Pipe)
;;; ============================================================================

(format t "Example 4: Partial Revolution (90-degree Pipe Section)~%")
(format t "~50,,,'-A~%" "-")

(let* ((sketch (clad.sketch:make-sketch :name "quarter-pipe"))
       (center (clad.sketch:make-point-2d 50 0))
       (circle (clad.sketch:make-circle-2d center 8.0)))
  
  (clad.sketch:add-entity sketch circle)
  
  ;; Revolve 90 degrees
  (let ((quarter (clad.sketch:revolve-sketch sketch :angle (/ pi 2))))
    (format t "  Created 90-degree pipe section~%")
    (format t "  - Pipe radius: 8mm~%")
    (format t "  - Bend radius: 50mm~%")
    
    (clad.ffi:ffi-export-step quarter "/tmp/sketch-quarter-pipe.step")
    (format t "  Exported to: /tmp/sketch-quarter-pipe.step~%~%")))

;;; ============================================================================
;;; Example 5: Extrusion with Constraint Solving
;;; ============================================================================

(format t "Example 5: Constrained Sketch with Extrusion~%")
(format t "~50,,,'-A~%" "-")

(let* ((sketch (clad.sketch:make-sketch :name "constrained-rect"))
       ;; Points start at approximate positions
       (p1 (clad.sketch:make-point-2d 0 0 :fixed t))  ; Fixed at origin
       (p2 (clad.sketch:make-point-2d 80 0))
       (p3 (clad.sketch:make-point-2d 80 40))
       (p4 (clad.sketch:make-point-2d 0 40))
       ;; Lines
       (l1 (clad.sketch:make-line-2d p1 p2))
       (l2 (clad.sketch:make-line-2d p2 p3))
       (l3 (clad.sketch:make-line-2d p3 p4))
       (l4 (clad.sketch:make-line-2d p4 p1)))
  
  ;; Add entities
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
    (clad.sketch.constraints:make-horizontal-constraint l1))
  (clad.sketch:add-constraint sketch
    (clad.sketch.constraints:make-horizontal-constraint l3))
  (clad.sketch:add-constraint sketch
    (clad.sketch.constraints:make-vertical-constraint l2))
  (clad.sketch:add-constraint sketch
    (clad.sketch.constraints:make-vertical-constraint l4))
  (clad.sketch:add-constraint sketch
    (clad.sketch.constraints:make-distance-constraint p1 p2 80.0d0))
  (clad.sketch:add-constraint sketch
    (clad.sketch.constraints:make-distance-constraint p2 p3 40.0d0))
  
  ;; Solve constraints
  (format t "  Solving constraints...~%")
  (clad.sketch.solver:solve-sketch sketch)
  (format t "  Constraints solved!~%")
  
  ;; Extrude the solved sketch
  (let ((solid (clad.sketch:extrude-sketch sketch 15.0)))
    (format t "  Created solid from constrained sketch~%")
    (format t "  - Dimensions: 80mm x 40mm x 15mm~%")
    
    (clad.ffi:ffi-export-step solid "/tmp/sketch-constrained.step")
    (format t "  Exported to: /tmp/sketch-constrained.step~%~%")))

;;; ============================================================================
;;; Example 6: Extrusion on YZ Plane
;;; ============================================================================

(format t "Example 6: Extrusion on YZ Plane~%")
(format t "~50,,,'-A~%" "-")

(let* ((plane (clad.sketch:make-sketch-plane :type :yz))
       (sketch (clad.sketch:make-sketch :name "yz-profile"))
       (center (clad.sketch:make-point-2d 0 0))
       (circle (clad.sketch:make-circle-2d center 15.0)))
  
  (clad.sketch:add-entity sketch circle)
  
  ;; Extrude on YZ plane (extrudes in X direction)
  (let ((solid (clad.sketch:extrude-sketch sketch 40.0 :plane plane)))
    (format t "  Created cylinder on YZ plane~%")
    (format t "  - Extrusion is along X axis~%")
    
    (clad.ffi:ffi-export-step solid "/tmp/sketch-yz-extrusion.step")
    (format t "  Exported to: /tmp/sketch-yz-extrusion.step~%~%")))

;;; ============================================================================
;;; Example 7: Revolution Around Z Axis
;;; ============================================================================

(format t "Example 7: Revolution Around Z Axis~%")
(format t "~50,,,'-A~%" "-")

(let* ((sketch (clad.sketch:make-sketch :name "z-revolve"))
       (center (clad.sketch:make-point-2d 25 0))
       (circle (clad.sketch:make-circle-2d center 6.0)))
  
  (clad.sketch:add-entity sketch circle)
  
  ;; Revolve around Z axis
  (let ((solid (clad.sketch:revolve-sketch sketch :axis-direction '(0 0 1))))
    (format t "  Created ring around Z axis~%")
    
    (clad.ffi:ffi-export-step solid "/tmp/sketch-z-revolve.step")
    (format t "  Exported to: /tmp/sketch-z-revolve.step~%~%")))

;;; ============================================================================
;;; Example 8: Combined Operations (Part with Multiple Features)
;;; ============================================================================

(format t "Example 8: Multiple Sketches Combined~%")
(format t "~50,,,'-A~%" "-")

;; Create base from rectangle extrusion
(let* ((base-sketch (clad.sketch:make-sketch :name "base"))
       (p1 (clad.sketch:make-point-2d -50 -30))
       (p2 (clad.sketch:make-point-2d 50 -30))
       (p3 (clad.sketch:make-point-2d 50 30))
       (p4 (clad.sketch:make-point-2d -50 30)))
  (clad.sketch:add-entity base-sketch (clad.sketch:make-line-2d p1 p2))
  (clad.sketch:add-entity base-sketch (clad.sketch:make-line-2d p2 p3))
  (clad.sketch:add-entity base-sketch (clad.sketch:make-line-2d p3 p4))
  (clad.sketch:add-entity base-sketch (clad.sketch:make-line-2d p4 p1))
  
  (let ((base (clad.sketch:extrude-sketch base-sketch 10.0)))
    (format t "  Created base plate: 100mm x 60mm x 10mm~%")

    ;; Create cylinder from circle extrusion
    (let* ((cyl-sketch (clad.sketch:make-sketch :name "cylinder"))
           (center (clad.sketch:make-point-2d 0 0))
           (circle (clad.sketch:make-circle-2d center 15.0)))
      (clad.sketch:add-entity cyl-sketch circle)

      ;; Extrude on XY plane at Z=10 (top of base)
      (let* ((cyl-plane (clad.sketch:make-sketch-plane :type :xy
                                                         :origin '(0.0d0 0.0d0 10.0d0)))
             (cylinder (clad.sketch:extrude-sketch cyl-sketch 30.0 :plane cyl-plane)))
        (format t "  Created cylinder boss: R=15mm, H=30mm~%")

        ;; Union them using FFI directly (both are handles)
        (let ((result (clad.ffi:ffi-union base cylinder)))
          (format t "  Combined base + cylinder~%")

          (clad.ffi:ffi-export-step result "/tmp/sketch-combined.step")
          (format t "  Exported to: /tmp/sketch-combined.step~%~%"))))))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(format t "============================================================~%")
(format t "                    Summary~%")
(format t "============================================================~%")
(format t "~%")
(format t "Created 8 example files in /tmp/:~%")
(format t "  1. sketch-cylinder.step      - Circle extrusion~%")
(format t "  2. sketch-box.step           - Rectangle extrusion~%")
(format t "  3. sketch-torus.step         - Full revolution (torus)~%")
(format t "  4. sketch-quarter-pipe.step  - 90-degree revolution~%")
(format t "  5. sketch-constrained.step   - Constrained sketch extrusion~%")
(format t "  6. sketch-yz-extrusion.step  - YZ plane extrusion~%")
(format t "  7. sketch-z-revolve.step     - Revolution around Z~%")
(format t "  8. sketch-combined.step      - Multiple sketches combined~%")
(format t "~%")
(format t "All examples completed successfully!~%")
