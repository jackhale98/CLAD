;;;; examples/05-assemblies.lisp --- Assembly System Examples
;;;;
;;;; This file demonstrates the assembly and constraint system in CLAD.
;;;; Updated for centered primitives - component positioning is now more intuitive!
;;;; Run with: (load "examples/05-assemblies.lisp")

(asdf:load-system :clad)
(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Simple Assembly - Two Parts
;;; ============================================================================

(defun demo-simple-assembly ()
  (format t "~%Example 1: Simple Assembly~%")
  (format t "==========================~%~%")
  (format t "Creating an assembly with two boxes~%")

  ;; Create assembly
  (let ((assy (clad.assembly:make-assembly :name "Two Box Assembly")))

    ;; Create box parts
    (let ((box1 (clad.core:make-box 50 50 10))
          (box2 (clad.core:make-box 50 50 10)))

      ;; Add components
      (clad.assembly:add-component assy :box-1 box1 :fixed t)
      (clad.assembly:add-component assy :box-2 box2)

      ;; Set position of second box
      (let ((comp2 (clad.assembly:get-component assy :box-2)))
        (clad.assembly:set-component-position comp2 0 0 15))

      (format t "Assembly created with 2 components~%")
      (format t "Components: ~{~A~^, ~}~%"
              (mapcar #'clad.assembly:component-name
                      (clad.assembly:list-components assy)))

      assy)))

;;; ============================================================================
;;; Example 2: Assembly with Simple Constraints
;;; ============================================================================

(defun demo-constrained-assembly ()
  (format t "~%Example 2: Constrained Assembly~%")
  (format t "================================~%~%")
  (format t "Assembly with mate constraints~%")

  (let ((assy (clad.assembly:make-assembly :name "Constrained Assembly")))

    (let ((box1 (clad.core:make-box 50 50 10))
          (box2 (clad.core:make-box 50 50 10))
          (cyl (clad.core:make-cylinder 5 20)))

      ;; Add components
      (clad.assembly:add-component assy :base box1 :fixed t)
      (clad.assembly:add-component assy :top box2)
      (clad.assembly:add-component assy :post cyl)

      ;; Add coincident constraint between boxes
      (clad.assembly.constraints:add-mate
        assy :coincident
        :base :face-top
        :top :face-bottom)

      ;; Add concentric constraint for post
      (clad.assembly.constraints:add-mate
        assy :concentric
        :base :hole
        :post :axis)

      (format t "Assembly with ~A constraints~%"
              (length (clad.assembly:assembly-constraints assy)))

      assy)))

;;; ============================================================================
;;; Example 3: Nested Assembly
;;; ============================================================================

(defun demo-nested-assembly ()
  (format t "~%Example 3: Nested Assembly~%")
  (format t "===========================~%~%")
  (format t "Assembly containing sub-assemblies~%")

  ;; Create sub-assembly 1
  (let ((sub-assy-1 (clad.assembly:make-assembly :name "Sub Assembly 1"))
        (box1 (clad.core:make-box 30 30 10))
        (box2 (clad.core:make-box 30 30 10)))

    (clad.assembly:add-component sub-assy-1 :part-a box1 :fixed t)
    (clad.assembly:add-component sub-assy-1 :part-b box2)

    ;; Create sub-assembly 2
    (let ((sub-assy-2 (clad.assembly:make-assembly :name "Sub Assembly 2"))
          (cyl1 (clad.core:make-cylinder 10 20))
          (cyl2 (clad.core:make-cylinder 10 20)))

      (clad.assembly:add-component sub-assy-2 :cyl-a cyl1 :fixed t)
      (clad.assembly:add-component sub-assy-2 :cyl-b cyl2)

      ;; Create main assembly
      (let ((main-assy (clad.assembly:make-assembly :name "Main Assembly")))

        ;; Add sub-assemblies as components
        (clad.assembly:add-component main-assy :sub-1 sub-assy-1 :fixed t)
        (clad.assembly:add-component main-assy :sub-2 sub-assy-2)

        ;; Position second sub-assembly
        (let ((comp2 (clad.assembly:get-component main-assy :sub-2)))
          (clad.assembly:set-component-position comp2 50 0 0))

        (format t "Main assembly contains 2 sub-assemblies~%")
        (format t "Total top-level components: ~A~%"
                (hash-table-count (clad.assembly:assembly-components main-assy)))
        (format t "Assembly hierarchy depth: 2~%")

        main-assy))))

;;; ============================================================================
;;; Example 4: Assembly with Multiple Constraint Types
;;; ============================================================================

(defun demo-multiple-constraints ()
  (format t "~%Example 4: Multiple Constraint Types~%")
  (format t "=====================================~%~%")
  (format t "Demonstrating different mate constraint types~%")

  (let ((assy (clad.assembly:make-assembly :name "Multi-Constraint Assembly")))

    (let ((base (clad.core:make-box 100 100 10))
          (plate (clad.core:make-box 80 80 8))
          (post1 (clad.core:make-cylinder 5 40))
          (post2 (clad.core:make-cylinder 5 40)))

      ;; Add all components
      (clad.assembly:add-component assy :base base :fixed t)
      (clad.assembly:add-component assy :plate plate)
      (clad.assembly:add-component assy :post-1 post1)
      (clad.assembly:add-component assy :post-2 post2)

      ;; Position components initially
      (clad.assembly:set-component-position
        (clad.assembly:get-component assy :plate) 10 10 50)
      (clad.assembly:set-component-position
        (clad.assembly:get-component assy :post-1) 20 20 10)
      (clad.assembly:set-component-position
        (clad.assembly:get-component assy :post-2) 70 70 10)

      ;; Parallel: Plate parallel to base
      (clad.assembly.constraints:add-mate
        assy :parallel
        :base :face-top
        :plate :face-bottom)

      ;; Distance: Between posts
      (clad.assembly.constraints:add-mate
        assy :distance
        :post-1 :axis
        :post-2 :axis
        :offset 50.0)

      (format t "Assembly constraints:~%")
      (format t "  - Parallel (plate to base)~%")
      (format t "  - Distance (between posts)~%")
      (format t "Total constraints: ~A~%"
              (length (clad.assembly:assembly-constraints assy)))

      assy)))

;;; ============================================================================
;;; Example 5: Assembly with Component Metadata
;;; ============================================================================

(defun demo-assembly-metadata ()
  (format t "~%Example 5: Assembly with Metadata~%")
  (format t "==================================~%~%")
  (format t "Using component metadata for BOM generation~%")

  (let ((assy (clad.assembly:make-assembly
                :name "Mounting Bracket Assembly")))

    (let ((bracket (clad.core:make-box 60 40 8))
          (bolt (clad.core:make-cylinder 3 15))
          (washer (clad.core:make-cylinder 6 1)))

      ;; Add components with detailed metadata
      (clad.assembly:add-component
        assy :bracket bracket
        :quantity 1
        :fixed t
        :metadata '(:description "L-bracket"
                   :part-number "BRK-6040"
                   :material "6061-T6 Aluminum"
                   :finish "Anodized Black"
                   :vendor "McMaster-Carr"))

      (clad.assembly:add-component
        assy :bolt bolt
        :quantity 4
        :metadata '(:description "M6 socket head cap screw"
                   :part-number "M6x15-SHCS"
                   :material "Grade 12.9 Steel"
                   :finish "Black Oxide"))

      (clad.assembly:add-component
        assy :washer washer
        :quantity 4
        :metadata '(:description "M6 flat washer"
                   :part-number "M6-WASHER"
                   :material "Steel"
                   :finish "Zinc Plated"))

      (format t "Assembly components with metadata:~%")
      (dolist (comp (clad.assembly:list-components assy))
        (let ((meta (clad.assembly:component-metadata comp)))
          (format t "  - ~A (qty: ~A)~%"
                  (getf meta :description)
                  (clad.assembly:component-quantity comp))))

      (format t "~%Component metadata available for BOM generation~%")

      assy)))

;;; ============================================================================
;;; Example 6: Parametric Assembly
;;; ============================================================================

(defun demo-parametric-assembly ()
  (format t "~%Example 6: Parametric Assembly~%")
  (format t "===============================~%~%")
  (format t "Assembly with adjustable parameters~%")

  (let ((assy (clad.assembly:make-assembly :name "Parametric Stack")))

    ;; Set assembly parameters
    (clad.assembly:set-parameter assy :spacing 15)
    (clad.assembly:set-parameter assy :layer-count 4)
    (clad.assembly:set-parameter assy :layer-height 10)

    ;; Create layers based on parameters
    (let ((spacing (clad.assembly:get-parameter assy :spacing))
          (count (clad.assembly:get-parameter assy :layer-count))
          (height (clad.assembly:get-parameter assy :layer-height)))

      (format t "Creating stack with:~%")
      (format t "  Layers: ~A~%" count)
      (format t "  Spacing: ~Amm~%" spacing)
      (format t "  Layer height: ~Amm~%~%" height)

      ;; Add layers
      (dotimes (i count)
        (let ((layer (clad.core:make-box 50 50 height))
              (layer-name (intern (format nil "LAYER-~A" (1+ i)) :keyword)))
          (clad.assembly:add-component
            assy layer-name layer
            :fixed (zerop i))  ; First layer is fixed

          ;; Position layer
          (unless (zerop i)
            (let ((comp (clad.assembly:get-component assy layer-name)))
              (clad.assembly:set-component-position
                comp 0 0 (* i (+ height spacing)))))))

      (format t "Created ~A-layer parametric stack~%" count)

      assy)))

;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(defun run-assembly-demos ()
  "Run all assembly demonstrations"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                      CLAD Assembly Examples~%")
  (format t "================================================================================~%")

  (let ((assy1 (demo-simple-assembly))
        (assy2 (demo-constrained-assembly))
        (assy3 (demo-nested-assembly))
        (assy4 (demo-multiple-constraints))
        (assy5 (demo-assembly-metadata))
        (assy6 (demo-parametric-assembly)))

    (format t "~%~%")
    (format t "================================================================================~%")
    (format t "All assembly examples completed!~%")
    (format t "~%These examples demonstrate:~%")
    (format t "  - Creating assemblies and adding components~%")
    (format t "  - Positioning components with transforms~%")
    (format t "  - Adding mate constraints (coincident, concentric, distance, parallel)~%")
    (format t "  - Nested sub-assemblies~%")
    (format t "  - Component metadata for BOM generation~%")
    (format t "  - Parametric assemblies~%")
    (format t "~%Assembly system features:~%")
    (format t "  - Fixed and floating components~%")
    (format t "  - Multiple constraint types~%")
    (format t "  - Component quantities for BOM~%")
    (format t "  - Metadata for manufacturing/purchasing~%")
    (format t "================================================================================~%")
    (format t "~%")

    (values assy1 assy2 assy3 assy4 assy5 assy6)))

;; Auto-run when loaded
(run-assembly-demos)
