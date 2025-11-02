;;;; examples/dsl-advanced-features-demo.lisp
;;;; Demonstrates Phase 8 advanced features in defpart DSL

(unless (find-package :clad.core)
  (asdf:load-system :clad))

(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Simple Loft - Vase Shape
;;; ============================================================================

(clad.dsl:defpart lofted-vase ((height 35))
  "A vase created using loft - demonstrates smooth lofting"
  (:body
    (clad.core:make-loft
      (list (clad.core:make-circle-wire '(0 0 0) 20)
            (clad.core:make-circle-wire `(0 0 ,(/ height 2)) 12)
            (clad.core:make-circle-wire `(0 0 ,height) 5))
      :solid t
      :ruled nil)))

(format t "~%Example 1: Lofted Vase~%")
(let ((vase (lofted-vase)))
  (format t "✓ Created lofted vase~%")
  (clad:view vase :name "lofted-vase")
  (format t "✓ Viewing at http://localhost:8080/?model=/models/lofted-vase.glb~%~%"))

;;; ============================================================================
;;; Example 2: Pipe Sweep - Curved Tube
;;; ============================================================================

(clad.dsl:defpart curved-tube ((radius 3))
  "A tube following a curved 3D path"
  (:body
    (clad.core:make-pipe
      (clad.core:make-spline '((0 0 0) (10 10 10) (20 5 15) (30 0 20)) :closed nil)
      radius)))

(format t "Example 2: Curved Tube~%")
(let ((tube (curved-tube)))
  (format t "✓ Created curved tube~%")
  (clad:view tube :name "curved-tube")
  (format t "✓ Viewing at http://localhost:8080/?model=/models/curved-tube.glb~%~%"))

;;; ============================================================================
;;; Example 3: Profile Sweep - Custom Handle
;;; ============================================================================

(clad.dsl:defpart swept-handle ((length 100))
  "Handle created by sweeping a circular profile along a curved path"
  (:body
    (clad.core:make-sweep
      (clad.core:make-circle-wire '(0 0 0) 5 :axis '(1 0 0))
      (clad.core:make-spline `((0 0 0)
                                (,(/ length 3) 5 0)
                                (,(* 2 (/ length 3)) 5 0)
                                (,length 0 0))
                              :closed nil))))

(format t "Example 3: Swept Handle~%")
(let ((handle (swept-handle)))
  (format t "✓ Created swept handle~%")
  (clad:view handle :name "swept-handle")
  (format t "✓ Viewing at http://localhost:8080/?model=/models/swept-handle.glb~%~%"))

;;; ============================================================================
;;; Example 4: Mirroring - Symmetric Bracket
;;; ============================================================================

(clad.dsl:defpart symmetric-bracket ((width 60) (height 40) (thickness 5))
  "Symmetric bracket using DSL :mirror top-level form"
  ;; Create half bracket
  (:body
    (clad.core:fuse-shapes
      (clad.core:make-box (/ width 2) height thickness)
      (clad.core:translate
        (clad.core:make-box 10 10 (* thickness 2))
        (- (/ width 4)) (/ height 2) 0)))

  ;; Mirror across YZ plane to create full bracket
  (:mirror :plane-origin (0 0 0) :plane-normal (1 0 0))

  ;; Add mounting holes in circular pattern
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
      :count 4
      :radius 20
      :center-x 0
      :center-y (/ height 2)
      (:cut (clad.core:make-cylinder 3 10)))))

(format t "Example 4: Symmetric Bracket~%")
(let ((bracket (symmetric-bracket)))
  (format t "✓ Created symmetric bracket with mirroring~%")
  (clad:view bracket :name "symmetric-bracket")
  (format t "✓ Viewing at http://localhost:8080/?model=/models/symmetric-bracket.glb~%~%"))

;;; ============================================================================
;;; Example 5: Ruled Loft - Transition Shape
;;; ============================================================================

(clad.dsl:defpart ruled-transition ((height 25))
  "Sharp transition using ruled loft (straight lines between sections)"
  (:body
    (clad.core:make-loft
      (list (clad.core:make-circle-wire '(0 0 0) 15)
            (clad.core:make-circle-wire `(0 0 ,height) 8))
      :solid t
      :ruled t)))  ;; Note: ruled=t for straight edges

(format t "Example 5: Ruled Loft Transition~%")
(let ((transition (ruled-transition)))
  (format t "✓ Created ruled loft transition~%")
  (clad:view transition :name "ruled-transition")
  (format t "✓ Viewing at http://localhost:8080/?model=/models/ruled-transition.glb~%~%"))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════╗~%")
(format t "║         Phase 8 Advanced Features - DSL Integration         ║~%")
(format t "╚══════════════════════════════════════════════════════════════╝~%~%")

(format t "All Phase 8 features demonstrated:~%")
(format t "  ✓ Loft (smooth & ruled)~%")
(format t "  ✓ Pipe sweep~%")
(format t "  ✓ Profile sweep~%")
(format t "  ✓ Mirroring~%~%")

(format t "View models in your browser:~%")
(format t "  → http://localhost:8080~%~%")

(format t "The viewer will stay open - press Ctrl+C when done.~%~%")

;; Keep the viewer server running
(format t "Viewer server running...~%")
(sleep 600)  ; Keep alive for 10 minutes
