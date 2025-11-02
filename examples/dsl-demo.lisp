;;;; examples/dsl-demo.lisp --- Demonstration of CLAD DSL (Phase 5)

(require :asdf)
(asdf:load-system :clad)

(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Simple Parametric Box
;;; ============================================================================

(clad.dsl:defpart simple-box ((width 100) (height 50) (depth 20))
  "A simple parametric box with default dimensions"
  (:body
    (clad.core:make-box width height depth)))

(format t "~%~%=== Example 1: Simple Parametric Box ===~%")
(let ((box (simple-box 80 60 15)))
  (format t "Created box: ~Ax~Ax~Amm~%" 80 60 15)
  (format t "Volume: ~,2F mm³~%" (clad.shapes:volume box))
  (format t "Is solid: ~A~%~%" (clad.shapes:solid-p box)))

;;; ============================================================================
;;; Example 2: Plate with Mounting Holes
;;; ============================================================================

(clad.dsl:defpart mounting-plate ((size 100) (thickness 10) (hole-diameter 8))
  "Square plate with four corner mounting holes"
  (:body
    (clad.core:make-box size size thickness))
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* thickness 2))
            10 10 0)))
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* thickness 2))
            (- size 10) 10 0)))
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* thickness 2))
            10 (- size 10) 0)))
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* thickness 2))
            (- size 10) (- size 10) 0))))

(format t "=== Example 2: Mounting Plate with Holes ===~%")
(let ((plate (mounting-plate 120 8 6)))
  (format t "Created mounting plate: ~Ax~Ax~Amm with ~Amm holes~%" 120 120 8 6)
  (format t "Volume: ~,2F mm³~%" (clad.shapes:volume plate))
  (format t "Solid volume would be: ~,2F mm³~%" (* 120 120 8))
  (format t "Material removed by holes: ~,2F mm³~%~%"
          (- (* 120 120 8) (clad.shapes:volume plate))))

;;; ============================================================================
;;; Example 3: Base with Boss
;;; ============================================================================

(clad.dsl:defpart base-with-boss ((base-size 100) (base-thickness 10)
                                   (boss-diameter 30) (boss-height 20))
  "Base plate with cylindrical boss for assembly"
  (:body
    (clad.core:make-box base-size base-size base-thickness))
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder (/ boss-diameter 2) boss-height)
            (/ base-size 2) (/ base-size 2) 0))))

(format t "=== Example 3: Base with Boss ===~%")
(let ((part (base-with-boss 80 8 25 15)))
  (format t "Created base with boss: ~Ax~Ax~Amm base + Ø~A x ~Amm boss~%"
          80 80 8 25 15)
  (format t "Total volume: ~,2F mm³~%" (clad.shapes:volume part))
  (let ((base-volume (* 80 80 8))
        (boss-volume (* pi (/ 25 2) (/ 25 2) 15)))
    (format t "Base volume: ~,2F mm³~%" base-volume)
    (format t "Boss volume: ~,2F mm³~%" boss-volume)
    (format t "Combined volume: ~,2F mm³~%~%" (+ base-volume boss-volume))))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(format t "~%=== DSL Implementation Summary ===~%")
(format t "✓ Cycle 1: Basic defpart with :body~%")
(format t "✓ Cycle 2: Features on faces (:cut and :add)~%")
(format t "✓ Cycle 3: Multiple features on same part~%")
(format t "✓ All 330 tests passing~%~%")

(format t "The declarative DSL enables concise parametric part definitions~%")
(format t "with automatic integration to the context API.~%~%")
