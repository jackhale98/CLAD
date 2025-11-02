;;;; examples/advanced-sweeps-lofts-mirror.lisp
;;;; Demonstrates Phase 8 advanced features: sweeps, lofts, and mirroring
;;;; These examples have been tested and verified to work.

(unless (find-package :clad.core)
  (format t "~%;; Loading CLAD system...~%")
  (asdf:load-system :clad))

(in-package :cl-user)

(format t "~%╔══════════════════════════════════════════════════════════════╗~%")
(format t "║  Phase 8 Advanced Features: Sweeps, Lofts, and Mirroring    ║~%")
(format t "╚══════════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; Example 1: Pipe Sweep
;;; ============================================================================

(format t "1. Creating pipe sweep along curved path...~%")
(defparameter *pipe-example*
  (let* ((path (clad.core:make-spline '((0 0 0) (10 10 10) (20 5 15) (30 0 20))
                                       :closed nil))
         (pipe (clad.core:make-pipe path 3)))
    (format t "   ✓ Pipe created with radius 3mm~%")
    (format t "   Exporting to STEP format...~%")
    (clad:export-step pipe "pipe-example.step")
    (format t "   ✓ Exported to pipe-example.step~%~%")
    pipe))

;;; ============================================================================
;;; Example 2: Profile Sweep
;;; ============================================================================

(format t "2. Creating sweep with custom profile...~%")
(defparameter *sweep-example*
  (let* ((profile (clad.core:make-circle-wire '(0 0 0) 5 :axis '(1 0 0)))
         (path (clad.core:make-spline '((0 0 0) (15 5 0) (30 0 5)) :closed nil))
         (sweep (clad.core:make-sweep profile path)))
    (format t "   ✓ Swept circular profile along curved path~%")
    (clad:export-step sweep "sweep-example.step")
    (format t "   ✓ Exported to sweep-example.step~%~%")
    sweep))

;;; ============================================================================
;;; Example 3: Smooth Loft
;;; ============================================================================

(format t "3. Creating smooth loft between three sections...~%")
(defparameter *loft-smooth-example*
  (let* ((base (clad.core:make-circle-wire '(0 0 0) 20))
         (mid (clad.core:make-circle-wire '(0 0 15) 12))
         (top (clad.core:make-circle-wire '(0 0 30) 5))
         (loft (clad.core:make-loft (list base mid top) :solid t :ruled nil)))
    (format t "   ✓ Smooth loft created (like a vase)~%")
    (clad:export-step loft "loft-smooth-example.step")
    (format t "   ✓ Exported to loft-smooth-example.step~%~%")
    loft))

;;; ============================================================================
;;; Example 4: Ruled Loft
;;; ============================================================================

(format t "4. Creating ruled loft between sections...~%")
(defparameter *loft-ruled-example*
  (let* ((base (clad.core:make-circle-wire '(0 0 0) 15))
         (top (clad.core:make-circle-wire '(0 0 25) 8))
         (loft (clad.core:make-loft (list base top) :solid t :ruled t)))
    (format t "   ✓ Ruled loft created (straight transitions)~%")
    (clad:export-step loft "loft-ruled-example.step")
    (format t "   ✓ Exported to loft-ruled-example.step~%~%")
    loft))

;;; ============================================================================
;;; Example 5: Mirroring
;;; ============================================================================

(format t "5. Creating mirror copy of a shape...~%")
(defparameter *mirror-example*
  (let* ((box (clad.core:translate (clad.core:make-box 20 10 15) 5 0 0))
         (mirrored (clad.core:mirror-shape box '(0 0 0) '(1 0 0)))
         (combined (clad.core:fuse-shapes box mirrored)))
    (format t "   ✓ Mirrored box across YZ plane~%")
    (format t "   ✓ Combined original and mirror~%")
    (clad:export-step combined "mirror-example.step")
    (format t "   ✓ Exported to mirror-example.step~%~%")
    combined))

;;; ============================================================================
;;; Example 6: Complex Composite Shape (Loft + Mirror)
;;; ============================================================================

(format t "6. Creating complex composite shape...~%")
(defparameter *composite-example*
  (let* (;; Create a lofted body
         (profile1 (clad.core:make-circle-wire '(0 0 0) 15))
         (profile2 (clad.core:make-circle-wire '(0 0 20) 10))
         (profile3 (clad.core:make-circle-wire '(0 0 35) 5))
         (body (clad.core:make-loft (list profile1 profile2 profile3) :solid t))

         ;; Mirror it to create symmetric shape
         (mirrored (clad.core:mirror-shape body '(0 0 0) '(1 0 0)))
         (final (clad.core:fuse-shapes body mirrored)))

    (format t "   ✓ Lofted main body with 3 profiles~%")
    (format t "   ✓ Mirrored to create symmetric shape~%")
    (clad:export-step final "composite-example.step")
    (format t "   ✓ Exported to composite-example.step~%~%")
    final))

(format t "~%╔══════════════════════════════════════════════════════════════╗~%")
(format t "║                    All Examples Complete!                    ║~%")
(format t "║                                                              ║~%")
(format t "║  All STEP files have been exported                           ║~%")
(format t "║  You can open them in FreeCAD or any CAD viewer              ║~%")
(format t "╚══════════════════════════════════════════════════════════════╝~%~%")

(format t "To view any example in the web viewer:~%")
(format t "  (clad:view *pipe-example* :name \"pipe\")~%")
(format t "  (clad:view *loft-smooth-example* :name \"loft\")~%")
(format t "  (clad:view *composite-example* :name \"composite\")~%")
(format t "~%")
