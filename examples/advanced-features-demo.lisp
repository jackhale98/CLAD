;;;; examples/advanced-features-demo.lisp --- Demonstration of Phase 8 advanced features

;;; Load the CLAD system first
(unless (find-package :clad.core)
  (format t "~%;; Loading CLAD system...~%")
  (asdf:load-system :clad))

(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Pipe Sweep - Create tube from path
;;; ============================================================================

(defun example-1-pipe-sweep ()
  "Create a tube by sweeping circular profile along a spline path."
  (format t "~%Example 1: Pipe Sweep~%")
  (format t "Creating tube by sweeping along 3D curve...~%")

  (let* (;; Create a 3D curved path using spline
         (path (clad.core:make-spline '((0 0 0)
                                         (20 10 10)
                                         (40 5 20)
                                         (60 15 25))
                                       :closed nil))
         ;; Sweep circular profile with radius 3mm along the path
         (tube (clad.core:make-pipe path 3)))

    (format t "✓ Created tube with 3mm radius along spline path~%")
    tube))

;;; ============================================================================
;;; Example 2: Profile Sweep - Custom cross-section
;;; ============================================================================

(defun example-2-profile-sweep ()
  "Sweep a custom profile along a path."
  (format t "~%Example 2: Profile Sweep~%")
  (format t "Sweeping rectangular profile along curved path...~%")

  (let* (;; Create path - an arc
         (path (clad.core:make-arc '(0 0 0) 30 0 180 :axis '(0 0 1)))

         ;; Create rectangular wire profile at path start
         ;; We'll create a simple square profile
         (profile-points '((0 -2 -2)
                          (0 2 -2)
                          (0 2 2)
                          (0 -2 2)
                          (0 -2 -2)))
         (profile-edges (loop for (p1 p2) on profile-points
                             while p2
                             collect (clad.core:make-line (apply #'list p1)
                                                          (apply #'list p2))))
         (profile (clad.core:make-wire profile-edges))

         ;; Sweep the profile along the arc
         (swept (clad.core:make-sweep profile path)))

    (format t "✓ Created swept shape with rectangular cross-section~%")
    swept))

;;; ============================================================================
;;; Example 3: Loft - Smooth transition between profiles
;;; ============================================================================

(defun example-3-loft ()
  "Create smooth loft through multiple circular profiles."
  (format t "~%Example 3: Loft (Smooth Transition)~%")
  (format t "Creating loft through 4 circular sections...~%")

  (let* (;; Create 4 circular profiles at different heights and sizes
         (circle1 (clad.core:make-arc '(0 0 0) 10 0 360))
         (circle2 (clad.core:make-arc '(0 0 15) 7 0 360))
         (circle3 (clad.core:make-arc '(0 0 30) 5 0 360))
         (circle4 (clad.core:make-arc '(0 0 45) 2 0 360))

         ;; Loft through all sections to create smooth shape
         (lofted (clad.core:make-loft (list circle1 circle2 circle3 circle4)
                                       :solid t
                                       :ruled nil)))  ; smooth, not ruled

    (format t "✓ Created smooth lofted solid~%")
    lofted))

;;; ============================================================================
;;; Example 4: Ruled Loft - Sharp transitions
;;; ============================================================================

(defun example-4-ruled-loft ()
  "Create ruled loft for sharp transitions."
  (format t "~%Example 4: Ruled Loft~%")
  (format t "Creating ruled loft between square and circle...~%")

  (let* (;; Square profile at bottom
         (square-pts '((10 10 0) (-10 10 0) (-10 -10 0) (10 -10 0) (10 10 0)))
         (square-edges (loop for (p1 p2) on square-pts
                            while p2
                            collect (clad.core:make-line p1 p2)))
         (square (clad.core:make-wire square-edges))

         ;; Circle profile at top
         (circle (clad.core:make-arc '(0 0 30) 8 0 360))

         ;; Ruled loft creates straight lines between profiles
         (lofted (clad.core:make-loft (list square circle)
                                       :solid t
                                       :ruled t)))

    (format t "✓ Created ruled loft from square to circle~%")
    lofted))

;;; ============================================================================
;;; Example 5: Shelling - Hollow out a solid
;;; ============================================================================

(defun example-5-shelling ()
  "Create hollow shell from solid box."
  (format t "~%Example 5: Shelling~%")
  (format t "Creating hollow box with 2mm walls...~%")

  (let* (;; Create solid box
         (box (clad.core:make-box 50 50 50))

         ;; Get faces to remove (we'll remove the top face for open box)
         (faces (clad.shapes:faces box))
         (top-face (first faces))  ; Get one face to remove

         ;; Shell the box with 2mm wall thickness, removing top face
         (hollow-box (clad.core:make-shell box
                                            (list top-face)
                                            -2)))  ; Negative = inward offset

    (format t "✓ Created hollow box with top face removed~%")
    hollow-box))

;;; ============================================================================
;;; Example 6: Shelling without face removal
;;; ============================================================================

(defun example-6-shell-no-removal ()
  "Create completely enclosed shell."
  (format t "~%Example 6: Complete Shelling~%")
  (format t "Creating enclosed hollow sphere...~%")

  (let* (;; Create solid sphere
         (sphere (clad.core:make-sphere 20))

         ;; Shell without removing any faces
         (hollow-sphere (clad.core:make-shell sphere nil -2)))

    (format t "✓ Created completely enclosed hollow sphere~%")
    hollow-sphere))

;;; ============================================================================
;;; Example 7: Mirroring - Create symmetric shapes
;;; ============================================================================

(defun example-7-mirror ()
  "Mirror shape to create symmetric part."
  (format t "~%Example 7: Mirroring~%")
  (format t "Creating symmetric shape by mirroring across YZ plane...~%")

  (let* (;; Create half of a shape
         (half (clad.core:make-box 20 30 40))

         ;; Mirror across YZ plane (X=0)
         (mirrored (clad.core:mirror-shape half
                                            '(0 0 0)    ; Point on plane
                                            '(1 0 0)))  ; Normal (X-axis)

         ;; Union original and mirrored to create full symmetric shape
         (symmetric (clad.core:union-shapes half mirrored)))

    (format t "✓ Created symmetric shape~%")
    symmetric))

;;; ============================================================================
;;; Example 8: Combined - Loft + Shell + Mirror
;;; ============================================================================

(defun example-8-combined ()
  "Combine multiple advanced features."
  (format t "~%Example 8: Combined Advanced Features~%")
  (format t "Creating complex part with loft, shell, and mirror...~%")

  (let* (;; Create lofted shape
         (circle1 (clad.core:make-arc '(0 0 0) 15 0 360))
         (circle2 (clad.core:make-arc '(0 0 25) 8 0 360))
         (lofted (clad.core:make-loft (list circle1 circle2) :solid t))

         ;; Shell it
         (faces (clad.shapes:faces lofted))
         (top-face (first faces))
         (shelled (clad.core:make-shell lofted (list top-face) -1.5))

         ;; Mirror it
         (mirrored (clad.core:mirror-shape shelled '(0 0 0) '(1 0 0)))
         (combined (clad.core:union-shapes shelled mirrored)))

    (format t "✓ Created complex symmetric hollow part~%")
    combined))

;;; ============================================================================
;;; Main Demo Function
;;; ============================================================================

(defun run-advanced-features-demo ()
  "Run all advanced feature demonstrations."
  (format t "~%╔════════════════════════════════════════════════════════╗~%")
  (format t "║    CLAD Phase 8: Advanced Features Demo              ║~%")
  (format t "╚════════════════════════════════════════════════════════╝~%")

  ;; Run all examples
  (let ((results (list
                  (example-1-pipe-sweep)
                  (example-2-profile-sweep)
                  (example-3-loft)
                  (example-4-ruled-loft)
                  (example-5-shelling)
                  (example-6-shell-no-removal)
                  (example-7-mirror)
                  (example-8-combined))))

    (format t "~%╔════════════════════════════════════════════════════════╗~%")
    (format t "║         All Examples Completed Successfully!          ║~%")
    (format t "╚════════════════════════════════════════════════════════╝~%")
    (format t "~%Created ~A advanced feature examples.~%"  (length results))
    (format t "~%Phase 8 Advanced Features:~%")
    (format t "  ✓ Sweeps (pipe and profile)~%")
    (format t "  ✓ Lofts (smooth and ruled)~%")
    (format t "  ✓ Shelling (with and without face removal)~%")
    (format t "  ✓ Mirroring (for symmetric parts)~%")
    (format t "~%To visualize any example in the web viewer:~%")
    (format t "  (clad:view (example-1-pipe-sweep) :name \\\"pipe\\\")~%")
    (format t "  (clad:view (example-3-loft) :name \\\"loft\\\")~%")
    (format t "  (clad:view (example-8-combined) :name \\\"combined\\\")~%")
    (format t "~%")

    ;; Return the most interesting one
    (example-8-combined)))

;;; ============================================================================
;;; Interactive Testing Functions
;;; ============================================================================

(defun test-feature (feature-name)
  "Quick test function to view a specific example."
  (format t "~%Loading and viewing ~A...~%" feature-name)
  (let ((shape (case feature-name
                 (:pipe (example-1-pipe-sweep))
                 (:profile-sweep (example-2-profile-sweep))
                 (:loft (example-3-loft))
                 (:ruled-loft (example-4-ruled-loft))
                 (:shell (example-5-shelling))
                 (:shell-enclosed (example-6-shell-no-removal))
                 (:mirror (example-7-mirror))
                 (:combined (example-8-combined))
                 (otherwise (error "Unknown example: ~A" feature-name)))))
    (clad:view shape :name (format nil "~A" feature-name))
    (format t "~%Viewer started at http://localhost:8080~%")
    shape))

;;; ============================================================================
;;; Usage Examples
;;; ============================================================================

(format t "~%;; Load this file with: (load \\\"examples/advanced-features-demo.lisp\\\")~%")
(format t ";; Run all demos: (run-advanced-features-demo)~%")
(format t ";; Test specific feature: (test-feature :loft)~%")
(format t ";; Available features: :pipe :profile-sweep :loft :ruled-loft~%")
(format t ";;                    :shell :shell-enclosed :mirror :combined~%")
