;;;; examples/curves-demo.lisp --- Demonstration of splines, Bezier curves, arcs, and wires

;;; Load the CLAD system first
(unless (find-package :clad.core)
  (format t "~%;; Loading CLAD system...~%")
  (asdf:load-system :clad))

(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Simple Spline Curve
;;; ============================================================================

(defun example-1-simple-spline ()
  "Create a simple open spline through points."
  (format t "~%Example 1: Simple Spline Curve~%")
  (format t "Creating smooth spline through 5 points...~%")

  (let* ((points '((0 0 0) (10 5 0) (20 8 0) (30 5 0) (40 0 0)))
         (spline (clad.core:make-spline points :closed nil)))

    (format t "✓ Created spline curve~%")
    spline))

;;; ============================================================================
;;; Example 2: Closed Spline Profile
;;; ============================================================================

(defun example-2-closed-spline ()
  "Create a closed spline forming a smooth profile."
  (format t "~%Example 2: Closed Spline Profile~%")
  (format t "Creating closed spline for a smooth organic shape...~%")

  (let* ((points '((0 0 0)
                  (15 0 0)
                  (20 10 0)
                  (15 20 0)
                  (0 20 0)
                  (-5 10 0)))
         (spline (clad.core:make-spline points :closed t)))

    (format t "✓ Created closed spline~%")
    spline))

;;; ============================================================================
;;; Example 3: Bezier Curves
;;; ============================================================================

(defun example-3-bezier-curves ()
  "Demonstrate different types of Bezier curves."
  (format t "~%Example 3: Bezier Curves~%")

  ;; Quadratic Bezier (3 control points)
  (format t "Creating quadratic Bezier curve (3 control points)...~%")
  (let ((quadratic (clad.core:make-bezier '((0 0 0) (5 10 0) (10 0 0)))))
    (format t "✓ Quadratic Bezier~%")

    ;; Cubic Bezier (4 control points)
    (format t "Creating cubic Bezier curve (4 control points)...~%")
    (let ((cubic (clad.core:make-bezier '((0 0 0) (3 5 0) (7 5 0) (10 0 0)))))
      (format t "✓ Cubic Bezier~%")

      ;; Higher-degree Bezier (6 control points)
      (format t "Creating higher-degree Bezier (6 control points)...~%")
      (let ((complex (clad.core:make-bezier '((0 0 0)
                                               (2 8 0)
                                               (4 12 0)
                                               (6 12 0)
                                               (8 8 0)
                                               (10 0 0)))))
        (format t "✓ Complex Bezier~%")

        ;; Return the complex one for visualization
        complex))))

;;; ============================================================================
;;; Example 4: Circular Arcs
;;; ============================================================================

(defun example-4-circular-arcs ()
  "Demonstrate arc creation methods."
  (format t "~%Example 4: Circular Arcs~%")

  ;; Arc through 3 points
  (format t "Creating arc through 3 points...~%")
  (let ((arc-3pt (clad.core:make-arc-3points '(0 0 0) '(5 5 0) '(10 0 0))))
    (format t "✓ Arc through 3 points~%")

    ;; Arc by center/radius/angles
    (format t "Creating quarter-circle arc by center/radius...~%")
    (let ((quarter-circle (clad.core:make-arc '(20 0 0) 10 0 90)))
      (format t "✓ Quarter circle~%")

      ;; Semicircle
      (format t "Creating semicircle...~%")
      (let ((semicircle (clad.core:make-arc '(50 50 0) 20 0 180)))
        (format t "✓ Semicircle~%")

        ;; Arc in YZ plane (custom axis)
        (format t "Creating arc in YZ plane...~%")
        (let ((yz-arc (clad.core:make-arc '(0 0 0) 15 0 90 :axis '(1 0 0))))
          (format t "✓ Arc in YZ plane~%")

          semicircle)))))

;;; ============================================================================
;;; Example 5: Wires from Mixed Curve Types
;;; ============================================================================

(defun example-5-mixed-wire ()
  "Create wire from lines, arcs, and splines."
  (format t "~%Example 5: Wire from Mixed Curve Types~%")
  (format t "Creating smooth path with lines, arcs, and splines...~%")

  (let* (;; Start with a straight line
         (line1 (clad.core:make-line '(0 0 0) '(10 0 0)))
         (_ (format t "  ✓ Added line segment~%"))

         ;; Transition with an arc (curved transition)
         (arc1 (clad.core:make-arc-3points '(10 0 0) '(13 4 0) '(20 10 0)))
         (_ (format t "  ✓ Added arc transition~%"))

         ;; Continue with a smooth spline
         (spline1 (clad.core:make-spline '((20 10 0) (30 15 0) (40 12 0)) :closed nil))
         (_ (format t "  ✓ Added spline section~%"))

         ;; Final arc back down (with curved arc, not collinear)
         (arc2 (clad.core:make-arc-3points '(40 12 0) '(45 8 0) '(50 6 0)))
         (_ (format t "  ✓ Added final arc~%"))

         ;; Combine into wire
         (wire (clad.core:make-wire (list line1 arc1 spline1 arc2))))

    (format t "✓ Created smooth path wire~%")
    wire))

;;; ============================================================================
;;; Example 6: Bezier S-Curve Path
;;; ============================================================================

(defun example-6-bezier-path ()
  "Create smooth S-curve using Bezier curves."
  (format t "~%Example 6: Bezier S-Curve Path~%")
  (format t "Creating smooth S-curve with connected Bezier curves...~%")

  (let* (;; First curve: upward bend
         (bezier1 (clad.core:make-bezier '((0 0 0) (10 20 0) (20 20 0) (30 0 0))))
         (_ (format t "  ✓ First Bezier curve~%"))

         ;; Second curve: downward bend (continues from first)
         (bezier2 (clad.core:make-bezier '((30 0 0) (40 -20 0) (50 -20 0) (60 0 0))))
         (_ (format t "  ✓ Second Bezier curve~%"))

         ;; Combine into wire
         (wire (clad.core:make-wire (list bezier1 bezier2))))

    (format t "✓ Created S-curve path~%")
    wire))

;;; ============================================================================
;;; Example 7: Complex Wave Pattern with Splines
;;; ============================================================================

(defun example-7-wave-pattern ()
  "Create wave pattern using spline with many points."
  (format t "~%Example 7: Wave Pattern with Splines~%")
  (format t "Creating wave pattern with interpolated points...~%")

  (let* ((wave-points '((0 0 0)
                       (5 5 0)
                       (10 0 0)
                       (15 -5 0)
                       (20 0 0)
                       (25 5 0)
                       (30 0 0)
                       (35 -5 0)
                       (40 0 0)
                       (45 5 0)
                       (50 0 0)))
         (spline (clad.core:make-spline wave-points :closed nil)))

    (format t "✓ Created wave pattern with ~A points~%" (length wave-points))
    spline))

;;; ============================================================================
;;; Example 8: Organic Profile with Mixed Curves
;;; ============================================================================

(defun example-8-organic-profile ()
  "Create organic-looking profile using mixed curve types."
  (format t "~%Example 8: Organic Profile (Smooth Spline)~%")
  (format t "Creating organic shape profile...~%")

  (let* (;; Create organic profile using a single smooth spline
         (profile-points '((0 0 0)
                          (10 -2 0)
                          (20 0 0)
                          (30 5 0)
                          (35 12 0)
                          (30 18 0)
                          (20 22 0)
                          (10 22 0)
                          (0 18 0)
                          (-5 12 0)
                          (-5 5 0)))
         (profile-spline (clad.core:make-spline profile-points :closed nil)))

    (format t "✓ Created organic profile with ~A points~%" (length profile-points))
    profile-spline))

;;; ============================================================================
;;; Example 9: 3D Helical Spline
;;; ============================================================================

(defun example-9-helical-spline ()
  "Create 3D helical path using spline."
  (format t "~%Example 9: 3D Helical Spline~%")
  (format t "Creating helical path in 3D space...~%")

  (let* ((helix-points (loop for i from 0 to 8
                            for angle = (* i (/ pi 4))
                            for z = (* i 5)
                            for x = (* 10 (cos angle))
                            for y = (* 10 (sin angle))
                            collect (list x y z)))
         (helix (clad.core:make-spline helix-points :closed nil)))

    (format t "✓ Created helical spline with ~A points~%" (length helix-points))
    helix))

;;; ============================================================================
;;; Main Demo Function
;;; ============================================================================

(defun run-curves-demo ()
  "Run all curve demonstrations."
  (format t "~%╔════════════════════════════════════════════════════════╗~%")
  (format t "║      CLAD Phase 8: Splines and Curves Demo           ║~%")
  (format t "╚════════════════════════════════════════════════════════╝~%")

  ;; Run all examples
  (let ((results (list
                  (example-1-simple-spline)
                  (example-2-closed-spline)
                  (example-3-bezier-curves)
                  (example-4-circular-arcs)
                  (example-5-mixed-wire)
                  (example-6-bezier-path)
                  (example-7-wave-pattern)
                  (example-8-organic-profile)
                  (example-9-helical-spline))))

    (format t "~%╔════════════════════════════════════════════════════════╗~%")
    (format t "║           All Examples Completed Successfully!        ║~%")
    (format t "╚════════════════════════════════════════════════════════╝~%")
    (format t "~%Created ~A curve examples.~%" (length results))
    (format t "~%To visualize any example in the web viewer:~%")
    (format t "  (clad:view (example-1-simple-spline) :name \"spline\")~%")
    (format t "  (clad:view (example-5-mixed-wire) :name \"mixed-wire\")~%")
    (format t "  (clad:view (example-8-organic-profile) :name \"organic\")~%")
    (format t "~%")

    ;; Return the most interesting one
    (example-8-organic-profile)))

;;; ============================================================================
;;; Interactive Testing Functions
;;; ============================================================================

(defun test-curve (curve-name)
  "Quick test function to view a specific example."
  (format t "~%Loading and viewing ~A...~%" curve-name)
  (let ((shape (case curve-name
                 (:spline (example-1-simple-spline))
                 (:closed-spline (example-2-closed-spline))
                 (:bezier (example-3-bezier-curves))
                 (:arcs (example-4-circular-arcs))
                 (:mixed (example-5-mixed-wire))
                 (:s-curve (example-6-bezier-path))
                 (:wave (example-7-wave-pattern))
                 (:organic (example-8-organic-profile))
                 (:helix (example-9-helical-spline))
                 (otherwise (error "Unknown example: ~A" curve-name)))))
    (clad:view shape :name (format nil "~A" curve-name))
    (format t "~%Viewer started at http://localhost:8080~%")
    shape))

;;; ============================================================================
;;; Usage Examples
;;; ============================================================================

(format t "~%;; Load this file with: (load \"examples/curves-demo.lisp\")~%")
(format t ";; Run all demos: (run-curves-demo)~%")
(format t ";; Test specific example: (test-curve :organic)~%")
(format t ";; Available examples: :spline :closed-spline :bezier :arcs :mixed~%")
(format t ";;                    :s-curve :wave :organic :helix~%")
