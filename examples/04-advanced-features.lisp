;;;; examples/04-advanced-features.lisp --- Advanced CAD Features
;;;;
;;;; This file demonstrates loft, sweep, pipe, and other advanced operations.
;;;; Run with: (load "examples/04-advanced-features.lisp")

(asdf:load-system :clad)
(in-package :cl-user)

;;; ============================================================================
;;; Example 1: Loft - Morphing Between Profiles
;;; ============================================================================

(defun demo-simple-loft ()
  (format t "~%Example 1: Simple Loft~%")
  (format t "======================~%~%")
  (format t "Creating a transition from square to circle~%")
  (format t "Lofting creates smooth transitions between profiles~%")

  ;; Create two profiles at different heights
  (let* (;; Create square wire from 4 line segments
         (square-wire (let* ((p1 '(0 0 0))
                             (p2 '(40 0 0))
                             (p3 '(40 40 0))
                             (p4 '(0 40 0))
                             (e1 (clad.core:make-line p1 p2))
                             (e2 (clad.core:make-line p2 p3))
                             (e3 (clad.core:make-line p3 p4))
                             (e4 (clad.core:make-line p4 p1)))
                        (clad.core:make-wire (list e1 e2 e3 e4))))
         (circle-wire (clad.core:make-circle-wire '(20 20 60) 15))
         ;; Loft between them
         (lofted (clad.core:make-loft (list square-wire circle-wire)
                                       :solid t :ruled nil)))
    (format t "Created loft from 40x40mm square to 30mm diameter circle~%")
    (format t "Height: 60mm~%")
    lofted))

;;; ============================================================================
;;; Example 2: Multi-Section Loft
;;; ============================================================================

(defun demo-multi-section-loft ()
  (format t "~%Example 2: Multi-Section Loft~%")
  (format t "==============================~%~%")
  (format t "Lofting through multiple profiles~%")
  (format t "Creates complex organic shapes~%")

  (let* ((profile1 (clad.core:make-circle-wire '(25 25 0) 20))
         ;; Create rectangle wire from 4 line segments
         (profile2 (let* ((x 10) (y 10) (z 20) (w 30) (h 30))
                     (let* ((p1 (list x y z))
                            (p2 (list (+ x w) y z))
                            (p3 (list (+ x w) (+ y h) z))
                            (p4 (list x (+ y h) z))
                            (e1 (clad.core:make-line p1 p2))
                            (e2 (clad.core:make-line p2 p3))
                            (e3 (clad.core:make-line p3 p4))
                            (e4 (clad.core:make-line p4 p1)))
                       (clad.core:make-wire (list e1 e2 e3 e4)))))
         (profile3 (clad.core:make-circle-wire '(25 25 40) 15))
         ;; Create rectangle wire from 4 line segments
         (profile4 (let* ((x 15) (y 15) (z 60) (w 20) (h 20))
                     (let* ((p1 (list x y z))
                            (p2 (list (+ x w) y z))
                            (p3 (list (+ x w) (+ y h) z))
                            (p4 (list x (+ y h) z))
                            (e1 (clad.core:make-line p1 p2))
                            (e2 (clad.core:make-line p2 p3))
                            (e3 (clad.core:make-line p3 p4))
                            (e4 (clad.core:make-line p4 p1)))
                       (clad.core:make-wire (list e1 e2 e3 e4)))))
         (lofted (clad.core:make-loft (list profile1 profile2 profile3 profile4)
                                       :solid t :ruled nil)))
    (format t "Created loft through 4 sections~%")
    (format t "Alternating between circles and rectangles~%")
    lofted))

;;; ============================================================================
;;; Example 3: Sweep - Following a Path
;;; ============================================================================

(defun demo-sweep-path ()
  (format t "~%Example 3: Sweep Along Path~%")
  (format t "============================~%~%")
  (format t "Sweeping a circular profile along a spline path~%")
  (format t "Creates tubes and rails~%")

  ;; Create a path (spline curve)
  (let* ((path-points '((0 0 0) (20 20 10) (40 20 20) (60 0 30) (80 -20 40)))
         (path (clad.core:make-spline path-points :closed nil))
         ;; Create profile (circle perpendicular to path start)
         (profile (clad.core:make-circle-wire '(0 0 0) 5 :axis '(1 0 0)))
         ;; Sweep the profile along the path
         (swept (clad.core:make-sweep profile path)))
    (format t "Created a tube following a curved path~%")
    (format t "Tube diameter: 10mm~%")
    (format t "Path: 5 points~%")
    swept))

;;; ============================================================================
;;; Example 4: Pipe - Simplified Sweep
;;; ============================================================================

(defun demo-pipe ()
  (format t "~%Example 4: Pipe Operation~%")
  (format t "==========================~%~%")
  (format t "Pipe is a simplified sweep with circular cross-section~%")

  (let* ((spine '((0 0 0) (30 0 15) (60 20 30) (90 40 30) (120 60 15)))
         (path (clad.core:make-spline spine :closed nil))
         (pipe (clad.core:make-pipe path 8)))
    (format t "Created a pipe with 8mm radius~%")
    (format t "Following a smooth spline curve~%")
    pipe))

;;; ============================================================================
;;; Example 5: Combining Loft with Boolean Operations
;;; ============================================================================

(defun demo-loft-with-boolean ()
  (format t "~%Example 5: Loft with Boolean Operations~%")
  (format t "========================================~%~%")
  (format t "Creating a funnel by lofting and subtracting~%")

  (let* (;; Outer loft (funnel shape)
         (outer-top (clad.core:make-circle-wire '(30 30 40) 25))
         (outer-bottom (clad.core:make-circle-wire '(30 30 0) 10))
         (outer (clad.core:make-loft (list outer-bottom outer-top)
                                      :solid t :ruled nil))
         ;; Inner loft (hollow it out)
         (inner-top (clad.core:make-circle-wire '(30 30 41) 22))
         (inner-bottom (clad.core:make-circle-wire '(30 30 -1) 8))
         (inner (clad.core:make-loft (list inner-bottom inner-top)
                                      :solid t :ruled nil))
         ;; Subtract to create hollow funnel
         (funnel (clad.core:cut-shapes outer inner)))
    (format t "Created a hollow funnel using two lofts~%")
    (format t "Outer diameter: 50mm (top) to 20mm (bottom)~%")
    (format t "Wall thickness: 3mm~%")
    funnel))

;;; ============================================================================
;;; Example 6: Swept Handle
;;; ============================================================================

(defun demo-swept-handle ()
  (format t "~%Example 6: Swept Handle~%")
  (format t "========================~%~%")
  (format t "Creating an ergonomic handle using sweep~%")

  ;; Create handle path (curved)
  (let* ((handle-path-points '((0 0 0) (20 10 0) (40 15 0)
                               (60 15 0) (80 10 0) (100 0 0)))
         (handle-path (clad.core:make-spline handle-path-points :closed nil))
         ;; Create varying profile (thicker in middle)
         ;; For simplicity, use constant circular profile
         (handle-profile (clad.core:make-circle-wire '(0 0 0) 8 :axis '(1 0 0)))
         ;; Sweep it
         (handle (clad.core:make-sweep handle-profile handle-path)))
    (format t "Created ergonomic handle~%")
    (format t "Length: 100mm~%")
    (format t "Diameter: 16mm~%")
    handle))

;;; ============================================================================
;;; Example 7: Complex Shape - Transition Duct
;;; ============================================================================

(defun demo-transition-duct ()
  (format t "~%Example 7: Transition Duct~%")
  (format t "===========================~%~%")
  (format t "HVAC duct transitioning from rectangle to circle~%")

  (let* (;; Rectangular inlet (100x60mm) - create from line segments
         (inlet (let* ((p1 '(0 0 0))
                       (p2 '(100 0 0))
                       (p3 '(100 60 0))
                       (p4 '(0 60 0))
                       (e1 (clad.core:make-line p1 p2))
                       (e2 (clad.core:make-line p2 p3))
                       (e3 (clad.core:make-line p3 p4))
                       (e4 (clad.core:make-line p4 p1)))
                  (clad.core:make-wire (list e1 e2 e3 e4))))
         ;; Circular outlet (80mm diameter)
         (outlet (clad.core:make-circle-wire '(50 30 200) 40))
         ;; Loft between them
         (duct (clad.core:make-loft (list inlet outlet) :solid t :ruled nil)))
    (format t "Created transition duct:~%")
    (format t "  Inlet: 100x60mm rectangle~%")
    (format t "  Outlet: 80mm diameter circle~%")
    (format t "  Length: 200mm~%")
    duct))

;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(defun run-advanced-demos ()
  "Run all advanced feature demonstrations"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                   CLAD Advanced Features Examples~%")
  (format t "================================================================================~%")

  (let ((part1 (demo-simple-loft))
        (part2 (demo-multi-section-loft))
        (part3 (demo-sweep-path))
        (part4 (demo-pipe))
        (part5 (demo-loft-with-boolean))
        (part6 (demo-swept-handle))
        (part7 (demo-transition-duct)))

    (format t "~%~%")
    (format t "================================================================================~%")
    (format t "All advanced feature examples completed!~%")
    (format t "~%To visualize:~%")
    (format t "  (clad:view part1 :name \"simple-loft\")~%")
    (format t "  (clad:view part2 :name \"multi-loft\")~%")
    (format t "  (clad:view part3 :name \"sweep\")~%")
    (format t "  (clad:view part4 :name \"pipe\")~%")
    (format t "  (clad:view part5 :name \"funnel\")~%")
    (format t "  (clad:view part6 :name \"handle\")~%")
    (format t "  (clad:view part7 :name \"duct\")~%")
    (format t "~%Note: These examples showcase:~%")
    (format t "  - Lofting between 2+ profiles~%")
    (format t "  - Sweeping profiles along paths~%")
    (format t "  - Pipe creation along splines~%")
    (format t "  - Combining advanced features with booleans~%")
    (format t "================================================================================~%")
    (format t "~%")

    (values part1 part2 part3 part4 part5 part6 part7)))

;; Auto-run when loaded
(run-advanced-demos)
