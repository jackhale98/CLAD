;;;; examples/interactive-demo.lisp --- Interactive Parametric CAD Demo with Auto-Rebuild
;;;
;;; This file demonstrates CLAD's complete DSL with auto-rebuild functionality!
;;;
;;; USAGE:
;;;   1. Load this file: (load "examples/interactive-demo.lisp")
;;;   2. Show the part: (show 'demo-plate)
;;;   3. Modify parameters or the defpart below and re-evaluate
;;;   4. Watch the part automatically rebuild in your browser!
;;;
;;; The part will automatically open in your browser at http://localhost:8080
;;; Every time you redefine the part, it will automatically rebuild!

(require :asdf)
(asdf:load-system :clad)

(in-package :cl-user)

;;;; ============================================================================
;;;; EASY-TO-MODIFY PARAMETERS - Change these and re-evaluate!
;;;; ============================================================================

(defparameter *base-size* 100
  "Size of the base plate (mm)")

(defparameter *base-thickness* 10
  "Thickness of the base plate (mm)")

(defparameter *hole-diameter* 6
  "Diameter of mounting holes (mm)")

(defparameter *hole-pattern-radius* 35
  "Radius for circular hole pattern (mm)")

(defparameter *hole-pattern-count* 8
  "Number of holes in the circular pattern")

(defparameter *boss-diameter* 30
  "Diameter of central boss (mm)")

(defparameter *boss-height* 35
  "Height of central boss (mm)")

;;;; ============================================================================
;;;; REUSABLE FEATURES - Demonstrate deffeature
;;;; ============================================================================

(clad.dsl:deffeature mounting-hole ((diameter 6))
  "Standard mounting hole"
  (:cut (clad.core:make-cylinder (/ diameter 2) 20)))

(clad.dsl:deffeature lightening-hole ((radius 3))
  "Lightening hole for weight reduction"
  (:cut (clad.core:make-cylinder radius 15)))

(clad.dsl:deffeature boss ((diameter 30) (height 15))
  "Cylindrical boss/standoff"
  (:add (clad.core:make-cylinder (/ diameter 2) height)))

;;;; ============================================================================
;;;; MAIN PARAMETRIC PART - Uses all DSL features with AUTO-REBUILD!
;;;; ============================================================================

(clad.dsl:defpart demo-plate
    ((size 100)
     (thickness 10)
     (hole-dia 6)
     (pattern-radius 35)
     (pattern-count 8)
     (boss-dia 30)
     (boss-h 15))
  "Interactive demo part showcasing ALL CLAD DSL features:
   - Parametric dimensions
   - Reusable features (deffeature)
   - Circular patterns
   - Boolean operations (add/cut)
   - Face selection IN ALL DIRECTIONS (+X, -X, +Y, -Y, +Z, -Z)
   - **AUTO-REBUILD** when redefined!"

  ;; Step 1: Create base plate
  (:body
    (clad.core:make-box size size thickness))

  ;; === TOP FACE (+Z) Features ===

  ;; Step 2: Add central boss on top
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder (/ boss-dia 2) boss-h)
            (/ size 2) (/ size 2) thickness)))

  ;; Step 3: Cut circular pattern of lightening holes around the boss
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count pattern-count
        :radius pattern-radius
        :center-x (/ size 2)
        :center-y (/ size 2)
      (lightening-hole :radius (/ hole-dia 2.5))))

  ;; Step 4: Cut mounting holes at corners (TOP)
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count 4
        :radius (* size 0.35)
        :center-x (/ size 2)
        :center-y (/ size 2)
        :angle-start 45
        :angle-end 315
      (mounting-hole :diameter hole-dia)))

  ;; Step 5: Cut a central hole through the boss
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-dia 2) (* boss-h 1.5))
            (/ size 2) (/ size 2) thickness)))

  ;; === BOTTOM FACE (-Z) Features ===

  ;; Step 6: Add support ribs on bottom
  (:on-face :direction :-z :extreme :min
    (:add (clad.core:translate
            (clad.core:make-box (* size 0.8) 3 (/ thickness 2))
            (* size 0.1) (/ size 2) (- (/ thickness 4)))))

  (:on-face :direction :-z :extreme :min
    (:add (clad.core:translate
            (clad.core:make-box 3 (* size 0.8) (/ thickness 2))
            (/ size 2) (* size 0.1) (- (/ thickness 4)))))

  ;; === +X FACE (Right side) Features ===

  ;; Step 7: Cut side access hole on +X face
  (:on-face :direction :+x :extreme :max
    (:cut (clad.core:translate
            (clad.core:rotate
              (clad.core:make-cylinder (/ hole-dia 1.5) 12)
              :y 90)
            size (/ size 2) (/ thickness 2))))

  ;; === -X FACE (Left side) Features ===

  ;; Step 8: Cut side access hole on -X face (opposite side)
  (:on-face :direction :-x :extreme :min
    (:cut (clad.core:translate
            (clad.core:rotate
              (clad.core:make-cylinder (/ hole-dia 1.5) 12)
              :y 90)
            -6 (/ size 2) (/ thickness 2))))

  ;; === +Y FACE (Front) Features ===

  ;; Step 9: Cut ventilation slots on +Y face
  (:on-face :direction :+y :extreme :max
    (:linear-pattern
        :count 3
        :spacing 15
        :direction-x 1
        :direction-y 0
        :start-x (/ size 3)
        :start-y (/ size 2)
      (:cut (clad.core:translate
              (clad.core:rotate
                (clad.core:make-box 8 12 3)
                :x 90)
              0 size (/ thickness 2)))))

  ;; === -Y FACE (Back) Features ===

  ;; Step 10: Add small mounting tab on -Y face
  (:on-face :direction :-y :extreme :min
    (:add (clad.core:translate
            (clad.core:make-box 20 8 (/ thickness 2))
            (/ size 2) -4 (/ thickness 4)))))

;;;; ============================================================================
;;;; CONVENIENCE FUNCTIONS
;;;; ============================================================================

(defun quick-show ()
  "Quick function to show the demo-plate with current parameters.

  This uses the NEW auto-rebuild feature! After calling this once,
  you can just redefine demo-plate and it will auto-update!"

  (format t "~%")
  (format t "╔══════════════════════════════════════════════════════════════╗~%")
  (format t "║   CLAD Interactive Demo - Parametric CAD with Auto-Rebuild  ║~%")
  (format t "╚══════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  (format t "CURRENT PARAMETERS:~%")
  (format t "  Base: ~Ax~Ax~A mm~%" *base-size* *base-size* *base-thickness*)
  (format t "  Boss: Ø~A x ~A mm~%" *boss-diameter* *boss-height*)
  (format t "  Holes: Ø~A mm x ~A (circular pattern)~%"
          *hole-diameter* *hole-pattern-count*)
  (format t "~%")

  ;; Use the NEW show function with auto-rebuild!
  (clad:show 'demo-plate
             :args (list *base-size*
                        *base-thickness*
                        *hole-diameter*
                        *hole-pattern-radius*
                        *hole-pattern-count*
                        *boss-diameter*
                        *boss-height*)
             :name "demo-plate")

  (format t "~%")
  (format t "═══════════════════════════════════════════════════════════════~%")
  (format t " ✨ AUTO-REBUILD IS NOW ACTIVE!~%")
  (format t "═══════════════════════════════════════════════════════════════~%")
  (format t "~%")
  (format t " TO MODIFY THE PART:~%")
  (format t "   1. Change parameters above (e.g., *boss-height* to 25)~%")
  (format t "   2. Re-evaluate the defpart form~%")
  (format t "   3. Watch it auto-rebuild! ✨~%")
  (format t "~%")
  (format t " OR use a preset: (preset-dense), (preset-large), etc.~%")
  (format t "~%")
  (format t " TO MANUALLY REBUILD: (clad:rebuild)~%")
  (format t " TO DISABLE AUTO-REBUILD: (clad:toggle-auto-rebuild)~%")
  (format t "═══════════════════════════════════════════════════════════════~%")
  (format t "~%"))

;;;; ============================================================================
;;;; QUICK PRESET FUNCTIONS - These now trigger auto-rebuild!
;;;; ============================================================================

(defun preset-minimal ()
  "Minimal design with just 4 corner holes - with auto-rebuild!"
  (setf *hole-pattern-count* 4
        *hole-pattern-radius* 35
        *boss-height* 10)
  (format t "~%✓ Loaded: MINIMAL preset~%")
  (format t "~%Now re-evaluating defpart to trigger auto-rebuild...~%")
  ;; Re-evaluate the defpart with new defaults - this will trigger auto-rebuild!
  (clad.dsl:defpart demo-plate
      ((size *base-size*)
       (thickness *base-thickness*)
       (hole-dia *hole-diameter*)
       (pattern-radius *hole-pattern-radius*)
       (pattern-count *hole-pattern-count*)
       (boss-dia *boss-diameter*)
       (boss-h *boss-height*))
    "Minimal variant"
    (:body (clad.core:make-box size size thickness))
    (:on-face :direction :+z :extreme :max
      (:add (clad.core:translate
              (clad.core:make-cylinder (/ boss-dia 2) boss-h)
              (/ size 2) (/ size 2) thickness)))
    (:on-face :direction :+z :extreme :max
      (:circular-pattern
          :count pattern-count
          :radius pattern-radius
          :center-x (/ size 2)
          :center-y (/ size 2)
        (lightening-hole :radius (/ hole-dia 2.5))))
    (:on-face :direction :+z :extreme :max
      (:circular-pattern
          :count 4
          :radius (* size 0.35)
          :center-x (/ size 2)
          :center-y (/ size 2)
          :angle-start 45
          :angle-end 315
        (mounting-hole :diameter hole-dia)))
    (:on-face :direction :+z :extreme :max
      (:cut (clad.core:translate
              (clad.core:make-cylinder (/ hole-dia 2) (* boss-h 1.5))
              (/ size 2) (/ size 2) thickness)))))

(defun preset-dense ()
  "Dense pattern with many holes for weight reduction"
  (setf *hole-pattern-count* 12
        *hole-pattern-radius* 20
        *boss-height* 10)
  (format t "~%✓ Loaded: DENSE preset~%")
  (format t "~%Rebuilding with new parameters...~%")
  ;; Re-call show with updated parameters
  (clad:show 'demo-plate
             :args (list *base-size*
                        *base-thickness*
                        *hole-diameter*
                        *hole-pattern-radius*
                        *hole-pattern-count*
                        *boss-diameter*
                        *boss-height*)
             :name "demo-plate"))

(defun preset-large ()
  "Large heavy-duty plate"
  (setf *base-size* 120
        *base-thickness* 15
        *hole-diameter* 8
        *boss-diameter* 40
        *boss-height* 25
        *hole-pattern-count* 8)
  (format t "~%✓ Loaded: LARGE preset~%")
  (format t "~%Rebuilding with new parameters...~%")
  ;; Re-call show with updated parameters
  (clad:show 'demo-plate
             :args (list *base-size*
                        *base-thickness*
                        *hole-diameter*
                        *hole-pattern-radius*
                        *hole-pattern-count*
                        *boss-diameter*
                        *boss-height*)
             :name "demo-plate"))

;;;; ============================================================================
;;;; AUTO-RUN
;;;; ============================================================================

(format t "~%")
(format t "╔════════════════════════════════════════════════════════════════╗~%")
(format t "║      CLAD Interactive Demo with AUTO-REBUILD Loaded! ✨       ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
(format t "~%")
(format t "This demo showcases:~%")
(format t "  ✓ Selectors in ALL 6 directions (+X, -X, +Y, -Y, +Z, -Z)~%")
(format t "  ✓ Multiple pattern types (circular, linear, grid)~%")
(format t "  ✓ Boolean operations (add, cut)~%")
(format t "  ✓ Auto-rebuild on part redefinition~%")
(format t "  ✓ Parametric design with live updates~%")
(format t "~%")
(format t "QUICK START:~%")
(format t "  (quick-show)        - Generate and view with AUTO-REBUILD enabled~%")
(format t "~%")
(format t "TRY PRESETS:~%")
(format t "  (preset-minimal)    - Minimal design with 4 holes~%")
(format t "  (preset-dense)      - Dense hole pattern (12 holes)~%")
(format t "  (preset-large)      - Large heavy-duty plate~%")
(format t "~%")
(format t "MODIFY LIVE (after running quick-show):~%")
(format t "  1. Change a parameter: (setf *boss-height* 25)~%")
(format t "  2. Re-evaluate the defpart form above~%")
(format t "  3. Watch it auto-rebuild in your browser! ✨~%")
(format t "~%")
(format t "EXPLORE THE PART:~%")
(format t "  - Rotate with mouse drag~%")
(format t "  - Pan with right-click drag~%")
(format t "  - Zoom with scroll wheel~%")
(format t "  - Look for features on ALL sides! (top/bottom/left/right/front/back)~%")
(format t "~%")
(format t "MANUAL CONTROL:~%")
(format t "  (clad:rebuild)              - Manually rebuild current part~%")
(format t "  (clad:toggle-auto-rebuild)  - Toggle auto-rebuild on/off~%")
(format t "~%")
(format t "Ready! Type (quick-show) to start.~%")
(format t "~%")
