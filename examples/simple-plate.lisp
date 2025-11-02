;;;; examples/simple-plate.lisp --- Simple REPL-driven CAD workflow
;;;
;;; PERFECT WORKFLOW FOR EMACS C-c C-c:
;;;
;;; 1. Load this file once: (load "examples/simple-plate.lisp")
;;; 2. View your part once: (quick-view)
;;; 3. Edit parameters below and press C-c C-c on the defpart
;;; 4. Watch it auto-update in browser within 2 seconds!
;;;
;;; No need to call anything else - just edit and C-c C-c!

(require :asdf)
(asdf:load-system :clad)

(in-package :cl-user)

;;;; ============================================================================
;;;; YOUR PARAMETRIC PART - Edit values below and press C-c C-c!
;;;; ============================================================================

(clad.dsl:defpart mounting-plate
    ((size 100)              ; Overall plate size (mm)
     (thickness 50)          ; Plate thickness (mm)
     (hole-diameter 6)       ; Mounting hole diameter (mm)
     (hole-count 10)          ; Number of lightening holes
     (boss-diameter 30)      ; Central boss diameter (mm)
     (boss-height 15))       ; Boss height (mm) - TRY CHANGING THIS TO 25!
  "A simple mounting plate with parametric features.

  This demonstrates the perfect REPL-driven workflow:
  - Change any parameter above
  - Press C-c C-c in Emacs (or re-evaluate in SLIME/Sly)
  - Watch it auto-rebuild in browser!

  No function calls needed, no global variables, just pure code!"

  ;; Step 1: Create the base plate
  (:body
    (clad.core:make-box size size thickness))

  ;; Step 2: Add a cylindrical boss on top
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder (/ boss-diameter 2) boss-height)
            (/ size 2) (/ size 2) thickness)))

  ;; Step 3: Cut a hole through the boss
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* boss-height 1.5))
            (/ size 2) (/ size 2) thickness)))

  ;; Step 4: Add lightening holes in a circular pattern
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count hole-count
        :radius (* size 0.35)
        :center-x (/ size 2)
        :center-y (/ size 2)
      (:cut (clad.core:make-cylinder (/ hole-diameter 2.5) thickness))))

  ;; Step 5: Add corner mounting holes
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count 4
        :radius (* size 0.55)
        :center-x (/ size 2)
        :center-y (/ size 2)
        :angle-start 45
        :angle-end 315
      (:cut (clad.core:make-cylinder (/ hole-diameter 2) thickness))))

  ;; Step 6: Add support ribs on bottom
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-box (* size 0.8) 3 (/ thickness 2))
            (* size 0.1) (/ size 2) (- (/ thickness 10)))))

  (:on-face :direction :-z :extreme :min
    (:add (clad.core:translate
            (clad.core:make-box 3 (* size 0.8) (/ thickness 2))
            (/ size 2) (* size 0.1) (- (/ thickness 4))))))

;;;; ============================================================================
;;;; ONE-TIME SETUP - Just call (quick-view) once!
;;;; ============================================================================

(defun quick-view ()
  "Call this ONCE to view the part. Then just edit and C-c C-c!"
  (format t "~%")
  (format t "╔═══════════════════════════════════════════════════════════╗~%")
  (format t "║  CLAD Simple Plate - REPL-Driven CAD Workflow             ║~%")
  (format t "╚═══════════════════════════════════════════════════════════╝~%")
  (format t "~%")
  (format t "Opening browser to view your part...~%")
  (format t "~%")

  ;; View the part (opens browser)
  (clad:show 'mounting-plate)

  (format t "~%")
  (format t "╔═══════════════════════════════════════════════════════════╗~%")
  (format t "║  ✨ AUTO-REBUILD IS ACTIVE!                                ║~%")
  (format t "╚═══════════════════════════════════════════════════════════╝~%")
  (format t "~%")
  (format t "PERFECT WORKFLOW:~%")
  (format t "~%")
  (format t "  1. Edit any parameter in the defpart above~%")
  (format t "     (Try changing boss-height from 15 to 25)~%")
  (format t "~%")
  (format t "  2. Press C-c C-c in Emacs (or re-evaluate in SLIME/Sly)~%")
  (format t "~%")
  (format t "  3. Watch the browser auto-update within 2 seconds! ✨~%")
  (format t "~%")
  (format t "  That's it! No function calls, no rebuilds, just code.~%")
  (format t "~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "~%")
  (format t "TIP: Keep the browser window visible while you code!~%")
  (format t "~%"))

;;;; ============================================================================
;;;; AUTO-LOAD MESSAGE
;;;; ============================================================================

(format t "~%")
(format t "╔═══════════════════════════════════════════════════════════╗~%")
(format t "║  Simple Plate Example Loaded!                             ║~%")
(format t "╚═══════════════════════════════════════════════════════════╝~%")
(format t "~%")
(format t "QUICKSTART:~%")
(format t "~%")
(format t "  (quick-view)        - Open in browser (call once)~%")
(format t "~%")
(format t "Then edit the defpart and press C-c C-c to auto-update!~%")
(format t "~%")
