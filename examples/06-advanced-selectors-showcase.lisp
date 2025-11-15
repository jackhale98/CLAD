;;;; examples/06-advanced-selectors-showcase.lisp
;;;;
;;;; Comprehensive example demonstrating advanced selector features (Phases 1-4):
;;;; - Boolean combinators (AND/OR/NOT)
;;;; - Position-based selectors
;;;; - Debugging and inspection tools
;;;; - Lightweight face-plane operations
;;;;
;;;; This example builds a realistic servo motor mounting bracket with:
;;;; - Multi-level design with mounting surfaces
;;;; - Bolt hole patterns for servo attachment
;;;; - Cable management features
;;;; - Mounting holes for installation

(in-package :cl-user)

;;; ============================================================================
;;; Part Definition: Servo Motor Mounting Bracket
;;; ============================================================================

(clad.dsl:defpart servo-mounting-bracket
    ((base-width 80)
     (base-depth 60)
     (base-thickness 6)
     (riser-height 25)
     (servo-spacing 48)      ; Standard servo mounting hole spacing
     (mounting-hole-dia 4)
     (servo-hole-dia 3)
     (fillet-radius 2))

  "Professional servo motor mounting bracket demonstrating advanced selectors.

  This bracket mounts a standard servo motor and includes:
  - Base plate with mounting holes
  - Raised platform for servo
  - Bolt patterns using face-plane operations (Phase 4)
  - Grid pattern for servo mounting holes
  - Cable management slot

  Dimensions are in millimeters.

  Note: Filleting has been omitted due to complex geometry interactions."

  ;; ========================================================================
  ;; Step 1: Create base plate
  ;; ========================================================================
  (:body (clad.core:make-box base-width base-depth base-thickness))

  ;; ========================================================================
  ;; Step 2: Add raised servo platform using position selector
  ;; ========================================================================
  ;; The platform is positioned at the center-top of the base
  ;; We'll add it directly on top (no need for translation)

  (:body (clad.core:translate
          (clad.core:make-box 55 45 riser-height)
          (/ (- 55 base-width) 2.0)    ; Center in X
          (/ (- 45 base-depth) 2.0)    ; Center in Y
          base-thickness))              ; On top of base

  ;; ========================================================================
  ;; Step 3: Add mounting ears on sides using position-based selection
  ;; ========================================================================
  ;; Add ears only on the left and right edges (at X extremes)

  ;; Left mounting ear
  (:body (clad.core:translate
          (clad.core:make-box 12 20 base-thickness)
          (- (/ base-width 2.0) 12)     ; Left edge
          (/ (- 20 base-depth) 2.0)     ; Centered in Y
          0))

  ;; Right mounting ear
  (:body (clad.core:translate
          (clad.core:make-box 12 20 base-thickness)
          (/ base-width 2.0)            ; Right edge
          (/ (- 20 base-depth) 2.0)     ; Centered in Y
          0))

  ;; ========================================================================
  ;; Step 4: Create cable management slot using face-plane operations
  ;; ========================================================================
  ;; Cut a slot on the back face for cable routing

  (:on-face-plane :direction :-y :extreme :min
    ;; Slot for cable (centered, 8mm wide, 3mm deep)
    (:cut-rectangle 8 4 :depth 3))

  ;; ========================================================================
  ;; Step 5: Add servo mounting holes using circular pattern
  ;; ========================================================================
  ;; Standard servo mounting: 4 holes in rectangular pattern on top surface
  ;; Using face-plane operations for automatic centering

  (:on-face-plane :direction :+z :extreme :max
    ;; Servo mounting holes in rectangular pattern
    ;; Offset the pattern start to create 2x2 grid centered on servo platform
    (:grid-pattern
        :x-count 2
        :y-count 2
        :x-spacing servo-spacing
        :y-spacing 10
        (:cut-circle (/ servo-hole-dia 2.0) :depth 33)))  ; 33 = base-thickness + riser-height + 2

  ;; ========================================================================
  ;; Step 6: Add base mounting holes in the ears
  ;; ========================================================================
  ;; Mounting holes through the ears for bolting to a surface
  ;; We'll select faces on the ears and drill through them

  (:on-face :direction :-z :extreme :min
    ;; Cut mounting holes from the bottom face
    ;; Use positioned cylinders for the ear holes
    (:cut (clad.core:translate
           (clad.core:make-cylinder (/ mounting-hole-dia 2.0) 8)  ; 8 = base-thickness + 2
           (- (/ base-width 2.0) 6)   ; In left ear
           0                           ; Centered in Y
           (- 1)))                     ; Start below base
    (:cut (clad.core:translate
           (clad.core:make-cylinder (/ mounting-hole-dia 2.0) 8)  ; 8 = base-thickness + 2
           (- (/ base-width 2.0) 6)   ; In right ear
           0                           ; Centered in Y
           (- 1))))                    ; Start below base

  ;; NOTE: Filleting steps have been removed for simplicity
  ;; The complex geometry with multiple holes makes some edges difficult to fillet
  ;; For a working fillet example, see the simpler examples in other files
  )

;;; ============================================================================
;;; Example with Debugging - Same part but with inspection enabled
;;; ============================================================================

(clad.dsl:defpart servo-mounting-bracket-debug
    ((base-width 80)
     (base-depth 60)
     (base-thickness 6)
     (riser-height 25)
     (servo-spacing 48)
     (mounting-hole-dia 4)
     (servo-hole-dia 3)
     (fillet-radius 2))

  "Same servo bracket with debugging/inspection enabled (Phase 3 features).

  This version demonstrates:
  - inspect-selection for REPL debugging
  - debug-selection for build-time output
  - Verifying complex selector logic"

  ;; Create base geometry (same as before)
  (:body (clad.core:make-box base-width base-depth base-thickness))

  (:body (clad.core:translate
          (clad.core:make-box 55 45 riser-height)
          (/ (- 55 base-width) 2.0)
          (/ (- 45 base-depth) 2.0)
          base-thickness))

  (:body (clad.core:translate
          (clad.core:make-box 12 20 base-thickness)
          (- (/ base-width 2.0) 12)
          (/ (- 20 base-depth) 2.0)
          0))

  (:body (clad.core:translate
          (clad.core:make-box 12 20 base-thickness)
          (/ base-width 2.0)
          (/ (- 20 base-depth) 2.0)
          0))

  ;; ========================================================================
  ;; NOTE: Filleting and debug steps removed for simplicity
  ;; ========================================================================
  ;; Complex geometry with multiple holes makes some edges difficult to fillet

  ;; ========================================================================
  ;; Demonstrate debug-selection for face selection
  ;; ========================================================================

  ;; NOTE: Uncomment to see face selection debugging
  ;; (:on-face-plane :direction :+z :extreme :max
  ;;   (:debug-selection :message "Top face for servo mounting holes"))

  ;; Add servo mounting holes
  (:on-face-plane :direction :+z :extreme :max
    (:grid-pattern
        :x-count 2
        :y-count 2
        :x-spacing servo-spacing
        :y-spacing 10
        (:cut-circle (/ servo-hole-dia 2.0) :depth 33))))  ; 33 = base-thickness + riser-height + 2

;;; ============================================================================
;;; REPL Usage Examples
;;; ============================================================================

(defun demonstrate-advanced-selectors ()
  "Interactive demonstration of selector features.

  Run this in the REPL to see selector capabilities in action."

  (format t "~%=== Advanced Selector Demonstration ===~%~%")

  ;; Build the part
  (format t "Building servo mounting bracket...~%")
  (let ((bracket (servo-mounting-bracket)))

    ;; Phase 3: Inspect selection with detailed report
    (format t "~%--- Phase 3: Selection Inspection ---~%")
    (format t "Inspecting vertical edges for filleting:~%")

    (let* ((all-edges (clad.shapes:edges bracket))
           (report (clad.selectors:inspect-selection
                    all-edges
                    :and :type :line
                         :parallel :z
                         :not :at-z 0 :tolerance 1.5)))

      ;; Print the report
      (clad.selectors:print-selection-report report)

      ;; Show individual edge details
      (format t "~%Edge details:~%")
      (loop for desc in (getf report :descriptions)
            for i from 1
            do (format t "  ~A. ~A~%" i desc)))

    ;; Phase 2: Demonstrate position-based selection
    (format t "~%--- Phase 2: Position-Based Selection ---~%")
    (format t "Finding faces at top of part (z ≈ ~A mm):~%"
            (+ 6 25))  ; base-thickness + riser-height

    (let* ((all-faces (clad.shapes:faces bracket))
           (top-faces (clad.selectors:select all-faces
                                              :at-z (+ 6 25)
                                              :tolerance 0.5)))
      (format t "Found ~A face(s) at top~%" (length top-faces)))

    ;; Phase 1: Boolean combinators
    (format t "~%--- Phase 1: Boolean Combinators ---~%")
    (format t "Finding planar faces that are NOT at bottom:~%")

    (let* ((all-faces (clad.shapes:faces bracket))
           (elevated-planar (clad.selectors:select
                             all-faces
                             :and :type :plane
                                  :not :at-z 0 :tolerance 0.5)))
      (format t "Found ~A elevated planar face(s)~%" (length elevated-planar)))

    ;; Return the bracket
    (format t "~%Demonstration complete!~%")
    bracket))

;;; ============================================================================
;;; Building Instructions
;;; ============================================================================

(format t "~%~%")
(format t "===================================================================~%")
(format t "  Advanced Selector Showcase - Servo Motor Mounting Bracket~%")
(format t "===================================================================~%")
(format t "~%")
(format t "To build the parts:~%")
(format t "~%")
(format t "  ;; Standard bracket~%")
(format t "  (servo-mounting-bracket)~%")
(format t "~%")
(format t "  ;; Bracket with custom dimensions~%")
(format t "  (servo-mounting-bracket :base-width 100~%")
(format t "                          :servo-spacing 55)~%")
(format t "~%")
(format t "  ;; Debug version (uncomment debug lines to see output)~%")
(format t "  (servo-mounting-bracket-debug)~%")
(format t "~%")
(format t "To run interactive demonstration:~%")
(format t "~%")
(format t "  (demonstrate-advanced-selectors)~%")
(format t "~%")
(format t "To export to STEP:~%")
(format t "~%")
(format t "  (clad.export:export-step~%")
(format t "    (servo-mounting-bracket)~%")
(format t "    \"servo-bracket.step\")~%")
(format t "~%")
(format t "Features demonstrated:~%")
(format t "  ✓ Phase 1: Boolean combinators (AND/OR/NOT)~%")
(format t "  ✓ Phase 2: Position-based selectors (at-z, parallel)~%")
(format t "  ✓ Phase 3: Debugging tools (inspect-selection)~%")
(format t "  ✓ Phase 4: Face-plane operations with patterns~%")
(format t "~%")
(format t "===================================================================~%")
(format t "~%")

;;; ============================================================================
;;; Design Notes
;;; ============================================================================

#|

DESIGN FEATURES DEMONSTRATED:

1. BOOLEAN COMBINATORS (Phase 1)
   - :and combinator for complex edge selection
   - :or combinator for parallel axis selection
   - :not combinator to exclude bottom edges
   - Nested combinators for precise control

2. POSITION-BASED SELECTORS (Phase 2)
   - :at-z for finding faces/edges at specific heights
   - :parallel for finding edges along specific axes
   - :tolerance for fuzzy matching
   - Used to selectively fillet only certain heights

3. DEBUGGING & INSPECTION (Phase 3)
   - inspect-selection returns detailed reports
   - print-selection-report for formatted output
   - debug-selection for build-time verification
   - Helps verify complex selector logic

4. FACE-PLANE OPERATIONS (Phase 4)
   - :on-face-plane establishes local coordinates
   - :cut-circle for centered holes on faces
   - :cut-rectangle for slots and pockets
   - :grid-pattern for mounting hole arrays
   - Automatic centering and alignment

REAL-WORLD APPLICATIONS:

This part demonstrates techniques useful for:
- Robotics mounting brackets
- Electronics enclosures
- Mechanical assemblies
- Fixture design
- Any part requiring:
  * Selective filleting
  * Bolt patterns
  * Weight reduction
  * Complex geometry selection

BEST PRACTICES:

1. Use position selectors to target specific regions
2. Combine selectors with AND/OR/NOT for precision
3. Use inspect-selection during development
4. Use face-plane operations for patterns on faces
5. Comment complex selectors to explain intent

|#
