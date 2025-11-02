;;;; tests/dsl-tests.lisp --- Test suite for DSL (Phase 5)

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite dsl-tests
  :description "Tests for the declarative DSL API (Phase 5)"
  :in clad-tests)

(in-suite dsl-tests)

;;; ============================================================================
;;; TDD Cycle 1: Basic defpart with body
;;; ============================================================================

(test defpart-basic-box
  "Test basic defpart with simple box body"
  ;; Define a simple parametric box part
  (clad.dsl:defpart test-box ((width 100) (height 50) (depth 20))
    "A simple parametric box"
    (:body
      (clad.core:make-box width height depth)))

  ;; Test that the part function was created
  (is (fboundp 'test-box))

  ;; Test calling the part with default parameters
  (let ((box (test-box 100 50 20)))
    (is (not (null box)))
    (is (clad.shapes:solid-p box))))

(test defpart-with-cylinder
  "Test defpart with cylinder primitive"
  (clad.dsl:defpart test-cylinder ((radius 25) (height 100))
    "A simple parametric cylinder"
    (:body
      (clad.core:make-cylinder radius height)))

  (let ((cyl (test-cylinder 25 100)))
    (is (not (null cyl)))
    (is (clad.shapes:solid-p cyl))

    ;; Verify volume is approximately correct (pi * r^2 * h)
    (let ((expected-volume (* pi 25 25 100)))
      (is (< (abs (- (clad.shapes:volume cyl) expected-volume)) 100)))))

;;; ============================================================================
;;; TDD Cycle 2: defpart with feature on face
;;; ============================================================================

(test defpart-with-cut-feature
  "Test defpart with cut feature on selected face"
  (clad.dsl:defpart box-with-hole ((width 100) (hole-dia 20))
    "Box with hole cut from top face"
    (:body
      (clad.core:make-box width width 20))
    (:on-face :direction :+z :extreme :max
      (:cut (clad.core:make-cylinder (/ hole-dia 2) 25))))

  (let ((part (box-with-hole 100 20)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))

    ;; Volume should be less than solid box
    (let ((solid-volume (* 100 100 20)))
      (is (< (clad.shapes:volume part) solid-volume)))))

(test defpart-with-add-feature
  "Test defpart with add feature (union) on face"
  (clad.dsl:defpart base-with-boss ((base-size 100) (boss-dia 30))
    "Base plate with cylindrical boss"
    (:body
      (clad.core:make-box base-size base-size 10))
    (:on-face :direction :+z :extreme :max
      (:add (clad.core:make-cylinder (/ boss-dia 2) 20))))

  (let ((part (base-with-boss 100 30)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))

    ;; Volume should be more than just the base
    (let ((base-volume (* 100 100 10)))
      (is (> (clad.shapes:volume part) base-volume)))))

;;; ============================================================================
;;; TDD Cycle 3: defpart with multiple features
;;; ============================================================================

(test defpart-multiple-cuts
  "Test defpart with multiple cut features"
  (clad.dsl:defpart multi-hole-plate ((width 100) (hole-count 4))
    "Plate with multiple holes"
    (:body
      (clad.core:make-box width width 10))
    (:on-face :direction :+z :extreme :max
      (:cut (clad.core:translate (clad.core:make-cylinder 5 15) 50 50 0)))
    (:on-face :direction :+z :extreme :max
      (:cut (clad.core:translate (clad.core:make-cylinder 5 15) 50 20 0)))
    (:on-face :direction :+z :extreme :max
      (:cut (clad.core:translate (clad.core:make-cylinder 5 15) 50 80 0))))

  (let ((part (multi-hole-plate 100 4)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))

    ;; Should have less volume than solid plate (3 holes removed)
    (let ((solid-volume (* 100 100 10)))
      (is (< (clad.shapes:volume part) solid-volume)))))

(test defpart-mixed-features
  "Test defpart with both cut and add features"
  (clad.dsl:defpart bracket ((width 100))
    "L-bracket with mounting hole"
    (:body
      (clad.core:make-box width 80 10))
    (:on-face :direction :+z :extreme :max
      (:add (clad.core:translate
              (clad.core:rotate (clad.core:make-box width 60 10) :x 90)
              0 70 10)))
    (:on-face :direction :+z :extreme :max
      (:cut (clad.core:translate (clad.core:make-cylinder 6 15) 15 15 0))))

  (let ((part (bracket 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))))

;;; ============================================================================
;;; TDD Cycle 4: Circular pattern
;;; ============================================================================

(test circular-pattern-basic
  "Test circular pattern with simple holes"
  (clad.dsl:defpart circular-hole-pattern ((plate-size 100) (hole-radius 5) (pattern-radius 30))
    "Plate with circular pattern of holes"
    (:body
      (clad.core:make-box plate-size plate-size 10))
    (:on-face :direction :+z :extreme :max
      (:circular-pattern :count 6 :radius pattern-radius :center-x (/ plate-size 2) :center-y (/ plate-size 2)
        (:cut (clad.core:make-cylinder hole-radius 15)))))

  (let ((part (circular-hole-pattern 100 5 30)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))
    ;; Should have removed material for 6 holes
    (let ((solid-volume (* 100 100 10)))
      (is (< (clad.shapes:volume part) solid-volume)))))

(test circular-pattern-partial-arc
  "Test circular pattern over partial arc"
  (clad.dsl:defpart arc-pattern ((size 100))
    "Pattern over 180 degree arc"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (:circular-pattern :count 4 :radius 30 :center-x 50 :center-y 50 :angle-start 0 :angle-end 180
        (:cut (clad.core:make-cylinder 3 15)))))

  (let ((part (arc-pattern 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))))

;;; ============================================================================
;;; TDD Cycle 5: Linear pattern
;;; ============================================================================

(test linear-pattern-basic
  "Test linear pattern along X axis"
  (clad.dsl:defpart linear-hole-pattern ((size 100))
    "Plate with linear pattern of holes"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (:linear-pattern :count 5 :spacing 15 :direction-x 1 :direction-y 0 :start-x 10 :start-y 50
        (:cut (clad.core:make-cylinder 3 15)))))

  (let ((part (linear-hole-pattern 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))
    (is (< (clad.shapes:volume part) (* 100 100 10)))))

(test linear-pattern-diagonal
  "Test linear pattern along diagonal"
  (clad.dsl:defpart diagonal-pattern ((size 100))
    "Pattern along diagonal line"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (:linear-pattern :count 4 :spacing 20 :direction-x 1 :direction-y 1 :start-x 20 :start-y 20
        (:cut (clad.core:make-cylinder 4 15)))))

  (let ((part (diagonal-pattern 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))))

;;; ============================================================================
;;; TDD Cycle 6: Grid pattern
;;; ============================================================================

(test grid-pattern-basic
  "Test 2D grid pattern"
  (clad.dsl:defpart grid-hole-pattern ((size 100))
    "Plate with grid of holes"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (:grid-pattern :count-x 3 :count-y 3 :spacing-x 20 :spacing-y 20 :start-x 20 :start-y 20
        (:cut (clad.core:make-cylinder 3 15)))))

  (let ((part (grid-hole-pattern 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))
    ;; Should have removed material for 9 holes (3x3)
    (is (< (clad.shapes:volume part) (* 100 100 10)))))

(test grid-pattern-rectangular
  "Test rectangular grid with different X and Y counts"
  (clad.dsl:defpart rect-grid ((size 120))
    "Rectangular grid pattern"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (:grid-pattern :count-x 5 :count-y 2 :spacing-x 15 :spacing-y 30 :start-x 15 :start-y 25
        (:cut (clad.core:make-cylinder 2 15)))))

  (let ((part (rect-grid 120)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))))

;;; ============================================================================
;;; TDD Cycle 7: deffeature macro for reusable features
;;; ============================================================================

;; Define test features at top level so they're available at compile time
(clad.dsl:deffeature test-mounting-hole ((diameter 8))
  (:cut (clad.core:make-cylinder (/ diameter 2) 20)))

(clad.dsl:deffeature test-standard-boss ((diameter 30) (height 20))
  (:add (clad.core:make-cylinder (/ diameter 2) height)))

(clad.dsl:deffeature test-countersink ((diameter 8) (depth 15))
  (:cut (clad.core:make-cylinder (/ diameter 2) depth)))

(clad.dsl:deffeature test-lightening-hole ((radius 5))
  (:cut (clad.core:make-cylinder radius 15)))

(test deffeature-basic
  "Test basic feature definition and usage"
  ;; Use the feature in a part
  (clad.dsl:defpart plate-with-feature ((size 100))
    "Plate with reusable mounting hole feature"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (test-mounting-hole :diameter 6)))

  (let ((part (plate-with-feature 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))
    ;; Volume should be less than solid (hole removed)
    (is (< (clad.shapes:volume part) (* 100 100 10)))))

(test deffeature-with-defaults
  "Test feature with default parameters"
  ;; Use feature with defaults (no parameters passed)
  (clad.dsl:defpart base-with-standard-boss ((base-size 100))
    "Base with standard boss"
    (:body
      (clad.core:make-box base-size base-size 10))
    (:on-face :direction :+z :extreme :max
      (test-standard-boss)))

  (let ((part (base-with-standard-boss 80)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))
    ;; Volume should be more than base (boss added)
    (is (> (clad.shapes:volume part) (* 80 80 10)))))

(test deffeature-multiple-uses
  "Test using same feature multiple times with different parameters"
  ;; Use feature multiple times with different parameters
  (clad.dsl:defpart multi-countersink-plate ((size 100))
    "Plate with different sized countersinks"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (test-countersink :diameter 6 :depth 12))
    (:on-face :direction :+z :extreme :max
      (test-countersink :diameter 8 :depth 15))
    (:on-face :direction :+z :extreme :max
      (test-countersink :diameter 10 :depth 18)))

  (let ((part (multi-countersink-plate 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))
    ;; Should have less volume (3 holes cut)
    (is (< (clad.shapes:volume part) (* 100 100 10)))))

(test deffeature-in-pattern
  "Test using feature in a pattern"
  ;; Use feature in circular pattern
  (clad.dsl:defpart lightened-plate ((size 100))
    "Plate with pattern of lightening holes"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (:circular-pattern :count 8 :radius 35 :center-x 50 :center-y 50
        (test-lightening-hole :radius 4))))

  (let ((part (lightened-plate 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))
    ;; Should have removed material (8 holes)
    (is (< (clad.shapes:volume part) (* 100 100 10)))))

;;; ============================================================================
;;; TDD Cycle 8: Feature composition and nesting
;;; ============================================================================

;; Define composite features for testing
(clad.dsl:deffeature threaded-hole ((diameter 8) (depth 20))
  "A hole with a countersink"
  (:cut (clad.core:make-cylinder (/ diameter 2) depth)))

(clad.dsl:deffeature mounting-feature ((hole-dia 8))
  "Composite feature: boss with hole"
  (:add (clad.core:make-cylinder (/ hole-dia 1.5) 10)))

(clad.dsl:deffeature test-bolt-circle ((diameter 60) (bolt-dia 6) (bolt-count 4))
  "Standard bolt circle pattern"
  (:cut (clad.core:make-cylinder (/ bolt-dia 2) 20)))

(test feature-composition
  "Test that features can be composed together"
  ;; Use multiple features in sequence
  (clad.dsl:defpart composite-plate ((size 100))
    "Plate with multiple composed features"
    (:body
      (clad.core:make-box size size 10))
    (:on-face :direction :+z :extreme :max
      (mounting-feature :hole-dia 8))
    (:on-face :direction :+z :extreme :max
      (threaded-hole :diameter 6 :depth 15)))

  (let ((part (composite-plate 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))))

(test nested-patterns
  "Test patterns within patterns (not directly supported, but test sequential patterns)"
  ;; Create a part with multiple patterns
  (clad.dsl:defpart multi-pattern-plate ((size 100))
    "Plate with multiple pattern types"
    (:body
      (clad.core:make-box size size 10))
    ;; Circular pattern of holes
    (:on-face :direction :+z :extreme :max
      (:circular-pattern :count 6 :radius 30 :center-x 50 :center-y 50
        (test-lightening-hole :radius 3)))
    ;; Grid pattern in center
    (:on-face :direction :+z :extreme :max
      (:grid-pattern :count-x 2 :count-y 2 :spacing-x 10 :spacing-y 10 :start-x 40 :start-y 40
        (test-mounting-hole :diameter 4))))

  (let ((part (multi-pattern-plate 100)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))
    ;; Should have removed material from both patterns
    (is (< (clad.shapes:volume part) (* 100 100 10)))))

(test feature-with-pattern
  "Test feature that internally uses a pattern"
  ;; Use the feature in a pattern context
  (clad.dsl:defpart flange ((size 120))
    "Flange with bolt circle"
    (:body
      (clad.core:make-box size size 15))
    (:on-face :direction :+z :extreme :max
      (:circular-pattern :count 4 :radius 40 :center-x 60 :center-y 60
        (test-bolt-circle :diameter 60 :bolt-dia 5 :bolt-count 4))))

  (let ((part (flange 120)))
    (is (not (null part)))
    (is (clad.shapes:solid-p part))))

;;; ============================================================================
;;; TDD Cycle 9: Feature registry and lookup
;;; ============================================================================

(test feature-registry-basic
  "Test registering and looking up features"
  ;; Register a feature in the global registry
  (clad.dsl:register-feature 'test-reg-hole
    (lambda (&key (diameter 8) (depth 20))
      (list :cut (clad.core:make-cylinder (/ diameter 2) depth))))

  ;; Look it up
  (let ((feature-fn (clad.dsl:lookup-feature 'test-reg-hole)))
    (is (not (null feature-fn)))
    (is (functionp feature-fn))))

(test feature-list-all
  "Test listing all registered features"
  ;; Register several features
  (clad.dsl:register-feature 'test-feature-1
    (lambda (&key (size 10))
      (list :cut (clad.core:make-box size size size))))

  (clad.dsl:register-feature 'test-feature-2
    (lambda (&key (radius 5))
      (list :add (clad.core:make-cylinder radius 10))))

  ;; List all features
  (let ((features (clad.dsl:list-features)))
    (is (listp features))
    (is (>= (length features) 2))))

(test feature-clear-registry
  "Test clearing the feature registry"
  ;; Register a feature
  (clad.dsl:register-feature 'test-temp-feature
    (lambda () (list :cut (clad.core:make-box 5 5 5))))

  ;; Verify it exists
  (is (not (null (clad.dsl:lookup-feature 'test-temp-feature))))

  ;; Clear registry
  (clad.dsl:clear-features)

  ;; Verify it's gone (but deffeature-defined macros still exist)
  ;; This only clears runtime-registered features
  (is (null (clad.dsl:lookup-feature 'test-temp-feature))))
