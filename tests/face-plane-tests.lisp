;;;; tests/face-plane-tests.lisp --- Tests for lightweight face-plane operations (Phase 4)

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite face-plane-tests
    :description "Tests for lightweight workplane operations on faces (Phase 4)"
    :in clad-tests)

(in-suite face-plane-tests)

;;; ============================================================================
;;; Phase 4.1: Face Plane Context (RED)
;;; ============================================================================

(test on-face-plane-basic
  "on-face-plane establishes local coordinate system on a face"
  ;; Create a box and drill a hole on the top face using face-plane context
  (let ((part (test-face-plane-basic-part)))
    ;; Part should exist
    (is (not (null part)))
    ;; Part should be a valid CLOS object
    (is (typep part 'clad.shapes:cad-shape))))

(clad.dsl:defpart test-face-plane-basic-part ()
  "Simple part with hole drilled on top face using face-plane context"
  (:body (clad.core:make-box 100 100 20))

  ;; Use :on-face-plane to work in face-local coordinates
  (:on-face-plane :direction :+z :extreme :max
    ;; In local coords, (0,0) is face center, Z is face normal
    (:cut-circle 10 :depth 15)))

(test on-face-plane-auto-centering
  ":on-face-plane centers operations on the face"
  (let ((part (test-face-plane-centered-part)))
    ;; Verify hole is actually centered
    (is (not (null part)))))

(clad.dsl:defpart test-face-plane-centered-part ()
  "Part with hole that should be automatically centered on face"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; Circle at (0,0) in local coords = face center in global coords
    (:cut-circle 5 :depth 10)))

(test on-face-plane-with-selector-combinator
  ":on-face-plane works with selector combinators"
  (let ((part (test-face-plane-with-combinator-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-face-plane-with-combinator-part ()
  "Use face-plane with complex selector"
  (:body (clad.core:make-box 100 100 50))

  ;; Select top planar face
  (:on-face-plane (:and :type :plane :direction :+z :extreme :max)
    (:cut-circle 8 :depth 20)))

(test on-face-plane-side-face
  ":on-face-plane works on side faces"
  (let ((part (test-face-plane-side-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-face-plane-side-part ()
  "Drill hole on side face using face-plane context"
  (:body (clad.core:make-box 100 100 50))

  ;; Work on +X face
  (:on-face-plane :direction :+x :extreme :max
    (:cut-circle 6 :depth 15)))

;;; ============================================================================
;;; Phase 4.2: Simple 2D Operations (RED)
;;; ============================================================================

(test cut-circle-basic
  ":cut-circle creates hole on face"
  (let ((part (test-cut-circle-part)))
    (is (not (null part)))
    (is (typep part 'clad.shapes:cad-shape))))

(clad.dsl:defpart test-cut-circle-part ()
  "Part with circular hole"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    (:cut-circle 10 :depth 15)))

(test cut-circle-through-hole
  ":cut-circle can create through holes with depth > thickness"
  (let ((part (test-cut-circle-through-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-cut-circle-through-part ()
  "Part with through hole"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; Depth > thickness creates through hole
    (:cut-circle 8 :depth 30)))

(test add-circle-basic
  ":add-circle creates boss on face"
  (let ((part (test-add-circle-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-add-circle-part ()
  "Part with circular boss"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    (:add-circle 15 :height 10)))

(test cut-rectangle-basic
  ":cut-rectangle creates rectangular pocket on face"
  (let ((part (test-cut-rectangle-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-cut-rectangle-part ()
  "Part with rectangular pocket"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    (:cut-rectangle 40 30 :depth 10)))

(test add-rectangle-basic
  ":add-rectangle creates rectangular boss on face"
  (let ((part (test-add-rectangle-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-add-rectangle-part ()
  "Part with rectangular boss"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    (:add-rectangle 30 20 :height 5)))

(test multiple-operations-on-face-plane
  "Multiple operations in same :on-face-plane context"
  (let ((part (test-multiple-ops-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-multiple-ops-part ()
  "Part with multiple operations on same face"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; Multiple operations in sequence
    (:cut-circle 8 :depth 10)
    (:add-circle 20 :height 5)))

(test face-plane-with-offset
  "Operations can be offset from face center"
  (let ((part (test-offset-operations-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-offset-operations-part ()
  "Part with operations offset from center"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; TODO: Add :at parameter for positioning in Phase 4.2
    (:cut-circle 5 :depth 8)))

;;; ============================================================================
;;; Phase 4.3: Pattern Integration (RED)
;;; ============================================================================

(test circular-pattern-on-face-plane
  "Circular pattern works in face-plane context"
  (let ((part (test-circular-pattern-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-circular-pattern-part ()
  "Part with circular bolt hole pattern on face"
  (:body (clad.core:make-box 150 150 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; Circular pattern of holes
    (:circular-pattern :count 8 :radius 50
      (:cut-circle 4 :depth 15))))

(test linear-pattern-on-face-plane
  "Linear pattern works in face-plane context"
  (let ((part (test-linear-pattern-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-linear-pattern-part ()
  "Part with linear pattern of holes on face"
  (:body (clad.core:make-box 200 100 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; Linear pattern along X
    (:linear-pattern :count 5 :spacing 30 :direction :x
      (:cut-circle 4 :depth 12))))

(test grid-pattern-on-face-plane
  "Grid pattern works in face-plane context"
  (let ((part (test-grid-pattern-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-grid-pattern-part ()
  "Part with grid of holes on face"
  (:body (clad.core:make-box 150 150 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; Grid pattern
    (:grid-pattern :x-count 4 :y-count 4 :x-spacing 30 :y-spacing 30
      (:cut-circle 3 :depth 10))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(test on-face-plane-no-face-selected
  ":on-face-plane errors gracefully when no face is selected"
  ;; Define the part
  (eval '(clad.dsl:defpart test-no-face-part ()
           (:body (clad.core:make-box 100 100 20))
           ;; Selector that won't match any face
           (:on-face-plane :type :cylinder
             (:cut-circle 10 :depth 5))))
  ;; Calling it should error
  (signals error
    (test-no-face-part)))

(test on-face-plane-empty-body
  ":on-face-plane with no operations should work (no-op)"
  (let ((part (test-empty-face-plane-part)))
    (is (not (null part)))))

(clad.dsl:defpart test-empty-face-plane-part ()
  "Part with empty face-plane context (no operations)"
  (:body (clad.core:make-box 100 100 20))

  (:on-face-plane :direction :+z :extreme :max
    ;; No operations - should be harmless
    ))
