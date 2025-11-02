;;;; tests/context-tests.lisp --- Test suite for modeling context API

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite context-tests
  :description "Tests for the modeling context API"
  :in clad-tests)

(in-suite context-tests)

;;; ============================================================================
;;; TDD Cycle 6: Basic Context Class
;;; ============================================================================

(test context-creation
  "Test creating a modeling context"
  (let ((ctx (clad.context:make-context)))
    ;; Context should exist
    (is (not (null ctx)))

    ;; Initial shape should be nil
    (is (null (clad.context:current-shape ctx)))

    ;; Initial selection should be empty
    (is (null (clad.context:current-selection ctx)))

    ;; Initial workplane should be XY plane
    (let ((wp (clad.context:current-workplane ctx)))
      (is (not (null wp))))))

;;; ============================================================================
;;; TDD Cycle 7: with-context Macro
;;; ============================================================================

(test with-context-macro
  "Test the with-context macro"
  ;; Should be able to use with-context to implicitly reference the context
  (clad.context:with-context ()
    (is (not (null clad.context:*context*)))

    ;; Can access context properties
    (is (null (clad.context:current-shape)))
    (is (not (null (clad.context:current-workplane))))))

;;; ============================================================================
;;; TDD Cycle 8: Add Operation
;;; ============================================================================

(test context-add-box
  "Test adding a box to the context"
  (clad.context:with-context ()
    ;; Add a box
    (clad.context:add (clad.core:make-box 100 100 100))

    ;; Current shape should now be the box
    (let ((shape (clad.context:current-shape)))
      (is (not (null shape)))

      ;; Should be a valid shape
      (is (clad.shapes:solid-p shape)))))

(test context-add-multiple
  "Test adding multiple shapes unions them together"
  (clad.context:with-context ()
    ;; Add first box
    (clad.context:add (clad.core:make-box 100 100 100))

    ;; Add second box (should union with first)
    (clad.context:add (clad.core:translate
                       (clad.core:make-box 100 100 100)
                       50 0 0))

    ;; Should have a single shape (union of both boxes)
    (let ((shape (clad.context:current-shape)))
      (is (not (null shape)))
      (is (clad.shapes:solid-p shape)))))

(test context-get-result
  "Test extracting the final result from context"
  (clad.context:with-context ()
    (clad.context:add (clad.core:make-box 100 100 100))

    ;; Get result should return the current shape
    (let ((result (clad.context:get-result)))
      (is (not (null result)))
      (is (clad.shapes:solid-p result)))))

;;; ============================================================================
;;; TDD Cycles 9-11: Advanced Context Features
;;; ============================================================================

(test context-select-faces
  "Test selecting faces in context"
  (clad.context:with-context ()
    (clad.context:add (clad.core:make-box 100 100 100))

    ;; Select top face
    (clad.context:select-faces :direction :+z :extreme :max)

    ;; Should have 1 face selected
    (let ((selection (clad.context:current-selection)))
      (is (= 1 (length selection))))))

(test context-workplane-management
  "Test workplane stack operations"
  (clad.context:with-context ()
    ;; Initial workplane should be XY plane
    (let ((wp1 (clad.context:current-workplane)))
      (is (not (null wp1))))

    ;; Push a new workplane
    (clad.context:push-workplane (clad.workplane:xy-plane :origin '(0 0 50)))

    ;; Current workplane should be at Z=50
    (let* ((wp2 (clad.context:current-workplane))
           (origin (clad.workplane:workplane-origin wp2)))
      (is (< (abs (- (third origin) 50)) 0.1)))

    ;; Pop back to original
    (clad.context:pop-workplane)

    ;; Should be back to origin
    (let* ((wp3 (clad.context:current-workplane))
           (origin (clad.workplane:workplane-origin wp3)))
      (is (< (abs (third origin)) 0.1)))))
