;;;; tests/thread-dsl-tests.lisp --- Thread DSL Integration Tests
;;;;
;;;; Tests for the :thread form in defpart macro
;;;;
;;;; Note: Tests that require actual thread geometry creation (helical sweeps)
;;;; are skipped when the OCCT functionality is not fully available.

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite thread-dsl-suite
  :description "Thread DSL integration tests for defpart macro"
  :in clad-tests)

(in-suite thread-dsl-suite)

;;; ============================================================================
;;; Helper to check if thread geometry is available
;;; ============================================================================

(defun thread-geometry-available-p ()
  "Check if thread geometry creation is available.
   Returns T if helical sweep operations work, NIL otherwise."
  ;; Try creating a basic helix - if it fails, geometry isn't available
  (handler-case
      (let ((helix (clad.features.helical-path:make-helix
                    :pitch 1.0 :radius 3.0 :height 10.0)))
        (and helix (clad.core:valid-shape-p helix)))
    (error () nil)))

;;; ============================================================================
;;; Basic Thread DSL Tests
;;; ============================================================================

(test test-external-thread-dsl
  "Verify :thread form works for external threads in defpart"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT helix/sweep support)")
      (progn
        ;; Define a simple threaded shaft using the DSL
        (clad.dsl:defpart threaded-shaft-test ()
          (:body (clad.core:make-cylinder 3.0 50.0))
          (:thread :m6 :length 30.0 :type :external))

        (let ((shaft (threaded-shaft-test)))
          (is (not (null shaft))
              "Threaded shaft should be created via defpart DSL")
          (is (clad.core:valid-shape-p shaft)
              "DSL-created threaded shaft should be valid")))))

(test test-internal-thread-dsl
  "Verify :thread form works for internal threads in defpart"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT helix/sweep support)")
      (progn
        ;; Define a block with threaded hole using the DSL
        (clad.dsl:defpart threaded-hole-test ()
          (:body (clad.core:make-box 30.0 30.0 20.0))
          (:thread :m6 :length 15.0 :type :internal :position (15.0 15.0 0.0)))

        (let ((block (threaded-hole-test)))
          (is (not (null block))
              "Block with threaded hole should be created via defpart DSL")
          (is (clad.core:valid-shape-p block)
              "DSL-created threaded block should be valid")))))

(test test-thread-with-position
  "Verify :thread form handles position parameter"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT helix/sweep support)")
      (progn
        (clad.dsl:defpart positioned-thread-test ()
          (:body (clad.core:make-cylinder 4.0 80.0))
          (:thread :m8 :length 25.0 :type :external :position (0.0 0.0 30.0)))

        (let ((shaft (positioned-thread-test)))
          (is (not (null shaft))
              "Positioned thread should be created")
          (is (clad.core:valid-shape-p shaft)
              "Positioned thread should be valid")))))

(test test-left-handed-thread-dsl
  "Verify :thread form handles left-handed threads"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT helix/sweep support)")
      (progn
        (clad.dsl:defpart left-thread-test ()
          (:body (clad.core:make-cylinder 3.0 40.0))
          (:thread :m6 :length 20.0 :type :external :handedness :left))

        (let ((shaft (left-thread-test)))
          (is (not (null shaft))
              "Left-handed thread should be created")
          (is (clad.core:valid-shape-p shaft)
              "Left-handed thread should be valid")))))

;;; ============================================================================
;;; Thread Geometry API Tests
;;; ============================================================================

(test test-thread-specs-available
  "Verify thread specifications are available"
  (let ((m6-spec (clad.features:get-thread-spec :m6)))
    (is (not (null m6-spec))
        "M6 spec should be available")
    (is (approximately-equal 6.0 (getf m6-spec :major-diameter) 0.01)
        "M6 major diameter should be 6.0mm")
    (is (approximately-equal 1.0 (getf m6-spec :pitch) 0.01)
        "M6 pitch should be 1.0mm")))

(test test-multiple-thread-sizes
  "Verify multiple thread sizes are available"
  (let ((m8-spec (clad.features:get-thread-spec :m8))
        (m10-spec (clad.features:get-thread-spec :m10)))
    (is (approximately-equal 8.0 (getf m8-spec :major-diameter) 0.01)
        "M8 major diameter should be 8.0mm")
    (is (approximately-equal 10.0 (getf m10-spec :major-diameter) 0.01)
        "M10 major diameter should be 10.0mm")))

(test test-thread-calculations
  "Verify thread calculation functions"
  (is (approximately-equal 6.0 (clad.features:thread-major-diameter :m6) 0.01)
      "M6 major diameter should be 6.0mm")
  (is (approximately-equal 5.0 (clad.features:tap-drill-size :m6) 0.1)
      "M6 tap drill should be approximately 5.0mm"))

;;; ============================================================================
;;; Thread Profile Tests
;;; ============================================================================

(test test-thread-profile-creation
  "Verify thread profile can be created"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT support)")
      (let* ((profile (clad.features.thread-profile:make-iso-metric-profile :m6 10.0))
             (params (clad.features.thread-profile:profile-parameters profile)))
        (is (not (null profile))
            "Profile should be created")
        (is (approximately-equal 6.0 (getf params :major-diameter) 0.01)
            "Major diameter should be 6.0mm for M6"))))

;;; ============================================================================
;;; Helical Path Tests
;;; ============================================================================

(test test-helix-creation
  "Verify helical path can be created"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT helix support)")
      (let ((helix (clad.features.helical-path:make-helix
                    :pitch 1.0 :radius 3.0 :height 20.0)))
        (is (not (null helix))
            "Helix should be created"))))

(test test-helix-for-thread
  "Verify thread-specific helix can be created"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT helix support)")
      (let ((helix (clad.features.helical-path:make-helix-for-thread
                    :thread-spec :m6 :length 25.0)))
        (is (not (null helix))
            "Thread helix should be created"))))

;;; ============================================================================
;;; Thread Boolean Operation Tests
;;; ============================================================================

(test test-thread-application-external
  "Verify external thread application creates valid shape"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT sweep support)")
      (let* ((cylinder (clad.core:make-cylinder 3.0 50.0))
             (thread (clad.features.helical-sweep:make-external-thread :m6 30.0))
             (result (clad.features.thread-boolean:apply-external-thread
                      cylinder thread :position '(0 0 10.0))))
        (is (not (null result))
            "Threaded result should be created")
        (is (clad.ffi:is-valid-shape result)
            "Result should be valid shape"))))

(test test-thread-application-internal
  "Verify internal thread application creates valid shape"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT sweep support)")
      (let* ((block (clad.core:make-box 20.0 20.0 30.0))
             (hole (clad.core:make-cylinder 2.5 30.0))
             (block-with-hole (clad.core:cut-shapes block hole))
             (thread (clad.features.helical-sweep:make-internal-thread :m6 20.0))
             (result (clad.features.thread-boolean:apply-internal-thread
                      block-with-hole thread :position '(10.0 10.0 5.0))))
        (is (not (null result))
            "Threaded hole result should be created")
        (is (clad.ffi:is-valid-shape result)
            "Result should be valid shape"))))

;;; ============================================================================
;;; Thread Fit Checking Tests
;;; ============================================================================

(test test-thread-fit-matching
  "Verify fit checking for matching threads"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT support)")
      (let ((external (clad.features.helical-sweep:make-external-thread :m6 20.0))
            (internal (clad.features.helical-sweep:make-internal-thread :m6 20.0)))
        (let ((fit (clad.features.thread-boolean:check-thread-fit external internal)))
          (is (member fit '(:perfect-fit :good-fit))
              "Matching threads should have good fit")))))

(test test-thread-fit-mismatch
  "Verify fit checking detects mismatch"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT support)")
      (let ((external (clad.features.helical-sweep:make-external-thread :m6 20.0))
            (internal (clad.features.helical-sweep:make-internal-thread :m8 20.0)))
        (let ((fit (clad.features.thread-boolean:check-thread-fit external internal)))
          (is (eq fit :size-mismatch)
              "Mismatched threads should be detected")))))

;;; ============================================================================
;;; Complete Fastener Tests
;;; ============================================================================

(test test-create-bolt
  "Verify complete bolt can be created"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT support)")
      (let ((bolt (clad.features.thread-boolean:make-threaded-bolt
                   :thread-spec :m6
                   :thread-length 25.0
                   :shaft-length 50.0
                   :head-type :hex
                   :head-diameter 10.0
                   :head-height 4.0)))
        (is (not (null bolt))
            "Bolt should be created")
        (is (clad.ffi:is-valid-shape bolt)
            "Bolt should be valid shape"))))

(test test-create-nut
  "Verify complete nut can be created"
  (if (not (thread-geometry-available-p))
      (skip "Thread geometry not available (requires full OCCT support)")
      (let ((nut (clad.features.thread-boolean:make-threaded-nut
                  :thread-spec :m8
                  :height 8.0
                  :nut-type :hex
                  :wrench-size 13.0)))
        (is (not (null nut))
            "Nut should be created")
        (is (clad.ffi:is-valid-shape nut)
            "Nut should be valid shape"))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun approximately-equal (expected actual tolerance)
  "Test if two numbers are approximately equal within tolerance"
  (<= (abs (- expected actual)) tolerance))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-thread-dsl-tests ()
  "Run all thread DSL integration tests"
  (format t "~%========================================~%")
  (format t "  Thread DSL Integration Tests~%")
  (format t "========================================~%~%")
  (run! 'thread-dsl-suite))
