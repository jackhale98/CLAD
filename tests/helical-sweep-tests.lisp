;;;; tests/helical-sweep-tests.lisp --- Helical Sweep Operation Tests
;;;;
;;;; Test-Driven Development for 3D Thread Geometry - Phase 3
;;;; These tests define the expected behavior of helical sweep operations

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite helical-sweep-suite
  :description "Helical sweep operation and thread geometry generation tests")

(in-suite helical-sweep-suite)

;;; ============================================================================
;;; Basic Sweep Operation Tests
;;; ============================================================================

(test test-basic-profile-sweep
  "Verify basic profile sweeping along helical path"
  (let* ((profile (clad.features.thread-profile:make-iso-metric-profile :m6 :external))
         (helix (clad.features.helical-path:make-helix-for-thread
                 :thread-spec :m6
                 :length 20.0
                 :right-handed t))
         (swept-shape (clad.features.helical-sweep:sweep-profile-along-helix
                       profile helix)))

    (is (not (null swept-shape))
        "Sweep operation should produce a shape")

    ;; Swept shape should be a valid OCCT solid
    (is (clad.ffi:is-valid-shape swept-shape)
        "Swept thread geometry should be valid OCCT shape")

    ;; Should be a solid (not wire, face, or shell)
    (is (eq :solid (clad.ffi:get-shape-type swept-shape))
        "Swept thread should be a solid shape")))

(test test-sweep-maintains-profile-dimensions
  "Verify thread profile dimensions are preserved after sweeping"
  (let* ((profile (clad.features.thread-profile:make-iso-metric-profile :m8 :external))
         (params (clad.features.thread-profile:profile-parameters profile))
         (helix (clad.features.helical-path:make-helix-for-thread
                 :thread-spec :m8
                 :length 30.0
                 :right-handed t))
         (swept-shape (clad.features.helical-sweep:sweep-profile-along-helix
                       profile helix))
         (bbox (clad.ffi:get-bounding-box swept-shape)))

    ;; Bounding box diameter should approximate major diameter
    ;; (allowing for helix spiral)
    (let* ((major-d (getf params :major-diameter))
           (bbox-width (- (getf bbox :x-max) (getf bbox :x-min)))
           (bbox-depth (- (getf bbox :y-max) (getf bbox :y-min))))

      ;; Bounding box should contain the thread
      (is (>= bbox-width major-d)
          "Bounding box width should be at least major diameter")
      (is (>= bbox-depth major-d)
          "Bounding box depth should be at least major diameter")

      ;; But not excessively larger (within 10%)
      (is (<= bbox-width (* major-d 1.1))
          "Bounding box should not be much larger than major diameter"))))

;;; ============================================================================
;;; External Thread Generation Tests
;;; ============================================================================

(test test-external-thread-generation
  "Verify complete external thread geometry generation"
  (let ((thread (clad.features.helical-sweep:make-thread-geometry
                 :thread-spec :m6
                 :length 25.0
                 :profile-type :external
                 :right-handed t)))

    (is (not (null thread))
        "External thread should be created")

    (is (clad.ffi:is-valid-shape thread)
        "External thread should be valid shape")

    ;; External thread should have positive volume
    (is (> (clad.ffi:get-volume thread) 0)
        "External thread should have positive volume")))

(test test-external-thread-m8-dimensions
  "Verify M8 external thread has correct dimensions"
  (let* ((thread (clad.features.helical-sweep:make-thread-geometry
                  :thread-spec :m8
                  :length 30.0
                  :profile-type :external
                  :right-handed t))
         (bbox (clad.ffi:get-bounding-box thread))
         (height (- (getf bbox :z-max) (getf bbox :z-min))))

    ;; Thread height should match specified length
    (is (approximately-equal 30.0 height 0.5)
        "Thread height should match specified length")

    ;; Diameter should be approximately major diameter (8mm for M8)
    (let ((diameter (max (- (getf bbox :x-max) (getf bbox :x-min))
                        (- (getf bbox :y-max) (getf bbox :y-min)))))
      (is (approximately-equal 8.0 diameter 0.5)
          "Thread diameter should approximate major diameter"))))

;;; ============================================================================
;;; Internal Thread Generation Tests
;;; ============================================================================

(test test-internal-thread-generation
  "Verify complete internal thread geometry generation"
  (let ((thread (clad.features.helical-sweep:make-thread-geometry
                 :thread-spec :m6
                 :length 25.0
                 :profile-type :internal
                 :right-handed t)))

    (is (not (null thread))
        "Internal thread should be created")

    (is (clad.ffi:is-valid-shape thread)
        "Internal thread should be valid shape")

    ;; Internal thread should have positive volume
    (is (> (clad.ffi:get-volume thread) 0)
        "Internal thread should have positive volume")))

(test test-internal-vs-external-volume
  "Verify internal thread volume is less than external (hollow core)"
  (let ((external (clad.features.helical-sweep:make-thread-geometry
                   :thread-spec :m8
                   :length 20.0
                   :profile-type :external
                   :right-handed t))
        (internal (clad.features.helical-sweep:make-thread-geometry
                   :thread-spec :m8
                   :length 20.0
                   :profile-type :internal
                   :right-handed t)))

    ;; Internal thread has hollow core, so volume should be less
    (is (< (clad.ffi:get-volume internal)
           (clad.ffi:get-volume external))
        "Internal thread volume should be less than external (hollow core)")))

;;; ============================================================================
;;; Thread Handedness Tests
;;; ============================================================================

(test test-right-handed-vs-left-handed-threads
  "Verify right-handed and left-handed threads are mirror images"
  (let ((rh-thread (clad.features.helical-sweep:make-thread-geometry
                    :thread-spec :m6
                    :length 20.0
                    :profile-type :external
                    :right-handed t))
        (lh-thread (clad.features.helical-sweep:make-thread-geometry
                    :thread-spec :m6
                    :length 20.0
                    :profile-type :external
                    :right-handed nil)))

    ;; Both should be valid
    (is (clad.ffi:is-valid-shape rh-thread)
        "Right-handed thread should be valid")
    (is (clad.ffi:is-valid-shape lh-thread)
        "Left-handed thread should be valid")

    ;; Volumes should be approximately equal
    (is (approximately-equal (clad.ffi:get-volume rh-thread)
                            (clad.ffi:get-volume lh-thread)
                            1.0)
        "Right and left-handed threads should have similar volumes")

    ;; Bounding boxes should be similar
    (let ((rh-bbox (clad.ffi:get-bounding-box rh-thread))
          (lh-bbox (clad.ffi:get-bounding-box lh-thread)))
      (is (approximately-equal
           (- (getf rh-bbox :z-max) (getf rh-bbox :z-min))
           (- (getf lh-bbox :z-max) (getf lh-bbox :z-min))
           0.1)
          "Thread heights should be equal"))))

;;; ============================================================================
;;; Multiple Thread Sizes Tests
;;; ============================================================================

(test test-various-thread-sizes
  "Verify thread generation works for various thread sizes"
  (dolist (spec '(:m3 :m4 :m5 :m6 :m8 :m10 :m12))
    (let ((thread (clad.features.helical-sweep:make-thread-geometry
                   :thread-spec spec
                   :length 20.0
                   :profile-type :external
                   :right-handed t)))

      (is (not (null thread))
          (format nil "Should create thread for ~A" spec))

      (is (clad.ffi:is-valid-shape thread)
          (format nil "~A thread should be valid" spec))

      (is (> (clad.ffi:get-volume thread) 0)
          (format nil "~A thread should have positive volume" spec)))))

(test test-fine-pitch-thread
  "Verify fine pitch thread generation (M8x1.0)"
  (let ((thread (clad.features.helical-sweep:make-thread-geometry
                 :thread-spec :m8x1.0
                 :length 30.0
                 :profile-type :external
                 :right-handed t)))

    (is (not (null thread))
        "Fine pitch thread should be created")

    (is (clad.ffi:is-valid-shape thread)
        "Fine pitch thread should be valid")))

;;; ============================================================================
;;; Sweep Quality Tests
;;; ============================================================================

(test test-sweep-orientation-perpendicular
  "Verify profile maintains perpendicularity to helix (Frenet frame)"
  (let* ((profile (clad.features.thread-profile:make-iso-metric-profile :m6 :external))
         (helix (clad.features.helical-path:make-helix-for-thread
                 :thread-spec :m6
                 :length 20.0
                 :right-handed t))
         (swept-shape (clad.features.helical-sweep:sweep-profile-along-helix
                       profile helix
                       :orientation :frenet)))

    (is (clad.ffi:is-valid-shape swept-shape)
        "Sweep with Frenet orientation should be valid")

    ;; Frenet frame ensures profile stays perpendicular to path
    ;; This is indicated by a smooth, non-twisted thread
    ;; We verify by checking the shape is valid and has no self-intersections
    (is (not (clad.ffi:has-self-intersections swept-shape))
        "Thread should not have self-intersections with Frenet orientation")))

(test test-sweep-creates-closed-solid
  "Verify sweep operation creates a closed solid (watertight)"
  (let ((thread (clad.features.helical-sweep:make-thread-geometry
                 :thread-spec :m6
                 :length 20.0
                 :profile-type :external
                 :right-handed t)))

    ;; Thread should be a closed solid
    (is (clad.ffi:is-closed-solid thread)
        "Thread geometry should be a closed (watertight) solid")))

;;; ============================================================================
;;; Integration Tests (Phase 1 + Phase 2 + Phase 3)
;;; ============================================================================

(test test-full-thread-pipeline
  "Verify complete thread generation pipeline (Profile + Helix + Sweep)"
  ;; Step 1: Create thread profile (Phase 1)
  (let* ((profile (clad.features.thread-profile:make-iso-metric-profile :m10 :external))
         (profile-params (clad.features.thread-profile:profile-parameters profile)))

    (is (not (null profile))
        "Profile creation should succeed")

    ;; Step 2: Create helical path (Phase 2)
    (let ((helix (clad.features.helical-path:make-helix-for-thread
                  :thread-spec :m10
                  :length 40.0
                  :right-handed t)))

      (is (not (null helix))
          "Helix creation should succeed")

      ;; Step 3: Sweep profile along helix (Phase 3)
      (let ((thread (clad.features.helical-sweep:sweep-profile-along-helix
                     profile helix)))

        (is (not (null thread))
            "Sweep operation should succeed")

        (is (clad.ffi:is-valid-shape thread)
            "Final thread should be valid")

        ;; Verify dimensions match original spec
        (let ((bbox (clad.ffi:get-bounding-box thread)))
          (is (approximately-equal 10.0
                                  (max (- (getf bbox :x-max) (getf bbox :x-min))
                                       (- (getf bbox :y-max) (getf bbox :y-min)))
                                  1.0)
              "Thread diameter should match M10 major diameter")

          (is (approximately-equal 40.0
                                  (- (getf bbox :z-max) (getf bbox :z-min))
                                  1.0)
              "Thread length should match specified length"))))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(test test-invalid-sweep-parameters
  "Verify error handling for invalid sweep parameters"
  ;; Nil profile
  (signals error
    (clad.features.helical-sweep:sweep-profile-along-helix nil
      (clad.features.helical-path:make-helix :pitch 1.0 :radius 2.5 :height 10.0))
    "Should signal error for nil profile")

  ;; Nil helix
  (signals error
    (clad.features.helical-sweep:sweep-profile-along-helix
      (clad.features.thread-profile:make-iso-metric-profile :m6 :external)
      nil)
    "Should signal error for nil helix"))

;;; ============================================================================
;;; Helper Functions for Tests
;;; ============================================================================

(defun approximately-equal (expected actual tolerance)
  "Test if two numbers are approximately equal within tolerance"
  (<= (abs (- expected actual)) tolerance))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-helical-sweep-tests ()
  "Run all helical sweep tests and report results"
  (format t "~%~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║       Helical Sweep Operation Tests - Phase 3                ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  (let ((results (run 'helical-sweep-suite)))
    (format t "~%Test Results:~%")
    (format t "  Tests run: ~A~%" (length results))
    (format t "  Passed: ~A~%" (count-if #'test-passed-p results))
    (format t "  Failed: ~A~%" (count-if #'test-failed-p results))
    (format t "~%")

    results))
