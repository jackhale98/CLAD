;;;; tests/thread-boolean-tests.lisp --- Thread Boolean Operations Tests
;;;;
;;;; Test-Driven Development for 3D Thread Geometry - Phase 4
;;;; These tests define the expected behavior of thread boolean operations

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite thread-boolean-suite
  :description "Thread boolean operations and thread application tests")

(in-suite thread-boolean-suite)

;;; ============================================================================
;;; External Thread to Cylinder Tests
;;; ============================================================================

(test test-apply-external-thread-basic
  "Verify external thread can be applied to a cylinder"
  (let* ((cylinder (clad.core:make-cylinder :radius 3.0 :height 50.0))
         (thread (clad.features.helical-sweep:make-external-thread :m6 30.0))
         (threaded-shaft (clad.features.thread-boolean:apply-external-thread
                          cylinder thread
                          :position '(0 0 10.0))))  ; 10mm from bottom

    (is (not (null threaded-shaft))
        "Threaded shaft should be created")

    (is (clad.ffi:is-valid-shape threaded-shaft)
        "Threaded shaft should be valid OCCT shape")

    ;; Threaded shaft should be a solid
    (is (eq :solid (clad.ffi:get-shape-type threaded-shaft))
        "Threaded shaft should be a solid")))

(test test-threaded-shaft-dimensions
  "Verify threaded shaft maintains correct dimensions"
  (let* ((shaft-radius 3.0)
         (shaft-height 60.0)
         (thread-length 40.0)
         (cylinder (clad.core:make-cylinder :radius shaft-radius :height shaft-height))
         (thread (clad.features.helical-sweep:make-external-thread :m6 thread-length))
         (threaded-shaft (clad.features.thread-boolean:apply-external-thread
                          cylinder thread :position '(0 0 10.0)))
         (bbox (clad.ffi:get-bounding-box threaded-shaft)))

    ;; Total height should be shaft height
    (is (approximately-equal shaft-height
                            (- (getf bbox :z-max) (getf bbox :z-min))
                            1.0)
        "Threaded shaft height should match cylinder height")

    ;; Thread region should have thread diameter
    ;; Non-thread region should have shaft diameter
    (let ((max-diameter (max (- (getf bbox :x-max) (getf bbox :x-min))
                             (- (getf bbox :y-max) (getf bbox :y-min)))))
      (is (>= max-diameter (* shaft-radius 2))
          "Shaft should maintain at least the base cylinder diameter"))))

(test test-multiple-threads-on-shaft
  "Verify multiple thread sections can be applied to one shaft"
  (let* ((cylinder (clad.core:make-cylinder :radius 4.0 :height 100.0))
         (thread1 (clad.features.helical-sweep:make-external-thread :m6 20.0))
         (thread2 (clad.features.helical-sweep:make-external-thread :m8 25.0))
         (shaft-with-one (clad.features.thread-boolean:apply-external-thread
                          cylinder thread1 :position '(0 0 10.0)))
         (shaft-with-two (clad.features.thread-boolean:apply-external-thread
                          shaft-with-one thread2 :position '(0 0 50.0))))

    (is (not (null shaft-with-two))
        "Should be able to apply multiple threads")

    (is (clad.ffi:is-valid-shape shaft-with-two)
        "Shaft with multiple threads should be valid")))

;;; ============================================================================
;;; Internal Thread to Hole Tests
;;; ============================================================================

(test test-apply-internal-thread-basic
  "Verify internal thread can be cut into a hole"
  (let* ((outer-block (clad.core:make-box :width 20.0 :depth 20.0 :height 30.0))
         (hole-cylinder (clad.core:make-cylinder :radius 2.5 :height 30.0))
         (block-with-hole (clad.core:cut-shapes outer-block hole-cylinder))
         (thread (clad.features.helical-sweep:make-internal-thread :m6 20.0))
         (threaded-hole (clad.features.thread-boolean:apply-internal-thread
                         block-with-hole thread
                         :position '(10.0 10.0 5.0))))

    (is (not (null threaded-hole))
        "Threaded hole should be created")

    (is (clad.ffi:is-valid-shape threaded-hole)
        "Block with threaded hole should be valid")

    ;; Volume should be less than block with smooth hole
    ;; (thread material was removed)
    (is (< (clad.ffi:get-volume threaded-hole)
           (clad.ffi:get-volume block-with-hole))
        "Threaded hole should have less volume than smooth hole (thread material removed)")))

(test test-threaded-hole-depth
  "Verify threaded hole has correct depth"
  (let* ((block-height 40.0)
         (thread-depth 25.0)
         (outer-block (clad.core:make-box :width 30.0 :depth 30.0 :height block-height))
         (hole-cylinder (clad.core:make-cylinder :radius 3.0 :height block-height))
         (block-with-hole (clad.core:cut-shapes outer-block hole-cylinder))
         (thread (clad.features.helical-sweep:make-internal-thread :m8 thread-depth))
         (threaded-hole (clad.features.thread-boolean:apply-internal-thread
                         block-with-hole thread
                         :position '(15.0 15.0 5.0))))

    (is (clad.ffi:is-valid-shape threaded-hole)
        "Threaded hole should be valid")

    ;; Block height should be unchanged
    (let ((bbox (clad.ffi:get-bounding-box threaded-hole)))
      (is (approximately-equal block-height
                              (- (getf bbox :z-max) (getf bbox :z-min))
                              0.5)
          "Block height should be unchanged"))))

;;; ============================================================================
;;; Thread Engagement Tests
;;; ============================================================================

(test test-thread-fit-perfect
  "Verify perfect fit detection between matching threads"
  (let ((external (clad.features.helical-sweep:make-external-thread :m6 20.0))
        (internal (clad.features.helical-sweep:make-internal-thread :m6 20.0)))

    (let ((fit (clad.features.thread-boolean:check-thread-fit external internal)))
      (is (or (eq fit :perfect-fit) (eq fit :good-fit))
          "M6 external and M6 internal should have good fit"))))

(test test-thread-fit-mismatch-size
  "Verify mismatch detection for different thread sizes"
  (let ((external (clad.features.helical-sweep:make-external-thread :m6 20.0))
        (internal (clad.features.helical-sweep:make-internal-thread :m8 20.0)))

    (let ((fit (clad.features.thread-boolean:check-thread-fit external internal)))
      (is (eq fit :size-mismatch)
          "M6 and M8 threads should not fit"))))

(test test-thread-fit-length-mismatch
  "Verify detection of length mismatches"
  (let ((external (clad.features.helical-sweep:make-external-thread :m6 30.0))
        (internal (clad.features.helical-sweep:make-internal-thread :m6 15.0)))

    (let ((fit (clad.features.thread-boolean:check-thread-fit external internal)))
      (is (eq fit :length-mismatch)
          "Different length threads should be flagged"))))

(test test-thread-engagement-length
  "Verify calculation of thread engagement length"
  (let ((external (clad.features.helical-sweep:make-external-thread :m8 40.0))
        (internal (clad.features.helical-sweep:make-internal-thread :m8 25.0)))

    (let ((engagement (clad.features.thread-boolean:calculate-engagement-length
                       external internal)))
      ;; Engagement should be the shorter of the two
      (is (approximately-equal 25.0 engagement 1.0)
          "Engagement length should be the shorter thread length"))))

;;; ============================================================================
;;; Thread Specifications Tests
;;; ============================================================================

(test test-get-thread-spec-info
  "Verify thread specification information retrieval"
  (let ((spec-info (clad.features.thread-boolean:get-thread-spec-info :m6)))

    (is (not (null spec-info))
        "Should retrieve M6 specification")

    (is (approximately-equal 6.0 (getf spec-info :major-diameter) 0.01)
        "M6 major diameter should be 6.0mm")

    (is (approximately-equal 1.0 (getf spec-info :pitch) 0.01)
        "M6 pitch should be 1.0mm")))

(test test-calculate-tap-drill-size
  "Verify tap drill size calculation for internal threads"
  (let ((tap-drill (clad.features.thread-boolean:calculate-tap-drill-size :m6)))

    ;; Tap drill = Major diameter - pitch
    ;; For M6: 6.0 - 1.0 = 5.0mm
    (is (approximately-equal 5.0 tap-drill 0.1)
        "M6 tap drill should be approximately 5.0mm")))

(test test-thread-designation-string
  "Verify thread designation string generation"
  (is (string= "M6 x 1.0" (clad.features.thread-boolean:thread-designation :m6))
      "M6 designation should be 'M6 x 1.0'")

  (is (string= "M8 x 1.25" (clad.features.thread-boolean:thread-designation :m8))
      "M8 designation should be 'M8 x 1.25'")

  (is (string= "M8 x 1.0" (clad.features.thread-boolean:thread-designation :m8x1.0))
      "M8x1.0 fine pitch designation should be 'M8 x 1.0'"))

;;; ============================================================================
;;; Thread Assembly Tests
;;; ============================================================================

(test test-create-threaded-bolt
  "Verify complete threaded bolt creation"
  (let ((bolt (clad.features.thread-boolean:make-threaded-bolt
               :thread-spec :m6
               :thread-length 25.0
               :shaft-length 50.0
               :head-type :hex
               :head-diameter 10.0
               :head-height 4.0)))

    (is (not (null bolt))
        "Threaded bolt should be created")

    (is (clad.ffi:is-valid-shape bolt)
        "Threaded bolt should be valid shape")

    ;; Bolt should have positive volume
    (is (> (clad.ffi:get-volume bolt) 0)
        "Bolt should have positive volume")))

(test test-create-threaded-nut
  "Verify complete threaded nut creation"
  (let ((nut (clad.features.thread-boolean:make-threaded-nut
              :thread-spec :m8
              :height 8.0
              :nut-type :hex
              :wrench-size 13.0)))

    (is (not (null nut))
        "Threaded nut should be created")

    (is (clad.ffi:is-valid-shape nut)
        "Threaded nut should be valid shape")

    ;; Nut should have hole through center
    (is (> (clad.ffi:get-volume nut) 0)
        "Nut should have positive volume")))

;;; ============================================================================
;;; Thread Position and Orientation Tests
;;; ============================================================================

(test test-thread-positioning
  "Verify thread can be positioned at specific coordinates"
  (let* ((cylinder (clad.core:make-cylinder :radius 3.0 :height 100.0))
         (thread (clad.features.helical-sweep:make-external-thread :m6 20.0))
         (position '(0 0 30.0))  ; 30mm from origin
         (threaded-shaft (clad.features.thread-boolean:apply-external-thread
                          cylinder thread :position position)))

    (is (not (null threaded-shaft))
        "Thread should be positioned correctly")

    (is (clad.ffi:is-valid-shape threaded-shaft)
        "Positioned thread should create valid shape")))

;;; ============================================================================
;;; Helper Functions for Tests
;;; ============================================================================

(defun approximately-equal (expected actual tolerance)
  "Test if two numbers are approximately equal within tolerance"
  (<= (abs (- expected actual)) tolerance))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-thread-boolean-tests ()
  "Run all thread boolean operation tests and report results"
  (format t "~%~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║      Thread Boolean Operations Tests - Phase 4               ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  (let ((results (run 'thread-boolean-suite)))
    (format t "~%Test Results:~%")
    (format t "  Tests run: ~A~%" (length results))
    (format t "  Passed: ~A~%" (count-if #'test-passed-p results))
    (format t "  Failed: ~A~%" (count-if #'test-failed-p results))
    (format t "~%")

    results))
