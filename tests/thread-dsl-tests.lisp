;;;; tests/thread-dsl-tests.lisp --- Thread DSL Integration Tests
;;;;
;;;; Test-Driven Development for 3D Thread Geometry - Phase 4
;;;; These tests define how threads integrate with the defpart DSL

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite thread-dsl-suite
  :description "Thread DSL integration tests for defpart macro")

(in-suite thread-dsl-suite)

;;; ============================================================================
;;; Basic Thread DSL Tests
;;; ============================================================================

(test test-defpart-with-external-thread
  "Verify defpart can include external thread operations"
  ;; Define a threaded bolt using defpart
  (clad.dsl:defpart simple-bolt ()
    (cylinder :radius 3.0 :height 50.0)
    (thread :m6 :length 30.0 :type :external :position '(0 0 10.0)))

  ;; Create the part
  (let ((bolt (simple-bolt)))
    (is (not (null bolt))
        "Threaded bolt should be created via defpart")

    (is (clad:valid-shape-p bolt)
        "DSL-created bolt should be valid shape")))

(test test-defpart-with-internal-thread
  "Verify defpart can create parts with internal threads"
  ;; Define a block with threaded hole
  (clad.dsl:defpart threaded-block ()
    (box :width 20.0 :depth 20.0 :height 30.0)
    (hole :radius 2.5 :height 30.0 :position '(10.0 10.0 0))
    (thread :m6 :length 20.0 :type :internal :position '(10.0 10.0 5.0)))

  (let ((block (threaded-block)))
    (is (not (null block))
        "Threaded block should be created")

    (is (clad:valid-shape-p block)
        "DSL-created threaded block should be valid")))

(test test-thread-with-parameters
  "Verify thread operations support parametric design"
  (clad.dsl:defpart parametric-bolt (thread-spec length)
    (let ((shaft-dia (* 0.9 (clad.features:thread-major-diameter thread-spec))))
      (cylinder :radius (/ shaft-dia 2) :height length)
      (thread thread-spec
              :length (* 0.6 length)
              :type :external
              :position `(0 0 ,(* 0.2 length)))))

  ;; Create M6 bolt, 60mm long
  (let ((m6-bolt (parametric-bolt :m6 60.0)))
    (is (not (null m6-bolt))
        "Parametric bolt should be created")

    (is (clad:valid-shape-p m6-bolt)
        "Parametric bolt should be valid"))

  ;; Create M8 bolt, 80mm long
  (let ((m8-bolt (parametric-bolt :m8 80.0)))
    (is (not (null m8-bolt))
        "Should create different size bolt with same defpart")

    (is (clad:valid-shape-p m8-bolt)
        "Different size bolt should be valid")))

;;; ============================================================================
;;; Thread with Head/Nut Tests
;;; ============================================================================

(test test-bolt-with-hex-head
  "Verify defpart can create complete bolt with hex head and thread"
  (clad.dsl:defpart hex-bolt (thread-spec shaft-length thread-length)
    ;; Hex head
    (hex-prism :circumradius 5.0 :height 4.0)

    ;; Shaft
    (cylinder :radius 2.5 :height shaft-length :position '(0 0 4.0))

    ;; Thread
    (thread thread-spec
            :length thread-length
            :type :external
            :position `(0 0 ,(+ 4.0 (- shaft-length thread-length)))))

  (let ((bolt (hex-bolt :m6 50.0 30.0)))
    (is (not (null bolt))
        "Complete hex bolt should be created")

    (is (clad:valid-shape-p bolt)
        "Hex bolt with thread should be valid")))

(test test-hex-nut-with-thread
  "Verify defpart can create hex nut with internal thread"
  (clad.dsl:defpart hex-nut (thread-spec wrench-size height)
    ;; Hex outer shape
    (hex-prism :circumradius (/ wrench-size 2) :height height)

    ;; Center hole
    (let ((hole-radius (/ (clad.features:thread-major-diameter thread-spec) 2)))
      (hole :radius hole-radius :height height))

    ;; Internal thread
    (thread thread-spec :length height :type :internal))

  (let ((nut (hex-nut :m8 13.0 8.0)))
    (is (not (null nut))
        "Hex nut should be created")

    (is (clad:valid-shape-p nut)
        "Hex nut with internal thread should be valid")))

;;; ============================================================================
;;; Multiple Threads Tests
;;; ============================================================================

(test test-multiple-threads-in-defpart
  "Verify multiple thread sections in one defpart"
  (clad.dsl:defpart dual-threaded-shaft ()
    ;; Main shaft
    (cylinder :radius 5.0 :height 120.0)

    ;; First thread section (M6)
    (thread :m6 :length 25.0 :type :external :position '(0 0 10.0))

    ;; Second thread section (M8)
    (thread :m8 :length 30.0 :type :external :position '(0 0 70.0)))

  (let ((shaft (dual-threaded-shaft)))
    (is (not (null shaft))
        "Dual-threaded shaft should be created")

    (is (clad:valid-shape-p shaft)
        "Shaft with multiple threads should be valid")))

;;; ============================================================================
;;; Thread with Lead-In/Lead-Out Tests
;;; ============================================================================

(test test-thread-with-lead
  "Verify DSL supports threads with lead-in and lead-out"
  (clad.dsl:defpart smooth-engagement-bolt ()
    (cylinder :radius 3.0 :height 50.0)
    (thread :m6
            :length 30.0
            :type :external
            :position '(0 0 10.0)
            :lead-in 0.5
            :lead-out 0.5))

  (let ((bolt (smooth-engagement-bolt)))
    (is (not (null bolt))
        "Bolt with lead-in/out should be created")

    (is (clad:valid-shape-p bolt)
        "Bolt with gradual thread engagement should be valid")))

;;; ============================================================================
;;; Left-Handed Thread Tests
;;; ============================================================================

(test test-left-handed-thread-dsl
  "Verify DSL supports left-handed threads"
  (clad.dsl:defpart left-handed-bolt ()
    (cylinder :radius 3.0 :height 50.0)
    (thread :m6
            :length 30.0
            :type :external
            :position '(0 0 10.0)
            :handedness :left))  ; Left-handed thread

  (let ((bolt (left-handed-bolt)))
    (is (not (null bolt))
        "Left-handed bolt should be created")

    (is (clad:valid-shape-p bolt)
        "Left-handed threaded bolt should be valid")))

;;; ============================================================================
;;; Thread as Feature Tests
;;; ============================================================================

(test test-thread-as-reusable-feature
  "Verify threads can be defined as reusable features"
  ;; Define a thread feature
  (clad.dsl:deffeature standard-thread (spec length)
    (thread spec :length length :type :external))

  ;; Use feature in multiple parts
  (clad.dsl:defpart bolt-using-feature ()
    (cylinder :radius 3.0 :height 50.0)
    (standard-thread :m6 30.0))

  (let ((bolt (bolt-using-feature)))
    (is (not (null bolt))
        "Bolt using thread feature should be created")

    (is (clad:valid-shape-p bolt)
        "Bolt with feature-based thread should be valid")))

;;; ============================================================================
;;; Helper Functions for Tests
;;; ============================================================================

(defun approximately-equal (expected actual tolerance)
  "Test if two numbers are approximately equal within tolerance"
  (<= (abs (- expected actual)) tolerance))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-thread-dsl-tests ()
  "Run all thread DSL integration tests and report results"
  (format t "~%~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║         Thread DSL Integration Tests - Phase 4               ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  (let ((results (run 'thread-dsl-suite)))
    (format t "~%Test Results:~%")
    (format t "  Tests run: ~A~%" (length results))
    (format t "  Passed: ~A~%" (count-if #'test-passed-p results))
    (format t "  Failed: ~A~%" (count-if #'test-failed-p results))
    (format t "~%")

    results))
