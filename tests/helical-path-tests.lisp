;;;; tests/helical-path-tests.lisp --- Helical Path Generation Tests
;;;;
;;;; Test-Driven Development for 3D Thread Geometry - Phase 2
;;;; These tests define the expected behavior of helical path generation

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite helical-path-suite
  :description "Helical path generation and curve construction tests")

(in-suite helical-path-suite)

;;; ============================================================================
;;; Basic Helix Creation Tests
;;; ============================================================================

(test test-helix-creation-basic
  "Verify basic helix creation with standard parameters"
  (let ((helix (clad.features.helical-path:make-helix
                :pitch 1.0
                :radius 2.5
                :height 10.0
                :right-handed t)))

    (is (not (null helix))
        "Helix should be created successfully")

    ;; Helix should be a valid OCCT edge
    (is (clad.ffi:is-valid-shape helix)
        "Helix should be a valid OCCT shape")))

(test test-helix-pitch-verification
  "Verify helix has correct pitch (rise per revolution)"
  (let* ((pitch 1.5)
         (radius 3.0)
         (height 15.0)
         (helix (clad.features.helical-path:make-helix
                 :pitch pitch
                 :radius radius
                 :height height
                 :right-handed t))
         (helix-info (clad.features.helical-path:get-helix-info helix)))

    ;; Verify pitch matches specification
    (is (approximately-equal pitch (getf helix-info :pitch) 0.01)
        "Helix pitch should match specification")

    ;; Verify number of turns
    (let ((expected-turns (/ height pitch)))
      (is (approximately-equal expected-turns (getf helix-info :turns) 0.1)
          "Number of turns should equal height/pitch"))))

;;; ============================================================================
;;; Start and End Point Tests
;;; ============================================================================

(test test-helix-start-point
  "Verify helix starts at correct position"
  (let* ((radius 2.5)
         (helix (clad.features.helical-path:make-helix
                 :pitch 1.0
                 :radius radius
                 :height 10.0
                 :right-handed t))
         (start-point (clad.ffi:get-curve-start-point helix)))

    ;; Start point should be at (radius, 0, 0) in cylindrical coords
    ;; In Cartesian: (radius, 0, 0)
    (is (approximately-equal radius (clad.ffi:point-x start-point) 0.01)
        "Helix should start at X = radius")
    (is (approximately-equal 0.0 (clad.ffi:point-y start-point) 0.01)
        "Helix should start at Y = 0")
    (is (approximately-equal 0.0 (clad.ffi:point-z start-point) 0.01)
        "Helix should start at Z = 0")))

(test test-helix-end-point
  "Verify helix ends at correct height"
  (let* ((height 12.0)
         (radius 3.0)
         (helix (clad.features.helical-path:make-helix
                 :pitch 1.5
                 :radius radius
                 :height height
                 :right-handed t))
         (end-point (clad.ffi:get-curve-end-point helix)))

    ;; End point Z coordinate should equal height
    (is (approximately-equal height (clad.ffi:point-z end-point) 0.01)
        "Helix should end at specified height")

    ;; End point should still be at same radius
    (let ((end-radius (sqrt (+ (expt (clad.ffi:point-x end-point) 2)
                               (expt (clad.ffi:point-y end-point) 2)))))
      (is (approximately-equal radius end-radius 0.01)
          "Helix end point should maintain constant radius"))))

;;; ============================================================================
;;; Handedness Tests
;;; ============================================================================

(test test-right-handed-helix
  "Verify right-handed helix rotates clockwise when viewed from top"
  (let* ((helix (clad.features.helical-path:make-helix
                 :pitch 2.0
                 :radius 4.0
                 :height 10.0
                 :right-handed t))
         (helix-info (clad.features.helical-path:get-helix-info helix)))

    (is (eq :right-handed (getf helix-info :handedness))
        "Helix should be marked as right-handed")

    ;; Sample a point at 1/4 turn (pitch/4 height, 90° rotation)
    ;; For right-handed: should be at (0, radius, pitch/4)
    (let* ((sample-param 0.25) ; 1/4 of the way along
           (sample-point (clad.ffi:evaluate-curve-at helix sample-param)))
      (is (approximately-equal 0.0 (clad.ffi:point-x sample-point) 0.5)
          "At 1/4 turn, X should be near 0 for right-handed helix")
      (is (> (clad.ffi:point-y sample-point) 0)
          "At 1/4 turn, Y should be positive for right-handed helix"))))

(test test-left-handed-helix
  "Verify left-handed helix rotates counter-clockwise when viewed from top"
  (let* ((helix (clad.features.helical-path:make-helix
                 :pitch 2.0
                 :radius 4.0
                 :height 10.0
                 :right-handed nil))
         (helix-info (clad.features.helical-path:get-helix-info helix)))

    (is (eq :left-handed (getf helix-info :handedness))
        "Helix should be marked as left-handed")

    ;; Sample a point at 1/4 turn
    ;; For left-handed: rotation direction is reversed
    (let* ((sample-param 0.25)
           (sample-point (clad.ffi:evaluate-curve-at helix sample-param)))
      (is (approximately-equal 0.0 (clad.ffi:point-x sample-point) 0.5)
          "At 1/4 turn, X should be near 0 for left-handed helix")
      (is (< (clad.ffi:point-y sample-point) 0)
          "At 1/4 turn, Y should be negative for left-handed helix"))))

;;; ============================================================================
;;; Thread-Specific Helix Tests
;;; ============================================================================

(test test-m6-thread-helix
  "Verify helix generation for M6 thread specification"
  (let* ((length 30.0)
         (helix (clad.features.helical-path:make-helix-for-thread
                 :thread-spec :m6
                 :length length
                 :right-handed t))
         (helix-info (clad.features.helical-path:get-helix-info helix)))

    ;; M6 has 1.0mm pitch
    (is (approximately-equal 1.0 (getf helix-info :pitch) 0.01)
        "M6 helix should have 1.0mm pitch")

    ;; Radius should be pitch diameter / 2 = 5.3505 / 2
    (is (approximately-equal 2.675 (getf helix-info :radius) 0.01)
        "M6 helix radius should be at pitch diameter")

    ;; Height should match specified length
    (is (approximately-equal length (getf helix-info :height) 0.01)
        "Helix height should match specified length")))

(test test-various-thread-helices
  "Verify helix generation for various thread specifications"
  (dolist (spec '(:m3 :m4 :m5 :m6 :m8 :m10 :m12))
    (let ((helix (clad.features.helical-path:make-helix-for-thread
                  :thread-spec spec
                  :length 20.0
                  :right-handed t)))

      (is (not (null helix))
          (format nil "Should create helix for ~A" spec))

      (is (clad.ffi:is-valid-shape helix)
          (format nil "~A helix should be valid OCCT shape" spec)))))

;;; ============================================================================
;;; Helix Curve Quality Tests
;;; ============================================================================

(test test-helix-curve-smoothness
  "Verify helix curve is smooth with sufficient control points"
  (let* ((helix (clad.features.helical-path:make-helix
                 :pitch 1.0
                 :radius 2.5
                 :height 10.0
                 :right-handed t))
         (curve-info (clad.ffi:get-curve-properties helix)))

    ;; Helix should be a B-spline curve
    (is (eq :bspline (getf curve-info :type))
        "Helix should be represented as B-spline curve")

    ;; Should have sufficient control points for smooth curve (>= 100)
    (is (>= (getf curve-info :num-poles) 100)
        "Helix should have at least 100 control points for smoothness")

    ;; Curve should be C2 continuous (smooth second derivative)
    (is (>= (getf curve-info :continuity) 2)
        "Helix should be at least C2 continuous")))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(test test-invalid-helix-parameters
  "Verify error handling for invalid helix parameters"
  ;; Negative pitch
  (signals error
    (clad.features.helical-path:make-helix
     :pitch -1.0
     :radius 2.5
     :height 10.0
     :right-handed t)
    "Should signal error for negative pitch")

  ;; Negative radius
  (signals error
    (clad.features.helical-path:make-helix
     :pitch 1.0
     :radius -2.5
     :height 10.0
     :right-handed t)
    "Should signal error for negative radius")

  ;; Zero or negative height
  (signals error
    (clad.features.helical-path:make-helix
     :pitch 1.0
     :radius 2.5
     :height 0.0
     :right-handed t)
    "Should signal error for zero height"))

;;; ============================================================================
;;; Helper Functions for Tests
;;; ============================================================================

(defun approximately-equal (expected actual tolerance)
  "Test if two numbers are approximately equal within tolerance"
  (<= (abs (- expected actual)) tolerance))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-helical-path-tests ()
  "Run all helical path tests and report results"
  (format t "~%~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║        Helical Path Generation Tests - Phase 2               ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  (let ((results (run 'helical-path-suite)))
    (format t "~%Test Results:~%")
    (format t "  Tests run: ~A~%"  (length results))
    (format t "  Passed: ~A~%" (count-if #'test-passed-p results))
    (format t "  Failed: ~A~%"  (count-if #'test-failed-p results))
    (format t "~%")

    results))
