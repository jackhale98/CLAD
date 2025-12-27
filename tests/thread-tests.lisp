;;;; tests/thread-tests.lisp --- Test suite for thread modeling

(in-package :clad.tests)

(def-suite thread-tests
  :description "Test suite for thread modeling and fastener features"
  :in clad-tests)

(in-suite thread-tests)

;;; ============================================================================
;;; Category 1: Thread Database Tests
;;; ============================================================================

(test thread-database-m6
  "Retrieve M6 thread parameters from database"
  (let* ((params (clad.features:get-thread-spec :m6)))
    (is (not (null params)))
    (is (= (getf params :major-diameter) 6.0))
    (is (= (getf params :pitch) 1.0))
    (is (string= (getf params :standard) "ISO Metric"))))

(test thread-database-m8
  "Retrieve M8 thread parameters from database"
  (let* ((params (clad.features:get-thread-spec :m8)))
    (is (not (null params)))
    (is (= (getf params :major-diameter) 8.0))
    (is (= (getf params :pitch) 1.25))
    (is (string= (getf params :standard) "ISO Metric"))))

(test thread-database-m10
  "Retrieve M10 thread parameters from database"
  (let* ((params (clad.features:get-thread-spec :m10)))
    (is (not (null params)))
    (is (= (getf params :major-diameter) 10.0))
    (is (= (getf params :pitch) 1.5))))

(test thread-database-quarter-20
  "Retrieve 1/4-20 UNC thread parameters"
  ;; Note: Symbol requires pipe escaping due to slash
  (let* ((params (clad.features:get-thread-spec :|1/4-20|)))
    (is (not (null params)))
    (is (= (getf params :major-diameter) 6.35))  ; 1/4" in mm
    (is (= (getf params :tpi) 20))  ; Threads per inch
    (is (string= (getf params :standard) "UNC"))))

(test thread-database-list-standards
  "List all available thread standards"
  (let* ((standards (clad.features:list-thread-specs)))
    (is (listp standards))
    (is (>= (length standards) 7))
    (is (member :m6 standards))
    (is (member :m8 standards))
    (is (member :m10 standards))))

;;; ============================================================================
;;; Category 2: Helix Creation Tests
;;; ============================================================================

(test helix-basic-creation
  "Create a basic helical curve"
  (let* ((helix (clad.features:make-helix :radius 5
                                           :pitch 2
                                           :height 20)))
    (is (not (null helix)))
    (is (clad.core:valid-shape-p helix))))

(test helix-left-hand
  "Create left-hand (counterclockwise) helix"
  (let* ((helix (clad.features:make-helix :radius 3
                                           :pitch 1.5
                                           :height 10
                                           :right-hand nil)))
    (is (not (null helix)))
    (is (clad.core:valid-shape-p helix))))

(test helix-parameters
  "Verify helix parameters are stored in metadata"
  (let* ((helix (clad.features:make-helix :radius 10
                                           :pitch 3
                                           :height 30)))
    (is (= (getf (clad.core:shape-metadata helix) :radius) 10))
    (is (= (getf (clad.core:shape-metadata helix) :pitch) 3))
    (is (= (getf (clad.core:shape-metadata helix) :height) 30))))

;;; ============================================================================
;;; Category 3: Thread Profile Creation
;;; ============================================================================

(test thread-profile-iso-metric
  "Create ISO metric thread profile (60 degree angle)"
  (let* ((profile (clad.features:make-thread-profile :type :iso-metric
                                                       :pitch 1.5)))
    (is (not (null profile)))
    (is (clad.core:valid-shape-p profile))
    ;; ISO metric has 60 degree thread angle
    (is (= (getf (clad.core:shape-metadata profile) :thread-angle) 60))))

(test thread-profile-unified
  "Create unified thread profile (60 degree angle, different proportions)"
  (let* ((profile (clad.features:make-thread-profile :type :unified
                                                       :pitch 1.27)))  ; 20 TPI ≈ 1.27mm
    (is (not (null profile)))
    (is (clad.core:valid-shape-p profile))
    (is (= (getf (clad.core:shape-metadata profile) :thread-angle) 60))))

;;; ============================================================================
;;; Category 4: External Thread Creation (Cosmetic)
;;; ============================================================================

(test external-thread-m6-cosmetic
  "Create cosmetic M6 external thread (simple representation)"
  (let* ((thread (clad.features:make-external-thread :m6
                                                       :length 20
                                                       :cosmetic t)))
    (is (not (null thread)))
    (is (clad.core:valid-shape-p thread))
    ;; Cosmetic should be marked in metadata
    (is (eq (getf (clad.core:shape-metadata thread) :representation) :cosmetic))))

(test external-thread-m8-cosmetic
  "Create cosmetic M8 external thread"
  (let* ((thread (clad.features:make-external-thread :m8
                                                       :length 30
                                                       :cosmetic t)))
    (is (not (null thread)))
    (is (clad.core:valid-shape-p thread))))

(test internal-thread-cosmetic
  "Create cosmetic M6 internal thread"
  (let* ((thread (clad.features:make-internal-thread :m6
                                                       :depth 20
                                                       :cosmetic t)))
    (is (not (null thread)))
    (is (clad.core:valid-shape-p thread))
    (is (eq (getf (clad.core:shape-metadata thread) :representation) :cosmetic))))

;;; ============================================================================
;;; Category 5: Thread Parameter Calculations
;;; ============================================================================

(test calculate-minor-diameter-m6
  "Calculate minor diameter for M6 thread"
  (let* ((minor-dia (clad.features:thread-minor-diameter :m6)))
    ;; M6 minor diameter ≈ 4.917mm (using ISO 68-1 formula)
    (is (< (abs (- minor-dia 4.917)) 0.1))))

(test calculate-minor-diameter-m8
  "Calculate minor diameter for M8 thread"
  (let* ((minor-dia (clad.features:thread-minor-diameter :m8)))
    ;; M8 minor diameter ≈ 6.647mm
    (is (< (abs (- minor-dia 6.647)) 0.1))))

(test calculate-tap-drill-size-m6
  "Calculate tap drill size for M6 thread"
  (let* ((tap-drill (clad.features:tap-drill-size :m6)))
    ;; M6 tap drill ≈ 5.0mm (for standard tap)
    (is (< (abs (- tap-drill 5.0)) 0.2))))

(test calculate-tap-drill-size-m8
  "Calculate tap drill size for M8 thread"
  (let* ((tap-drill (clad.features:tap-drill-size :m8)))
    ;; M8 tap drill ≈ 6.75mm (major - pitch = 8.0 - 1.25)
    (is (< (abs (- tap-drill 6.75)) 0.2))))

(test calculate-pitch-diameter-m6
  "Calculate pitch diameter for M6 thread"
  (let* ((pitch-dia (clad.features:thread-pitch-diameter :m6)))
    ;; M6 pitch diameter ≈ 5.35mm (using ISO 68-1 formula)
    (is (< (abs (- pitch-dia 5.35)) 0.1))))

;;; ============================================================================
;;; Category 6: Thread Designation Strings
;;; ============================================================================

(test thread-designation-m6
  "Generate M6 thread designation string"
  (let ((designation (clad.features:thread-designation-string :m6)))
    (is (stringp designation))
    (is (search "M6" designation))))

(test thread-designation-m8
  "Generate M8 thread designation string"
  (let ((designation (clad.features:thread-designation-string :m8)))
    (is (stringp designation))
    (is (search "M8" designation))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-thread-tests ()
  "Run all thread modeling tests"
  (format t "~%========================================~%")
  (format t "  Thread Modeling Tests~%")
  (format t "========================================~%~%")
  (run! 'thread-tests))
