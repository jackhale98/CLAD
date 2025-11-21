;;;; tests/thread-tests.lisp --- Test suite for thread modeling (TDD RED Phase)

(in-package :clad.tests)

(def-suite thread-tests
  :description "Test suite for thread modeling and fastener features"
  :in clad-tests)

(in-suite thread-tests)

;;; ============================================================================
;;; Category 1: Helical Curve Creation (prerequisite for threads)
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
;;; Category 2: Thread Profile Creation
;;; ============================================================================

(test thread-profile-iso-metric
  "Create ISO metric thread profile (60° angle)"
  (let* ((profile (clad.features:make-thread-profile :type :iso-metric
                                                       :pitch 1.5)))
    (is (not (null profile)))
    (is (clad.core:valid-shape-p profile))
    ;; ISO metric has 60° thread angle
    (is (= (getf (clad.core:shape-metadata profile) :thread-angle) 60))))

(test thread-profile-unified
  "Create unified thread profile (60° angle, different proportions)"
  (let* ((profile (clad.features:make-thread-profile :type :unified
                                                       :pitch 1.27)))  ; 20 TPI ≈ 1.27mm
    (is (not (null profile)))
    (is (clad.core:valid-shape-p profile))
    (is (= (getf (clad.core:shape-metadata profile) :thread-angle) 60))))

;;; ============================================================================
;;; Category 3: Thread Database and Standards
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
  (let* ((params (clad.features:get-thread-spec :1/4-20)))
    (is (not (null params)))
    (is (= (getf params :major-diameter) 6.35))  ; 1/4" in mm
    (is (= (getf params :tpi) 20))  ; Threads per inch
    (is (string= (getf params :standard) "UNC"))))

(test thread-database-list-standards
  "List all available thread standards"
  (let* ((standards (clad.features:list-thread-specs)))
    (is (listp standards))
    (is (>= (length standards) 7))  ; M3, M6, M8, M10, M8x1.0, M10x1.25, 1/4-20
    (is (member :m6 standards))
    (is (member :m8 standards))
    (is (member :m10 standards))))

;;; ============================================================================
;;; Category 4: External Thread Creation (Detailed)
;;; ============================================================================

(test external-thread-m6-detailed
  "Create detailed M6 external thread"
  (let* ((thread (clad.features:make-external-thread :m6 :length 20)))
    (is (not (null thread)))
    (is (clad.core:valid-shape-p thread))
    ;; Check bounding box to verify dimensions
    (let ((bbox (clad.shapes:bounding-box thread)))
      ;; Major diameter should be approximately 6mm (±0.5mm for tolerance)
      (is (< (abs (- (- (nth 3 bbox) (nth 0 bbox)) 6.0)) 0.5)))))

(test external-thread-m8-detailed
  "Create detailed M8 external thread with custom length"
  (let* ((thread (clad.features:make-external-thread :m8 :length 30)))
    (is (not (null thread)))
    (is (clad.core:valid-shape-p thread))
    ;; Verify metadata
    (is (eq (getf (clad.core:shape-metadata thread) :thread-type) :m8))
    (is (= (getf (clad.core:shape-metadata thread) :length) 30))))

(test external-thread-m10-short
  "Create M10 external thread with short length"
  (let* ((thread (clad.features:make-external-thread :m10 :length 15)))
    (is (not (null thread)))
    (is (clad.core:valid-shape-p thread))))

(test external-thread-unified-quarter-20
  "Create 1/4-20 UNC external thread"
  (let* ((thread (clad.features:make-external-thread :1/4-20 :length 25.4)))  ; 1 inch
    (is (not (null thread)))
    (is (clad.core:valid-shape-p thread))))

;;; ============================================================================
;;; Category 5: External Thread Creation (Cosmetic)
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

(test cosmetic-vs-detailed-size
  "Cosmetic and detailed threads should have appropriate metadata"
  (let* ((detailed (clad.features:make-external-thread :m8 :length 20))
         (cosmetic (clad.features:make-external-thread :m8 :length 20 :cosmetic t)))
    ;; Both should be valid shapes
    (is (not (null detailed)))
    (is (not (null cosmetic)))
    ;; Check metadata
    (is (eq (getf (clad.core:shape-metadata cosmetic) :representation) :cosmetic))
    (is (eq (getf (clad.core:shape-metadata detailed) :representation) :detailed))))

;;; ============================================================================
;;; Category 6: Internal Thread Creation
;;; ============================================================================

(test internal-thread-m6-detailed
  "Create detailed M6 internal thread (threaded hole)"
  (let* ((thread (clad.features:make-internal-thread :m6 :depth 20)))
    (is (not (null thread)))
    (is (clad.core:valid-shape-p thread))
    ;; Should be a subtraction shape
    (is (eq (getf (clad.core:shape-metadata thread) :thread-direction) :internal))))

(test internal-thread-m8-blind-hole
  "Create M8 internal thread for blind hole"
  (let* ((thread (clad.features:make-internal-thread :m8 :depth 25)))
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
;;; Category 7: Thread Integration with Parts
;;; ============================================================================

(test add-external-thread-to-cylinder
  "Add external thread to existing cylinder"
  (let* ((cylinder (clad.core:make-cylinder 3 20))  ; 6mm diameter, 20mm tall
         (threaded (clad.features:add-external-thread cylinder :m6)))
    (is (not (null threaded)))
    (is (clad.core:valid-shape-p threaded))))

(test cut-internal-thread-in-hole
  "Cut internal thread into existing hole"
  (let* ((block (clad.core:make-box 20 20 10))
         (hole (clad.core:translate (clad.core:make-cylinder 2.5 15) 10 10 -2.5))
         (block-with-hole (clad.core:cut-shapes block hole))
         (threaded (clad.features:cut-internal-thread block-with-hole :m6 10 10 10)))
    (is (not (null threaded)))
    (is (clad.core:valid-shape-p threaded))))

;;; ============================================================================
;;; Category 8: Thread Parameters and Calculations
;;; ============================================================================

(test calculate-minor-diameter-m6
  "Calculate minor diameter for M6 thread"
  (let* ((minor-dia (clad.features:thread-minor-diameter :m6)))
    ;; M6 minor diameter ≈ 4.917mm
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
    ;; M8 tap drill ≈ 6.8mm
    (is (< (abs (- tap-drill 6.8)) 0.2))))

;;; ============================================================================
;;; Category 9: Thread Depth and Height Validation
;;; ============================================================================

(test thread-height-validation
  "Thread height should match specified length"
  (let* ((thread (clad.features:make-external-thread :m6 :length 25))
         (bbox (clad.shapes:bounding-box thread))
         (height (- (nth 5 bbox) (nth 2 bbox))))  ; zmax - zmin
    ;; Height should be approximately 25mm
    (is (< (abs (- height 25.0)) 1.0))))

(test thread-pitch-count
  "Verify thread has correct number of turns for pitch and height"
  (let* ((length 20)
         (pitch 1.5)  ; M10 has pitch 1.5
         (expected-turns (/ length pitch))
         (thread (clad.features:make-external-thread :m10 :length length)))
    ;; Metadata should record number of turns
    (is (= (getf (clad.core:shape-metadata thread) :turns)
           (floor expected-turns)))))

;;; ============================================================================
;;; Category 10: Error Handling
;;; ============================================================================

(test error-invalid-thread-spec
  "Error when requesting non-existent thread spec"
  (signals error
    (clad.features:get-thread-spec :m999)))

(test error-zero-length-thread
  "Error when thread length is zero or negative"
  (signals error
    (clad.features:make-external-thread :m6 :length 0))
  (signals error
    (clad.features:make-external-thread :m6 :length -5)))

(test error-invalid-depth-internal
  "Error when internal thread depth is invalid"
  (signals error
    (clad.features:make-internal-thread :m6 :depth 0))
  (signals error
    (clad.features:make-internal-thread :m6 :depth -10)))

;;; ============================================================================
;;; Category 11: Advanced Thread Standards
;;; ============================================================================

(test metric-fine-thread-m8x1
  "Create M8x1.0 fine pitch thread"
  (let* ((params (clad.features:get-thread-spec :m8x1.0)))
    (is (not (null params)))
    (is (= (getf params :major-diameter) 8.0))
    (is (= (getf params :pitch) 1.0))  ; Fine pitch instead of standard 1.25
    (is (string= (getf params :standard) "ISO Metric Fine"))))

(test metric-fine-thread-m10x1.25
  "Create M10x1.25 fine pitch thread"
  (let* ((params (clad.features:get-thread-spec :m10x1.25)))
    (is (not (null params)))
    (is (= (getf params :major-diameter) 10.0))
    (is (= (getf params :pitch) 1.25))))  ; Fine pitch instead of standard 1.5

;;; ============================================================================
;;; Test Summary
;;; ============================================================================
;;
;; Total Tests: 38
;;
;; Categories:
;; 1. Helical Curve Creation: 3 tests
;; 2. Thread Profile Creation: 2 tests
;; 3. Thread Database: 6 tests
;; 4. External Threads (Detailed): 4 tests
;; 5. External Threads (Cosmetic): 2 tests
;; 6. Internal Threads: 3 tests
;; 7. Thread Integration: 2 tests
;; 8. Thread Calculations: 4 tests
;; 9. Thread Validation: 2 tests
;; 10. Error Handling: 3 tests
;; 11. Advanced Standards: 2 tests
;;
;; Coverage:
;; - Helix geometry creation
;; - Thread profile generation (ISO metric, unified)
;; - External thread modeling (detailed and cosmetic)
;; - Internal thread modeling (detailed and cosmetic)
;; - Thread database (M3, M6, M8, M10, 1/4-20, etc.)
;; - Thread calculations (minor diameter, tap drill)
;; - Integration with existing shapes
;; - Error handling and validation
;; - Fine pitch threads
;;
;; Following TDD RED-GREEN-REFACTOR:
;; - RED: This test suite (all tests currently fail)
;; - GREEN: Implement src/features/threads.lisp to pass all tests
;; - REFACTOR: Clean up code and add documentation
