;;;; examples/thread-modeling-examples.lisp
;;;;
;;;; Complete Thread Modeling Examples
;;;; Demonstrates Phase 1-4 thread geometry implementation with DSL integration

(in-package :clad)

;;; ============================================================================
;;; Example 1: Simple External Thread on Shaft (Low-Level API)
;;; ============================================================================

(defun example-1-simple-external-thread ()
  "Create a simple shaft with external thread using low-level API"

  ;; Create base shaft
  (let ((shaft (clad.core:make-cylinder :radius 3.0 :height 50.0)))

    ;; Create M6 external thread, 30mm long
    (let ((thread (clad.features.helical-sweep:make-external-thread :m6 30.0)))

      ;; Apply thread to shaft at position 10mm from bottom
      (clad.features.thread-boolean:apply-external-thread
       shaft thread :position '(0 0 10.0)))))

;;; ============================================================================
;;; Example 2: Simple External Thread on Shaft (DSL)
;;; ============================================================================

(defpart simple-threaded-shaft ()
  "Create a simple shaft with external thread using DSL"
  ;; Base shaft
  (cylinder :radius 3.0 :height 50.0)

  ;; Add M6 thread, 30mm long, starting at 10mm from bottom
  (thread :m6 :length 30.0 :type :external :position '(0 0 10.0)))

;;; ============================================================================
;;; Example 3: Block with Internal Thread (DSL)
;;; ============================================================================

(defpart threaded-block ()
  "Create a block with threaded hole"
  ;; Base block
  (box :width 20.0 :depth 20.0 :height 30.0)

  ;; Center hole (5mm diameter, through entire block)
  (hole :radius 2.5 :height 30.0 :position '(10.0 10.0 0))

  ;; Internal M6 thread, 20mm deep, starting 5mm from top
  (thread :m6 :length 20.0 :type :internal :position '(10.0 10.0 5.0)))

;;; ============================================================================
;;; Example 4: Complete Hex Bolt with Thread (DSL)
;;; ============================================================================

(defpart hex-bolt (thread-spec shaft-length thread-length)
  "Create a complete hex bolt with head and thread

  THREAD-SPEC: Thread size (e.g., :m6, :m8)
  SHAFT-LENGTH: Total length of shaft (mm)
  THREAD-LENGTH: Length of threaded section (mm)"

  ;; Get thread parameters
  (let* ((spec (clad.features:get-thread-spec thread-spec))
         (major-d (getf spec :major-diameter))
         (head-dia (* major-d 1.5))
         (head-height (* major-d 0.6)))

    ;; Hex head
    (cylinder :radius (/ head-dia 2.0) :height head-height)

    ;; Shaft
    (cylinder :radius (/ major-d 2.0)
              :height shaft-length
              :position `(0 0 ,head-height))

    ;; Thread at end of shaft
    (thread thread-spec
            :length thread-length
            :type :external
            :position `(0 0 ,(+ head-height (- shaft-length thread-length))))))

;;; Usage:
;;; (view (hex-bolt :m6 50.0 30.0))  ; M6 bolt, 50mm long, 30mm threaded

;;; ============================================================================
;;; Example 5: Complete Hex Nut with Internal Thread (DSL)
;;; ============================================================================

(defpart hex-nut (thread-spec wrench-size height)
  "Create a complete hex nut with internal thread

  THREAD-SPEC: Thread size (e.g., :m6, :m8)
  WRENCH-SIZE: Wrench size across flats (mm)
  HEIGHT: Nut height (mm)"

  ;; Get thread parameters
  (let* ((spec (clad.features:get-thread-spec thread-spec))
         (major-d (getf spec :major-diameter))
         (hole-radius (/ major-d 2.0)))

    ;; Hex outer shape (circumradius for hex)
    (cylinder :radius (/ wrench-size 2.0) :height height)

    ;; Center hole
    (hole :radius hole-radius :height height)

    ;; Internal thread
    (thread thread-spec :length height :type :internal)))

;;; Usage:
;;; (view (hex-nut :m8 13.0 8.0))  ; M8 nut, 13mm wrench, 8mm height

;;; ============================================================================
;;; Example 6: Parametric Bolt Family
;;; ============================================================================

(defpart parametric-bolt (thread-spec total-length
                          &key (thread-ratio 0.6) (head-type :hex))
  "Create parametric bolt with adjustable thread length ratio

  THREAD-SPEC: Thread size
  TOTAL-LENGTH: Total shaft length (mm)
  THREAD-RATIO: Ratio of threaded section (0.0-1.0, default 0.6)
  HEAD-TYPE: :hex, :socket, or :pan"

  (let* ((spec (clad.features:get-thread-spec thread-spec))
         (major-d (getf spec :major-diameter))
         (shaft-radius (/ major-d 2.0))
         (thread-length (* total-length thread-ratio))
         (head-dia (* major-d 1.5))
         (head-height (* major-d 0.6)))

    ;; Head (different shapes based on type)
    (case head-type
      (:hex (cylinder :radius (/ head-dia 2.0) :height head-height))
      (:socket (cylinder :radius (/ (* major-d 1.7) 2.0) :height head-height))
      (:pan (cylinder :radius (/ (* major-d 1.8) 2.0) :height (* head-height 0.5))))

    ;; Shaft
    (cylinder :radius shaft-radius
              :height total-length
              :position `(0 0 ,head-height))

    ;; Thread
    (thread thread-spec
            :length thread-length
            :type :external
            :position `(0 0 ,(+ head-height (- total-length thread-length))))))

;;; Usage:
;;; (view (parametric-bolt :m6 60.0 :thread-ratio 0.7))
;;; (view (parametric-bolt :m8 80.0 :head-type :socket))

;;; ============================================================================
;;; Example 7: Dual-Threaded Shaft
;;; ============================================================================

(defpart dual-threaded-shaft ()
  "Shaft with two different thread sections"

  ;; Main shaft
  (cylinder :radius 5.0 :height 120.0)

  ;; First thread section (M6)
  (thread :m6 :length 25.0 :type :external :position '(0 0 10.0))

  ;; Second thread section (M8)
  (thread :m8 :length 30.0 :type :external :position '(0 0 70.0)))

;;; ============================================================================
;;; Example 8: Thread with Lead-In/Lead-Out (Smooth Engagement)
;;; ============================================================================

(defpart smooth-engagement-bolt ()
  "Bolt with gradual thread lead-in and lead-out for easier assembly"

  ;; Shaft
  (cylinder :radius 3.0 :height 50.0)

  ;; Thread with 0.5 turn lead-in and lead-out
  (thread :m6
          :length 30.0
          :type :external
          :position '(0 0 10.0)
          :lead-in 0.5
          :lead-out 0.5))

;;; ============================================================================
;;; Example 9: Left-Handed Thread
;;; ============================================================================

(defpart left-handed-bolt ()
  "Bolt with left-handed (reverse) thread"

  (cylinder :radius 3.0 :height 50.0)

  ;; Left-handed thread (turns counter-clockwise)
  (thread :m6
          :length 30.0
          :type :external
          :position '(0 0 10.0)
          :handedness :left))

;;; ============================================================================
;;; Example 10: Thread Fit Checking (Low-Level API)
;;; ============================================================================

(defun example-10-thread-fit-check ()
  "Check if bolt and nut threads are compatible"

  ;; Create matching threads
  (let ((bolt-thread (clad.features.helical-sweep:make-external-thread :m6 20.0))
        (nut-thread (clad.features.helical-sweep:make-internal-thread :m6 20.0)))

    ;; Check fit
    (let ((fit (clad.features.thread-boolean:check-thread-fit
                bolt-thread nut-thread)))

      (format t "~%Thread Fit Analysis:~%")
      (format t "  Fit Type: ~A~%" fit)

      ;; Calculate engagement length
      (let ((engagement (clad.features.thread-boolean:calculate-engagement-length
                         bolt-thread nut-thread)))
        (format t "  Engagement Length: ~,2F mm~%" engagement))

      ;; Detailed analysis
      (let ((analysis (clad.features.thread-boolean:analyze-thread-engagement
                       bolt-thread nut-thread)))
        (format t "~%Detailed Analysis:~%")
        (format t "  External diameter: ~,2F mm~%"
                (getf analysis :external-diameter))
        (format t "  Internal diameter: ~,2F mm~%"
                (getf analysis :internal-diameter))
        (format t "  Diameter difference: ~,3F mm~%"
                (getf analysis :diameter-difference))
        (format t "  External length: ~,2F mm~%"
                (getf analysis :external-length))
        (format t "  Internal length: ~,2F mm~%"
                (getf analysis :internal-length))
        (format t "  Engagement length: ~,2F mm~%"
                (getf analysis :engagement-length))))))

;;; ============================================================================
;;; Example 11: Thread Strength Estimation
;;; ============================================================================

(defun example-11-thread-strength ()
  "Estimate thread strength for M8 with 15mm engagement"

  (let* ((thread-spec :m8)
         (engagement-length 15.0)
         (steel-strength 400.0)  ; 400 MPa mild steel
         (force (clad.features.thread-boolean:thread-strength-estimate
                 thread-spec engagement-length steel-strength)))

    (format t "~%M8 Thread Strength Estimate:~%")
    (format t "  Engagement length: ~A mm~%" engagement-length)
    (format t "  Material: Mild steel (~A MPa)~%" steel-strength)
    (format t "  Estimated pull-out force: ~,0F N (~,1F kN)~%"
            force (/ force 1000.0))))

;;; ============================================================================
;;; Example 12: Complete Bolt and Nut Assembly (Low-Level API)
;;; ============================================================================

(defun example-12-bolt-nut-assembly ()
  "Create complete bolt and nut using high-level API"

  ;; Create M6 bolt: 50mm long, 30mm threaded, hex head
  (let ((bolt (clad.features.thread-boolean:make-threaded-bolt
               :thread-spec :m6
               :thread-length 30.0
               :shaft-length 50.0
               :head-type :hex
               :head-diameter 10.0
               :head-height 4.0)))

    ;; Create M6 nut: 8mm high, hex, 10mm wrench size
    (let ((nut (clad.features.thread-boolean:make-threaded-nut
                :thread-spec :m6
                :height 8.0
                :nut-type :hex
                :wrench-size 10.0)))

      (format t "~%Bolt and Nut Created:~%")
      (format t "  Bolt volume: ~,2F mm³~%" (clad.ffi:get-volume bolt))
      (format t "  Nut volume: ~,2F mm³~%" (clad.ffi:get-volume nut))

      ;; Return both as list for viewing
      (list bolt nut))))

;;; ============================================================================
;;; Example 13: Tap Drill Size Calculator
;;; ============================================================================

(defun example-13-tap-drill-calculator ()
  "Calculate tap drill sizes for common metric threads"

  (format t "~%Tap Drill Size Calculator:~%")
  (format t "~A~%" (make-string 50 :initial-element #\=))

  (dolist (spec '(:m3 :m4 :m5 :m6 :m8 :m10 :m12))
    (let ((tap-drill (clad.features.thread-boolean:calculate-tap-drill-size spec))
          (designation (clad.features.thread-boolean:thread-designation spec)))
      (format t "  ~A: ~,2F mm tap drill~%" designation tap-drill))))

;;; ============================================================================
;;; Example 14: Thread Specifications
;;; ============================================================================

(defun example-14-thread-specs ()
  "Display specifications for common metric threads"

  (format t "~%ISO Metric Thread Specifications (ISO 68-1):~%")
  (format t "~A~%" (make-string 80 :initial-element #\=))
  (format t "~10A ~10A ~10A ~10A ~10A~%"
          "Thread" "Major Ø" "Pitch Ø" "Minor Ø" "Pitch")
  (format t "~A~%" (make-string 80 :initial-element #\-))

  (dolist (spec '(:m3 :m4 :m5 :m6 :m8 :m10 :m12 :m16 :m20))
    (let ((info (clad.features.thread-boolean:get-thread-spec-info spec)))
      (format t "~10A ~10,2F ~10,2F ~10,2F ~10,2F~%"
              (clad.features.thread-boolean:thread-designation spec)
              (getf info :major-diameter)
              (getf info :pitch-diameter)
              (getf info :minor-diameter)
              (getf info :pitch)))))

;;; ============================================================================
;;; Running Examples
;;; ============================================================================

(defun run-thread-examples ()
  "Run all thread modeling examples"

  (format t "~%~%")
  (format t "╔════════════════════════════════════════════════════════════════╗~%")
  (format t "║           CLAD Thread Modeling Examples                       ║~%")
  (format t "║           Phase 4: Complete Thread System                     ║~%")
  (format t "╚════════════════════════════════════════════════════════════════╝~%")
  (format t "~%")

  ;; Example 1: Low-level API
  (format t "~%Example 1: Simple External Thread (Low-Level API)~%")
  (let ((result (example-1-simple-external-thread)))
    (format t "  Created threaded shaft: ~A~%" (clad.core:valid-shape-p result)))

  ;; Example 2: DSL
  (format t "~%Example 2: Simple External Thread (DSL)~%")
  (let ((result (simple-threaded-shaft)))
    (format t "  Created threaded shaft: ~A~%" (clad.core:valid-shape-p result)))

  ;; Example 3: Internal thread
  (format t "~%Example 3: Block with Internal Thread~%")
  (let ((result (threaded-block)))
    (format t "  Created threaded block: ~A~%" (clad.core:valid-shape-p result)))

  ;; Example 4: Hex bolt
  (format t "~%Example 4: Complete Hex Bolt~%")
  (let ((result (hex-bolt :m6 50.0 30.0)))
    (format t "  Created M6 hex bolt: ~A~%" (clad.core:valid-shape-p result)))

  ;; Example 5: Hex nut
  (format t "~%Example 5: Complete Hex Nut~%")
  (let ((result (hex-nut :m8 13.0 8.0)))
    (format t "  Created M8 hex nut: ~A~%" (clad.core:valid-shape-p result)))

  ;; Example 10: Thread fit check
  (example-10-thread-fit-check)

  ;; Example 11: Strength estimation
  (example-11-thread-strength)

  ;; Example 13: Tap drill calculator
  (example-13-tap-drill-calculator)

  ;; Example 14: Thread specs
  (example-14-thread-specs)

  (format t "~%~%All examples completed successfully!~%")
  (format t "~%To view examples interactively:~%")
  (format t "  (view (simple-threaded-shaft))~%")
  (format t "  (view (hex-bolt :m6 50.0 30.0))~%")
  (format t "  (view (hex-nut :m8 13.0 8.0))~%")
  (format t "  (view (dual-threaded-shaft))~%")
  (format t "~%"))

;;; ============================================================================
;;; Quick Start Guide
;;; ============================================================================

#|

CLAD Thread Modeling Quick Start
=================================

1. SIMPLE EXTERNAL THREAD (DSL):

   (defpart my-bolt ()
     (cylinder :radius 3.0 :height 50.0)
     (thread :m6 :length 30.0 :type :external :position '(0 0 10.0)))

   (view (my-bolt))


2. SIMPLE INTERNAL THREAD (DSL):

   (defpart my-nut ()
     (cylinder :radius 8.0 :height 10.0)
     (hole :radius 2.5 :height 10.0)
     (thread :m6 :length 10.0 :type :internal))

   (view (my-nut))


3. COMPLETE BOLT (DSL):

   (view (hex-bolt :m8 60.0 40.0))   ; M8, 60mm long, 40mm threaded


4. COMPLETE NUT (DSL):

   (view (hex-nut :m10 17.0 10.0))   ; M10, 17mm wrench, 10mm high


5. PARAMETRIC DESIGN:

   (view (parametric-bolt :m6 80.0 :thread-ratio 0.75))


6. SPECIAL FEATURES:

   ;; Left-handed thread
   (thread :m6 :length 30.0 :type :external :handedness :left)

   ;; Thread with lead-in/lead-out
   (thread :m8 :length 40.0 :type :external :lead-in 0.5 :lead-out 0.5)


7. THREAD FIT CHECKING (Low-Level):

   (let ((bolt-thread (clad.features.helical-sweep:make-external-thread :m6 20.0))
         (nut-thread (clad.features.helical-sweep:make-internal-thread :m6 20.0)))
     (clad.features.thread-boolean:check-thread-fit bolt-thread nut-thread))


8. UTILITIES:

   ;; Calculate tap drill size
   (clad.features.thread-boolean:calculate-tap-drill-size :m8)
   => 6.75

   ;; Get thread designation
   (clad.features.thread-boolean:thread-designation :m8)
   => "M8 x 1.25"

   ;; Get full thread spec
   (clad.features:get-thread-spec :m8)
   => (:major-diameter 8.0 :pitch-diameter 7.188 :minor-diameter 6.466 :pitch 1.25)


For more examples, run: (run-thread-examples)

|#
