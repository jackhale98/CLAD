;;;; examples/07-threads.lisp --- Thread Modeling Tutorial
;;;;
;;;; Comprehensive thread modeling examples demonstrating:
;;;; - ISO Metric threads (M1.6 to M64)
;;;; - ISO Metric Fine threads
;;;; - UNC (Unified National Coarse) threads
;;;; - UNF (Unified National Fine) threads
;;;; - External threads (bolts, studs)
;;;; - Internal threads (threaded holes)
;;;; - Thread DSL integration
;;;; - Thread utilities and calculations

(in-package :cl-user)

(require :asdf)
(asdf:load-system :clad)

(format t "~%~%")
(format t "================================================================================~%")
(format t "                      CLAD Thread Modeling Tutorial                            ~%")
(format t "                       119 Thread Specifications                               ~%")
(format t "================================================================================~%~%")

;;; ============================================================================
;;; Part 1: Thread Database Overview
;;; ============================================================================

(format t "PART 1: Thread Database Overview~%")
(format t "=================================~%~%")

(format t "Available thread standards:~%")
(format t "  - ISO Metric Coarse: ~A threads~%"
        (length (clad.features:list-threads-by-standard "ISO Metric")))
(format t "  - UNC (Unified National Coarse): ~A threads~%"
        (length (clad.features:list-threads-by-standard "UNC")))
(format t "  - UNF (Unified National Fine): ~A threads~%"
        (length (clad.features:list-threads-by-standard "UNF")))
(format t "~%")

;; Show some common metric threads
(format t "Common ISO Metric Coarse Threads:~%")
(format t "  Thread    Major Ø    Pitch    Tap Drill~%")
(format t "  -------   -------    -----    ---------~%")
(dolist (spec '(:m3 :m4 :m5 :m6 :m8 :m10 :m12 :m16 :m20))
  (let* ((info (clad.features:get-thread-spec spec))
         (major-d (getf info :major-diameter))
         (pitch (getf info :pitch))
         (tap-drill (clad.features:tap-drill-size spec)))
    (format t "  ~6A    ~5,1Fmm    ~4,2Fmm   ~5,1Fmm~%"
            spec major-d pitch tap-drill)))
(format t "~%")

;; Show some UNC threads
(format t "Common UNC Threads:~%")
(format t "  Thread       Major Ø    Pitch    TPI~%")
(format t "  ----------   -------    -----    ---~%")
(dolist (spec '(:|#6-32| :|#8-32| :|#10-24| :|1/4-20| :|3/8-16| :|1/2-13|))
  (let* ((info (clad.features:get-thread-spec spec))
         (major-d (getf info :major-diameter))
         (pitch (getf info :pitch))
         (tpi (getf info :tpi)))
    (format t "  ~10A   ~5,2Fmm    ~4,2Fmm   ~A~%"
            spec major-d pitch tpi)))
(format t "~%~%")

;;; ============================================================================
;;; Part 2: Simple Threaded Parts
;;; ============================================================================

(format t "PART 2: Simple Threaded Parts~%")
(format t "=============================~%~%")

;; Example 1: Simple threaded shaft
(format t "Example 1: Simple Threaded Shaft~%")
(format t "  Create shaft with M6 external thread~%~%")

(clad.dsl:defpart simple-threaded-shaft
    ((diameter 6.0)
     (length 50.0)
     (thread-length 30.0))
  "Simple shaft with external thread"
  (:body (clad.core:make-cylinder (/ diameter 2.0) length))
  ;; Thread using DSL integration
  (thread :m6 :length thread-length :type :external :position '(0 0 10.0)))

(format t "  (defpart simple-threaded-shaft ...)~%")
(format t "  (thread :m6 :length 30.0 :type :external :position '(0 0 10.0))~%~%")

;; Example 2: Block with threaded hole
(format t "Example 2: Block with Threaded Hole~%")
(format t "  Create block with M8 internal thread~%~%")

(clad.dsl:defpart block-with-threaded-hole
    ((width 30.0)
     (height 20.0))
  "Block with internal threaded hole"
  (:body (clad.core:make-box width width height))
  ;; Drill hole first
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder 3.5 (* height 1.1))
            (/ width 2.0) (/ width 2.0) 0)))
  ;; Add internal thread
  (thread :m8 :length 15.0 :type :internal :position `(,(/ width 2.0) ,(/ width 2.0) 2.5)))

(format t "  (defpart block-with-threaded-hole ...)~%")
(format t "  (thread :m8 :length 15.0 :type :internal ...)~%~%")

;;; ============================================================================
;;; Part 3: Complete Fasteners
;;; ============================================================================

(format t "PART 3: Complete Fasteners~%")
(format t "==========================~%~%")

;; Example 3: Hex bolt
(format t "Example 3: Parametric Hex Bolt~%")

(clad.dsl:defpart hex-bolt-example
    ((thread-spec :m8)
     (shaft-length 50.0)
     (thread-length 30.0))
  "Complete hex bolt with thread"
  (let* ((info (clad.features:get-thread-spec thread-spec))
         (major-d (getf info :major-diameter))
         (head-dia (* major-d 1.5))
         (head-height (* major-d 0.7)))

    ;; Hex head (approximated as cylinder)
    (:body (clad.core:make-cylinder (/ head-dia 2.0) head-height))

    ;; Shaft
    (:on-face :direction :+z :extreme :max
      (:add (clad.core:make-cylinder (/ major-d 2.0) shaft-length)))

    ;; Thread at end of shaft
    (thread thread-spec
            :length thread-length
            :type :external
            :position `(0 0 ,(+ head-height (- shaft-length thread-length))))))

(format t "  Usage: (hex-bolt-example :m8 50.0 30.0)~%")
(format t "         (hex-bolt-example :m10 60.0 40.0)~%~%")

;; Example 4: Hex nut
(format t "Example 4: Parametric Hex Nut~%")

(clad.dsl:defpart hex-nut-example
    ((thread-spec :m8)
     (height 6.5)
     (wrench-size 13.0))
  "Complete hex nut with internal thread"
  (let* ((info (clad.features:get-thread-spec thread-spec))
         (major-d (getf info :major-diameter)))

    ;; Hex outer (approximated as cylinder)
    (:body (clad.core:make-cylinder (/ wrench-size 2.0) height))

    ;; Center hole
    (:on-face :direction :+z :extreme :max
      (:cut (clad.core:make-cylinder (/ major-d 2.0) (* height 1.1))))

    ;; Internal thread
    (thread thread-spec :length height :type :internal)))

(format t "  Usage: (hex-nut-example :m8 6.5 13.0)~%")
(format t "         (hex-nut-example :m10 8.0 17.0)~%~%")

;;; ============================================================================
;;; Part 4: Advanced Thread Features
;;; ============================================================================

(format t "PART 4: Advanced Thread Features~%")
(format t "=================================~%~%")

;; Example 5: Left-handed thread
(format t "Example 5: Left-Handed Thread~%")
(format t "  For reverse-threaded applications~%~%")

(clad.dsl:defpart left-handed-shaft ()
  "Shaft with left-handed thread"
  (:body (clad.core:make-cylinder 4.0 50.0))
  (thread :m8 :length 30.0 :type :external
          :position '(0 0 10.0)
          :handedness :left))

(format t "  (thread :m8 ... :handedness :left)~%~%")

;; Example 6: Thread with lead-in/lead-out
(format t "Example 6: Thread with Lead-In/Lead-Out~%")
(format t "  Smooth thread engagement for easier assembly~%~%")

(clad.dsl:defpart smooth-engagement-bolt ()
  "Bolt with gradual thread engagement"
  (:body (clad.core:make-cylinder 3.0 50.0))
  (thread :m6 :length 30.0 :type :external
          :position '(0 0 10.0)
          :lead-in 0.5
          :lead-out 0.5))

(format t "  (thread :m6 ... :lead-in 0.5 :lead-out 0.5)~%~%")

;; Example 7: Multiple threads on one part
(format t "Example 7: Dual-Threaded Shaft~%")
(format t "  Different thread sizes on one shaft~%~%")

(clad.dsl:defpart dual-threaded-shaft ()
  "Shaft with two different thread sections"
  (:body (clad.core:make-cylinder 6.0 120.0))
  ;; First thread (M8)
  (thread :m8 :length 25.0 :type :external :position '(0 0 10.0))
  ;; Second thread (M10)
  (thread :m10 :length 30.0 :type :external :position '(0 0 70.0)))

(format t "  Two thread sections: M8 at 10mm, M10 at 70mm~%~%")

;;; ============================================================================
;;; Part 5: Imperial Threads (UNC/UNF)
;;; ============================================================================

(format t "PART 5: Imperial Threads (UNC/UNF)~%")
(format t "===================================~%~%")

;; Example 8: UNC threaded shaft
(format t "Example 8: UNC Thread (1/4-20)~%")

(clad.dsl:defpart unc-threaded-shaft ()
  "Shaft with 1/4-20 UNC thread"
  (let* ((info (clad.features:get-thread-spec :|1/4-20|))
         (major-d (getf info :major-diameter)))
    (:body (clad.core:make-cylinder (/ major-d 2.0) 50.0))
    (thread :|1/4-20| :length 30.0 :type :external :position '(0 0 10.0))))

(format t "  (thread :|1/4-20| :length 30.0 :type :external ...)~%~%")

;; Example 9: UNF threaded shaft
(format t "Example 9: UNF Thread (1/4-28)~%")

(clad.dsl:defpart unf-threaded-shaft ()
  "Shaft with 1/4-28 UNF thread"
  (let* ((info (clad.features:get-thread-spec :|1/4-28|))
         (major-d (getf info :major-diameter)))
    (:body (clad.core:make-cylinder (/ major-d 2.0) 50.0))
    (thread :|1/4-28| :length 30.0 :type :external :position '(0 0 10.0))))

(format t "  (thread :|1/4-28| :length 30.0 :type :external ...)~%~%")

;;; ============================================================================
;;; Part 6: Thread Calculations
;;; ============================================================================

(format t "PART 6: Thread Calculations~%")
(format t "===========================~%~%")

(format t "Tap drill sizes:~%")
(dolist (spec '(:m6 :m8 :m10 :m12))
  (format t "  ~A: ~,1Fmm tap drill~%"
          spec (clad.features:tap-drill-size spec)))
(format t "~%")

(format t "Minor diameters:~%")
(dolist (spec '(:m6 :m8 :m10 :m12))
  (format t "  ~A: ~,2Fmm minor diameter~%"
          spec (clad.features:thread-minor-diameter spec)))
(format t "~%")

(format t "Thread designations:~%")
(dolist (spec '(:m6 :m8 :m8x1.0 :|1/4-20| :|1/4-28|))
  (format t "  ~A: ~A~%"
          spec (clad.features:thread-designation-string spec)))
(format t "~%~%")

;;; ============================================================================
;;; Part 7: Low-Level API (for advanced users)
;;; ============================================================================

(format t "PART 7: Low-Level Thread API~%")
(format t "=============================~%~%")

(format t "For advanced control, use the low-level API:~%~%")

(format t "  ;; Create thread profile (Phase 1)~%")
(format t "  (clad.features.thread-profile:make-iso-metric-profile :m6 :external)~%~%")

(format t "  ;; Create helical path (Phase 2)~%")
(format t "  (clad.features.helical-path:make-helix-for-thread~%")
(format t "    :thread-spec :m6 :length 30.0 :right-handed t)~%~%")

(format t "  ;; Sweep profile along helix (Phase 3)~%")
(format t "  (clad.features.helical-sweep:make-external-thread :m6 30.0)~%")
(format t "  (clad.features.helical-sweep:make-internal-thread :m8 25.0)~%~%")

(format t "  ;; Apply to part with boolean ops (Phase 4)~%")
(format t "  (clad.features.thread-boolean:apply-external-thread~%")
(format t "    shaft-shape thread-geom :position '(0 0 10.0))~%~%")

;;; ============================================================================
;;; Part 8: Thread Fit Checking
;;; ============================================================================

(format t "PART 8: Thread Fit Checking~%")
(format t "===========================~%~%")

(format t "Check compatibility between bolt and nut threads:~%~%")

(format t "  ;; Create matching threads~%")
(format t "  (let ((bolt (clad.features.helical-sweep:make-external-thread :m8 20.0))~%")
(format t "        (nut (clad.features.helical-sweep:make-internal-thread :m8 15.0)))~%")
(format t "    (clad.features.thread-boolean:check-thread-fit bolt nut))~%")
(format t "  => :good-fit (or :perfect-fit, :size-mismatch, :length-mismatch)~%~%")

(format t "  ;; Calculate engagement length~%")
(format t "  (clad.features.thread-boolean:calculate-engagement-length bolt nut)~%")
(format t "  => 15.0  ; mm (shorter of the two thread lengths)~%~%")

;;; ============================================================================
;;; Interactive Viewer
;;; ============================================================================

(defun view-thread-examples ()
  "View all thread examples in the web viewer"
  (format t "~%Starting CLAD web viewer...~%")
  (clad:start-viewer)

  (format t "Loading thread examples...~%")
  (clad:view (simple-threaded-shaft) :name "01-simple-shaft")
  (clad:view (block-with-threaded-hole) :name "02-threaded-hole")
  (clad:view (hex-bolt-example :m8 50.0 30.0) :name "03-hex-bolt-m8")
  (clad:view (hex-nut-example :m8 6.5 13.0) :name "04-hex-nut-m8")
  (clad:view (left-handed-shaft) :name "05-left-handed")
  (clad:view (smooth-engagement-bolt) :name "06-smooth-engagement")
  (clad:view (dual-threaded-shaft) :name "07-dual-threads")
  (clad:view (unc-threaded-shaft) :name "08-unc-1-4-20")
  (clad:view (unf-threaded-shaft) :name "09-unf-1-4-28")

  (format t "~%All examples loaded! Open http://localhost:8080~%"))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(format t "================================================================================~%")
(format t "                           Thread Tutorial Summary                             ~%")
(format t "================================================================================~%~%")

(format t "Thread DSL Syntax:~%")
(format t "  (thread <spec> :length <mm> :type :external|:internal~%")
(format t "          [:position (x y z)]~%")
(format t "          [:handedness :right|:left]~%")
(format t "          [:lead-in <turns>] [:lead-out <turns>])~%~%")

(format t "Available Thread Standards (119 total):~%")
(format t "  ISO Metric Coarse:  M1.6 to M64 (30 sizes)~%")
(format t "  ISO Metric Fine:    M3x0.35 to M30x2.0 (17 sizes)~%")
(format t "  UNC:                #0-80 to 2-4.5 (26 sizes)~%")
(format t "  UNF:                #0-80 to 1-1/2-12 (23 sizes)~%~%")

(format t "Key Functions:~%")
(format t "  (clad.features:get-thread-spec :m8)~%")
(format t "  (clad.features:tap-drill-size :m8)~%")
(format t "  (clad.features:thread-minor-diameter :m8)~%")
(format t "  (clad.features:list-threads-by-standard \"UNC\")~%")
(format t "  (clad.features:print-thread-database \"ISO Metric\")~%~%")

(format t "To view examples: (view-thread-examples)~%~%")

(format t "================================================================================~%~%")
