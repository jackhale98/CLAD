;;;; examples/08-real-thread-geometry.lisp
;;;;
;;;; Real Thread Geometry Examples using OpenCASCADE
;;;;
;;;; This example demonstrates creating actual 3D thread geometry using
;;;; the OCCT kernel. The threads created are real helical solids that can
;;;; be exported to CAD formats (STEP, STL) and used for visualization,
;;;; simulation, or manufacturing.
;;;;
;;;; The thread geometry system uses four phases:
;;;; - Phase 1: Thread profile generation (ISO 68-1 compliant)
;;;; - Phase 2: Helical path creation (B-spline helix)
;;;; - Phase 3: Helical sweep (profile along path)
;;;; - Phase 4: Boolean operations (apply to parts)

(in-package :cl-user)

(require :asdf)
(asdf:load-system :clad)

(format t "~%~%")
(format t "================================================================================~%")
(format t "              CLAD Real Thread Geometry Examples                               ~%")
(format t "           Using OpenCASCADE Kernel for True 3D Threads                        ~%")
(format t "================================================================================~%~%")

;;; ============================================================================
;;; Example 1: Basic External Thread (Simplest API)
;;; ============================================================================

(format t "Example 1: Basic External Thread~%")
(format t "--------------------------------~%")
(format t "Create a simple M6 external thread using the convenience function.~%~%")

(defun example-1-basic-external-thread ()
  "Create M6 external thread, 15mm long"
  (let ((thread (clad.features.helical-sweep:make-external-thread :m6 15.0)))
    (format t "  Created thread: ~A~%" thread)
    (format t "  Valid shape: ~A~%" (clad.ffi:is-valid-shape thread))

    ;; Get dimensions
    (multiple-value-bind (xmin ymin zmin xmax ymax zmax)
        (clad.ffi:ffi-get-bounding-box thread)
      (format t "  Bounding box: X(~,2F to ~,2F), Y(~,2F to ~,2F), Z(~,2F to ~,2F)~%"
              xmin xmax ymin ymax zmin zmax)
      (format t "  Thread diameter: ~,2Fmm~%" (max (- xmax xmin) (- ymax ymin)))
      (format t "  Thread height: ~,2Fmm~%~%" (- zmax zmin)))

    ;; Export
    (clad.ffi:ffi-export-step thread "/tmp/example-01-m6-external.step")
    (format t "  Exported to: /tmp/example-01-m6-external.step~%~%")
    thread))

(example-1-basic-external-thread)

;;; ============================================================================
;;; Example 2: Basic Internal Thread
;;; ============================================================================

(format t "Example 2: Basic Internal Thread~%")
(format t "--------------------------------~%")
(format t "Create M8 internal thread geometry (for threaded holes).~%~%")

(defun example-2-basic-internal-thread ()
  "Create M8 internal thread, 12mm long"
  (let ((thread (clad.features.helical-sweep:make-internal-thread :m8 12.0)))
    (format t "  Created thread: ~A~%" thread)
    (format t "  Valid shape: ~A~%~%" (clad.ffi:is-valid-shape thread))

    ;; Export
    (clad.ffi:ffi-export-step thread "/tmp/example-02-m8-internal.step")
    (format t "  Exported to: /tmp/example-02-m8-internal.step~%~%")
    thread))

(example-2-basic-internal-thread)

;;; ============================================================================
;;; Example 3: Full Control with make-thread-geometry
;;; ============================================================================

(format t "Example 3: Full Parameter Control~%")
(format t "---------------------------------~%")
(format t "Use make-thread-geometry for complete control over thread parameters.~%~%")

(defun example-3-full-control ()
  "Create thread with full parameter control"
  (let ((thread (clad.features.helical-sweep:make-thread-geometry
                  :thread-spec :m10        ; M10 thread
                  :length 25.0             ; 25mm long
                  :profile-type :external  ; External thread (bolt)
                  :right-handed t)))       ; Right-handed (standard)
    (format t "  M10 external thread, 25mm, right-handed~%")
    (format t "  Valid: ~A~%~%" (clad.ffi:is-valid-shape thread))

    ;; Export
    (clad.ffi:ffi-export-step thread "/tmp/example-03-m10-full-control.step")
    (format t "  Exported to: /tmp/example-03-m10-full-control.step~%~%")
    thread))

(example-3-full-control)

;;; ============================================================================
;;; Example 4: Left-Handed Thread
;;; ============================================================================

(format t "Example 4: Left-Handed Thread~%")
(format t "-----------------------------~%")
(format t "Create a left-handed thread (for special applications).~%~%")

(defun example-4-left-handed ()
  "Create left-handed M8 thread"
  (let ((thread (clad.features.helical-sweep:make-thread-geometry
                  :thread-spec :m8
                  :length 20.0
                  :profile-type :external
                  :right-handed nil)))  ; LEFT-HANDED
    (format t "  M8 left-handed thread created~%")
    (format t "  Valid: ~A~%~%" (clad.ffi:is-valid-shape thread))

    ;; Export
    (clad.ffi:ffi-export-step thread "/tmp/example-04-m8-left-handed.step")
    (format t "  Exported to: /tmp/example-04-m8-left-handed.step~%~%")
    thread))

(example-4-left-handed)

;;; ============================================================================
;;; Example 5: Low-Level API (Maximum Control)
;;; ============================================================================

(format t "Example 5: Low-Level API~%")
(format t "------------------------~%")
(format t "Use the low-level API for complete control over each phase.~%~%")

(defun example-5-low-level-api ()
  "Demonstrate the low-level thread creation API"

  ;; Phase 1: Create thread profile
  (format t "  Phase 1: Creating M6 external profile...~%")
  (let ((profile (clad.features.thread-profile:make-iso-metric-profile
                   :m6 :external)))
    (format t "    Profile vertices: ~A~%"
            (length (clad.features.thread-profile:profile-vertices profile)))
    (format t "    Profile parameters: ~A~%~%"
            (clad.features.thread-profile:profile-parameters profile))

    ;; Phase 2: Create helical path
    (format t "  Phase 2: Creating helical path...~%")
    (let ((helix (clad.features.helical-path:make-helix-for-thread
                   :thread-spec :m6
                   :length 20.0
                   :right-handed t
                   :num-points 200)))  ; High resolution
      (format t "    Helix info: ~A~%~%"
              (clad.features.helical-path:get-helix-info helix))

      ;; Phase 3: Sweep profile along helix
      (format t "  Phase 3: Sweeping profile along helix...~%")
      (let ((thread-shape (clad.features.helical-sweep:sweep-profile-along-helix
                            profile helix)))
        (format t "    Sweep complete!~%")
        (format t "    Valid shape: ~A~%~%" (clad.ffi:is-valid-shape thread-shape))

        ;; Export
        (clad.ffi:ffi-export-step thread-shape "/tmp/example-05-low-level.step")
        (format t "  Exported to: /tmp/example-05-low-level.step~%~%")
        thread-shape))))

(example-5-low-level-api)

;;; ============================================================================
;;; Example 6: Multiple Thread Sizes
;;; ============================================================================

(format t "Example 6: Thread Size Comparison~%")
(format t "---------------------------------~%")
(format t "Create multiple thread sizes for comparison.~%~%")

(defun example-6-multiple-sizes ()
  "Create M3, M6, M10, and M16 threads"
  (let ((threads '()))
    (dolist (spec '(:m3 :m6 :m10 :m16))
      (format t "  Creating ~A thread...~%" spec)
      (let* ((length (case spec (:m3 8.0) (:m6 15.0) (:m10 25.0) (:m16 40.0)))
             (thread (clad.features.helical-sweep:make-external-thread spec length))
             (filename (format nil "/tmp/example-06-~A.step" spec)))
        (clad.ffi:ffi-export-step thread filename)
        (format t "    Exported: ~A~%" filename)
        (push (cons spec thread) threads)))
    (format t "~%  All threads created and exported.~%~%")
    threads))

(example-6-multiple-sizes)

;;; ============================================================================
;;; Example 7: Thread Information and Analysis
;;; ============================================================================

(format t "Example 7: Thread Information~%")
(format t "-----------------------------~%")
(format t "Query thread specifications from the database.~%~%")

(defun example-7-thread-info ()
  "Display thread specifications"
  (format t "  ISO Metric Thread Specifications:~%")
  (format t "  ~8A  ~8A  ~8A  ~8A  ~10A~%"
          "Thread" "Major D" "Pitch D" "Minor D" "Pitch")
  (format t "  ~A~%" (make-string 50 :initial-element #\-))

  (dolist (spec '(:m3 :m4 :m5 :m6 :m8 :m10 :m12 :m16 :m20))
    (let ((info (clad.features:get-thread-spec spec)))
      (format t "  ~8A  ~7,2Fmm ~7,2Fmm ~7,2Fmm  ~5,2Fmm~%"
              spec
              (getf info :major-diameter)
              (getf info :pitch-diameter)
              (getf info :minor-diameter)
              (getf info :pitch))))
  (format t "~%"))

(example-7-thread-info)

;;; ============================================================================
;;; Example 8: Export to Multiple Formats
;;; ============================================================================

(format t "Example 8: Multi-Format Export~%")
(format t "------------------------------~%")
(format t "Export thread geometry to STEP and STL formats.~%~%")

(defun example-8-multi-format ()
  "Export M8 thread to STEP and STL"
  (let ((thread (clad.features.helical-sweep:make-external-thread :m8 20.0)))

    ;; Export to STEP (CAD interchange)
    (clad.ffi:ffi-export-step thread "/tmp/example-08-m8.step")
    (format t "  STEP: /tmp/example-08-m8.step~%")

    ;; Export to STL (3D printing / mesh)
    (clad.ffi:ffi-export-stl thread "/tmp/example-08-m8.stl"
                             :linear-deflection 0.01
                             :angular-deflection 0.1)
    (format t "  STL:  /tmp/example-08-m8.stl~%~%")
    thread))

(example-8-multi-format)

;;; ============================================================================
;;; Summary
;;; ============================================================================

(format t "================================================================================~%")
(format t "                           Summary                                              ~%")
(format t "================================================================================~%~%")

(format t "Key Functions:~%")
(format t "~%")
(format t "  HIGH-LEVEL (Recommended):~%")
(format t "    (clad.features.helical-sweep:make-external-thread :m6 15.0)~%")
(format t "    (clad.features.helical-sweep:make-internal-thread :m8 12.0)~%")
(format t "    (clad.features.helical-sweep:make-thread-geometry~%")
(format t "      :thread-spec :m10 :length 25.0 :profile-type :external :right-handed t)~%")
(format t "~%")
(format t "  LOW-LEVEL (Full Control):~%")
(format t "    (clad.features.thread-profile:make-iso-metric-profile :m6 :external)~%")
(format t "    (clad.features.helical-path:make-helix-for-thread :thread-spec :m6 :length 20.0)~%")
(format t "    (clad.features.helical-sweep:sweep-profile-along-helix profile helix)~%")
(format t "~%")
(format t "  EXPORT:~%")
(format t "    (clad.ffi:ffi-export-step thread \"/path/to/file.step\")~%")
(format t "    (clad.ffi:ffi-export-stl thread \"/path/to/file.stl\")~%")
(format t "~%")
(format t "Available Thread Sizes:~%")
(format t "  ISO Metric: M1.6, M2, M2.5, M3, M4, M5, M6, M8, M10, M12, M16, M20, M24...~%")
(format t "~%")
(format t "Thread Types:~%")
(format t "  :external - For bolts, studs, screws~%")
(format t "  :internal - For nuts, threaded holes~%")
(format t "~%")
(format t "Handedness:~%")
(format t "  :right-handed t  - Standard (clockwise to tighten)~%")
(format t "  :right-handed nil - Left-handed (counter-clockwise to tighten)~%")
(format t "~%")
(format t "================================================================================~%")
(format t "  All example files exported to /tmp/example-*.step~%")
(format t "  View in any CAD software (FreeCAD, Fusion 360, SolidWorks, etc.)~%")
(format t "================================================================================~%~%")
