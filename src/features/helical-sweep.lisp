;;;; src/features/helical-sweep.lisp --- Helical Sweep Operation Engine
;;;;
;;;; Phase 3: Helical Sweep Implementation
;;;; Sweeps thread profiles along helical paths to create 3D thread geometry

(in-package :clad.features.helical-sweep)

;;; ============================================================================
;;; Main Sweep Operation
;;; ============================================================================

(defun sweep-profile-along-helix (profile helix &key (orientation :frenet))
  "Sweep a thread profile along a helical path to create 3D thread geometry.

  PROFILE: Thread profile object (from Phase 1) containing (r, z) vertices
  HELIX: Helical path object (from Phase 2) or TopoDS_Edge
  ORIENTATION: Sweep orientation mode (currently not used - FFI uses Frenet)
    :frenet - Profile stays perpendicular to path (default, recommended)
    :fixed  - Profile maintains fixed orientation (can cause twisting)

  Returns: OCCT TopoDS_Shape (solid thread geometry)

  Technical Details:
  - Uses BRepOffsetAPI_MakePipeShell with Frenet frame for proper orientation
  - Profile is automatically positioned at helix start point
  - Profile is automatically oriented perpendicular to path tangent
  - Sweep follows the helical path maintaining profile shape

  The Frenet frame is a moving coordinate system that follows the curve:
  - T (tangent): direction of curve at each point
  - N (normal): direction of curvature
  - B (binormal): T × N (perpendicular to both)

  This ensures the profile stays perpendicular to the helix throughout
  the sweep, creating smooth, non-twisted thread geometry."
  ;; Note: orientation parameter is reserved for future FFI enhancements
  (declare (ignore orientation))

  ;; Validate inputs
  (when (null profile)
    (error "Profile cannot be nil"))
  (when (null helix)
    (error "Helix cannot be nil"))

  ;; Extract profile vertices (r, z pairs) and helix edge
  (let ((profile-vertices (extract-profile-vertices profile))
        (helix-edge (extract-helix-edge helix)))

    ;; Validate profile vertices
    (unless (and profile-vertices (>= (length profile-vertices) 3))
      (error "Profile must have at least 3 vertices"))

    ;; Validate helix edge
    (unless (and helix-edge
                 (clad.ffi:handle-valid-p helix-edge)
                 (not (clad.ffi:handle-null-p helix-edge)))
      (error "Helix edge is not a valid OCCT shape"))

    ;; Perform sweep operation using specialized helical sweep
    ;; This handles proper profile positioning and orientation automatically
    (clad.ffi:ffi-make-helical-sweep profile-vertices helix-edge)))

;;; ============================================================================
;;; High-Level Thread Geometry Creation
;;; ============================================================================

(defun make-thread-geometry (&key thread-spec length (profile-type :external)
                                  (right-handed t) (orientation :frenet))
  "Create complete 3D thread geometry from specification.

  THREAD-SPEC: Thread specification keyword (e.g., :m6, :m8, :m10)
  LENGTH: Total length of threaded section (in mm)
  PROFILE-TYPE: :external (for bolts) or :internal (for nuts/holes)
  RIGHT-HANDED: T for right-handed, NIL for left-handed threads
  ORIENTATION: Sweep orientation (:frenet or :fixed)

  Returns: OCCT TopoDS_Shape (solid thread geometry)

  This is the high-level API that combines all three phases:
  1. Creates thread profile (Phase 1)
  2. Creates helical path (Phase 2)
  3. Sweeps profile along path (Phase 3)

  Example Usage:
    (make-thread-geometry :thread-spec :m6
                         :length 30.0
                         :profile-type :external
                         :right-handed t)
    => Complete M6 external thread, 30mm long, right-handed"

  ;; Step 1: Create thread profile (Phase 1)
  (let ((profile (clad.features.thread-profile:make-iso-metric-profile
                  thread-spec profile-type)))

    ;; Step 2: Create helical path at pitch diameter (Phase 2)
    (let ((helix (clad.features.helical-path:make-helix-for-thread
                  :thread-spec thread-spec
                  :length length
                  :right-handed right-handed)))

      ;; Step 3: Sweep profile along helix (Phase 3)
      (sweep-profile-along-helix profile helix
                                :orientation orientation))))

;;; ============================================================================
;;; Thread Geometry Utilities
;;; ============================================================================

(defun make-external-thread (thread-spec length &key (right-handed t))
  "Create an external thread (bolt thread).

  Convenience function for external thread creation.

  THREAD-SPEC: Thread specification (e.g., :m6, :m8, :m10)
  LENGTH: Thread length in mm
  RIGHT-HANDED: T for RH (default), NIL for LH

  Returns: OCCT TopoDS_Shape (external thread solid)"

  (make-thread-geometry :thread-spec thread-spec
                       :length length
                       :profile-type :external
                       :right-handed right-handed))

(defun make-internal-thread (thread-spec length &key (right-handed t))
  "Create an internal thread (nut/hole thread).

  Convenience function for internal thread creation.

  THREAD-SPEC: Thread specification (e.g., :m6, :m8, :m10)
  LENGTH: Thread length in mm
  RIGHT-HANDED: T for RH (default), NIL for LH

  Returns: OCCT TopoDS_Shape (internal thread solid)"

  (make-thread-geometry :thread-spec thread-spec
                       :length length
                       :profile-type :internal
                       :right-handed right-handed))

;;; ============================================================================
;;; Thread with Lead-In/Lead-Out
;;; ============================================================================

(defun make-thread-with-lead (&key thread-spec length profile-type
                                   (right-handed t)
                                   (lead-in-turns 0.5)
                                   (lead-out-turns 0.5))
  "Create thread geometry with gradual lead-in and lead-out sections.

  Lead-in and lead-out provide smooth thread engagement/disengagement,
  making the thread easier to start and reducing cross-threading risk.

  LEAD-IN-TURNS: Number of turns for gradual start (default 0.5)
  LEAD-OUT-TURNS: Number of turns for gradual end (default 0.5)

  Returns: OCCT TopoDS_Shape (thread with smooth lead sections)"

  ;; Create profile
  (let ((profile (clad.features.thread-profile:make-iso-metric-profile
                  thread-spec profile-type)))

    ;; Create helix with lead-in/lead-out
    (let* ((spec-params (clad.features::get-thread-spec thread-spec))
           (pitch (getf spec-params :pitch))
           (pitch-radius (/ (getf spec-params :pitch-diameter) 2.0d0))
           (helix (clad.features.helical-path:make-helix-with-lead
                   :pitch pitch
                   :radius pitch-radius
                   :height length
                   :right-handed right-handed
                   :lead-in-turns lead-in-turns
                   :lead-out-turns lead-out-turns)))

      ;; Sweep profile along helix with lead
      (sweep-profile-along-helix profile helix))))

;;; ============================================================================
;;; Thread Geometry Analysis
;;; ============================================================================

(defun get-thread-info (thread-shape)
  "Get information about thread geometry.

  THREAD-SHAPE: Thread solid created by sweep operation

  Returns: Plist with thread properties"

  (multiple-value-bind (xmin ymin zmin xmax ymax zmax)
      (clad.ffi:ffi-get-bounding-box thread-shape)
    (let ((volume (clad.ffi:ffi-get-volume thread-shape))
          (surface-area (clad.ffi:ffi-get-area thread-shape)))

      (list :bounding-box (list xmin ymin zmin xmax ymax zmax)
            :volume volume
            :surface-area surface-area
            :height (- zmax zmin)
            :diameter (max (- xmax xmin) (- ymax ymin))
            :is-valid (clad.ffi:is-valid-shape thread-shape)
            :is-closed nil))))  ; Note: proper implementation requires FFI binding

(defun validate-thread-geometry (thread-shape expected-length expected-diameter
                                              &key (tolerance 1.0))
  "Validate thread geometry against expected dimensions.

  THREAD-SHAPE: Thread solid to validate
  EXPECTED-LENGTH: Expected thread length (mm)
  EXPECTED-DIAMETER: Expected major diameter (mm)
  TOLERANCE: Dimensional tolerance (mm, default 1.0)

  Returns: T if valid, NIL with warnings otherwise"

  (let ((info (get-thread-info thread-shape))
        (warnings '()))

    ;; Check if shape is valid
    (unless (getf info :is-valid)
      (push "Thread shape is not a valid OCCT shape" warnings))

    ;; Check if shape is closed (watertight)
    (unless (getf info :is-closed)
      (push "Thread shape is not closed (not watertight)" warnings))

    ;; Check length
    (let ((actual-length (getf info :height)))
      (unless (< (abs (- actual-length expected-length)) tolerance)
        (push (format nil "Thread length (~,2F mm) differs from expected (~,2F mm)"
                     actual-length expected-length)
              warnings)))

    ;; Check diameter
    (let ((actual-diameter (getf info :diameter)))
      (unless (< (abs (- actual-diameter expected-diameter)) tolerance)
        (push (format nil "Thread diameter (~,2F mm) differs from expected (~,2F mm)"
                     actual-diameter expected-diameter)
              warnings)))

    ;; Report results
    (if warnings
        (progn
          (format t "Thread validation warnings:~%")
          (dolist (w warnings)
            (format t "  - ~A~%" w))
          nil)
        t)))

;;; ============================================================================
;;; Internal Helper Functions
;;; ============================================================================

(defun extract-profile-vertices (profile)
  "Extract (r, z) vertex pairs from profile object.

  PROFILE: Thread-profile object containing vertices in cylindrical coordinates

  Returns: List of (r z) pairs"

  (if (typep profile 'clad.features.thread-profile:thread-profile)
      ;; It's a profile object, get vertices directly
      (clad.features.thread-profile:profile-vertices profile)
      ;; Assume it's already a list of vertices
      profile))

(defun extract-helix-edge (helix)
  "Extract TopoDS_Edge from helix object or pass through if already an edge.

  HELIX: Either a helix-curve object or TopoDS_Edge

  Returns: TopoDS_Edge"

  (if (typep helix 'clad.features.helical-path:helix-curve)
      ;; It's a helix object, extract edge
      (clad.features.helical-path:get-helix-edge helix)
      ;; Assume it's already an edge
      helix))

;;; ============================================================================
;;; Boolean Operations for Thread Application
;;; ============================================================================

(defun apply-external-thread-to-cylinder (cylinder thread-shape)
  "Apply external thread to a cylindrical shaft.

  CYLINDER: Base cylinder shape
  THREAD-SHAPE: Thread geometry from make-thread-geometry

  Returns: OCCT TopoDS_Shape (cylinder with thread)

  The thread is intersected with the cylinder to create the final
  threaded shaft. This ensures the thread core matches the shaft diameter."

  ;; Intersect thread with cylinder
  (clad.ffi:ffi-intersect cylinder thread-shape))

(defun apply-internal-thread-to-hole (hole thread-shape)
  "Apply internal thread to a cylindrical hole.

  HOLE: Base cylinder representing the hole
  THREAD-SHAPE: Internal thread geometry

  Returns: OCCT TopoDS_Shape (hole with internal thread)

  The thread is subtracted from the hole to create the threaded hole.
  This cuts the thread profile into the hole walls."

  ;; Cut thread from hole
  (clad.ffi:ffi-cut hole thread-shape))

;;; ============================================================================
;;; Export for Testing
;;; ============================================================================

(defun thread-summary (thread-shape)
  "Return summary of thread geometry for debugging/testing.

  Returns plist with key information."

  (get-thread-info thread-shape))
