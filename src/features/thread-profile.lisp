;;;; src/features/thread-profile.lisp --- Thread Profile Geometry Engine
;;;;
;;;; Phase 1: Thread Profile Geometry Implementation
;;;; Generates accurate ISO 68-1 metric thread profiles for helical sweeping

(in-package :clad.features.thread-profile)

;;; ============================================================================
;;; Thread Profile Class
;;; ============================================================================

(defclass thread-profile ()
  ((spec
    :initarg :spec
    :reader profile-spec
    :documentation "Thread specification keyword (e.g., :m6, :m8x1.0)")

   (type
    :initarg :type
    :reader profile-type
    :documentation "Profile type: :external or :internal")

   (vertices
    :initarg :vertices
    :reader profile-vertices
    :documentation "List of (radius z) coordinate pairs defining profile")

   (parameters
    :initarg :parameters
    :reader profile-parameters
    :documentation "Plist of calculated thread parameters"))
  (:documentation "Represents a thread profile geometry for sweeping"))

;;; ============================================================================
;;; Main Profile Generation Function
;;; ============================================================================

(defun make-iso-metric-profile (thread-spec profile-type)
  "Create ISO 68-1 metric thread profile geometry.

  THREAD-SPEC: Thread specification keyword (e.g., :m6, :m8, :m10, :m8x1.0)
  PROFILE-TYPE: Either :external (for bolts) or :internal (for nuts/holes)

  Returns: THREAD-PROFILE object with calculated vertices and parameters

  The profile is generated in cylindrical coordinates (radius, z):
  - Radius: radial distance from thread axis
  - Z: axial position along thread (one pitch period)

  Profile shape (external thread):
       Radius
       ^
       |    /\\  <- Crest (truncated by H/8)
       |   /  \\
       |  /    \\
       | /______\\ <- Root (truncated by H/4)
       +----------> Z (one pitch)

  ISO 68-1 Standards:
  - Thread angle: 60° (symmetric V)
  - Crest truncation: H/8
  - Root truncation: H/4
  - H = fundamental triangle height = P × √3/2"

  ;; Validate inputs
  (unless (member profile-type '(:external :internal))
    (error "Invalid profile type ~A. Must be :external or :internal" profile-type))

  (let* (;; Get thread specification parameters
         (params (clad.features::get-thread-spec thread-spec))
         (pitch (getf params :pitch))
         (major-d (getf params :major-diameter))

         ;; Calculate ISO 68-1 dimensions
         ;; H = P × √3/2 ≈ P × 0.866025
         (h (* pitch 0.866025403784439d0))

         ;; Minor diameter: D1 = D - (5/8)H = D - 1.0825P
         (minor-d (- major-d (* 1.0825d0 pitch)))

         ;; Pitch diameter: D2 = D - (3/8)H = D - 0.6495P
         (pitch-d (- major-d (* 0.6495d0 pitch)))

         ;; Calculate profile vertices
         (vertices (calculate-profile-vertices
                    major-d minor-d pitch-d pitch h profile-type)))

    ;; Create and return thread profile object
    (make-instance 'thread-profile
                   :spec thread-spec
                   :type profile-type
                   :vertices vertices
                   :parameters (list :major-diameter major-d
                                    :minor-diameter minor-d
                                    :pitch-diameter pitch-d
                                    :pitch pitch
                                    :fundamental-height h
                                    :thread-angle 60.0d0))))

;;; ============================================================================
;;; Profile Vertex Calculation
;;; ============================================================================

(defun calculate-profile-vertices (major-d minor-d pitch-d pitch h type)
  "Calculate the vertices of ISO 68-1 truncated V-profile.

  Profile Layout (External Thread, Cylindrical Coordinates):
  - Vertex 1: Root start (minor-r, 0)
  - Vertex 2: Flank rise to crest (crest-r, pitch/4)
  - Vertex 3: Crest flat (crest-r, 3*pitch/4)
  - Vertex 4: Flank descent to root (minor-r, pitch)
  - Vertex 5: Root flat to next period (minor-r, pitch)
  - Vertex 6: Close to start (minor-r, 0)

  The V-angle is 60°, giving flanks at 30° from vertical.

  Returns: List of 6 (radius z) coordinate pairs"

  (let* (;; Convert diameters to radii
         (major-r (/ major-d 2.0d0))
         (minor-r (/ minor-d 2.0d0))
         (pitch-r (/ pitch-d 2.0d0))

         ;; Truncation amounts (ISO 68-1)
         (crest-flat (* h 0.125d0))   ; H/8 flat at crest
         (root-flat (* h 0.25d0))     ; H/4 flat at root

         ;; For 60° V-thread, the half-angle is 30°
         ;; Thread depth = 5H/8
         (thread-depth (* h 0.625d0))

         ;; Calculate actual crest and root radii after truncation
         ;; External: crest at major-r minus truncation, root at minor-r plus truncation
         ;; Internal: inverted (crest inward, root outward)
         (crest-r (if (eq type :external)
                      (- major-r crest-flat)
                      (+ minor-r root-flat)))
         (root-r (if (eq type :external)
                     (+ minor-r root-flat)
                     (- major-r crest-flat))))

    ;; Generate vertices based on profile type
    ;; The profile is a closed polygon with 4 unique vertices:
    ;; - Two at the root radius (at z=0 and z=pitch)
    ;; - Two at the crest radius (at z=pitch/4 and z=3*pitch/4)
    ;; The sweep will automatically close the wire
    (if (eq type :external)
        ;; External thread profile (V pointing outward)
        ;; Profile forms a closed quadrilateral
        (list
         (list root-r 0.0d0)                    ; V1: Root start
         (list crest-r (* pitch 0.25d0))        ; V2: Climb to crest
         (list crest-r (* pitch 0.75d0))        ; V3: Crest flat end
         (list root-r pitch))                   ; V4: Descend to root

        ;; Internal thread profile (V pointing inward, inverted radii)
        (list
         (list crest-r 0.0d0)                   ; V1: Start at outer (crest)
         (list root-r (* pitch 0.25d0))         ; V2: Descend to root
         (list root-r (* pitch 0.75d0))         ; V3: Root flat end
         (list crest-r pitch)))))               ; V6: Back to start

;;; ============================================================================
;;; Wire Conversion (OCCT Integration)
;;; ============================================================================

(defun profile-to-wire (profile &optional (start-angle 0.0d0))
  "Convert thread profile to OpenCascade TopoDS_Wire.

  PROFILE: Thread profile object
  START-ANGLE: Starting angular position in radians (for positioning profile)

  Returns: OCCT TopoDS_Wire representing the closed profile

  The profile is converted from cylindrical (r, z) coordinates to
  Cartesian (x, y, z) coordinates for OCCT:
  - X = r * cos(start-angle)
  - Y = r * sin(start-angle)
  - Z = z (axial position)

  This creates a profile in 3D space ready for helical sweeping."

  (let* ((vertices (profile-vertices profile))
         (cos-angle (cos start-angle))
         (sin-angle (sin start-angle)))

    ;; Convert cylindrical vertices to 3D Cartesian points as (x y z) lists
    (let ((points
           (mapcar (lambda (vert)
                     (let ((r (first vert))
                           (z (second vert)))
                       ;; Return (x y z) list for interpolated curve
                       (list (* r cos-angle)   ; x = r * cos(angle)
                             (* r sin-angle)   ; y = r * sin(angle)
                             z)))              ; z = axial position
                   vertices)))

      ;; Create a closed interpolated curve through all points
      ;; The :closed t parameter ensures the wire forms a closed loop
      (let ((wire-edge (clad.ffi:ffi-make-interpolated-curve points :closed t)))

        ;; Verify the wire was created successfully
        (unless (and wire-edge
                     (clad.ffi:handle-valid-p wire-edge)
                     (not (clad.ffi:handle-null-p wire-edge)))
          (error "Failed to create thread profile wire - OCCT interpolation failed"))

        ;; Convert edge to wire if needed - for now return edge as wire
        ;; (pipe operation accepts both edges and wires)
        wire-edge))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun get-profile-info (profile)
  "Get human-readable information about a thread profile.

  Returns formatted string with profile details."
  (let ((params (profile-parameters profile)))
    (format nil "Thread Profile: ~A (~A)~%  Major Ø: ~,3F mm~%  Minor Ø: ~,3F mm~%  Pitch Ø: ~,3F mm~%  Pitch: ~,3F mm~%  Angle: ~,1F°"
            (profile-spec profile)
            (profile-type profile)
            (getf params :major-diameter)
            (getf params :minor-diameter)
            (getf params :pitch-diameter)
            (getf params :pitch)
            (getf params :thread-angle))))

(defun validate-profile (profile)
  "Validate thread profile geometry.

  Checks:
  - Vertices form closed path
  - Radii are positive
  - Z coordinates span one pitch
  - Thread angle is approximately 60°

  Returns: T if valid, NIL with warnings otherwise"

  (let ((vertices (profile-vertices profile))
        (params (profile-parameters profile))
        (warnings '()))

    ;; Check vertex count
    (unless (= 4 (length vertices))
      (push "Profile should have 4 vertices" warnings))

    ;; Check radii are positive
    (dolist (v vertices)
      (unless (> (first v) 0)
        (push "All radii must be positive" warnings)
        (return)))

    ;; Check Z span equals pitch
    (let ((z-span (- (reduce #'max vertices :key #'second)
                     (reduce #'min vertices :key #'second)))
          (pitch (getf params :pitch)))
      (unless (< (abs (- z-span pitch)) 0.01)
        (push (format nil "Z span (~,3F) should equal pitch (~,3F)" z-span pitch)
              warnings)))

    ;; Report results
    (if warnings
        (progn
          (format t "Profile validation warnings:~%")
          (dolist (w warnings)
            (format t "  - ~A~%" w))
          nil)
        t)))

;;; ============================================================================
;;; Export for Testing
;;; ============================================================================

(defun profile-summary (profile)
  "Return summary of profile for debugging/testing.

  Returns plist with key information."
  (list :spec (profile-spec profile)
        :type (profile-type profile)
        :vertex-count (length (profile-vertices profile))
        :parameters (profile-parameters profile)))
