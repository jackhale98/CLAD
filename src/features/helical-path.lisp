;;;; src/features/helical-path.lisp --- Helical Path Generation Engine
;;;;
;;;; Phase 2: Helical Path Generation Implementation
;;;; Generates mathematically precise helical curves for thread sweeping

(in-package :clad.features.helical-path)

;;; ============================================================================
;;; Helix Class (for storing helix metadata)
;;; ============================================================================

(defclass helix-curve ()
  ((edge
    :initarg :edge
    :reader helix-edge
    :documentation "OCCT TopoDS_Edge representing the helical curve")

   (parameters
    :initarg :parameters
    :reader helix-parameters
    :documentation "Plist of helix parameters (pitch, radius, height, etc.)"))
  (:documentation "Wrapper for helical curve with associated parameters"))

;;; ============================================================================
;;; Main Helix Generation Function
;;; ============================================================================

(defun make-helix (&key pitch radius height (right-handed t) (num-points 200))
  "Create a parametric helical curve using B-spline.

  PITCH: Rise per complete revolution (in mm)
  RADIUS: Constant radius of the helix (in mm)
  HEIGHT: Total height of the helix (in mm)
  RIGHT-HANDED: T for right-handed (clockwise from top), NIL for left-handed
  NUM-POINTS: Number of control points for B-spline (default 200 for smoothness)

  Returns: helix-curve object containing OCCT TopoDS_Edge and metadata

  Parametric Helix Equations:
    x(t) = radius × cos(θ(t))
    y(t) = radius × sin(θ(t)) × direction
    z(t) = height × t

  where:
    θ(t) = 2π × t × (height / pitch)  [total angle over height]
    direction = 1 (right-handed) or -1 (left-handed)
    t ∈ [0, 1]

  The helix is represented as a C2-continuous B-spline curve with
  sufficient control points for smooth manufacturing."

  ;; Validate inputs
  (validate-helix-parameters pitch radius height)

  (let* ((num-turns (/ height pitch))
         (total-angle (* 2.0d0 pi num-turns))
         (direction (if right-handed 1.0d0 -1.0d0))
         (points '()))

    ;; Generate control points along helix as (x y z) lists
    (loop for i from 0 to num-points
          for param = (/ (coerce i 'double-float) (coerce num-points 'double-float))
          for theta = (* total-angle param)
          for x = (* radius (cos theta))
          for y = (* radius (sin theta) direction)
          for z = (* height param)
          do (push (list x y z) points))

    ;; Reverse to get correct order
    (setf points (nreverse points))

    ;; Create B-spline curve through points using real FFI function
    ;; ffi-make-interpolated-curve creates edge directly from points
    (let ((edge (clad.ffi:ffi-make-interpolated-curve points :closed nil)))

      ;; Verify the edge was created successfully
      (unless (and edge (clad.ffi:handle-valid-p edge)
                   (not (clad.ffi:handle-null-p edge)))
        (error "Failed to create helical curve - OCCT interpolation failed"))

      ;; Store helix with metadata
      (make-instance 'helix-curve
                     :edge edge
                     :parameters (list :pitch pitch
                                      :radius radius
                                      :height height
                                      :handedness (if right-handed :right-handed :left-handed)
                                      :turns num-turns
                                      :num-control-points (1+ num-points))))))

;;; ============================================================================
;;; Thread-Specific Helix Generation
;;; ============================================================================

(defun make-helix-for-thread (&key thread-spec length (right-handed t) (num-points 200))
  "Create a helical path for a specific thread specification.

  THREAD-SPEC: Thread specification keyword (e.g., :m6, :m8, :m10)
  LENGTH: Total length of threaded section (in mm)
  RIGHT-HANDED: T for right-handed thread, NIL for left-handed
  NUM-POINTS: Number of control points for B-spline

  Returns: OCCT TopoDS_Edge representing the helical curve at pitch diameter

  The helix is positioned at the pitch diameter radius (where thread
  profile will be swept). This is the effective diameter where the
  thread profile's pitch line crosses the thread flank."

  ;; Get thread specification
  (let* ((spec (clad.features::get-thread-spec thread-spec))
         (pitch (getf spec :pitch))
         (pitch-diameter (getf spec :pitch-diameter))
         (pitch-radius (/ pitch-diameter 2.0d0)))

    ;; Create helix at pitch radius
    (make-helix :pitch pitch
                :radius pitch-radius
                :height length
                :right-handed right-handed
                :num-points num-points)))

;;; ============================================================================
;;; Helix Information and Validation
;;; ============================================================================

(defun get-helix-info (helix-or-edge)
  "Get information about a helix curve.

  HELIX-OR-EDGE: Either a helix-curve object or an OCCT edge

  Returns: Plist with helix properties"

  (if (typep helix-or-edge 'helix-curve)
      ;; Return stored parameters
      (helix-parameters helix-or-edge)

      ;; For raw edge, extract basic geometric properties
      (let* ((start-pt (clad.ffi::get-curve-start-point helix-or-edge))
             (end-pt (clad.ffi::get-curve-end-point helix-or-edge))
             (height (- (clad.ffi::point-z end-pt) (clad.ffi::point-z start-pt)))
             (radius (sqrt (+ (expt (clad.ffi::point-x start-pt) 2)
                             (expt (clad.ffi::point-y start-pt) 2)))))

        (list :radius radius
              :height height
              :handedness :unknown))))

(defun validate-helix-parameters (pitch radius height)
  "Validate helix parameters before creation.

  Checks:
  - All parameters are positive
  - Height is at least one pitch (at least one turn)

  Signals error if validation fails."

  (unless (and (numberp pitch) (> pitch 0))
    (error "Pitch must be a positive number, got: ~A" pitch))

  (unless (and (numberp radius) (> radius 0))
    (error "Radius must be a positive number, got: ~A" radius))

  (unless (and (numberp height) (> height 0))
    (error "Height must be a positive number, got: ~A" height))

  (when (< height pitch)
    (warn "Helix height (~A) is less than one pitch (~A)" height pitch))

  t)

;;; ============================================================================
;;; Helix Utility Functions
;;; ============================================================================

(defun get-helix-edge (helix)
  "Extract OCCT edge from helix object.

  Returns the underlying TopoDS_Edge that can be used for sweeping."

  (if (typep helix 'helix-curve)
      (helix-edge helix)
      helix)) ; Already an edge

(defun helix-length (helix)
  "Calculate the arc length of a helical curve.

  For a helix with radius r, height h, and n turns:
  Length = √((2πrn)² + h²)"

  (let* ((params (if (typep helix 'helix-curve)
                     (helix-parameters helix)
                     (get-helix-info helix)))
         (radius (getf params :radius))
         (height (getf params :height))
         (turns (getf params :turns 1.0)))

    (sqrt (+ (expt (* 2.0d0 pi radius turns) 2)
             (expt height 2)))))

(defun sample-helix-point (helix parameter)
  "Sample a point along the helix at given parameter (0.0 to 1.0).

  PARAMETER: Position along helix (0.0 = start, 1.0 = end)

  Returns: gp_Pnt at the specified position"

  (let ((edge (get-helix-edge helix)))
    (clad.ffi::evaluate-curve-at edge parameter)))

;;; ============================================================================
;;; Advanced Helix Features (Lead-in/Lead-out)
;;; ============================================================================

(defun make-helix-with-lead (&key pitch radius height (right-handed t)
                                  (lead-in-turns 0.5) (lead-out-turns 0.5)
                                  (num-points 200))
  "Create a helix with gradual lead-in and lead-out sections.

  Lead-in and lead-out provide smooth thread engagement/disengagement.
  The helix starts with a gradual radius increase, maintains constant
  radius in the main section, then gradually decreases.

  LEAD-IN-TURNS: Number of turns for gradual start (default 0.5)
  LEAD-OUT-TURNS: Number of turns for gradual end (default 0.5)

  Returns: OCCT TopoDS_Edge with smooth lead-in/out sections"

  (validate-helix-parameters pitch radius height)

  (let* ((lead-in-height (* lead-in-turns pitch))
         (lead-out-height (* lead-out-turns pitch))
         (main-height (- height lead-in-height lead-out-height))
         (direction (if right-handed 1.0d0 -1.0d0))
         (points '())
         (points-per-section (floor (/ num-points 3))))

    (when (< main-height 0)
      (error "Height (~A) too small for lead-in/out (~A + ~A)"
             height lead-in-height lead-out-height))

    ;; Generate lead-in section (radius grows from 0 to full radius)
    (loop for i from 0 to points-per-section
          for t-local = (/ (coerce i 'double-float) (coerce points-per-section 'double-float))
          for t-global = (* t-local (/ lead-in-height height))
          for r = (* radius t-local) ; Linear ramp
          for theta = (* 2.0d0 pi lead-in-turns t-local)
          for x = (* r (cos theta))
          for y = (* r (sin theta) direction)
          for z = (* lead-in-height t-local)
          do (push (list x y z) points))

    ;; Generate main section (constant radius)
    (let ((main-points (- num-points (* 2 points-per-section)))
          (main-turns (/ main-height pitch)))
      (loop for i from 1 to main-points
            for t-local = (/ (coerce i 'double-float) (coerce main-points 'double-float))
            for theta = (+ (* 2.0d0 pi lead-in-turns)
                          (* 2.0d0 pi main-turns t-local))
            for x = (* radius (cos theta))
            for y = (* radius (sin theta) direction)
            for z = (+ lead-in-height (* main-height t-local))
            do (push (list x y z) points)))

    ;; Generate lead-out section (radius shrinks from full to 0)
    (loop for i from 1 to points-per-section
          for t-local = (/ (coerce i 'double-float) (coerce points-per-section 'double-float))
          for r = (* radius (- 1.0d0 t-local)) ; Linear ramp down
          for theta = (+ (* 2.0d0 pi lead-in-turns)
                        (* 2.0d0 pi (/ main-height pitch))
                        (* 2.0d0 pi lead-out-turns t-local))
          for x = (* r (cos theta))
          for y = (* r (sin theta) direction)
          for z = (+ lead-in-height main-height (* lead-out-height t-local))
          do (push (list x y z) points))

    ;; Reverse and create curve using real FFI function
    (setf points (nreverse points))
    (let ((edge (clad.ffi:ffi-make-interpolated-curve points :closed nil)))
      (unless (and edge (clad.ffi:handle-valid-p edge)
                   (not (clad.ffi:handle-null-p edge)))
        (error "Failed to create helix with lead - OCCT interpolation failed"))
      edge)))

;;; ============================================================================
;;; Export for Testing
;;; ============================================================================

(defun helix-summary (helix)
  "Return summary of helix for debugging/testing.

  Returns plist with key information."

  (let ((params (if (typep helix 'helix-curve)
                    (helix-parameters helix)
                    (get-helix-info helix))))
    (append params
            (list :arc-length (helix-length helix)))))
