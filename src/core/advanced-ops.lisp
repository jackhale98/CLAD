;;;; core/advanced-ops.lisp --- Core API for advanced operations

(in-package :clad.core)

;;; ============================================================================
;;; Sweep Operations
;;; ============================================================================

(defun make-sweep (profile path)
  "Sweep a profile shape along a path.

   PROFILE can be a wire or face shape to sweep.
   PATH should be a wire or edge defining the sweep path.

   Returns a new shape (typically a solid or shell).

   Example:
     (let* ((circle (make-circle-wire '(0 0 0) 5))
            (path (make-spline '((0 0 0) (10 10 0) (20 5 0))))
            (tube (make-sweep circle path)))
       tube)"
  (ensure-shape profile)
  (ensure-shape path)
  (let ((handle (clad.ffi:ffi-make-pipe (shape-handle profile)
                                         (shape-handle path))))
    (make-shape handle
                :metadata (list :operation :sweep
                               :profile profile
                               :path path))))

(defun make-pipe (path radius)
  "Sweep a circular profile of given radius along a path.

   PATH should be a wire or edge defining the sweep path.
   RADIUS is the radius of the circular profile to sweep.

   Returns a new solid shape (tube/pipe).

   Example:
     (let* ((path (make-spline '((0 0 0) (10 0 10) (20 0 5))))
            (pipe (make-pipe path 2)))
       pipe)"
  (ensure-shape path)
  (when (<= radius 0)
    (error "Pipe radius must be positive, got ~A" radius))
  (let ((handle (clad.ffi:ffi-make-pipe-shell (shape-handle path) radius)))
    (make-shape handle
                :metadata (list :operation :pipe
                               :path path
                               :radius radius))))

;;; ============================================================================
;;; Loft Operations
;;; ============================================================================

(defun make-loft (sections &key (solid t) (ruled nil))
  "Create a loft through multiple cross-section profiles.

   SECTIONS is a list of shapes (wires or faces) defining the cross-sections.
   SOLID t creates a solid, nil creates a shell (default: t).
   RULED t creates ruled surface, nil creates smooth surface (default: nil).

   At least 2 sections are required.

   Returns a new shape (solid or shell depending on SOLID parameter).

   Example:
     (let* ((bottom (make-circle-wire '(0 0 0) 10))
            (middle (make-circle-wire '(0 0 10) 7))
            (top (make-circle-wire '(0 0 20) 3))
            (loft (make-loft (list bottom middle top) :solid t)))
       loft)"
  (when (< (length sections) 2)
    (error "Loft requires at least 2 sections, got ~A" (length sections)))
  (dolist (section sections)
    (ensure-shape section))

  (let* ((section-handles (mapcar #'shape-handle sections))
         (handle (clad.ffi:ffi-make-loft section-handles
                                          :solid solid
                                          :ruled ruled)))
    (make-shape handle
                :metadata (list :operation :loft
                               :sections sections
                               :solid solid
                               :ruled ruled))))

;;; ============================================================================
;;; Shelling Operations
;;; ============================================================================

(defun make-shell (solid faces-to-remove thickness &optional (tolerance 1.0e-6))
  "Create a hollow shell from a solid by removing faces and offsetting.

   SOLID is the solid shape to hollow out.
   FACES-TO-REMOVE is a list of faces to remove (can be empty list or nil).
   THICKNESS is the wall thickness:
     - Negative: offset inward (hollow out)
     - Positive: offset outward (thicken)
   TOLERANCE is the operation tolerance (default: 1.0e-6).

   Returns a new shelled shape.

   Example:
     ;; Create hollow box with top face removed
     (let* ((box (make-box 100 100 100))
            (faces (clad.shapes:faces box))
            (top-face (first faces))  ; Get top face
            (hollow-box (make-shell box (list top-face) -2)))
       hollow-box)"
  (ensure-shape solid)
  (when (zerop thickness)
    (error "Shell thickness cannot be zero"))

  (let* ((face-handles (if faces-to-remove
                           (mapcar #'shape-handle faces-to-remove)
                           nil))
         (handle (clad.ffi:ffi-make-shell (shape-handle solid)
                                           face-handles
                                           thickness
                                           tolerance)))
    (make-shape handle
                :metadata (list :operation :shell
                               :base-solid solid
                               :thickness thickness))))

;;; ============================================================================
;;; Mirroring Operations
;;; ============================================================================

(defun mirror-shape (shape plane-origin plane-normal)
  "Mirror a shape across a plane.

   SHAPE is the shape to mirror.
   PLANE-ORIGIN is (x y z) point on the mirror plane.
   PLANE-NORMAL is (nx ny nz) normal vector of the mirror plane.

   Returns a new mirrored shape.

   Example:
     ;; Mirror across YZ plane (X=0 plane)
     (let* ((box (make-box 10 10 10))
            (mirrored (mirror-shape box '(0 0 0) '(1 0 0))))
       mirrored)

     ;; Mirror across XY plane (Z=0 plane)
     (let* ((cylinder (make-cylinder 5 20))
            (mirrored (mirror-shape cylinder '(0 0 10) '(0 0 1))))
       mirrored)"
  (ensure-shape shape)
  (let ((handle (clad.ffi:ffi-mirror-shape (shape-handle shape)
                                            plane-origin
                                            plane-normal)))
    (make-shape handle
                :metadata (list :operation :mirror
                               :base-shape shape
                               :plane-origin plane-origin
                               :plane-normal plane-normal))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun make-circle-wire (center radius &key (axis '(0 0 1)))
  "Helper function to create a circular wire for sweeping/lofting.

   CENTER is (x y z) center point.
   RADIUS is the circle radius.
   AXIS is (x y z) normal vector (default: Z-axis).

   Returns a wire shape."
  (let ((arc (make-arc center radius 0 360 :axis axis)))
    ;; Arc from 0 to 360 degrees creates a closed wire
    arc))
