;;;; src/workplane/workplane.lisp --- Basic workplane class and operations

(in-package :clad.workplane)

;;; ============================================================================
;;; Workplane Class
;;; ============================================================================

(defclass workplane ()
  ((origin :initarg :origin
           :accessor workplane-origin
           :type list
           :documentation "Origin point (x y z) of the workplane")
   (x-dir :initarg :x-dir
          :accessor workplane-x-dir
          :type list
          :documentation "X-direction unit vector")
   (y-dir :initarg :y-dir
          :accessor workplane-y-dir
          :type list
          :documentation "Y-direction unit vector (computed)")
   (z-dir :initarg :z-dir
          :accessor workplane-z-dir
          :type list
          :documentation "Z-direction unit vector (normal to plane)"))
  (:documentation "Represents a local coordinate system in 3D space.

A workplane defines a right-handed coordinate system with:
- origin: A point in 3D space
- x-dir: Unit vector along the local X axis
- y-dir: Unit vector along the local Y axis (computed as z × x)
- z-dir: Unit vector normal to the plane (local Z axis)

The three direction vectors form an orthonormal basis."))

;;; ============================================================================
;;; Vector Utilities
;;; ============================================================================

(defun normalize-vector (vec)
  "Normalize a 3D vector to unit length.

  Args:
    vec - List of (x y z) coordinates

  Returns: Normalized vector as list (x y z)"
  (let* ((x (first vec))
         (y (second vec))
         (z (third vec))
         (length (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (< length 1e-10)
        (error "Cannot normalize zero-length vector")
        (list (/ x length) (/ y length) (/ z length)))))

(defun cross-product (v1 v2)
  "Compute cross product of two 3D vectors.

  Args:
    v1 - First vector (x y z)
    v2 - Second vector (x y z)

  Returns: Cross product v1 × v2 as list (x y z)"
  (let ((x1 (first v1)) (y1 (second v1)) (z1 (third v1))
        (x2 (first v2)) (y2 (second v2)) (z2 (third v2)))
    (list (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2)))))

(defun dot-product (v1 v2)
  "Compute dot product of two 3D vectors.

  Args:
    v1 - First vector (x y z)
    v2 - Second vector (x y z)

  Returns: Dot product v1 · v2 as scalar"
  (+ (* (first v1) (first v2))
     (* (second v1) (second v2))
     (* (third v1) (third v2))))

(defun orthogonalize-vectors (x-vec z-vec)
  "Ensure X and Z vectors are orthogonal using Gram-Schmidt.

  Args:
    x-vec - Desired X direction (will be preserved)
    z-vec - Desired Z direction (will be adjusted if not orthogonal)

  Returns: Two values - orthogonal X and Z vectors (both normalized)"
  ;; Normalize X vector
  (let* ((x-norm (normalize-vector x-vec))
         ;; Project Z onto X to find component parallel to X
         (proj-coeff (dot-product z-vec x-norm))
         ;; Subtract projection to make Z orthogonal to X
         (z-ortho (list (- (first z-vec) (* proj-coeff (first x-norm)))
                       (- (second z-vec) (* proj-coeff (second x-norm)))
                       (- (third z-vec) (* proj-coeff (third x-norm)))))
         ;; Normalize the orthogonalized Z
         (z-norm (normalize-vector z-ortho)))
    (values x-norm z-norm)))

;;; ============================================================================
;;; Constructor
;;; ============================================================================

(defun make-workplane (&key (origin '(0 0 0))
                            (x-dir '(1 0 0))
                            (z-dir '(0 0 1)))
  "Create a new workplane with the specified origin and directions.

  Args:
    :origin - Origin point (x y z), defaults to (0 0 0)
    :x-dir - X-direction vector, defaults to (1 0 0)
    :z-dir - Z-direction vector (normal), defaults to (0 0 1)

  Returns: New workplane instance

  The X and Z directions will be normalized and orthogonalized.
  The Y direction is computed as Z × X to form a right-handed system.

  Example:
    (make-workplane :origin '(10 0 0)
                    :x-dir '(1 0 0)
                    :z-dir '(0 0 1))"

  ;; Ensure X and Z are orthogonal
  (multiple-value-bind (x-norm z-norm)
      (orthogonalize-vectors x-dir z-dir)

    ;; Compute Y as cross product: Y = Z × X
    (let ((y-norm (cross-product z-norm x-norm)))

      ;; Create the workplane instance
      (make-instance 'workplane
                     :origin origin
                     :x-dir x-norm
                     :y-dir y-norm
                     :z-dir z-norm))))

;;; ============================================================================
;;; Standard Plane Constructors
;;; ============================================================================

(defun xy-plane (&key (origin '(0 0 0)))
  "Create a workplane aligned with the XY plane.

  Args:
    :origin - Origin point, defaults to (0 0 0)

  Returns: Workplane with X=(1 0 0), Y=(0 1 0), Z=(0 0 1)

  This is the default orientation with the normal pointing up (+Z).

  Example:
    (xy-plane :origin '(0 0 10))  ; XY plane at height 10"
  (make-workplane :origin origin
                  :x-dir '(1 0 0)
                  :z-dir '(0 0 1)))

(defun xz-plane (&key (origin '(0 0 0)))
  "Create a workplane aligned with the XZ plane.

  Args:
    :origin - Origin point, defaults to (0 0 0)

  Returns: Workplane with X=(1 0 0), Y=(0 0 1), Z=(0 -1 0)

  The XZ plane is horizontal in CAD terms, with the normal pointing
  backward (-Y direction).

  Example:
    (xz-plane :origin '(0 10 0))  ; XZ plane at Y=10"
  (make-workplane :origin origin
                  :x-dir '(1 0 0)
                  :z-dir '(0 -1 0)))

(defun yz-plane (&key (origin '(0 0 0)))
  "Create a workplane aligned with the YZ plane.

  Args:
    :origin - Origin point, defaults to (0 0 0)

  Returns: Workplane with X=(0 1 0), Y=(0 0 1), Z=(1 0 0)

  The YZ plane is vertical, with the normal pointing right (+X direction).

  Example:
    (yz-plane :origin '(10 0 0))  ; YZ plane at X=10"
  (make-workplane :origin origin
                  :x-dir '(0 1 0)
                  :z-dir '(1 0 0)))

;;; ============================================================================
;;; Coordinate Transformations
;;; ============================================================================

(defun local-to-global (workplane local-point)
  "Transform a point from workplane local coordinates to global coordinates.

  Args:
    workplane - The workplane defining the local coordinate system
    local-point - Point in local coordinates (x y z)

  Returns: Point in global coordinates (x y z)

  Formula: global = origin + x*x_dir + y*y_dir + z*z_dir

  Example:
    (local-to-global (xy-plane :origin '(10 0 0)) '(5 0 0))
    => (15 0 0)"

  (let ((origin (workplane-origin workplane))
        (x-dir (workplane-x-dir workplane))
        (y-dir (workplane-y-dir workplane))
        (z-dir (workplane-z-dir workplane))
        (x (first local-point))
        (y (second local-point))
        (z (third local-point)))

    ;; global = origin + x*x_dir + y*y_dir + z*z_dir
    (list (+ (first origin)
             (* x (first x-dir))
             (* y (first y-dir))
             (* z (first z-dir)))
          (+ (second origin)
             (* x (second x-dir))
             (* y (second y-dir))
             (* z (second z-dir)))
          (+ (third origin)
             (* x (third x-dir))
             (* y (third y-dir))
             (* z (third z-dir))))))

(defun global-to-local (workplane global-point)
  "Transform a point from global coordinates to workplane local coordinates.

  Args:
    workplane - The workplane defining the local coordinate system
    global-point - Point in global coordinates (x y z)

  Returns: Point in local coordinates (x y z)

  Formula:
    relative = global - origin
    local_x = relative · x_dir
    local_y = relative · y_dir
    local_z = relative · z_dir

  Example:
    (global-to-local (xy-plane :origin '(10 0 0)) '(15 0 0))
    => (5 0 0)"

  (let* ((origin (workplane-origin workplane))
         (x-dir (workplane-x-dir workplane))
         (y-dir (workplane-y-dir workplane))
         (z-dir (workplane-z-dir workplane))
         ;; Compute vector from origin to point
         (relative (list (- (first global-point) (first origin))
                        (- (second global-point) (second origin))
                        (- (third global-point) (third origin)))))

    ;; Project relative vector onto each axis
    (list (dot-product relative x-dir)
          (dot-product relative y-dir)
          (dot-product relative z-dir))))

;;; ============================================================================
;;; Workplane Operations
;;; ============================================================================

(defun offset-workplane (workplane distance)
  "Create a new workplane offset along the normal direction.

  Args:
    workplane - The workplane to offset
    distance - Distance to offset (positive = along normal, negative = opposite)

  Returns: New workplane with offset origin, same orientation

  The new workplane is parallel to the original, moved by 'distance' units
  along the normal (Z) direction.

  Example:
    (offset-workplane (xy-plane) 10)
    => XY plane at Z=10"

  (let* ((origin (workplane-origin workplane))
         (z-dir (workplane-z-dir workplane))
         ;; New origin = old origin + distance * normal
         (new-origin (list (+ (first origin) (* distance (first z-dir)))
                          (+ (second origin) (* distance (second z-dir)))
                          (+ (third origin) (* distance (third z-dir))))))

    (make-workplane :origin new-origin
                    :x-dir (workplane-x-dir workplane)
                    :z-dir z-dir)))

(defun rotate-vector-around-axis (vec axis angle)
  "Rotate a 3D vector around an axis using Rodrigues' rotation formula.

  Args:
    vec - Vector to rotate (x y z)
    axis - Axis keyword (:x, :y, or :z) or axis vector
    angle - Rotation angle in radians

  Returns: Rotated vector (x y z)

  Uses Rodrigues' formula:
    v_rot = v*cos(θ) + (k×v)*sin(θ) + k*(k·v)*(1-cos(θ))
  where k is the unit axis vector."

  (let* ((axis-vec (if (keywordp axis)
                      (multiple-value-list
                       (ecase axis
                         (:x (values 1.0d0 0.0d0 0.0d0))
                         (:y (values 0.0d0 1.0d0 0.0d0))
                         (:z (values 0.0d0 0.0d0 1.0d0))))
                      axis))
         (k axis-vec)
         (cos-theta (cos angle))
         (sin-theta (sin angle))
         (k-dot-v (dot-product k vec))
         (k-cross-v (cross-product k vec)))

    ;; v_rot = v*cos(θ) + (k×v)*sin(θ) + k*(k·v)*(1-cos(θ))
    (list (+ (* (first vec) cos-theta)
             (* (first k-cross-v) sin-theta)
             (* (first k) k-dot-v (- 1 cos-theta)))
          (+ (* (second vec) cos-theta)
             (* (second k-cross-v) sin-theta)
             (* (second k) k-dot-v (- 1 cos-theta)))
          (+ (* (third vec) cos-theta)
             (* (third k-cross-v) sin-theta)
             (* (third k) k-dot-v (- 1 cos-theta))))))

(defun rotate-workplane (workplane axis angle)
  "Create a new workplane rotated around a global axis.

  Args:
    workplane - The workplane to rotate
    axis - Axis keyword (:x, :y, or :z) or axis vector
    angle - Rotation angle in radians (counter-clockwise looking along axis)

  Returns: New workplane with rotated orientation, same origin

  The workplane is rotated around the specified global axis passing through
  the origin. The origin point remains fixed.

  Example:
    (rotate-workplane (xy-plane) :z (/ pi 2))
    => XY plane rotated 90° around Z axis"

  (let* ((origin (workplane-origin workplane))
         (x-dir (workplane-x-dir workplane))
         (z-dir (workplane-z-dir workplane))
         ;; Rotate both direction vectors
         (new-x-dir (rotate-vector-around-axis x-dir axis angle))
         (new-z-dir (rotate-vector-around-axis z-dir axis angle)))

    (make-workplane :origin origin
                    :x-dir new-x-dir
                    :z-dir new-z-dir)))

;;; ============================================================================
;;; Workplane from Face
;;; ============================================================================

(defun workplane-from-face (face)
  "Create a workplane from a face.

  Args:
    face - A cad-face object

  Returns: New workplane with origin at face center and normal aligned with face normal

  The workplane is positioned at the center of the face with its Z direction
  (normal) pointing outward from the face. The X direction is chosen to be
  horizontal if possible.

  Example:
    (let* ((box (make-box 100 100 100))
           (top-face (first (select (faces box) :direction :+z :extreme :max)))
           (wp (workplane-from-face top-face)))
      ...)"

  ;; Need to import FFI functions
  (let ((face-handle (clad.core:shape-handle (clad.shapes:unwrap-shape face))))

    ;; Get face center
    (multiple-value-bind (cx cy cz)
        (clad.ffi:ffi-get-face-center face-handle)

      ;; Get face normal
      (multiple-value-bind (nx ny nz)
          (clad.ffi:ffi-get-face-normal face-handle)

        ;; Choose X direction - try to be horizontal
        ;; If normal is vertical (mostly Z), use +X direction
        ;; Otherwise, cross with +Z to get a tangent vector
        (let* ((z-dir (list nx ny nz))
               (x-dir (if (> (abs nz) 0.9)
                          ;; Nearly vertical face - use +X
                          (list 1.0d0 0.0d0 0.0d0)
                          ;; Non-vertical - cross with +Z to get tangent
                          (let* ((global-z (list 0.0d0 0.0d0 1.0d0))
                                 (tangent (cross-product global-z z-dir)))
                            tangent))))

          (make-workplane :origin (list cx cy cz)
                         :x-dir x-dir
                         :z-dir z-dir))))))
