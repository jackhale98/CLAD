;;;; src/core/primitives.lisp --- Functional core for primitive shapes

(in-package :clad.core)

;;; ============================================================================
;;; Shape Structure
;;; ============================================================================

(defstruct (shape (:constructor %make-shape))
  "Represents a geometric shape with an OCCT handle.

  This is a pure functional data structure - operations return new shapes
  rather than modifying existing ones."
  (handle nil :type (or null clad.ffi:occt-handle)
          :read-only t)
  (metadata nil :type list
            :read-only nil))

(defmethod print-object ((s shape) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "~@[~A~]"
            (when (shape-handle s)
              (clad.ffi:handle-type (shape-handle s))))))

(defun make-shape (handle &key metadata)
  "Create a shape from an OCCT handle"
  (%make-shape :handle handle :metadata metadata))

;;; ============================================================================
;;; Primitive Constructors
;;; ============================================================================

(defun make-box (width height depth &key (center t) metadata)
  "Create a box primitive.

  By default, the box is centered on the XY plane and starts at Z=0.

  Arguments:
    width    - Box width in mm (X dimension, can be toleranced-dimension)
    height   - Box height in mm (Y dimension, can be toleranced-dimension)
    depth    - Box depth in mm (Z dimension, can be toleranced-dimension)
    center   - If T (default), center box in XY, start at Z=0
               If NIL, place corner at origin (legacy behavior)
    metadata - Optional metadata plist

  Returns: shape

  Examples:
    (make-box 100 50 30)              ; Centered: (-50,-25,0) to (50,25,30)
    (make-box 100 50 30 :center nil)  ; Corner: (0,0,0) to (100,50,30)
    (make-box (dim 100 :mm :tol 0.1) 50 30)  ; With tolerance on width"
  ;; Extract nominal values if toleranced
  (let* ((nominal-width (if (typep width 'clad.units:toleranced-dimension)
                            (clad.units:dimension-nominal width)
                            width))
         (nominal-height (if (typep height 'clad.units:toleranced-dimension)
                             (clad.units:dimension-nominal height)
                             height))
         (nominal-depth (if (typep depth 'clad.units:toleranced-dimension)
                            (clad.units:dimension-nominal depth)
                            depth))

         ;; Build tolerance metadata
         (tol-metadata nil))

    ;; Add dimension tolerances if present
    (when (typep width 'clad.units:toleranced-dimension)
      (push (list :feature :width
                  :dimension width
                  :type :linear)
            tol-metadata))

    (when (typep height 'clad.units:toleranced-dimension)
      (push (list :feature :height
                  :dimension height
                  :type :linear)
            tol-metadata))

    (when (typep depth 'clad.units:toleranced-dimension)
      (push (list :feature :depth
                  :dimension depth
                  :type :linear)
            tol-metadata))

    ;; Merge tolerance metadata with user metadata
    (let* ((full-metadata (append metadata
                                  (when tol-metadata
                                    (list :has-tolerances t
                                          :tolerance-features tol-metadata))))
           (handle (clad.ffi:ffi-make-box nominal-width nominal-height nominal-depth))
           (shape (make-shape handle :metadata full-metadata)))
      (if center
          (translate shape
                     (- (/ nominal-width 2.0))
                     (- (/ nominal-height 2.0))
                     0.0)
          shape))))

(defun make-cylinder (radius height &key (center t) metadata)
  "Create a cylinder primitive.

  By default, the cylinder is centered on the XY plane and starts at Z=0.

  Arguments:
    radius   - Cylinder radius in mm (can be toleranced-dimension)
    height   - Cylinder height in mm (can be toleranced-dimension)
    center   - If T (default), cylinder is already centered in XY, starts at Z=0
               If NIL, same behavior (cylinder is inherently XY-centered)
    metadata - Optional metadata plist

  Returns: shape

  Examples:
    (make-cylinder 10 50)              ; Centered: radius 10, (0,0,0) to (0,0,50)
    (make-cylinder 10 50 :center nil)  ; Same (cylinders are inherently centered)
    (make-cylinder (dim 25 :mm :fit :H7) 100)  ; With ISO H7 fit tolerance"
  ;; Extract nominal values if toleranced
  (let* ((nominal-radius (if (typep radius 'clad.units:toleranced-dimension)
                             (clad.units:dimension-nominal radius)
                             radius))
         (nominal-height (if (typep height 'clad.units:toleranced-dimension)
                             (clad.units:dimension-nominal height)
                             height))

         ;; Build tolerance metadata
         (tol-metadata nil))

    ;; Add radius tolerance if present
    (when (typep radius 'clad.units:toleranced-dimension)
      (push (list :feature :diameter
                  :dimension radius
                  :type :cylindrical)
            tol-metadata))

    ;; Add height tolerance if present
    (when (typep height 'clad.units:toleranced-dimension)
      (push (list :feature :length
                  :dimension height
                  :type :linear)
            tol-metadata))

    ;; Merge tolerance metadata with user metadata
    (let ((full-metadata (append metadata
                                 (when tol-metadata
                                   (list :has-tolerances t
                                         :tolerance-features tol-metadata)))))
      (let ((handle (clad.ffi:ffi-make-cylinder nominal-radius nominal-height)))
        (make-shape handle :metadata full-metadata)))))

(defun make-sphere (radius &key metadata)
  "Create a sphere primitive.

  Arguments:
    radius - Sphere radius in mm
    metadata - Optional metadata plist

  Returns: shape

  Example:
    (make-sphere 25)                ; Sphere centered at origin"
  (let ((handle (clad.ffi:ffi-make-sphere radius)))
    (make-shape handle :metadata metadata)))

(defun make-cone (radius1 radius2 height &key (center t) metadata)
  "Create a cone or truncated cone primitive.

  By default, the cone is centered on the XY plane and starts at Z=0.

  Arguments:
    radius1  - Bottom radius in mm
    radius2  - Top radius in mm
    height   - Cone height in mm
    center   - If T (default), cone is already centered in XY, starts at Z=0
               If NIL, same behavior (cones are inherently XY-centered)
    metadata - Optional metadata plist

  Returns: shape

  Examples:
    (make-cone 20 10 50)                  ; Truncated cone, (0,0,0) to (0,0,50)
    (make-cone 20 0 50)                   ; Full cone (tip at top)"
  (let ((handle (clad.ffi:ffi-make-cone radius1 radius2 height)))
    (make-shape handle :metadata metadata)))

;;; ============================================================================
;;; Validation
;;; ============================================================================

;; Note: shape-p is automatically created by defstruct

(defun valid-shape-p (shape)
  "Check if shape has a valid OCCT handle"
  (and (shape-p shape)
       (shape-handle shape)
       (not (clad.ffi:handle-null-p (shape-handle shape)))))

(defun ensure-shape (obj)
  "Ensure object is a valid shape, signal error otherwise"
  (unless (valid-shape-p obj)
    (error "Invalid shape: ~S" obj))
  obj)

;;; ============================================================================
;;; Forward declarations for transformations
;;; ============================================================================

(declaim (ftype (function (shape number number number) shape) translate))
