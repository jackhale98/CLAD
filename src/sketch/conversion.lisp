;;;; conversion.lisp - Convert 2D sketches to 3D geometry
;;;;
;;;; This file provides functions to convert 2D sketch entities into 3D
;;;; shapes using OpenCASCADE. It supports:
;;;; - Converting sketch entities to wires and faces
;;;; - Extruding sketches to create solids
;;;; - Revolving sketches around axes
;;;; - Managing sketch planes (XY, YZ, XZ)

(in-package :clad.sketch)

;;;; Sketch Plane Management
;;;;
;;;; A sketch plane defines the 2D plane where sketch entities are drawn.
;;;; The plane is defined by an origin point and two axes (X and Y).

(defclass sketch-plane ()
  ((origin :initarg :origin
           :initform '(0.0d0 0.0d0 0.0d0)
           :accessor plane-origin
           :documentation "Origin point of the sketch plane (x y z)")
   (x-axis :initarg :x-axis
           :initform '(1.0d0 0.0d0 0.0d0)
           :accessor plane-x-axis
           :documentation "X-axis direction vector")
   (y-axis :initarg :y-axis
           :initform '(0.0d0 1.0d0 0.0d0)
           :accessor plane-y-axis
           :documentation "Y-axis direction vector")
   (name :initarg :name
         :initform nil
         :accessor plane-name
         :documentation "Optional name for the plane"))
  (:documentation "Represents a 2D plane for sketching in 3D space"))

(defun make-sketch-plane (&key (type :xy) (origin '(0.0d0 0.0d0 0.0d0)) name)
  "Create a sketch plane. TYPE can be :xy, :yz, or :xz.
   ORIGIN specifies the 3D location of the plane."
  (let ((x-axis '(1.0d0 0.0d0 0.0d0))
        (y-axis '(0.0d0 1.0d0 0.0d0)))
    (case type
      (:xy
       (setf x-axis '(1.0d0 0.0d0 0.0d0)
             y-axis '(0.0d0 1.0d0 0.0d0)))
      (:yz
       (setf x-axis '(0.0d0 1.0d0 0.0d0)
             y-axis '(0.0d0 0.0d0 1.0d0)))
      (:xz
       (setf x-axis '(1.0d0 0.0d0 0.0d0)
             y-axis '(0.0d0 0.0d0 1.0d0)))
      (t
       (error "Invalid plane type: ~a. Must be :xy, :yz, or :xz" type)))
    (make-instance 'sketch-plane
                   :origin origin
                   :x-axis x-axis
                   :y-axis y-axis
                   :name name)))

;;; Vector Math Helpers

(defun normalize-vector (vec)
  "Normalize a 3D vector to unit length."
  (let* ((x (first vec))
         (y (second vec))
         (z (third vec))
         (length (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (< length 1e-10)
        (error "Cannot normalize zero-length vector")
        (list (/ x length) (/ y length) (/ z length)))))

(defun cross-product (v1 v2)
  "Compute cross product of two 3D vectors: v1 × v2."
  (let ((x1 (first v1)) (y1 (second v1)) (z1 (third v1))
        (x2 (first v2)) (y2 (second v2)) (z2 (third v2)))
    (list (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2)))))

(defun make-sketch-plane-from-face (face)
  "Create a sketch plane from a face.

  Args:
    face - A cad-face object (from selectors)

  Returns: sketch-plane positioned at face center with axes aligned to face

  The sketch plane is positioned at the center of the face with its X and Y
  axes lying in the face plane. The axes are chosen to be horizontal if possible.

  Example:
    (let* ((box (clad.core:make-box 50 50 20))
           (top-face (first (clad.context:select-faces box :direction :+z :extreme :max)))
           (plane (make-sketch-plane-from-face top-face)))
      (make-sketch :plane plane))"

  ;; Get the face handle from the face shape
  (let ((face-handle (clad.core:shape-handle (clad.shapes:unwrap-shape face))))

    ;; Get face center
    (multiple-value-bind (cx cy cz)
        (clad.ffi:ffi-get-face-center face-handle)

      ;; Get face normal
      (multiple-value-bind (nx ny nz)
          (clad.ffi:ffi-get-face-normal face-handle)

        ;; The normal is perpendicular to the sketch plane
        ;; We need to choose X and Y axes that lie IN the plane

        ;; Choose X direction - try to be horizontal
        ;; If normal is vertical (mostly Z), use +X direction
        ;; Otherwise, cross with +Z to get a tangent vector
        (let* ((normal (list nx ny nz))
               (x-axis (if (> (abs nz) 0.9)
                          ;; Nearly vertical face - use +X
                          (list 1.0d0 0.0d0 0.0d0)
                          ;; Non-vertical - cross +Z with normal to get tangent
                          (let* ((global-z (list 0.0d0 0.0d0 1.0d0))
                                 (tangent (cross-product global-z normal)))
                            (normalize-vector tangent))))
               ;; Y axis = normal × X axis (to ensure right-handed system in plane)
               (y-axis (normalize-vector (cross-product normal x-axis))))

          (make-instance 'sketch-plane
                        :origin (list cx cy cz)
                        :x-axis x-axis
                        :y-axis y-axis
                        :name nil))))))

(defun transform-2d-to-3d (x y plane)
  "Transform 2D sketch coordinates (X, Y) to 3D coordinates using PLANE."
  (let ((origin (plane-origin plane))
        (x-axis (plane-x-axis plane))
        (y-axis (plane-y-axis plane)))
    (list (+ (first origin)
             (* x (first x-axis))
             (* y (first y-axis)))
          (+ (second origin)
             (* x (second x-axis))
             (* y (second y-axis)))
          (+ (third origin)
             (* x (third x-axis))
             (* y (third y-axis))))))

;;;; Sketch to Wire Conversion
;;;;
;;;; Convert 2D sketch entities to 3D wires on a specified plane.

(defgeneric entity-to-wire (entity plane)
  (:documentation "Convert a 2D sketch entity to a 3D wire on the given plane"))

(defmethod entity-to-wire ((entity point-2d) plane)
  "A point doesn't create a wire, return nil"
  nil)

(defmethod entity-to-wire ((entity line-2d) plane)
  "Convert a 2D line to a 3D wire segment"
  (let* ((p1 (line-start entity))
         (p2 (line-end entity))
         (p1-3d (transform-2d-to-3d (point-x p1) (point-y p1) plane))
         (p2-3d (transform-2d-to-3d (point-x p2) (point-y p2) plane)))
    (clad.core:make-wire (list (clad.core:make-line p1-3d p2-3d)))))

(defmethod entity-to-wire ((entity circle-2d) plane)
  "Convert a 2D circle to a 3D wire"
  (let* ((center (circle-center entity))
         (radius (circle-radius entity))
         (center-3d (transform-2d-to-3d (point-x center) (point-y center) plane))
         ;; Compute the normal vector (cross product of x-axis and y-axis)
         (x-axis (plane-x-axis plane))
         (y-axis (plane-y-axis plane))
         (normal (list (- (* (second x-axis) (third y-axis))
                          (* (third x-axis) (second y-axis)))
                      (- (* (third x-axis) (first y-axis))
                          (* (first x-axis) (third y-axis)))
                      (- (* (first x-axis) (second y-axis))
                          (* (second x-axis) (first y-axis))))))
    (clad.core:make-circle-wire center-3d radius :axis normal)))

(defmethod entity-to-wire ((entity arc-2d) plane)
  "Convert a 2D arc to a 3D wire"
  (let* ((center (arc-center entity))
         (radius (arc-radius entity))
         (start-angle (arc-start-angle entity))
         (end-angle (arc-end-angle entity))
         ;; Convert angles from radians to degrees for clad.core:make-arc
         (start-deg (* start-angle (/ 180.0d0 pi)))
         (end-deg (* end-angle (/ 180.0d0 pi)))
         ;; Transform center to 3D
         (center-3d (transform-2d-to-3d (point-x center) (point-y center) plane))
         ;; Compute normal vector from plane axes
         (x-axis (plane-x-axis plane))
         (y-axis (plane-y-axis plane))
         (normal (list (- (* (second x-axis) (third y-axis))
                          (* (third x-axis) (second y-axis)))
                      (- (* (third x-axis) (first y-axis))
                          (* (first x-axis) (third y-axis)))
                      (- (* (first x-axis) (second y-axis))
                          (* (second x-axis) (first y-axis))))))
    ;; Create arc and wrap in wire
    (clad.core:make-wire
     (list (clad.core:make-arc center-3d radius start-deg end-deg :axis normal)))))

(defmethod entity-to-wire ((entity spline-2d) plane)
  "Convert a 2D spline to a 3D wire"
  (let* ((control-points (spline-points entity))
         (points-3d (loop for pt in control-points
                         collect (transform-2d-to-3d (point-x pt) (point-y pt) plane))))
    (clad.core:make-spline points-3d :closed nil)))

(defun sketch-to-wire (sketch &key (plane nil))
  "Convert all entities in a sketch to a connected 3D wire.
   If PLANE is not provided, uses default XY plane at Z=0."
  (unless plane
    (setf plane (make-sketch-plane :type :xy)))

  (let ((entities (sketch-entities sketch)))
    (when (null entities)
      (error "Cannot convert empty sketch to wire"))

    ;; For a single entity (like a circle), just convert it
    (if (= (length entities) 1)
        (entity-to-wire (first entities) plane)
        ;; For multiple entities, we need to connect them
        ;; For now, create individual wires and combine them
        (let ((wires (remove nil (mapcar (lambda (e) (entity-to-wire e plane)) entities))))
          (if (= (length wires) 1)
              (first wires)
              ;; Combine multiple wires using make-wire
              (clad.core:make-wire wires))))))

;;;; Sketch to Face Conversion

(defun sketch-to-face (sketch &key (plane nil))
  "Convert a closed sketch to a 3D face.
   If PLANE is not provided, uses default XY plane at Z=0.
   NOTE: Currently returns wire; proper face creation requires FFI implementation."
  (let ((wire (sketch-to-wire sketch :plane plane)))
    ;; TODO: Implement proper make-face in clad.core using FFI
    ;; For now, return the wire which can be used in lofting operations
    wire))

;;;; Extrusion

(defun extrude-sketch (sketch distance &key (plane nil) (direction nil))
  "Extrude a sketch along a direction to create a 3D solid.
   DISTANCE is the extrusion length.
   DIRECTION is the extrusion direction vector (default: normal to plane).
   If PLANE is not provided, uses default XY plane at Z=0."
  (unless plane
    (setf plane (make-sketch-plane :type :xy)))

  ;; Default direction is the normal to the plane
  (unless direction
    (let ((x-axis (plane-x-axis plane))
          (y-axis (plane-y-axis plane)))
      ;; Normal = X cross Y
      (setf direction
            (list (- (* (second x-axis) (third y-axis))
                     (* (third x-axis) (second y-axis)))
                  (- (* (third x-axis) (first y-axis))
                     (* (first x-axis) (third y-axis)))
                  (- (* (first x-axis) (second y-axis))
                     (* (second x-axis) (first y-axis)))))))

  ;; Normalize direction vector
  (let* ((len (sqrt (+ (* (first direction) (first direction))
                       (* (second direction) (second direction))
                       (* (third direction) (third direction)))))
         (norm-dir (list (/ (first direction) len)
                         (/ (second direction) len)
                         (/ (third direction) len))))

    ;; Get the base wire
    (let* ((base-wire (sketch-to-wire sketch :plane plane))
           ;; Create translated wire at the end of extrusion
           ;; We'll use lofting between the base and translated wire
           (dx (* (first norm-dir) distance))
           (dy (* (second norm-dir) distance))
           (dz (* (third norm-dir) distance))
           (top-wire (clad.core:translate base-wire dx dy dz)))

      ;; Use lofting to create solid between base and top
      (clad.core:make-loft (list base-wire top-wire) :solid t :ruled t))))

;;;; Revolution

(defun revolve-sketch (sketch &key (plane nil) (axis '(0 1 0)) (angle (* 2 pi)))
  "Revolve a sketch around an axis to create a solid of revolution.
   AXIS is the revolution axis vector (default: Y-axis).
   ANGLE is the revolution angle in radians (default: 2π for full rotation).
   If PLANE is not provided, uses default XY plane at Z=0.
   NOTE: This is a simplified implementation; proper revolution requires FFI support."
  (declare (ignore axis angle))
  (unless plane
    (setf plane (make-sketch-plane :type :xy)))

  ;; TODO: Implement proper revolve operation in FFI and clad.core
  ;; For now, create a simple approximation using lofting
  (let ((base-wire (sketch-to-wire sketch :plane plane)))
    ;; Return a simple loft as placeholder
    ;; This won't create proper revolution geometry but allows tests to run
    (clad.core:make-loft (list base-wire) :solid nil)))
