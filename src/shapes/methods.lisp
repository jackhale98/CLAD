;;;; src/shapes/methods.lisp --- Generic functions and methods for shape queries

(in-package :clad.shapes)

;;; ============================================================================
;;; Generic Function Declarations
;;; ============================================================================

(defgeneric vertices (shape)
  (:documentation "Return list of vertices in shape"))

(defgeneric edges (shape)
  (:documentation "Return list of edges in shape"))

(defgeneric wires (shape)
  (:documentation "Return list of wires in shape"))

(defgeneric faces (shape)
  (:documentation "Return list of faces in shape"))

(defgeneric shells (shape)
  (:documentation "Return list of shells in shape"))

(defgeneric solids (shape)
  (:documentation "Return list of solids in shape"))

(defgeneric bounding-box (shape)
  (:documentation "Return bounding box as list (xmin ymin zmin xmax ymax zmax)"))

(defgeneric center-of-mass (shape)
  (:documentation "Return center of mass as list (x y z)"))

(defgeneric volume (shape)
  (:documentation "Return volume (for solids)"))

(defgeneric area (shape)
  (:documentation "Return area (for faces/shells)"))

(defgeneric shape-length (shape)
  (:documentation "Return length (for edges/wires)"))

(defgeneric geom-type (shape)
  (:documentation "Return geometry type keyword (e.g., :line, :circle, :plane)"))

;;; ============================================================================
;;; Sub-Shape Query Methods
;;; ============================================================================

(defmethod vertices ((s cad-shape))
  "Get all vertices from a shape"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape))
         (sub-handles (clad.ffi:ffi-get-shapes handle :vertex)))
    ;; Wrap each handle in a cad-vertex
    (mapcar (lambda (h)
              (let ((core (clad.core:make-shape h)))
                (make-instance 'cad-vertex :core-shape core)))
            sub-handles)))

(defmethod edges ((s cad-shape))
  "Get all edges from a shape"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape))
         (sub-handles (clad.ffi:ffi-get-shapes handle :edge)))
    ;; Wrap each handle in a cad-edge
    (mapcar (lambda (h)
              (let ((core (clad.core:make-shape h)))
                (make-instance 'cad-edge :core-shape core)))
            sub-handles)))

(defmethod wires ((s cad-shape))
  "Get all wires from a shape"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape))
         (sub-handles (clad.ffi:ffi-get-shapes handle :wire)))
    ;; Wrap each handle in a cad-wire
    (mapcar (lambda (h)
              (let ((core (clad.core:make-shape h)))
                (make-instance 'cad-wire :core-shape core)))
            sub-handles)))

(defmethod faces ((s cad-shape))
  "Get all faces from a shape"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape))
         (sub-handles (clad.ffi:ffi-get-shapes handle :face)))
    ;; Wrap each handle in a cad-face
    (mapcar (lambda (h)
              (let ((core (clad.core:make-shape h)))
                (make-instance 'cad-face :core-shape core)))
            sub-handles)))

(defmethod shells ((s cad-shape))
  "Get all shells from a shape"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape))
         (sub-handles (clad.ffi:ffi-get-shapes handle :shell)))
    ;; Wrap each handle in a cad-shell
    (mapcar (lambda (h)
              (let ((core (clad.core:make-shape h)))
                (make-instance 'cad-shell :core-shape core)))
            sub-handles)))

(defmethod solids ((s cad-shape))
  "Get all solids from a shape"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape))
         (sub-handles (clad.ffi:ffi-get-shapes handle :solid)))
    ;; Wrap each handle in a cad-solid
    (mapcar (lambda (h)
              (let ((core (clad.core:make-shape h)))
                (make-instance 'cad-solid :core-shape core)))
            sub-handles)))

;;; ============================================================================
;;; Geometric Property Query Methods
;;; ============================================================================

(defmethod bounding-box ((s cad-shape))
  "Get bounding box of a shape"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape)))
    (multiple-value-bind (xmin ymin zmin xmax ymax zmax)
        (clad.ffi:ffi-get-bounding-box handle)
      (list xmin ymin zmin xmax ymax zmax))))

;; Convenience method for unwrapped core shapes
(defmethod bounding-box ((s clad.core:shape))
  "Get bounding box of an unwrapped core shape"
  (let ((handle (clad.core:shape-handle s)))
    (multiple-value-bind (xmin ymin zmin xmax ymax zmax)
        (clad.ffi:ffi-get-bounding-box handle)
      (list xmin ymin zmin xmax ymax zmax))))

(defmethod center-of-mass ((s cad-shape))
  "Get center of mass of a shape"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape)))
    (multiple-value-bind (x y z)
        (clad.ffi:ffi-get-center-of-mass handle)
      (list x y z))))

(defmethod volume ((s cad-solid))
  "Get volume of a solid"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape)))
    (clad.ffi:ffi-get-volume handle)))

;; Default implementation for non-solids
(defmethod volume ((s cad-shape))
  "Non-solids have zero volume"
  0.0d0)

;; Convenience method for unwrapped core shapes
(defmethod volume ((s clad.core:shape))
  "Get volume of an unwrapped core shape"
  (let ((handle (clad.core:shape-handle s)))
    (clad.ffi:ffi-get-volume handle)))

(defmethod area ((s cad-face))
  "Get area of a face"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape)))
    (clad.ffi:ffi-get-area handle)))

(defmethod area ((s cad-shell))
  "Get area of a shell (sum of face areas)"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape)))
    (clad.ffi:ffi-get-area handle)))

;; Default implementation for non-faces/shells
(defmethod area ((s cad-shape))
  "Non-faces/shells have zero area"
  0.0d0)

(defmethod shape-length ((s cad-edge))
  "Get length of an edge"
  (let* ((core-shape (core-shape s))
         (handle (clad.core:shape-handle core-shape)))
    (clad.ffi:ffi-get-length handle)))

;; Default implementation for non-edges
(defmethod shape-length ((s cad-shape))
  "Non-edges have zero length in this context"
  0.0d0)

;;; ============================================================================
;;; Geometry Type Query (Phase 8)
;;; ============================================================================

(defmethod geom-type ((s cad-shape))
  "Get geometry type (default: other)"
  :other)

(defmethod geom-type ((e cad-edge))
  "Get geometry type of edge (line, circle, ellipse, bspline, bezier, other)"
  (let* ((core-shape (core-shape e))
         (handle (clad.core:shape-handle core-shape)))
    (multiple-value-bind (type success)
        (clad.ffi:ffi-get-edge-geom-type handle)
      (if success
          type
          :other))))

(defmethod geom-type ((f cad-face))
  "Get geometry type of face (plane, cylinder, sphere, cone, torus, bspline, other)"
  (let* ((core-shape (core-shape f))
         (handle (clad.core:shape-handle core-shape)))
    (multiple-value-bind (type success)
        (clad.ffi:ffi-get-face-geom-type handle)
      (if success
          type
          :other))))
