;;;; src/shapes/classes.lisp --- CLOS shape class hierarchy

(in-package :clad.shapes)

;;; ============================================================================
;;; Base Shape Class
;;; ============================================================================

(defclass cad-shape ()
  ((core-shape :initarg :core-shape
               :accessor core-shape
               :type clad.core:shape
               :documentation "Underlying functional core shape"))
  (:documentation "Base class for all OCCT geometric shapes with CLOS interface"))

;;; ============================================================================
;;; Shape Type Hierarchy
;;; ============================================================================

(defclass cad-vertex (cad-shape)
  ()
  (:documentation "0D topological element - a point in space"))

(defclass cad-edge (cad-shape)
  ()
  (:documentation "1D topological element - a curve bounded by vertices"))

(defclass cad-wire (cad-shape)
  ()
  (:documentation "Connected set of edges forming a path"))

(defclass cad-face (cad-shape)
  ()
  (:documentation "2D topological element - a surface bounded by wires"))

(defclass cad-shell (cad-shape)
  ()
  (:documentation "Connected set of faces"))

(defclass cad-solid (cad-shape)
  ()
  (:documentation "3D topological element - a closed shell enclosing volume"))

(defclass cad-compound (cad-shape)
  ()
  (:documentation "Collection of shapes (any type)"))

;;; ============================================================================
;;; Shape Type Accessors
;;; ============================================================================

(defgeneric shape-type (shape)
  (:documentation "Return the type of shape as a keyword"))

(defmethod shape-type ((s cad-vertex)) :vertex)
(defmethod shape-type ((s cad-edge)) :edge)
(defmethod shape-type ((s cad-wire)) :wire)
(defmethod shape-type ((s cad-face)) :face)
(defmethod shape-type ((s cad-shell)) :shell)
(defmethod shape-type ((s cad-solid)) :solid)
(defmethod shape-type ((s cad-compound)) :compound)

;;; ============================================================================
;;; Shape Validation
;;; ============================================================================

(defgeneric shape-valid-p (shape)
  (:documentation "Check if shape is valid"))

(defmethod shape-valid-p ((s cad-shape))
  "Check if the underlying core shape is valid"
  (clad.core:valid-shape-p (core-shape s)))

;;; ============================================================================
;;; Type Predicates
;;; ============================================================================

(defun vertex-p (obj)
  "Check if object is a cad-vertex"
  (typep obj 'cad-vertex))

(defun edge-p (obj)
  "Check if object is a cad-edge"
  (typep obj 'cad-edge))

(defun wire-p (obj)
  "Check if object is a cad-wire"
  (typep obj 'cad-wire))

(defun face-p (obj)
  "Check if object is a cad-face"
  (typep obj 'cad-face))

(defun shell-p (obj)
  "Check if object is a cad-shell"
  (typep obj 'cad-shell))

(defun solid-p (obj)
  "Check if object is a cad-solid or contains solids (for unwrapped shapes)"
  (cond
    ;; If it's a wrapped CLOS object, check the type
    ((typep obj 'cad-solid) t)
    ;; If it's an unwrapped core shape, check if it has solids
    ((typep obj 'clad.core:shape)
     (let* ((handle (clad.core:shape-handle obj))
            (solids (clad.ffi:ffi-get-shapes handle :solid)))
       (not (null solids))))
    ;; Otherwise not a solid
    (t nil)))

(defun compound-p (obj)
  "Check if object is a cad-compound"
  (typep obj 'cad-compound))

;;; ============================================================================
;;; Conversion Functions
;;; ============================================================================

(defun wrap-shape (core-shape shape-class)
  "Wrap a functional core shape in the appropriate CLOS class.

  Arguments:
    core-shape - a clad.core:shape instance
    shape-class - the CLOS class to wrap it in

  Returns: Instance of shape-class"
  (make-instance shape-class :core-shape core-shape))

(defun unwrap-shape (cad-shape)
  "Extract the functional core shape from a CLOS shape.

  Arguments:
    cad-shape - a cad-shape instance

  Returns: The underlying clad.core:shape"
  (core-shape cad-shape))
