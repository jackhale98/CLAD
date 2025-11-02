;;;; src/core/fillets.lisp --- Core fillet operations (Phase 8)

(in-package :clad.core)

;;; ============================================================================
;;; Fillet API
;;; ============================================================================

(defun fillet (shape edges radius)
  "Apply fillet to edges of a shape.

  Args:
    shape - Shape to fillet (can be CLOS shape object or pure occt-handle)
    edges - List of edge shapes (CLOS shape objects or pure occt-handles)
    radius - Fillet radius in mm (number)

  Returns: New filleted shape (CLOS shape object)

  Examples:
    ;; Constant radius fillet on multiple edges
    (fillet box (list edge1 edge2 edge3) 5.0)

    ;; Single edge fillet
    (fillet box (list edge) 2.5)

  This function validates inputs and calls the FFI layer to perform
  the actual filleting operation using OCCT's BRepFilletAPI_MakeFillet."

  ;; Validate inputs
  (unless (and (numberp radius) (plusp radius))
    (error "Fillet radius must be a positive number, got: ~A" radius))

  (unless (and edges (listp edges))
    (error "Edges must be a non-empty list of edge handles"))

  ;; Unwrap shape to get pure handle
  (ensure-shape shape)
  (let ((shape-handle (shape-handle shape)))

    ;; Unwrap each edge to get pure handles
    (let ((edge-handles (mapcar (lambda (edge)
                                  (ensure-shape edge)
                                  (shape-handle edge))
                                edges)))

      ;; Call FFI layer to perform fillet
      (let ((result-handle (clad.ffi:ffi-fillet shape-handle edge-handles radius)))
        ;; Wrap result back into CLOS shape object
        (make-shape result-handle)))))

(defun fillet-chain (shape edge-chain radius)
  "Fillet a chain of connected edges.

  Args:
    shape - Shape to fillet (CLOS shape object or occt-handle)
    edge-chain - List of connected edges (CLOS shape objects or occt-handles)
    radius - Fillet radius in mm

  Returns: New filleted shape (CLOS shape object)

  This is optimized for chains of edges (e.g., all edges of a face).
  In OCCT, chained edges can often be filleted more efficiently than
  individual operations."

  ;; For now, this is the same as regular fillet
  ;; Future optimization: use OCCT's chain fillet API
  (fillet shape edge-chain radius))

;;; ============================================================================
;;; Chamfer API
;;; ============================================================================

(defun chamfer (shape edges distance)
  "Apply chamfer to edges of a shape.

  Args:
    shape - Shape to chamfer (can be CLOS shape object or pure occt-handle)
    edges - List of edge shapes (CLOS shape objects or pure occt-handles)
    distance - Chamfer distance in mm (number, symmetric chamfer)

  Returns: New chamfered shape (CLOS shape object)

  Examples:
    ;; Symmetric chamfer on multiple edges
    (chamfer box (list edge1 edge2 edge3) 2.0)

    ;; Single edge chamfer
    (chamfer box (list edge) 1.5)

  This function validates inputs and calls the FFI layer to perform
  the actual chamfering operation using OCCT's BRepFilletAPI_MakeChamfer.
  The chamfer is symmetric (same distance on both adjacent faces)."

  ;; Validate inputs
  (unless (and (numberp distance) (plusp distance))
    (error "Chamfer distance must be a positive number, got: ~A" distance))

  (unless (and edges (listp edges))
    (error "Edges must be a non-empty list of edge handles"))

  ;; Unwrap shape to get pure handle
  (ensure-shape shape)
  (let ((shape-handle (shape-handle shape)))

    ;; Unwrap each edge to get pure handles
    (let ((edge-handles (mapcar (lambda (edge)
                                  (ensure-shape edge)
                                  (shape-handle edge))
                                edges)))

      ;; Call FFI layer to perform chamfer
      (let ((result-handle (clad.ffi:ffi-chamfer shape-handle edge-handles distance)))
        ;; Wrap result back into CLOS shape object
        (make-shape result-handle)))))

(defun chamfer-chain (shape edge-chain distance)
  "Chamfer a chain of connected edges.

  Args:
    shape - Shape to chamfer (CLOS shape object or occt-handle)
    edge-chain - List of connected edges (CLOS shape objects or occt-handles)
    distance - Chamfer distance in mm

  Returns: New chamfered shape (CLOS shape object)

  This is optimized for chains of edges (e.g., all edges of a face).
  In OCCT, chained edges can often be chamfered more efficiently than
  individual operations."

  ;; For now, this is the same as regular chamfer
  ;; Future optimization: use OCCT's chain chamfer API
  (chamfer shape edge-chain distance))
