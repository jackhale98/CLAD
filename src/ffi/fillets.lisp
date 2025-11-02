;;;; src/ffi/fillets.lisp --- FFI bindings for fillet operations (Phase 8)

(in-package :clad.ffi)

;;; ============================================================================
;;; OCCT API Bindings (for future C++ wrapper implementation)
;;; ============================================================================

;; These will be implemented in the C++ wrapper later
;; For now, we use stub implementations

(defcfun ("occt_make_fillet" %occt-make-fillet) :int
  "Create fillet on edges of a shape (C++ wrapper function)"
  (shape-handle :pointer)
  (edge-handles :pointer)    ; Array of edge handles
  (num-edges :int)
  (radius :double)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_chamfer" %occt-make-chamfer) :int
  "Create chamfer on edges of a shape (C++ wrapper function)"
  (shape-handle :pointer)
  (edge-handles :pointer)    ; Array of edge handles
  (num-edges :int)
  (distance :double)
  (out-shape :pointer)
  (err-msg :pointer))

;;; ============================================================================
;;; High-Level FFI Function
;;; ============================================================================

(defun ffi-fillet (shape-handle edge-handles radius)
  "Create fillets on specified edges.

  Args:
    shape-handle - Base shape (occt-handle)
    edge-handles - List of edge occt-handles
    radius - Fillet radius in mm

  Returns: New shape with fillets applied (occt-handle)

  Signals: occt-error on failure

  This function applies constant-radius fillets to the specified edges.
  The edges must belong to the shape and form valid fillet candidates."

  (unless (and (plusp radius) (not (null edge-handles)))
    (error 'occt-domain-error
           :message (format nil "Invalid fillet parameters: r=~A, edges=~A"
                            radius (length edge-handles))))

  (if *occt-available-p*
      ;; Real OCCT implementation
      (with-foreign-object (edge-array :pointer (length edge-handles))
        ;; Fill array with edge handles
        (loop for edge-handle in edge-handles
              for i from 0
              do (setf (mem-aref edge-array :pointer i)
                      (handle-ptr edge-handle)))

        (with-foreign-objects ((shape-ptr :pointer)
                               (err-ptr :pointer))
          (let ((result-code (%occt-make-fillet
                              (handle-ptr shape-handle)
                              edge-array
                              (length edge-handles)
                              (coerce radius 'double-float)
                              shape-ptr
                              err-ptr)))
            (check-occt-result result-code "fillet")
            (make-occt-handle (mem-ref shape-ptr :pointer)
                             :type :solid
                             :inc-ref nil))))

      ;; Stub implementation for development/testing
      (stub-fillet shape-handle edge-handles radius)))

;;; ============================================================================
;;; Stub Implementation
;;; ============================================================================

(defun stub-fillet (shape-handle edge-handles radius)
  "Stub implementation of fillet for testing without OCCT.

  This returns the original shape handle unchanged, but validates
  that the fillet operation would be legal. Real filleting requires
  the full OCCT library.

  Args:
    shape-handle - Base shape handle
    edge-handles - List of edge handles
    radius - Fillet radius

  Returns: Original shape handle (unchanged)"

  (declare (ignore edge-handles radius))

  ;; Validate the shape handle is valid
  (unless (handle-valid-p shape-handle)
    (error 'occt-null-object-error
           :message "Cannot fillet invalid shape"))

  ;; In stub mode, we just return the original shape
  ;; In reality, OCCT would create a new shape with rounded edges
  (format *error-output* "~&;; STUB: fillet with radius ~Amm (OCCT not available)~%" radius)

  ;; Return the original shape unchanged
  ;; (In production, this would be a new filleted shape)
  shape-handle)

(defun ffi-chamfer (shape-handle edge-handles distance)
  "Create chamfers on specified edges.

  Args:
    shape-handle - Base shape (occt-handle)
    edge-handles - List of edge occt-handles
    distance - Chamfer distance in mm (symmetric chamfer)

  Returns: New shape with chamfers applied (occt-handle)

  Signals: occt-error on failure

  This function applies symmetric chamfers to the specified edges.
  The edges must belong to the shape and form valid chamfer candidates."

  (unless (and (plusp distance) (not (null edge-handles)))
    (error 'occt-domain-error
           :message (format nil "Invalid chamfer parameters: d=~A, edges=~A"
                            distance (length edge-handles))))

  (if *occt-available-p*
      ;; Real OCCT implementation
      (with-foreign-object (edge-array :pointer (length edge-handles))
        ;; Fill array with edge handles
        (loop for edge-handle in edge-handles
              for i from 0
              do (setf (mem-aref edge-array :pointer i)
                      (handle-ptr edge-handle)))

        (with-foreign-objects ((shape-ptr :pointer)
                               (err-ptr :pointer))
          (let ((result-code (%occt-make-chamfer
                              (handle-ptr shape-handle)
                              edge-array
                              (length edge-handles)
                              (coerce distance 'double-float)
                              shape-ptr
                              err-ptr)))
            (check-occt-result result-code "chamfer")
            (make-occt-handle (mem-ref shape-ptr :pointer)
                             :type :solid
                             :inc-ref nil))))

      ;; Stub implementation for development/testing
      (stub-chamfer shape-handle edge-handles distance)))

(defun stub-chamfer (shape-handle edge-handles distance)
  "Stub implementation of chamfer for testing without OCCT.

  This returns the original shape handle unchanged, but validates
  that the chamfer operation would be legal. Real chamfering requires
  the full OCCT library.

  Args:
    shape-handle - Base shape handle
    edge-handles - List of edge handles
    distance - Chamfer distance

  Returns: Original shape handle (unchanged)"

  (declare (ignore edge-handles distance))

  ;; Validate the shape handle is valid
  (unless (handle-valid-p shape-handle)
    (error 'occt-null-object-error
           :message "Cannot chamfer invalid shape"))

  ;; In stub mode, we just return the original shape
  ;; In reality, OCCT would create a new shape with beveled edges
  (format *error-output* "~&;; STUB: chamfer with distance ~Amm (OCCT not available)~%" distance)

  ;; Return the original shape unchanged
  ;; (In production, this would be a new chamfered shape)
  shape-handle)
