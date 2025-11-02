;;;; src/ffi/memory-management.lisp --- Memory management for OCCT handles

(in-package :clad.ffi)

;;; ============================================================================
;;; OCCT Handle System
;;; ============================================================================

;;; OCCT uses Handle<T> reference-counted smart pointers for memory management.
;;; We need to integrate with this system:
;;;
;;; 1. When creating a handle, increment ref count
;;; 2. When copying a handle, increment ref count
;;; 3. When handle is no longer needed, decrement ref count
;;; 4. When ref count reaches 0, object is automatically freed

(defclass occt-handle ()
  ((ptr :initarg :ptr
        :initform (null-pointer)
        :accessor handle-ptr
        :documentation "Foreign pointer to OCCT object")
   (type :initarg :type
         :initform :shape
         :accessor handle-type
         :documentation "Type of OCCT object (:shape, :solid, :face, etc.)")
   (valid-p :initarg :valid-p
            :initform t
            :accessor handle-valid-p
            :documentation "Whether this handle is still valid"))
  (:documentation "Wrapper for OCCT Handle<T> smart pointers"))

(defmethod print-object ((handle occt-handle) stream)
  (print-unreadable-object (handle stream :type t :identity t)
    (format stream "~A ~:[INVALID~;~:*~A~]"
            (handle-type handle)
            (when (handle-valid-p handle)
              (handle-ptr handle)))))

;;; ============================================================================
;;; C Wrapper Functions for Handle Management
;;; ============================================================================

;;; These would be implemented in a C++ wrapper library
;;; For now, we provide stubs

(defcfun ("occt_handle_inc_ref" %occt-handle-inc-ref) :void
  "Increment reference count of a handle"
  (handle :pointer))

(defcfun ("occt_handle_dec_ref" %occt-handle-dec-ref) :void
  "Decrement reference count of a handle"
  (handle :pointer))

(defcfun ("occt_handle_ref_count" %occt-handle-ref-count) :int
  "Get reference count of a handle"
  (handle :pointer))

(defcfun ("occt_handle_is_null" %occt-handle-is-null) occt-bool
  "Check if handle is null"
  (handle :pointer))

;;; Stub implementations
(defvar *handle-ref-counts* (make-hash-table :test 'eq)
  "Reference counts for handles in stub mode")

(defun occt-handle-inc-ref (ptr)
  "Increment reference count (stub)"
  (unless (null-pointer-p ptr)
    (incf (gethash ptr *handle-ref-counts* 0))))

(defun occt-handle-dec-ref (ptr)
  "Decrement reference count (stub)"
  (unless (null-pointer-p ptr)
    (let ((count (decf (gethash ptr *handle-ref-counts* 1))))
      (when (<= count 0)
        (remhash ptr *handle-ref-counts*)
        ;; In real implementation, this would free the OCCT object
        (format t "~&;; Handle ~A freed (ref count = 0)~%" ptr)))))

(defun occt-handle-ref-count (ptr)
  "Get reference count (stub)"
  (if (null-pointer-p ptr)
      0
      (gethash ptr *handle-ref-counts* 0)))

(defun occt-handle-is-null (ptr)
  "Check if handle is null (stub)"
  (null-pointer-p ptr))

;;; ============================================================================
;;; Handle Creation and Management
;;; ============================================================================

(defun make-occt-handle (ptr &key (type :shape) (inc-ref t))
  "Create a new OCCT handle wrapper.

  Arguments:
    ptr      - Foreign pointer to OCCT object
    type     - Type of object (:shape, :solid, :face, etc.)
    inc-ref  - Whether to increment reference count (default T)

  Returns: occt-handle instance

  The handle will be automatically finalized when garbage collected,
  decrementing the reference count."
  (when (null-pointer-p ptr)
    (error 'occt-null-object-error
           :message "Attempted to create handle from null pointer"))

  (when inc-ref
    (occt-handle-inc-ref ptr))

  (let ((handle (make-instance 'occt-handle
                               :ptr ptr
                               :type type
                               :valid-p t)))
    ;; Register finalizer to decrement ref count when handle is GC'd
    (trivial-garbage:finalize handle
                              (lambda ()
                                (occt-handle-dec-ref ptr)))
    handle))

(defun copy-occt-handle (handle)
  "Create a copy of an OCCT handle, incrementing the reference count"
  (unless (handle-valid-p handle)
    (error 'occt-null-object-error
           :message "Attempted to copy invalid handle"))

  (make-occt-handle (handle-ptr handle)
                    :type (handle-type handle)
                    :inc-ref t))

(defun invalidate-handle (handle)
  "Mark a handle as invalid without decrementing ref count.
  Used when ownership is transferred elsewhere."
  (setf (handle-valid-p handle) nil))

(defun handle-null-p (handle)
  "Check if a handle is null or invalid"
  (or (not (handle-valid-p handle))
      (occt-handle-is-null (handle-ptr handle))))

;;; ============================================================================
;;; Handle Type Checking
;;; ============================================================================

(defun check-handle-type (handle expected-type)
  "Verify that a handle is of the expected type"
  (unless (eq (handle-type handle) expected-type)
    (error 'occt-error
           :message (format nil "Expected handle of type ~A, got ~A"
                            expected-type
                            (handle-type handle)))))

(defun ensure-valid-handle (handle)
  "Ensure a handle is valid, signal error otherwise"
  (when (handle-null-p handle)
    (error 'occt-null-object-error
           :message "Handle is null or invalid")))

;;; ============================================================================
;;; Memory Management Utilities
;;; ============================================================================

(defun get-handle-statistics ()
  "Get statistics about currently tracked handles (for debugging)"
  (let ((total-handles 0)
        (total-refs 0))
    (maphash (lambda (ptr count)
               (declare (ignore ptr))
               (incf total-handles)
               (incf total-refs count))
             *handle-ref-counts*)
    (values total-handles total-refs)))

(defun clear-handle-cache ()
  "Clear the handle reference count cache (for testing)"
  (clrhash *handle-ref-counts*))
