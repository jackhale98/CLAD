;;;; src/ffi/booleans.lisp --- FFI bindings for OCCT boolean operations

(in-package :clad.ffi)

;;; ============================================================================
;;; C Wrapper Function Declarations
;;; ============================================================================

(defcfun ("occt_union" %occt-union) :int
  "Fuse (union) two shapes.
  Returns error code (0 = success)"
  (shape1 :pointer)
  (shape2 :pointer)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_cut" %occt-cut) :int
  "Cut (subtract) shape2 from shape1.
  Returns error code (0 = success)"
  (shape1 :pointer)
  (shape2 :pointer)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_intersect" %occt-intersect) :int
  "Compute intersection (common) of two shapes.
  Returns error code (0 = success)"
  (shape1 :pointer)
  (shape2 :pointer)
  (out-shape :pointer)
  (err-msg :pointer))

;;; ============================================================================
;;; High-Level FFI Functions
;;; ============================================================================

(defun ffi-union (shape1-handle shape2-handle)
  "Compute boolean union of two shapes.

  Arguments:
    shape1-handle - occt-handle for first shape
    shape2-handle - occt-handle for second shape

  Returns: occt-handle for result shape

  Signals: occt-error on failure"
  (ensure-valid-handle shape1-handle)
  (ensure-valid-handle shape2-handle)

  (if *occt-available-p*
      (with-foreign-objects ((result-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-union
                            (handle-ptr shape1-handle)
                            (handle-ptr shape2-handle)
                            result-ptr
                            err-ptr)))
          (check-occt-result result-code "union")
          (make-occt-handle (mem-ref result-ptr :pointer)
                           :type :solid
                           :inc-ref nil)))

      (stub-union shape1-handle shape2-handle)))

(defun ffi-cut (shape1-handle shape2-handle)
  "Subtract shape2 from shape1.

  Arguments:
    shape1-handle - occt-handle for base shape
    shape2-handle - occt-handle for tool shape

  Returns: occt-handle for result shape

  Signals: occt-error on failure"
  (ensure-valid-handle shape1-handle)
  (ensure-valid-handle shape2-handle)

  (if *occt-available-p*
      (with-foreign-objects ((result-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-cut
                            (handle-ptr shape1-handle)
                            (handle-ptr shape2-handle)
                            result-ptr
                            err-ptr)))
          (check-occt-result result-code "cut")
          (make-occt-handle (mem-ref result-ptr :pointer)
                           :type :solid
                           :inc-ref nil)))

      (stub-cut shape1-handle shape2-handle)))

(defun ffi-intersect (shape1-handle shape2-handle)
  "Compute intersection of two shapes.

  Arguments:
    shape1-handle - occt-handle for first shape
    shape2-handle - occt-handle for second shape

  Returns: occt-handle for result shape

  Signals: occt-error on failure"
  (ensure-valid-handle shape1-handle)
  (ensure-valid-handle shape2-handle)

  (if *occt-available-p*
      (with-foreign-objects ((result-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-intersect
                            (handle-ptr shape1-handle)
                            (handle-ptr shape2-handle)
                            result-ptr
                            err-ptr)))
          (check-occt-result result-code "intersect")
          (make-occt-handle (mem-ref result-ptr :pointer)
                           :type :solid
                           :inc-ref nil)))

      (stub-intersect shape1-handle shape2-handle)))

;;; ============================================================================
;;; Stub Implementations
;;; ============================================================================

(defun stub-union (h1 h2)
  "Stub implementation of union"
  (let ((ptr (make-stub-pointer :union (handle-ptr h1) (handle-ptr h2))))
    (format t "~&;; STUB: Union of ~A and ~A -> ~A~%"
            (handle-ptr h1) (handle-ptr h2) ptr)
    (make-occt-handle ptr :type :solid :inc-ref t)))

(defun stub-cut (h1 h2)
  "Stub implementation of cut"
  (let ((ptr (make-stub-pointer :cut (handle-ptr h1) (handle-ptr h2))))
    (format t "~&;; STUB: Cut ~A from ~A -> ~A~%"
            (handle-ptr h2) (handle-ptr h1) ptr)
    (make-occt-handle ptr :type :solid :inc-ref t)))

(defun stub-intersect (h1 h2)
  "Stub implementation of intersect"
  (let ((ptr (make-stub-pointer :intersect (handle-ptr h1) (handle-ptr h2))))
    (format t "~&;; STUB: Intersect ~A and ~A -> ~A~%"
            (handle-ptr h1) (handle-ptr h2) ptr)
    (make-occt-handle ptr :type :solid :inc-ref t)))
