;;;; src/ffi/transformations.lisp --- FFI bindings for OCCT transformations

(in-package :clad.ffi)

;;; ============================================================================
;;; C Wrapper Function Declarations
;;; ============================================================================

(defcfun ("occt_translate" %occt-translate) :int
  "Translate a shape.
  Returns error code (0 = success)"
  (shape :pointer)
  (dx occt-real)
  (dy occt-real)
  (dz occt-real)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_rotate" %occt-rotate) :int
  "Rotate a shape around an axis.
  Returns error code (0 = success)"
  (shape :pointer)
  (axis-x occt-real)
  (axis-y occt-real)
  (axis-z occt-real)
  (angle-rad occt-real)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_mirror" %occt-mirror) :int
  "Mirror a shape across a plane.
  Returns error code (0 = success)"
  (shape :pointer)
  (plane-normal-x occt-real)
  (plane-normal-y occt-real)
  (plane-normal-z occt-real)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_scale" %occt-scale) :int
  "Scale a shape.
  Returns error code (0 = success)"
  (shape :pointer)
  (factor occt-real)
  (out-shape :pointer)
  (err-msg :pointer))

;;; ============================================================================
;;; High-Level FFI Functions
;;; ============================================================================

(defun ffi-translate (shape-handle dx dy dz)
  "Translate a shape by (dx, dy, dz).

  Arguments:
    shape-handle - occt-handle for shape to translate
    dx, dy, dz   - Translation vector in mm

  Returns: occt-handle for translated shape

  Signals: occt-error on failure"
  (ensure-valid-handle shape-handle)

  (if *occt-available-p*
      (with-foreign-objects ((result-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-translate
                            (handle-ptr shape-handle)
                            (coerce dx 'double-float)
                            (coerce dy 'double-float)
                            (coerce dz 'double-float)
                            result-ptr
                            err-ptr)))
          (check-occt-result result-code "translate")
          (make-occt-handle (mem-ref result-ptr :pointer)
                           :type (handle-type shape-handle)
                           :inc-ref nil)))

      (stub-translate shape-handle dx dy dz)))

(defun ffi-rotate (shape-handle axis-x axis-y axis-z angle-degrees)
  "Rotate a shape around an axis.

  Arguments:
    shape-handle       - occt-handle for shape to rotate
    axis-x, axis-y, axis-z - Axis direction vector
    angle-degrees      - Rotation angle in degrees

  Returns: occt-handle for rotated shape

  Signals: occt-error on failure"
  (ensure-valid-handle shape-handle)

  (if *occt-available-p*
      (with-foreign-objects ((result-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-rotate
                            (handle-ptr shape-handle)
                            (coerce axis-x 'double-float)
                            (coerce axis-y 'double-float)
                            (coerce axis-z 'double-float)
                            (coerce angle-degrees 'double-float)
                            result-ptr
                            err-ptr)))
          (check-occt-result result-code "rotate")
          (make-occt-handle (mem-ref result-ptr :pointer)
                           :type (handle-type shape-handle)
                           :inc-ref nil)))

      (stub-rotate shape-handle axis-x axis-y axis-z angle-degrees)))

(defun ffi-mirror (shape-handle normal-x normal-y normal-z)
  "Mirror a shape across a plane.

  Arguments:
    shape-handle           - occt-handle for shape to mirror
    normal-x, normal-y, normal-z - Plane normal vector

  Returns: occt-handle for mirrored shape

  Signals: occt-error on failure"
  (ensure-valid-handle shape-handle)

  (if *occt-available-p*
      (with-foreign-objects ((result-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-mirror
                            (handle-ptr shape-handle)
                            (coerce normal-x 'double-float)
                            (coerce normal-y 'double-float)
                            (coerce normal-z 'double-float)
                            result-ptr
                            err-ptr)))
          (check-occt-result result-code "mirror")
          (make-occt-handle (mem-ref result-ptr :pointer)
                           :type (handle-type shape-handle)
                           :inc-ref nil)))

      (stub-mirror shape-handle normal-x normal-y normal-z)))

(defun ffi-scale (shape-handle factor)
  "Scale a shape uniformly.

  Arguments:
    shape-handle - occt-handle for shape to scale
    factor       - Scale factor (1.0 = no change)

  Returns: occt-handle for scaled shape

  Signals: occt-error on failure"
  (ensure-valid-handle shape-handle)

  (unless (plusp factor)
    (error 'occt-domain-error
           :message (format nil "Scale factor must be positive: ~A" factor)))

  (if *occt-available-p*
      (with-foreign-objects ((result-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-scale
                            (handle-ptr shape-handle)
                            (coerce factor 'double-float)
                            result-ptr
                            err-ptr)))
          (check-occt-result result-code "scale")
          (make-occt-handle (mem-ref result-ptr :pointer)
                           :type (handle-type shape-handle)
                           :inc-ref nil)))

      (stub-scale shape-handle factor)))

;;; ============================================================================
;;; Stub Implementations
;;; ============================================================================

(defun stub-translate (h dx dy dz)
  "Stub implementation of translate"
  (let ((ptr (make-stub-pointer :translate (handle-ptr h) dx dy dz)))
    (format t "~&;; STUB: Translate ~A by (~A, ~A, ~A) -> ~A~%"
            (handle-ptr h) dx dy dz ptr)
    (make-occt-handle ptr :type (handle-type h) :inc-ref t)))

(defun stub-rotate (h ax ay az angle)
  "Stub implementation of rotate"
  (let ((ptr (make-stub-pointer :rotate (handle-ptr h) ax ay az angle)))
    (format t "~&;; STUB: Rotate ~A around (~A,~A,~A) by ~A° -> ~A~%"
            (handle-ptr h) ax ay az angle ptr)
    (make-occt-handle ptr :type (handle-type h) :inc-ref t)))

(defun stub-mirror (h nx ny nz)
  "Stub implementation of mirror"
  (let ((ptr (make-stub-pointer :mirror (handle-ptr h) nx ny nz)))
    (format t "~&;; STUB: Mirror ~A across plane with normal (~A,~A,~A) -> ~A~%"
            (handle-ptr h) nx ny nz ptr)
    (make-occt-handle ptr :type (handle-type h) :inc-ref t)))

(defun stub-scale (h factor)
  "Stub implementation of scale"
  (let ((ptr (make-stub-pointer :scale (handle-ptr h) factor)))
    (format t "~&;; STUB: Scale ~A by ~A -> ~A~%"
            (handle-ptr h) factor ptr)
    (make-occt-handle ptr :type (handle-type h) :inc-ref t)))
