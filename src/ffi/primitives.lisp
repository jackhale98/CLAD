;;;; src/ffi/primitives.lisp --- FFI bindings for OCCT primitive shapes

(in-package :clad.ffi)

;;; ============================================================================
;;; C Wrapper Function Declarations
;;; ============================================================================

;;; These correspond to C wrapper functions that catch C++ exceptions
;;; and return error codes. See c-wrapper/occt-wrapper.cpp for implementation.

;;; Return codes (defined in exception-handling.lisp):
;;;   0 = success
;;;  -2 = domain error (invalid parameter)
;;;  -3 = construction error
;;;  -4 = null object error

(defcfun ("occt_make_box" %occt-make-box) :int
  "Create a box primitive.
  Returns error code (0 = success)"
  (width occt-real)
  (height occt-real)
  (depth occt-real)
  (out-shape :pointer)  ; TopoDS_Shape**
  (err-msg :pointer))   ; char**

(defcfun ("occt_make_cylinder" %occt-make-cylinder) :int
  "Create a cylinder primitive.
  Returns error code (0 = success)"
  (radius occt-real)
  (height occt-real)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_sphere" %occt-make-sphere) :int
  "Create a sphere primitive.
  Returns error code (0 = success)"
  (radius occt-real)
  (out-shape :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_cone" %occt-make-cone) :int
  "Create a cone primitive.
  Returns error code (0 = success)"
  (radius1 occt-real)
  (radius2 occt-real)
  (height occt-real)
  (out-shape :pointer)
  (err-msg :pointer))

;;; ============================================================================
;;; High-Level FFI Functions
;;; ============================================================================

(defun ffi-make-box (width height depth)
  "Create a box primitive using OCCT.

  Arguments:
    width  - Box width in mm (X dimension)
    height - Box height in mm (Y dimension)
    depth  - Box depth in mm (Z dimension)

  Returns: occt-handle wrapping the TopoDS_Shape

  Signals: occt-error on failure"
  (unless (and (plusp width) (plusp height) (plusp depth))
    (error 'occt-domain-error
           :message (format nil "Box dimensions must be positive: ~A x ~A x ~A"
                            width height depth)))

  (if *occt-available-p*
      ;; Real OCCT implementation
      (with-foreign-objects ((shape-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-make-box
                            (coerce width 'double-float)
                            (coerce height 'double-float)
                            (coerce depth 'double-float)
                            shape-ptr
                            err-ptr)))
          (check-occt-result result-code "make-box")
          (make-occt-handle (mem-ref shape-ptr :pointer)
                           :type :solid
                           :inc-ref nil)))  ; C wrapper already incremented

      ;; Stub implementation for testing
      (stub-make-box width height depth)))

(defun ffi-make-cylinder (radius height)
  "Create a cylinder primitive using OCCT.

  Arguments:
    radius - Cylinder radius in mm
    height - Cylinder height in mm

  Returns: occt-handle wrapping the TopoDS_Shape

  Signals: occt-error on failure"
  (unless (and (plusp radius) (plusp height))
    (error 'occt-domain-error
           :message (format nil "Cylinder dimensions must be positive: r=~A h=~A"
                            radius height)))

  (if *occt-available-p*
      (with-foreign-objects ((shape-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-make-cylinder
                            (coerce radius 'double-float)
                            (coerce height 'double-float)
                            shape-ptr
                            err-ptr)))
          (check-occt-result result-code "make-cylinder")
          (make-occt-handle (mem-ref shape-ptr :pointer)
                           :type :solid
                           :inc-ref nil)))

      (stub-make-cylinder radius height)))

(defun ffi-make-sphere (radius)
  "Create a sphere primitive using OCCT.

  Arguments:
    radius - Sphere radius in mm

  Returns: occt-handle wrapping the TopoDS_Shape

  Signals: occt-error on failure"
  (unless (plusp radius)
    (error 'occt-domain-error
           :message (format nil "Sphere radius must be positive: ~A" radius)))

  (if *occt-available-p*
      (with-foreign-objects ((shape-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-make-sphere
                            (coerce radius 'double-float)
                            shape-ptr
                            err-ptr)))
          (check-occt-result result-code "make-sphere")
          (make-occt-handle (mem-ref shape-ptr :pointer)
                           :type :solid
                           :inc-ref nil)))

      (stub-make-sphere radius)))

(defun ffi-make-cone (radius1 radius2 height)
  "Create a cone primitive using OCCT.

  Arguments:
    radius1 - Bottom radius in mm
    radius2 - Top radius in mm
    height  - Cone height in mm

  Returns: occt-handle wrapping the TopoDS_Shape

  Signals: occt-error on failure"
  (unless (and (not (minusp radius1))
               (not (minusp radius2))
               (plusp height)
               (or (plusp radius1) (plusp radius2)))
    (error 'occt-domain-error
           :message (format nil "Invalid cone dimensions: r1=~A r2=~A h=~A"
                            radius1 radius2 height)))

  (if *occt-available-p*
      (with-foreign-objects ((shape-ptr :pointer)
                             (err-ptr :pointer))
        (let ((result-code (%occt-make-cone
                            (coerce radius1 'double-float)
                            (coerce radius2 'double-float)
                            (coerce height 'double-float)
                            shape-ptr
                            err-ptr)))
          (check-occt-result result-code "make-cone")
          (make-occt-handle (mem-ref shape-ptr :pointer)
                           :type :solid
                           :inc-ref nil)))

      (stub-make-cone radius1 radius2 height)))

;;; ============================================================================
;;; Stub Implementations (for testing without OCCT)
;;; ============================================================================

(defvar *stub-shape-counter* 0
  "Counter for generating unique stub shape IDs")

(defun make-stub-pointer (type &rest dimensions)
  "Create a fake pointer for stub mode"
  (incf *stub-shape-counter*)
  (make-pointer (+ (* *stub-shape-counter* 1000)
                  (sxhash (list type dimensions)))))

(defun stub-make-box (width height depth)
  "Stub implementation of make-box for testing"
  (let ((ptr (make-stub-pointer :box width height depth)))
    (format t "~&;; STUB: Creating box ~A x ~A x ~A (ptr: ~A)~%"
            width height depth ptr)
    (make-occt-handle ptr :type :solid :inc-ref t)))

(defun stub-make-cylinder (radius height)
  "Stub implementation of make-cylinder for testing"
  (let ((ptr (make-stub-pointer :cylinder radius height)))
    (format t "~&;; STUB: Creating cylinder r=~A h=~A (ptr: ~A)~%"
            radius height ptr)
    (make-occt-handle ptr :type :solid :inc-ref t)))

(defun stub-make-sphere (radius)
  "Stub implementation of make-sphere for testing"
  (let ((ptr (make-stub-pointer :sphere radius)))
    (format t "~&;; STUB: Creating sphere r=~A (ptr: ~A)~%"
            radius ptr)
    (make-occt-handle ptr :type :solid :inc-ref t)))

(defun stub-make-cone (radius1 radius2 height)
  "Stub implementation of make-cone for testing"
  (let ((ptr (make-stub-pointer :cone radius1 radius2 height)))
    (format t "~&;; STUB: Creating cone r1=~A r2=~A h=~A (ptr: ~A)~%"
            radius1 radius2 height ptr)
    (make-occt-handle ptr :type :solid :inc-ref t)))

;;; ============================================================================
;;; C Wrapper Implementation Notes
;;; ============================================================================

#|

The C wrapper (c-wrapper/occt-wrapper.cpp) should implement these functions
following this pattern:

extern "C" {
    int occt_make_box(double width, double height, double depth,
                      void** out_shape, char** error_msg) {
        try {
            // Validate inputs
            if (width <= 0 || height <= 0 || depth <= 0) {
                *error_msg = strdup("Box dimensions must be positive");
                return -2;  // domain error
            }

            // Create the shape using OCCT
            BRepPrimAPI_MakeBox maker(width, height, depth);

            if (!maker.IsDone()) {
                *error_msg = strdup("Box construction failed");
                return -3;  // construction error
            }

            // Allocate and return the shape
            TopoDS_Shape* shape = new TopoDS_Shape(maker.Shape());
            *out_shape = shape;

            return 0;  // success
        }
        catch (const Standard_Failure& e) {
            *error_msg = strdup(e.GetMessageString());
            return 1;  // known OCCT error
        }
        catch (...) {
            *error_msg = strdup("Unknown OCCT error in make_box");
            return -1;  // unknown error
        }
    }

    // Similar implementations for other primitives...
}

|#
