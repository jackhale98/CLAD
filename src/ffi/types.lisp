;;;; src/ffi/types.lisp --- FFI type definitions for OCCT

(in-package :clad.ffi)

;;; ============================================================================
;;; Foreign Library Loading
;;; ============================================================================

;;; NOTE: We use our C wrapper library which links to OCCT internally
;;; The wrapper library is built in c-wrapper/build/libocct-wrapper.so

;;; Track whether OCCT is available
(defvar *occt-available-p* nil
  "T if OpenCASCADE libraries are loaded, NIL otherwise")

(defun load-occt-libraries ()
  "Attempt to load OCCT wrapper library. Returns T on success, NIL on failure."
  (handler-case
      (progn
        ;; Try to load with absolute path based on ASDF system location
        (let ((lib-path (merge-pathnames "c-wrapper/build/libocct-wrapper.so"
                                         (asdf:system-source-directory :clad))))
          (if (probe-file lib-path)
              (progn
                (load-foreign-library (namestring lib-path))
                (setf *occt-available-p* t)
                (format t "~&;; Successfully loaded OCCT wrapper library from: ~A~%"  (namestring lib-path))
                t)
              (progn
                (format t "~&;; Note: OCCT wrapper library not found at: ~A~%"  (namestring lib-path))
                (format t "~&;; Working in stub mode.~%")
                (setf *occt-available-p* nil)
                nil))))
    (error (e)
      (format t "~&;; Note: OCCT wrapper library not found. Working in stub mode.~%")
      (format t ";; Error: ~A~%" e)
      (setf *occt-available-p* nil)
      nil)))

;;; Try to load the library when this file is loaded
(load-occt-libraries)

;;; ============================================================================
;;; Basic FFI Types
;;; ============================================================================

(defctype occt-real :double
  "OCCT uses double precision for geometric calculations")

(defctype occt-int :int
  "Standard integer type")

(defctype occt-bool :int
  "OCCT Boolean (0 = false, non-zero = true)")

;;; ============================================================================
;;; Handle Types
;;; ============================================================================

;;; OCCT uses reference-counted Handle<T> smart pointers
;;; We represent these as opaque pointers in Lisp

(defctype occt-shape-ptr :pointer
  "Pointer to TopoDS_Shape")

(defctype occt-solid-ptr :pointer
  "Pointer to TopoDS_Solid")

(defctype occt-face-ptr :pointer
  "Pointer to TopoDS_Face")

(defctype occt-edge-ptr :pointer
  "Pointer to TopoDS_Edge")

(defctype occt-vertex-ptr :pointer
  "Pointer to TopoDS_Vertex")

;;; ============================================================================
;;; Geometric Types
;;; ============================================================================

(defcstruct gp-pnt
  "3D point (gp_Pnt)"
  (x occt-real)
  (y occt-real)
  (z occt-real))

(defcstruct gp-dir
  "3D direction vector (gp_Dir)"
  (x occt-real)
  (y occt-real)
  (z occt-real))

(defcstruct gp-vec
  "3D vector (gp_Vec)"
  (x occt-real)
  (y occt-real)
  (z occt-real))

(defcstruct gp-ax1
  "Axis (gp_Ax1) - point and direction"
  (location (:struct gp-pnt))
  (direction (:struct gp-dir)))

(defcstruct gp-ax2
  "Coordinate system (gp_Ax2)"
  (location (:struct gp-pnt))
  (direction (:struct gp-dir))
  (x-direction (:struct gp-dir)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun make-gp-pnt (x y z)
  "Create a gp_Pnt point"
  (let ((pnt (foreign-alloc '(:struct gp-pnt))))
    (setf (foreign-slot-value pnt '(:struct gp-pnt) 'x) (coerce x 'double-float)
          (foreign-slot-value pnt '(:struct gp-pnt) 'y) (coerce y 'double-float)
          (foreign-slot-value pnt '(:struct gp-pnt) 'z) (coerce z 'double-float))
    pnt))

(defun make-gp-dir (x y z)
  "Create a normalized gp_Dir direction vector"
  (let* ((len (sqrt (+ (* x x) (* y y) (* z z))))
         (dir (foreign-alloc '(:struct gp-dir))))
    (setf (foreign-slot-value dir '(:struct gp-dir) 'x) (coerce (/ x len) 'double-float)
          (foreign-slot-value dir '(:struct gp-dir) 'y) (coerce (/ y len) 'double-float)
          (foreign-slot-value dir '(:struct gp-dir) 'z) (coerce (/ z len) 'double-float))
    dir))

(defun make-gp-vec (x y z)
  "Create a gp_Vec vector"
  (let ((vec (foreign-alloc '(:struct gp-vec))))
    (setf (foreign-slot-value vec '(:struct gp-vec) 'x) (coerce x 'double-float)
          (foreign-slot-value vec '(:struct gp-vec) 'y) (coerce y 'double-float)
          (foreign-slot-value vec '(:struct gp-vec) 'z) (coerce z 'double-float))
    vec))
