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

;;; ============================================================================
;;; Point accessor functions
;;; ============================================================================

(defun point-x (pnt)
  "Get X coordinate from gp_Pnt pointer"
  (foreign-slot-value pnt '(:struct gp-pnt) 'x))

(defun point-y (pnt)
  "Get Y coordinate from gp_Pnt pointer"
  (foreign-slot-value pnt '(:struct gp-pnt) 'y))

(defun point-z (pnt)
  "Get Z coordinate from gp_Pnt pointer"
  (foreign-slot-value pnt '(:struct gp-pnt) 'z))

;;; ============================================================================
;;; Stub implementations for thread geometry FFI
;;; These functions provide minimal implementations to allow loading.
;;; Full OCCT implementations require C++ wrapper bindings.
;;; ============================================================================

(defun make-edge-from-points (p1 p2)
  "Create edge from two points. STUB: Returns placeholder handle.
   TODO: Implement via BRepBuilderAPI_MakeEdge in C++ wrapper."
  (declare (ignore p1 p2))
  (make-occt-handle (null-pointer) :type :edge :inc-ref nil))

(defun make-wire-from-edges (edges)
  "Create wire from list of edges. STUB: Returns placeholder handle.
   TODO: Implement via BRepBuilderAPI_MakeWire in C++ wrapper."
  (declare (ignore edges))
  (make-occt-handle (null-pointer) :type :wire :inc-ref nil))

(defun make-bspline-curve-through-points (points)
  "Create B-spline curve through points. STUB: Returns placeholder handle.
   TODO: Implement via GeomAPI_PointsToBSpline in C++ wrapper."
  (declare (ignore points))
  (make-occt-handle (null-pointer) :type :curve :inc-ref nil))

(defun make-edge-from-curve (curve)
  "Create edge from curve. STUB: Returns placeholder handle.
   TODO: Implement via BRepBuilderAPI_MakeEdge in C++ wrapper."
  (declare (ignore curve))
  (make-occt-handle (null-pointer) :type :edge :inc-ref nil))

(defun get-curve-start-point (curve)
  "Get start point of curve. STUB: Returns origin point.
   TODO: Implement via Geom_Curve::Value in C++ wrapper."
  (declare (ignore curve))
  (make-gp-pnt 0.0d0 0.0d0 0.0d0))

(defun get-curve-end-point (curve)
  "Get end point of curve. STUB: Returns origin point.
   TODO: Implement via Geom_Curve::Value in C++ wrapper."
  (declare (ignore curve))
  (make-gp-pnt 0.0d0 0.0d0 0.0d0))

(defun evaluate-curve-at (curve param)
  "Evaluate curve at parameter. STUB: Returns origin point.
   TODO: Implement via Geom_Curve::Value in C++ wrapper."
  (declare (ignore curve param))
  (make-gp-pnt 0.0d0 0.0d0 0.0d0))

(defun is-valid-shape (shape-handle)
  "Check if shape is valid. STUB: Returns T if handle is non-null.
   TODO: Implement via BRepCheck_Analyzer in C++ wrapper."
  (and shape-handle
       (handle-valid-p shape-handle)
       (not (handle-null-p shape-handle))))

(defun is-closed-wire (wire-handle)
  "Check if wire is closed. STUB: Returns NIL.
   TODO: Implement via BRep_Tool::IsClosed in C++ wrapper."
  (declare (ignore wire-handle))
  nil)

(defun count-edges (wire-handle)
  "Count edges in a wire. STUB: Returns 0.
   TODO: Implement via TopExp_Explorer in C++ wrapper."
  (declare (ignore wire-handle))
  0)

(defun get-curve-properties (curve)
  "Get curve properties. STUB: Returns empty plist.
   TODO: Implement to return (:length :start :end :type) properties."
  (declare (ignore curve))
  (list :length 0.0d0 :type :unknown))

(defun get-shape-type (shape-handle)
  "Get the shape type. STUB: Returns :solid.
   TODO: Implement via TopAbs_ShapeEnum in C++ wrapper."
  (declare (ignore shape-handle))
  :solid)

(defun get-volume (shape-handle)
  "Get volume of a shape. Alias for ffi-get-volume."
  (ffi-get-volume shape-handle))

(defun get-bounding-box (shape-handle)
  "Get bounding box of a shape. Alias for ffi-get-bounding-box."
  (ffi-get-bounding-box shape-handle))

(defun has-self-intersections (shape-handle)
  "Check if shape has self-intersections. STUB: Returns NIL.
   TODO: Implement via BRepAlgo_SelfIntersect in C++ wrapper."
  (declare (ignore shape-handle))
  nil)

(defun make-pipe (profile path &key use-frenet build-solid)
  "Create pipe shape by sweeping profile along path. STUB: Returns placeholder handle.
   TODO: Implement via BRepOffsetAPI_MakePipe in C++ wrapper."
  (declare (ignore profile path use-frenet build-solid))
  (make-occt-handle (null-pointer) :type :solid :inc-ref nil))

(defun is-closed-solid (shape-handle)
  "Check if solid shape is closed (watertight). STUB: Returns T.
   TODO: Implement via BRepLib::IsClosed in C++ wrapper."
  (declare (ignore shape-handle))
  t)
