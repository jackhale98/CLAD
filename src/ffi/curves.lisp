;;;; src/ffi/curves.lisp --- FFI bindings for curve operations (Phase 8)

(in-package :clad.ffi)

;;; ============================================================================
;;; OCCT API Bindings
;;; ============================================================================

(defcfun ("occt_make_interpolated_curve" %occt-make-interpolated-curve) :int
  "Create interpolated B-spline curve through points"
  (points :pointer)              ; Array of doubles (x,y,z triplets)
  (num-points :int)
  (closed :int)                  ; 1 for closed, 0 for open
  (out-edge :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_bezier_curve" %occt-make-bezier-curve) :int
  "Create Bezier curve from control points"
  (control-points :pointer)      ; Array of doubles (x,y,z triplets)
  (num-points :int)
  (out-edge :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_arc_3points" %occt-make-arc-3points) :int
  "Create circular arc through 3 points"
  (x1 :double) (y1 :double) (z1 :double)
  (x2 :double) (y2 :double) (z2 :double)
  (x3 :double) (y3 :double) (z3 :double)
  (out-edge :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_arc_center_radius" %occt-make-arc-center-radius) :int
  "Create circular arc by center, radius, and angles"
  (cx :double) (cy :double) (cz :double)
  (radius :double)
  (start-angle :double)
  (end-angle :double)
  (axis-x :double) (axis-y :double) (axis-z :double)
  (out-edge :pointer)
  (err-msg :pointer))

(defcfun ("occt_make_wire" %occt-make-wire) :int
  "Create wire from connected edges"
  (edge-handles :pointer)        ; Array of edge handles
  (num-edges :int)
  (out-wire :pointer)
  (err-msg :pointer))

(defcfun ("occt_wire_is_closed" %occt-wire-is-closed) :int
  "Check if wire is closed"
  (wire-handle :pointer)
  (is-closed :pointer)           ; Output: 1=closed, 0=open
  (err-msg :pointer))

;;; ============================================================================
;;; Helper Functions for Point Arrays
;;; ============================================================================

(defun points-to-foreign-array (points)
  "Convert list of points to foreign double array.

  Args:
    points - List of points: ((x1 y1 z1) (x2 y2 z2) ...)

  Returns: Foreign array pointer (must be freed by caller)

  Example:
    (points-to-foreign-array '((0 0 0) (10 5 0) (20 0 0)))"

  (let* ((num-points (length points))
         (array (foreign-alloc :double :count (* num-points 3))))
    (loop for (x y z) in points
          for i from 0 by 3
          do (setf (mem-aref array :double i) (coerce x 'double-float))
             (setf (mem-aref array :double (+ i 1)) (coerce y 'double-float))
             (setf (mem-aref array :double (+ i 2)) (coerce z 'double-float)))
    array))

;;; ============================================================================
;;; High-Level FFI Functions - Splines
;;; ============================================================================

(defun ffi-make-interpolated-curve (points &key (closed nil))
  "Create interpolated B-spline curve through points.

  Args:
    points - List of points: ((x1 y1 z1) (x2 y2 z2) ...)
    closed - T for closed curve, NIL for open curve

  Returns: Edge handle (occt-handle)

  Signals: occt-error on failure

  This creates a smooth B-spline curve that passes through all given points.
  The curve is automatically parameterized for smooth interpolation."

  (unless (>= (length points) 2)
    (error 'occt-domain-error
           :message (format nil "Need at least 2 points for curve, got ~A"
                            (length points))))

  (if *occt-available-p*
      ;; Real OCCT implementation
      (let ((pts-array (points-to-foreign-array points)))
        (unwind-protect
            (with-foreign-objects ((edge-ptr :pointer)
                                   (err-ptr :pointer))
              (let ((result-code (%occt-make-interpolated-curve
                                  pts-array
                                  (length points)
                                  (if closed 1 0)
                                  edge-ptr
                                  err-ptr)))
                (check-occt-result result-code "make-interpolated-curve")
                (make-occt-handle (mem-ref edge-ptr :pointer)
                                 :type :edge
                                 :inc-ref nil)))
          (foreign-free pts-array)))

      ;; Stub implementation
      (stub-interpolated-curve points closed)))

(defun ffi-make-bezier-curve (control-points)
  "Create Bezier curve from control points.

  Args:
    control-points - List of control points: ((x1 y1 z1) (x2 y2 z2) ...)
                    Bezier curves support 2-25 control points

  Returns: Edge handle (occt-handle)

  Signals: occt-error on failure

  Bezier curves are defined by control points. The curve starts at the first
  point and ends at the last point, but does not pass through intermediate
  control points (except in special cases)."

  (let ((num-points (length control-points)))
    (unless (and (>= num-points 2) (<= num-points 25))
      (error 'occt-domain-error
             :message (format nil "Bezier curve requires 2-25 control points, got ~A"
                              num-points))))

  (if *occt-available-p*
      ;; Real OCCT implementation
      (let ((pts-array (points-to-foreign-array control-points)))
        (unwind-protect
            (with-foreign-objects ((edge-ptr :pointer)
                                   (err-ptr :pointer))
              (let ((result-code (%occt-make-bezier-curve
                                  pts-array
                                  (length control-points)
                                  edge-ptr
                                  err-ptr)))
                (check-occt-result result-code "make-bezier-curve")
                (make-occt-handle (mem-ref edge-ptr :pointer)
                                 :type :edge
                                 :inc-ref nil)))
          (foreign-free pts-array)))

      ;; Stub implementation
      (stub-bezier-curve control-points)))

;;; ============================================================================
;;; High-Level FFI Functions - Arcs
;;; ============================================================================

(defun ffi-make-arc-3points (p1 p2 p3)
  "Create circular arc through three points.

  Args:
    p1 - First point (x y z)
    p2 - Second point (x y z)
    p3 - Third point (x y z)

  Returns: Edge handle (occt-handle)

  Signals: occt-error on failure

  Creates a circular arc passing through the three points.
  The points must not be collinear."

  (destructuring-bind (x1 y1 z1) p1
    (destructuring-bind (x2 y2 z2) p2
      (destructuring-bind (x3 y3 z3) p3
        (if *occt-available-p*
            ;; Real OCCT implementation
            (with-foreign-objects ((edge-ptr :pointer)
                                   (err-ptr :pointer))
              (let ((result-code (%occt-make-arc-3points
                                  (coerce x1 'double-float)
                                  (coerce y1 'double-float)
                                  (coerce z1 'double-float)
                                  (coerce x2 'double-float)
                                  (coerce y2 'double-float)
                                  (coerce z2 'double-float)
                                  (coerce x3 'double-float)
                                  (coerce y3 'double-float)
                                  (coerce z3 'double-float)
                                  edge-ptr
                                  err-ptr)))
                (check-occt-result result-code "make-arc-3points")
                (make-occt-handle (mem-ref edge-ptr :pointer)
                                 :type :edge
                                 :inc-ref nil)))

            ;; Stub implementation
            (stub-arc-3points p1 p2 p3))))))

(defun ffi-make-arc-center-radius (center radius start-angle end-angle
                                    &key (axis '(0 0 1)))
  "Create circular arc by center, radius, and angles.

  Args:
    center - Center point (x y z)
    radius - Arc radius (must be positive)
    start-angle - Start angle in degrees
    end-angle - End angle in degrees
    axis - Rotation axis (default: Z axis '(0 0 1))

  Returns: Edge handle (occt-handle)

  Signals: occt-error on failure

  Creates a circular arc on a plane perpendicular to the axis,
  centered at the given point."

  (unless (plusp radius)
    (error 'occt-domain-error
           :message (format nil "Arc radius must be positive, got ~A" radius)))

  (destructuring-bind (cx cy cz) center
    (destructuring-bind (ax ay az) axis
      (if *occt-available-p*
          ;; Real OCCT implementation
          (with-foreign-objects ((edge-ptr :pointer)
                                 (err-ptr :pointer))
            (let ((result-code (%occt-make-arc-center-radius
                                (coerce cx 'double-float)
                                (coerce cy 'double-float)
                                (coerce cz 'double-float)
                                (coerce radius 'double-float)
                                (coerce start-angle 'double-float)
                                (coerce end-angle 'double-float)
                                (coerce ax 'double-float)
                                (coerce ay 'double-float)
                                (coerce az 'double-float)
                                edge-ptr
                                err-ptr)))
              (check-occt-result result-code "make-arc-center-radius")
              (make-occt-handle (mem-ref edge-ptr :pointer)
                               :type :edge
                               :inc-ref nil)))

          ;; Stub implementation
          (stub-arc-center-radius center radius start-angle end-angle axis)))))

;;; ============================================================================
;;; High-Level FFI Functions - Wires
;;; ============================================================================

(defun ffi-make-wire (edge-handles)
  "Create wire from connected edges.

  Args:
    edge-handles - List of edge occt-handles (must form connected path)

  Returns: Wire handle (occt-handle)

  Signals: occt-error on failure

  Creates a wire by connecting edges. The edges must form a continuous path
  (endpoint of one edge connects to startpoint of next edge)."

  (unless (plusp (length edge-handles))
    (error 'occt-domain-error
           :message "Need at least one edge to make wire"))

  (if *occt-available-p*
      ;; Real OCCT implementation
      (with-foreign-object (edge-array :pointer (length edge-handles))
        ;; Fill array with edge handles
        (loop for edge-handle in edge-handles
              for i from 0
              do (setf (mem-aref edge-array :pointer i)
                      (handle-ptr edge-handle)))

        (with-foreign-objects ((wire-ptr :pointer)
                               (err-ptr :pointer))
          (let ((result-code (%occt-make-wire
                              edge-array
                              (length edge-handles)
                              wire-ptr
                              err-ptr)))
            (check-occt-result result-code "make-wire")
            (make-occt-handle (mem-ref wire-ptr :pointer)
                             :type :wire
                             :inc-ref nil))))

      ;; Stub implementation
      (stub-make-wire edge-handles)))

(defun ffi-wire-is-closed (wire-handle)
  "Check if wire is closed.

  Args:
    wire-handle - Wire occt-handle

  Returns: T if wire is closed, NIL if open

  Signals: occt-error on failure"

  (if *occt-available-p*
      ;; Real OCCT implementation
      (with-foreign-objects ((closed-ptr :int)
                             (err-ptr :pointer))
        (let ((result-code (%occt-wire-is-closed
                            (handle-ptr wire-handle)
                            closed-ptr
                            err-ptr)))
          (check-occt-result result-code "wire-is-closed")
          (not (zerop (mem-ref closed-ptr :int)))))

      ;; Stub implementation
      (stub-wire-is-closed wire-handle)))

;;; ============================================================================
;;; Stub Implementations (for testing without OCCT)
;;; ============================================================================

(defun stub-interpolated-curve (points closed)
  "Stub implementation of interpolated curve."
  (declare (ignore points closed))
  (format *error-output*
          "~&;; STUB: interpolated curve through ~A points (OCCT not available)~%"
          (length points))
  ;; Return a dummy handle
  (make-occt-handle (cffi:null-pointer) :type :edge :inc-ref nil))

(defun stub-bezier-curve (control-points)
  "Stub implementation of Bezier curve."
  (declare (ignore control-points))
  (format *error-output*
          "~&;; STUB: Bezier curve with ~A control points (OCCT not available)~%"
          (length control-points))
  ;; Return a dummy handle
  (make-occt-handle (cffi:null-pointer) :type :edge :inc-ref nil))

(defun stub-arc-3points (p1 p2 p3)
  "Stub implementation of arc through 3 points."
  (declare (ignore p1 p2 p3))
  (format *error-output*
          "~&;; STUB: arc through 3 points (OCCT not available)~%")
  ;; Return a dummy handle
  (make-occt-handle (cffi:null-pointer) :type :edge :inc-ref nil))

(defun stub-arc-center-radius (center radius start-angle end-angle axis)
  "Stub implementation of arc by center/radius."
  (declare (ignore center radius start-angle end-angle axis))
  (format *error-output*
          "~&;; STUB: arc by center/radius (OCCT not available)~%")
  ;; Return a dummy handle
  (make-occt-handle (cffi:null-pointer) :type :edge :inc-ref nil))

(defun stub-make-wire (edge-handles)
  "Stub implementation of wire creation."
  (declare (ignore edge-handles))
  (format *error-output*
          "~&;; STUB: wire from ~A edges (OCCT not available)~%"
          (length edge-handles))
  ;; Return a dummy handle
  (make-occt-handle (cffi:null-pointer) :type :wire :inc-ref nil))

(defun stub-wire-is-closed (wire-handle)
  "Stub implementation of wire-is-closed check."
  (declare (ignore wire-handle))
  (format *error-output*
          "~&;; STUB: wire-is-closed check (OCCT not available)~%")
  ;; Assume open for stub
  nil)
