;;;; src/core/curves.lisp --- Pure functional API for curves and splines (Phase 8)

(in-package :clad.core)

;;; ============================================================================
;;; Spline Curves
;;; ============================================================================

(defun make-spline (points &key (closed nil))
  "Create a B-spline curve interpolating through points.

  Args:
    points - List of 3D points: ((x1 y1 z1) (x2 y2 z2) ...)
    closed - T for closed curve, NIL for open curve (default: NIL)

  Returns: Edge shape representing the spline curve

  Examples:
    ;; Simple curved path
    (make-spline '((0 0 0) (10 5 0) (20 8 0) (30 5 0)))

    ;; Closed profile
    (make-spline '((0 0 0) (10 0 0) (10 10 0) (0 10 0))
                 :closed t)

  The spline passes through all given points with smooth interpolation.
  This is ideal for creating smooth curves from discrete point data."

  (unless (listp points)
    (error "Points must be a list of (x y z) coordinates"))

  (unless (>= (length points) 2)
    (error "Need at least 2 points to create spline, got ~A" (length points)))

  ;; Validate point format
  (dolist (pt points)
    (unless (and (listp pt) (= (length pt) 3))
      (error "Each point must be a list of 3 coordinates: ~A" pt)))

  ;; Create spline via FFI
  (let ((edge-handle (clad.ffi:ffi-make-interpolated-curve points :closed closed)))
    ;; Return core shape
    (make-shape edge-handle :metadata '(:type :spline))))

(defun make-bezier (control-points)
  "Create a Bezier curve from control points.

  Args:
    control-points - List of control points: ((x1 y1 z1) (x2 y2 z2) ...)
                    Bezier curves support 2-25 control points

  Returns: Edge shape representing the Bezier curve

  Examples:
    ;; Quadratic Bezier (3 control points)
    (make-bezier '((0 0 0) (5 10 0) (10 0 0)))

    ;; Cubic Bezier (4 control points)
    (make-bezier '((0 0 0) (3 5 0) (7 5 0) (10 0 0)))

  Bezier curves start at the first point and end at the last point,
  but do not pass through intermediate control points. The control
  points define the curve's shape."

  (unless (listp control-points)
    (error "Control points must be a list of (x y z) coordinates"))

  (let ((num-points (length control-points)))
    (unless (and (>= num-points 2) (<= num-points 25))
      (error "Bezier curve requires 2-25 control points, got ~A" num-points)))

  ;; Validate point format
  (dolist (pt control-points)
    (unless (and (listp pt) (= (length pt) 3))
      (error "Each control point must be a list of 3 coordinates: ~A" pt)))

  ;; Create Bezier curve via FFI
  (let ((edge-handle (clad.ffi:ffi-make-bezier-curve control-points)))
    (make-shape edge-handle :metadata '(:type :bezier))))

;;; ============================================================================
;;; Arc Curves
;;; ============================================================================

(defun make-arc-3points (p1 p2 p3)
  "Create circular arc through three points.

  Args:
    p1 - First point (x y z)
    p2 - Second point (x y z)
    p3 - Third point (x y z)

  Returns: Edge shape representing the arc

  Example:
    ;; Arc in XY plane
    (make-arc-3points '(0 0 0) '(5 5 0) '(10 0 0))

    ;; Arc in 3D space
    (make-arc-3points '(0 0 0) '(5 5 5) '(10 0 10))

  Creates a circular arc passing through all three points.
  The points must not be collinear."

  ;; Validate points
  (dolist (pt (list p1 p2 p3))
    (unless (and (listp pt) (= (length pt) 3))
      (error "Each point must be a list of 3 coordinates: ~A" pt)))

  ;; Create arc via FFI
  (let ((edge-handle (clad.ffi:ffi-make-arc-3points p1 p2 p3)))
    (make-shape edge-handle :metadata '(:type :arc))))

(defun make-arc (center radius start-angle end-angle &key (axis '(0 0 1)))
  "Create circular arc by center, radius, and angles.

  Args:
    center - Center point (x y z)
    radius - Arc radius (must be positive)
    start-angle - Start angle in degrees
    end-angle - End angle in degrees
    axis - Rotation axis (default: Z axis '(0 0 1))

  Returns: Edge shape representing the arc

  Examples:
    ;; Quarter circle in XY plane
    (make-arc '(0 0 0) 10 0 90)

    ;; Semi-circle in XY plane
    (make-arc '(50 50 0) 20 0 180)

    ;; Arc in YZ plane
    (make-arc '(0 0 0) 15 0 90 :axis '(1 0 0))

  Creates a circular arc on a plane perpendicular to the axis,
  centered at the given point, from start-angle to end-angle."

  ;; Validate inputs
  (unless (and (listp center) (= (length center) 3))
    (error "Center must be a list of 3 coordinates: ~A" center))

  (unless (and (listp axis) (= (length axis) 3))
    (error "Axis must be a list of 3 coordinates: ~A" axis))

  (unless (plusp radius)
    (error "Arc radius must be positive, got ~A" radius))

  ;; Create arc via FFI
  (let ((edge-handle (clad.ffi:ffi-make-arc-center-radius
                      center radius start-angle end-angle :axis axis)))
    (make-shape edge-handle :metadata '(:type :arc))))

;;; ============================================================================
;;; Wire Operations
;;; ============================================================================

(defun make-wire (edges)
  "Create wire from connected edges.

  Args:
    edges - List of edge shapes (must form connected path)

  Returns: Wire shape

  Example:
    (let* ((e1 (make-line '(0 0 0) '(10 0 0)))
           (e2 (make-arc-3points '(10 0 0) '(15 5 0) '(20 0 0)))
           (e3 (make-line '(20 0 0) '(20 10 0))))
      (make-wire (list e1 e2 e3)))

  Creates a wire by connecting edges. The edges must form a continuous
  path (endpoint of one edge connects to startpoint of next edge).
  Edges are automatically oriented to form a continuous wire."

  (unless (listp edges)
    (error "Edges must be a list of edge shapes"))

  (unless (plusp (length edges))
    (error "Need at least one edge to create wire"))

  ;; Extract handles from core shapes
  (let ((edge-handles (mapcar #'shape-handle edges)))
    ;; Create wire via FFI
    (let ((wire-handle (clad.ffi:ffi-make-wire edge-handles)))
      (make-shape wire-handle :metadata '(:type :wire)))))

(defun wire-closed-p (wire)
  "Check if wire is closed.

  Args:
    wire - Wire shape

  Returns: T if wire is closed, NIL if open

  Example:
    (let* ((pts '((0 0 0) (10 0 0) (10 10 0) (0 10 0)))
           (spline (make-spline pts :closed t))
           (wire (make-wire (list spline))))
      (wire-closed-p wire))  ; => T

  A wire is closed if its start point coincides with its end point."

  (unless (shape-p wire)
    (error "Argument must be a shape: ~A" wire))

  ;; Check via FFI
  (clad.ffi:ffi-wire-is-closed (shape-handle wire)))

;;; ============================================================================
;;; Helper: Line Creation (for wire building)
;;; ============================================================================

(defun make-line (p1 p2)
  "Create straight line edge between two points.

  Args:
    p1 - Start point (x y z)
    p2 - End point (x y z)

  Returns: Edge shape representing the line

  Example:
    (make-line '(0 0 0) '(10 0 0))

  This is a convenience function for creating straight line segments,
  particularly useful when building wires from mixed straight and curved edges."

  ;; Validate points
  (dolist (pt (list p1 p2))
    (unless (and (listp pt) (= (length pt) 3))
      (error "Each point must be a list of 3 coordinates: ~A" pt)))

  ;; For a line, we can use an arc with 3 collinear points
  ;; Or better: create via B-spline with degree 1
  (make-spline (list p1 p2) :closed nil))
