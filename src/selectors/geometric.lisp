;;;; src/selectors/geometric.lisp --- Geometric selectors (parallel, perpendicular)

(in-package :clad.selectors)

;;; ============================================================================
;;; Parallel Selector
;;; ============================================================================

(defclass parallel-selector (base-selector)
  ((axis :initarg :axis
         :accessor selector-axis
         :type keyword
         :documentation "Axis keyword: :x, :y, or :z"))
  (:documentation "Selector for finding faces parallel to an axis.

  A face is parallel to an axis if its normal vector is perpendicular
  to that axis (i.e., dot product ≈ 0).

  Examples:
    (:axis :z) => Selects top and bottom faces of a box"))

;;; ============================================================================
;;; Implementation
;;; ============================================================================

(defmethod apply-selector ((selector parallel-selector) shape-list)
  "Select faces or edges whose orientation is parallel to the specified axis.

  For faces:
    1. Get axis vector from keyword
    2. For each face, extract normal vector using FFI
    3. A face is parallel to axis Z if its normal is also parallel to Z
       (i.e., abs(dot product) ≈ 1)

  For edges:
    1. Get axis vector from keyword
    2. For each edge, check bounding box
    3. An edge is parallel to Z if it has extent in Z but not in X or Y
       (i.e., bbox has xmin≈xmax and ymin≈ymax, but zmin<zmax)"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let* ((axis-kw (selector-axis selector))
         (tolerance (selector-tolerance selector)))

    ;; Get axis vector
    (multiple-value-bind (ax ay az)
        (axis-keyword-to-vector axis-kw)

      ;; Check if we're working with faces or edges
      (let ((first-shape (first shape-list)))
        (cond
          ;; Handle faces using normal vector
          ((clad.shapes:face-p first-shape)
           (remove-if-not
            (lambda (face)
              (handler-case
                  (multiple-value-bind (nx ny nz)
                      (ffi-get-face-normal
                       (clad.core:shape-handle (clad.shapes:unwrap-shape face)))

                    ;; Dot product of normal and axis
                    ;; If abs(dot product) ≈ 1, normal is parallel to axis,
                    ;; which means face is parallel to axis
                    (let ((dot (dot-product nx ny nz ax ay az)))
                      (> (abs dot) (- 1.0d0 tolerance))))

                ;; If we can't get normal, exclude this face
                (error () nil)))
            shape-list))

          ;; Handle edges using bounding box
          ((clad.shapes:edge-p first-shape)
           (remove-if-not
            (lambda (edge)
              (handler-case
                  (let* ((bbox (clad.shapes:bounding-box edge))
                         (xmin (nth 0 bbox))
                         (ymin (nth 1 bbox))
                         (zmin (nth 2 bbox))
                         (xmax (nth 3 bbox))
                         (ymax (nth 4 bbox))
                         (zmax (nth 5 bbox))
                         ;; Calculate extents in each direction
                         (x-extent (abs (- xmax xmin)))
                         (y-extent (abs (- ymax ymin)))
                         (z-extent (abs (- zmax zmin)))
                         ;; Determine which direction has the maximum extent
                         (max-extent (max x-extent y-extent z-extent)))

                    ;; Edge is parallel to X if x-extent is dominant and y,z extents are ~0
                    ;; Edge is parallel to Y if y-extent is dominant and x,z extents are ~0
                    ;; Edge is parallel to Z if z-extent is dominant and x,y extents are ~0
                    (cond
                      ((and (> (abs ax) 0.5d0)  ; Checking for X axis
                            (> x-extent (* 0.5d0 max-extent))  ; X has significant extent
                            (< y-extent tolerance)  ; Y extent is negligible
                            (< z-extent tolerance))  ; Z extent is negligible
                       t)
                      ((and (> (abs ay) 0.5d0)  ; Checking for Y axis
                            (> y-extent (* 0.5d0 max-extent))  ; Y has significant extent
                            (< x-extent tolerance)  ; X extent is negligible
                            (< z-extent tolerance))  ; Z extent is negligible
                       t)
                      ((and (> (abs az) 0.5d0)  ; Checking for Z axis
                            (> z-extent (* 0.5d0 max-extent))  ; Z has significant extent
                            (< x-extent tolerance)  ; X extent is negligible
                            (< y-extent tolerance))  ; Y extent is negligible
                       t)
                      (t nil)))

                ;; If we can't get bbox, exclude this edge
                (error () nil)))
            shape-list))

          ;; Unknown shape type
          (t nil))))))

;;; ============================================================================
;;; Perpendicular Selector
;;; ============================================================================

(defclass perpendicular-selector (base-selector)
  ((axis :initarg :axis
         :accessor selector-axis
         :type keyword
         :documentation "Axis keyword: :x, :y, or :z"))
  (:documentation "Selector for finding faces perpendicular to an axis.

  A face is perpendicular to an axis if its normal vector is parallel
  to that axis (i.e., abs(dot product) ≈ 0).

  Examples:
    (:axis :z) => Selects the 4 side faces of a box (not top/bottom)"))

(defmethod apply-selector ((selector perpendicular-selector) shape-list)
  "Select faces whose normals are perpendicular to the specified axis.

  Algorithm:
    1. Get axis vector from keyword
    2. For each face, extract normal vector using FFI
    3. A face is perpendicular to axis Z if its normal is perpendicular to Z
       (i.e., abs(dot product) ≈ 0)
    4. Return all matching faces"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let* ((axis-kw (selector-axis selector))
         (tolerance (selector-tolerance selector)))

    ;; Get axis vector
    (multiple-value-bind (ax ay az)
        (axis-keyword-to-vector axis-kw)

      ;; Filter faces by checking if normal is perpendicular to axis
      (remove-if-not
       (lambda (face)
         (handler-case
             (multiple-value-bind (nx ny nz)
                 (ffi-get-face-normal
                  (clad.core:shape-handle (clad.shapes:unwrap-shape face)))

               ;; Dot product of normal and axis
               ;; If abs(dot product) ≈ 0, normal is perpendicular to axis,
               ;; which means face is perpendicular to axis
               (let ((dot (dot-product nx ny nz ax ay az)))
                 (< (abs dot) tolerance)))

           ;; If we can't get normal, exclude this face
           (error () nil)))
       shape-list))))
