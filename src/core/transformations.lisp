;;;; src/core/transformations.lisp --- Functional core for transformations

(in-package :clad.core)

;;; ============================================================================
;;; Geometric Transformations
;;; ============================================================================

(defun translate (shape dx dy dz)
  "Translate shape by vector (dx, dy, dz).

  Arguments:
    shape  - Shape to translate
    dx, dy, dz - Translation vector in mm

  Returns: New translated shape (original unchanged)

  Example:
    (translate box 10 0 0)      ; Move 10mm in +X direction
    (translate part 0 0 -5)     ; Move 5mm in -Z direction"
  (ensure-shape shape)
  (let ((result-handle (clad.ffi:ffi-translate
                        (shape-handle shape)
                        dx dy dz)))
    (make-shape result-handle
                :metadata (shape-metadata shape))))

(defun rotate (shape axis angle &key (center-x 0) (center-y 0) (center-z 0))
  "Rotate shape around an axis.

  Arguments:
    shape  - Shape to rotate
    axis   - Rotation axis: :x, :y, :z, or vector (x y z)
    angle  - Rotation angle in degrees
    center-x, center-y, center-z - Rotation center (default: origin)

  Returns: New rotated shape

  Example:
    (rotate box :z 45)                    ; Rotate 45° around Z axis
    (rotate part '(1 1 0) 90)            ; Rotate around diagonal axis
    (rotate gear :z 15 :center-x 10 :center-y 10) ; Rotate around point"
  (ensure-shape shape)

  ;; Parse axis specification
  (multiple-value-bind (axis-x axis-y axis-z)
      (etypecase axis
        (keyword
         (case axis
           (:x (values 1 0 0))
           (:y (values 0 1 0))
           (:z (values 0 0 1))
           (t (error "Unknown axis keyword: ~A (use :x, :y, or :z)" axis))))
        (list
         (destructuring-bind (x y z) axis
           (values x y z))))

    ;; If rotation center is not origin, translate, rotate, translate back
    (let ((shape-to-rotate
           (if (or (not (zerop center-x))
                   (not (zerop center-y))
                   (not (zerop center-z)))
               (translate shape (- center-x) (- center-y) (- center-z))
               shape)))

      (let* ((rotated-handle (clad.ffi:ffi-rotate
                              (shape-handle shape-to-rotate)
                              axis-x axis-y axis-z
                              angle))
             (rotated-shape (make-shape rotated-handle
                                       :metadata (shape-metadata shape))))

        (if (or (not (zerop center-x))
                (not (zerop center-y))
                (not (zerop center-z)))
            (translate rotated-shape center-x center-y center-z)
            rotated-shape)))))

(defun mirror (shape plane-normal &key (plane-origin-x 0) (plane-origin-y 0) (plane-origin-z 0))
  "Mirror shape across a plane.

  Arguments:
    shape        - Shape to mirror
    plane-normal - Plane normal: :x, :y, :z, or vector (x y z)
    plane-origin-x, plane-origin-y, plane-origin-z - Point on plane (default: origin)

  Returns: New mirrored shape

  Example:
    (mirror part :z)                    ; Mirror across XY plane
    (mirror gear '(1 0 1))             ; Mirror across diagonal plane"
  (ensure-shape shape)

  ;; Parse plane normal specification
  (multiple-value-bind (normal-x normal-y normal-z)
      (etypecase plane-normal
        (keyword
         (case plane-normal
           (:x (values 1 0 0))
           (:y (values 0 1 0))
           (:z (values 0 0 1))
           (t (error "Unknown plane normal: ~A (use :x, :y, or :z)" plane-normal))))
        (list
         (destructuring-bind (x y z) plane-normal
           (values x y z))))

    ;; For now, we only support mirroring through origin
    ;; TODO: Implement plane-origin offset
    (when (or (not (zerop plane-origin-x))
              (not (zerop plane-origin-y))
              (not (zerop plane-origin-z)))
      (warn "Mirror plane origin offset not yet implemented, using origin"))

    (let ((result-handle (clad.ffi:ffi-mirror
                          (shape-handle shape)
                          normal-x normal-y normal-z)))
      (make-shape result-handle
                  :metadata (shape-metadata shape)))))

(defun scale-shape (shape factor &key (center-x 0) (center-y 0) (center-z 0))
  "Scale shape uniformly by factor.

  Arguments:
    shape  - Shape to scale
    factor - Scale factor (1.0 = no change, 2.0 = double size, 0.5 = half size)
    center-x, center-y, center-z - Scaling center (default: origin)

  Returns: New scaled shape

  Example:
    (scale-shape box 2.0)              ; Double the size
    (scale-shape part 0.5)             ; Half the size
    (scale-shape gear 1.1 :center-x 10) ; Scale from offset center"
  (ensure-shape shape)

  ;; If scaling center is not origin, translate, scale, translate back
  (let ((shape-to-scale
         (if (or (not (zerop center-x))
                 (not (zerop center-y))
                 (not (zerop center-z)))
             (translate shape (- center-x) (- center-y) (- center-z))
             shape)))

    (let* ((scaled-handle (clad.ffi:ffi-scale
                           (shape-handle shape-to-scale)
                           factor))
           (scaled-shape (make-shape scaled-handle
                                    :metadata (shape-metadata shape))))

      (if (or (not (zerop center-x))
              (not (zerop center-y))
              (not (zerop center-z)))
          (translate scaled-shape (* center-x factor)
                                 (* center-y factor)
                                 (* center-z factor))
          scaled-shape))))
