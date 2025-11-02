;;;; ffi/advanced-ops.lisp --- FFI bindings for advanced operations

(in-package :clad.ffi)

;;; ============================================================================
;;; Sweep Operations
;;; ============================================================================

(defcfun ("occt_make_pipe" %occt-make-pipe) :int
  "Sweep profile along path"
  (profile :pointer)
  (path :pointer)
  (out-shape :pointer)
  (out-error :pointer))

(defun ffi-make-pipe (profile path)
  "Sweep profile shape along path wire/edge.
   Returns new shape handle."
  (if *occt-available-p*
      (with-foreign-objects ((out-shape :pointer)
                             (out-error :pointer))
        (setf (mem-ref out-error :pointer) (null-pointer))

        (let ((result (%occt-make-pipe (handle-ptr profile)
                                        (handle-ptr path)
                                        out-shape
                                        out-error)))
          (check-occt-result result out-error)
          (make-occt-handle (mem-ref out-shape :pointer)
                           :type :shape
                           :inc-ref nil)))

      ;; Stub implementation
      (stub-make-pipe profile path)))

(defcfun ("occt_make_pipe_shell" %occt-make-pipe-shell) :int
  "Sweep circular profile along path"
  (path :pointer)
  (radius :double)
  (out-shape :pointer)
  (out-error :pointer))

(defun ffi-make-pipe-shell (path radius)
  "Sweep circular profile of given radius along path.
   Returns new shape handle."
  (if *occt-available-p*
      (with-foreign-objects ((out-shape :pointer)
                             (out-error :pointer))
        (setf (mem-ref out-error :pointer) (null-pointer))

        (let ((result (%occt-make-pipe-shell (handle-ptr path)
                                              (coerce radius 'double-float)
                                              out-shape
                                              out-error)))
          (check-occt-result result out-error)
          (make-occt-handle (mem-ref out-shape :pointer)
                           :type :shape
                           :inc-ref nil)))

      ;; Stub implementation
      (stub-make-pipe-shell path radius)))

;;; ============================================================================
;;; Loft Operations
;;; ============================================================================

(defcfun ("occt_make_loft" %occt-make-loft) :int
  "Create loft through sections"
  (sections :pointer)
  (num-sections :int)
  (solid :int)
  (ruled :int)
  (out-shape :pointer)
  (out-error :pointer))

(defun ffi-make-loft (sections &key (solid t) (ruled nil))
  "Create loft through list of section shapes.
   SECTIONS is a list of shape handles.
   SOLID t creates solid, nil creates shell.
   RULED t creates ruled surface, nil creates smooth.
   Returns new shape handle."
  (let ((num-sections (length sections)))
    (when (< num-sections 2)
      (error 'occt-construction-error
             :message "Need at least 2 sections for loft"))

    (if *occt-available-p*
        (with-foreign-objects ((sections-array :pointer num-sections)
                               (out-shape :pointer)
                               (out-error :pointer))
          (setf (mem-ref out-error :pointer) (null-pointer))

          ;; Fill array with section handles
          (loop for section in sections
                for i from 0
                do (setf (mem-aref sections-array :pointer i)
                         (handle-ptr section)))

          (let ((result (%occt-make-loft sections-array
                                         num-sections
                                         (if solid 1 0)
                                         (if ruled 1 0)
                                         out-shape
                                         out-error)))
            (check-occt-result result out-error)
            (make-occt-handle (mem-ref out-shape :pointer)
                             :type :shape
                             :inc-ref nil)))

        ;; Stub implementation
        (stub-make-loft sections solid ruled))))

;;; ============================================================================
;;; Shelling Operations
;;; ============================================================================

(defcfun ("occt_make_shell" %occt-make-shell) :int
  "Create hollow shell from solid"
  (solid :pointer)
  (faces-to-remove :pointer)
  (num-faces :int)
  (thickness :double)
  (tolerance :double)
  (out-shape :pointer)
  (out-error :pointer))

(defun ffi-make-shell (solid faces-to-remove thickness &optional (tolerance 1.0e-6))
  "Create hollow shell from solid by offsetting faces.
   SOLID is the shape handle to shell.
   FACES-TO-REMOVE is a list of face handles to remove (or NIL for none).
   THICKNESS is wall thickness (negative for inward, positive for outward).
   TOLERANCE is the operation tolerance.
   Returns new shape handle."
  (let ((num-faces (length faces-to-remove)))
    (if *occt-available-p*
        (if (zerop num-faces)
            ;; No faces to remove
            (with-foreign-objects ((out-shape :pointer)
                                   (out-error :pointer))
              (setf (mem-ref out-error :pointer) (null-pointer))

              (let ((result (%occt-make-shell (handle-ptr solid)
                                               (null-pointer)
                                               0
                                               (coerce thickness 'double-float)
                                               (coerce tolerance 'double-float)
                                               out-shape
                                               out-error)))
                (check-occt-result result out-error)
                (make-occt-handle (mem-ref out-shape :pointer)
                                 :type :shape
                                 :inc-ref nil)))

            ;; Have faces to remove
            (with-foreign-objects ((faces-array :pointer num-faces)
                                   (out-shape :pointer)
                                   (out-error :pointer))
              (setf (mem-ref out-error :pointer) (null-pointer))

              ;; Fill array with face handles
              (loop for face in faces-to-remove
                    for i from 0
                    do (setf (mem-aref faces-array :pointer i)
                             (handle-ptr face)))

              (let ((result (%occt-make-shell (handle-ptr solid)
                                               faces-array
                                               num-faces
                                               (coerce thickness 'double-float)
                                               (coerce tolerance 'double-float)
                                               out-shape
                                               out-error)))
                (check-occt-result result out-error)
                (make-occt-handle (mem-ref out-shape :pointer)
                                 :type :shape
                                 :inc-ref nil))))

        ;; Stub implementation
        (stub-make-shell solid faces-to-remove thickness tolerance))))

;;; ============================================================================
;;; Mirroring Operations
;;; ============================================================================

(defcfun ("occt_mirror_shape" %occt-mirror-shape) :int
  "Mirror shape across plane"
  (shape :pointer)
  (plane-ox :double)
  (plane-oy :double)
  (plane-oz :double)
  (plane-nx :double)
  (plane-ny :double)
  (plane-nz :double)
  (out-shape :pointer)
  (out-error :pointer))

(defun ffi-mirror-shape (shape plane-origin plane-normal)
  "Mirror shape across a plane.
   SHAPE is the shape handle to mirror.
   PLANE-ORIGIN is (x y z) point on plane.
   PLANE-NORMAL is (nx ny nz) normal vector of plane.
   Returns new mirrored shape handle."
  (destructuring-bind (ox oy oz) plane-origin
    (destructuring-bind (nx ny nz) plane-normal
      (if *occt-available-p*
          (with-foreign-objects ((out-shape :pointer)
                                 (out-error :pointer))
            (setf (mem-ref out-error :pointer) (null-pointer))

            (let ((result (%occt-mirror-shape (handle-ptr shape)
                                               (coerce ox 'double-float)
                                               (coerce oy 'double-float)
                                               (coerce oz 'double-float)
                                               (coerce nx 'double-float)
                                               (coerce ny 'double-float)
                                               (coerce nz 'double-float)
                                               out-shape
                                               out-error)))
              (check-occt-result result out-error)
              (make-occt-handle (mem-ref out-shape :pointer)
                               :type :shape
                               :inc-ref nil)))

          ;; Stub implementation
          (stub-mirror-shape shape plane-origin plane-normal)))))

;;; ============================================================================
;;; Stub Implementations (for testing without OCCT)
;;; ============================================================================

(defun stub-make-pipe (profile path)
  "Stub implementation of pipe sweep."
  (declare (ignore profile path))
  (format *error-output*
          "~&;; STUB: pipe sweep (OCCT not available)~%")
  (make-occt-handle (cffi:null-pointer) :type :shape :inc-ref nil))

(defun stub-make-pipe-shell (path radius)
  "Stub implementation of pipe shell."
  (declare (ignore path radius))
  (format *error-output*
          "~&;; STUB: pipe shell with radius ~A (OCCT not available)~%"
          radius)
  (make-occt-handle (cffi:null-pointer) :type :shape :inc-ref nil))

(defun stub-make-loft (sections solid ruled)
  "Stub implementation of loft."
  (declare (ignore sections solid ruled))
  (format *error-output*
          "~&;; STUB: loft through ~A sections (OCCT not available)~%"
          (length sections))
  (make-occt-handle (cffi:null-pointer) :type :shape :inc-ref nil))

(defun stub-make-shell (solid faces-to-remove thickness tolerance)
  "Stub implementation of shelling."
  (declare (ignore solid faces-to-remove tolerance))
  (format *error-output*
          "~&;; STUB: shell with thickness ~A (OCCT not available)~%"
          thickness)
  (make-occt-handle (cffi:null-pointer) :type :shape :inc-ref nil))

(defun stub-mirror-shape (shape plane-origin plane-normal)
  "Stub implementation of mirroring."
  (declare (ignore shape plane-origin plane-normal))
  (format *error-output*
          "~&;; STUB: mirror shape (OCCT not available)~%")
  (make-occt-handle (cffi:null-pointer) :type :shape :inc-ref nil))
