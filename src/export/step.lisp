;;;; src/export/step.lisp --- CAD file export (STEP, STL, glTF)

(in-package :clad.export)

;;; ============================================================================
;;; STEP Export
;;; ============================================================================

;;; Helper Functions

(defun add-tolerance-metadata-to-step-file (shape filename)
  "Add tolerance metadata as comments to STEP file header.

  This provides human-readable tolerance information in the STEP file
  until full AP242 PMI export is implemented (Phase 4)."
  (let ((metadata (clad.core:shape-metadata shape)))
    (when (and metadata (getf metadata :has-tolerances))
      ;; Read the STEP file
      (let ((lines (with-open-file (stream filename)
                     (loop for line = (read-line stream nil)
                           while line
                           collect line))))

        ;; Find the ENDSEC; line that ends the HEADER section
        (let ((header-end-pos (position "ENDSEC;" lines :test #'string=)))
          (when header-end-pos
            ;; Insert tolerance comments before ENDSEC;
            (let ((tolerance-comments (format-tolerance-comments metadata)))
              (setf lines (append (subseq lines 0 header-end-pos)
                                  tolerance-comments
                                  (subseq lines header-end-pos))))

            ;; Write the modified STEP file
            (with-open-file (stream filename
                                    :direction :output
                                    :if-exists :supersede)
              (dolist (line lines)
                (write-line line stream)))))))))

(defun format-tolerance-comments (metadata)
  "Format tolerance metadata as STEP comment lines.

  Returns a list of comment lines to insert into STEP header."
  (let ((comments '("/* ======================================================================"
                    "   CLAD Tolerance Information"
                    "   ======================================================================")))
    (let ((tolerance-features (getf metadata :tolerance-features)))
      (dolist (feature tolerance-features)
        (let ((feature-name (getf feature :feature))
              (dimension (getf feature :dimension))
              (type (getf feature :type)))
          ;; Add feature comment
          (push (format nil "   Feature: ~A (~A)" feature-name type) comments)

          ;; Add tolerance details
          (when (typep dimension 'clad.units:toleranced-dimension)
            (let ((nominal (clad.units:dimension-nominal dimension))
                  (tol (clad.units:dimension-tolerance dimension)))
              (push (format nil "     Nominal: ~,3F mm" nominal) comments)

              (cond
                ;; ISO Fit
                ((typep tol 'clad.units:fit-tolerance-spec)
                 (push (format nil "     Tolerance: ISO ~A (+~,3F/~,3F mm)"
                               (clad.units:tolerance-fit-class tol)
                               (clad.units:tolerance-upper tol)
                               (clad.units:tolerance-lower tol))
                       comments))

                ;; Bilateral
                ((typep tol 'clad.units:bilateral-tolerance-spec)
                 (if (< (abs (+ (clad.units:tolerance-upper tol)
                                (clad.units:tolerance-lower tol)))
                        0.0001)
                     ;; Symmetric
                     (push (format nil "     Tolerance: ±~,3F mm"
                                   (clad.units:tolerance-upper tol))
                           comments)
                     ;; Asymmetric
                     (push (format nil "     Tolerance: +~,3F/~,3F mm"
                                   (clad.units:tolerance-upper tol)
                                   (clad.units:tolerance-lower tol))
                           comments)))

                ;; Limit
                ((typep tol 'clad.units:limit-tolerance-spec)
                 (push (format nil "     Limits: ~,3F/~,3F mm"
                               (clad.units:tolerance-upper-limit tol)
                               (clad.units:tolerance-lower-limit tol))
                       comments)))))))

    (push "   ======================================================================" comments)
    (push "*/" comments)
    (nreverse comments))))

(defun export-step (shape filename)
  "Export shape to STEP file (ISO 10303-21 AP203) with tolerance metadata.

  Arguments:
    shape    - clad.core:shape or clad.shapes:cad-shape to export
    filename - Path to output STEP file

  Returns: T on success

  Signals: error on failure

  Example:
    (export-step (make-box 100 50 30) \"box.step\")

  The STEP format is industry-standard and compatible with:
    - FreeCAD
    - SolidWorks
    - AutoCAD
    - Fusion 360
    - And most other CAD systems

  If the shape contains tolerance metadata, it will be included as
  comments in the STEP file header for reference."
  ;; Unwrap CLOS shape if needed
  (let ((core-shape (if (typep shape 'clad.shapes:cad-shape)
                        (clad.shapes::core-shape shape)
                        shape)))
    (unless (clad.core:valid-shape-p core-shape)
      (error "Invalid shape: ~S" shape))

    (unless (stringp filename)
      (error "Filename must be a string: ~S" filename))

    ;; Ensure directory exists
    (ensure-directories-exist filename)

    ;; Call FFI export function
    (clad.ffi:ffi-export-step
     (clad.core:shape-handle core-shape)
     (namestring (merge-pathnames filename)))

    ;; Add tolerance metadata to STEP file if present
    (add-tolerance-metadata-to-step-file core-shape filename)

    (format t "~&Exported STEP file: ~A~%" filename)
    t))

(defun export-step-assembly (shapes filename &key (assembly-name "Assembly"))
  "Export multiple shapes to a STEP file as an assembly.

  Arguments:
    shapes        - List of clad.core:shape instances
    filename      - Path to output STEP file
    assembly-name - Name for the assembly (default: \"Assembly\")

  Returns: T on success

  Note: This is a placeholder for Phase 10 (Assembly Support).
        Currently exports shapes as separate bodies in one STEP file."
  (declare (ignore assembly-name))  ; Will be used in Phase 10

  (unless (listp shapes)
    (error "SHAPES must be a list of shape objects"))

  (dolist (shape shapes)
    (unless (clad.core:valid-shape-p shape)
      (error "Invalid shape in list: ~S" shape)))

  ;; For now, compound all shapes and export
  ;; In Phase 10, this will use proper assembly structure
  (if (= (length shapes) 1)
      (export-step (first shapes) filename)
      (let ((compound (apply #'clad.core:union-shapes shapes)))
        (export-step compound filename))))

;;; ============================================================================
;;; glTF Export
;;; ============================================================================

(defun export-gltf (shape filename &key (linear-deflection 0.1) (angular-deflection 0.5))
  "Export shape to glTF file (GL Transmission Format, binary .glb).

  glTF is optimized for web and real-time 3D applications like three.js.

  Arguments:
    shape              - clad.core:shape or clad.shapes:cad-shape to export
    filename           - Path to output glTF file (.glb or .gltf)
    linear-deflection  - Maximum distance between mesh and surface (mm, default 0.1)
    angular-deflection - Maximum angular deviation (radians, default 0.5)

  Returns: T on success

  Signals: error on failure

  Example:
    (export-gltf (make-box 100 50 30) \"box.glb\")
    (export-gltf (make-sphere 50) \"sphere.glb\" :linear-deflection 0.05)

  The glTF format is ideal for:
    - Web-based 3D viewers (three.js, Babylon.js)
    - Real-time rendering
    - Game engines (Unity, Unreal)
    - AR/VR applications"
  ;; Unwrap CLOS shape if needed
  (let ((core-shape (if (typep shape 'clad.shapes:cad-shape)
                        (clad.shapes::core-shape shape)
                        shape)))
    (unless (clad.core:valid-shape-p core-shape)
      (error "Invalid shape: ~S" shape))

    (unless (stringp filename)
      (error "Filename must be a string: ~S" filename))

    ;; Ensure directory exists
    (ensure-directories-exist filename)

    ;; Call FFI export function
    (clad.ffi:ffi-export-gltf
     (clad.core:shape-handle core-shape)
     (namestring (pathname filename))
     :linear-deflection linear-deflection
     :angular-deflection angular-deflection)

    (format t "~&Exported glTF file: ~A~%" filename)
    t))
