;;;; src/export/step.lisp --- CAD file export (STEP, STL, glTF)

(in-package :clad.export)

;;; ============================================================================
;;; STEP Export
;;; ============================================================================

(defun export-step (shape filename)
  "Export shape to STEP file (ISO 10303-21 AP203).

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
    - And most other CAD systems"
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
