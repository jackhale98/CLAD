;;;; src/ffi/export.lisp --- FFI bindings for STEP/STL export

(in-package :clad.ffi)

;;; ============================================================================
;;; C Wrapper Function Declarations
;;; ============================================================================

(defcfun ("occt_export_step" %occt-export-step) :int
  "Export shape to STEP file.
  Returns error code (0 = success)"
  (shape :pointer)
  (filename :string)
  (err-msg :pointer))

(defcfun ("occt_export_stl" %occt-export-stl) :int
  "Export shape to STL file.
  Returns error code (0 = success)"
  (shape :pointer)
  (filename :string)
  (linear-deflection occt-real)
  (angular-deflection occt-real)
  (err-msg :pointer))

(defcfun ("occt_export_gltf" %occt-export-gltf) :int
  "Export shape to glTF file (binary .glb format).
  Returns error code (0 = success)"
  (shape :pointer)
  (filename :string)
  (linear-deflection occt-real)
  (angular-deflection occt-real)
  (err-msg :pointer))

;;; ============================================================================
;;; High-Level FFI Functions
;;; ============================================================================

(defun ffi-export-step (shape-handle filename)
  "Export shape to STEP file (ISO 10303-21 AP203).

  Arguments:
    shape-handle - occt-handle for shape to export
    filename     - Path to output STEP file

  Returns: T on success

  Signals: occt-error on failure"
  (ensure-valid-handle shape-handle)

  (if *occt-available-p*
      (with-foreign-object (err-ptr :pointer)
        (let ((result-code (%occt-export-step
                            (handle-ptr shape-handle)
                            filename
                            err-ptr)))
          (check-occt-result result-code "export-step")
          t))

      (stub-export-step shape-handle filename)))

(defun ffi-export-stl (shape-handle filename
                       &key (linear-deflection 0.1)
                            (angular-deflection 0.5))
  "Export shape to STL file (triangulated mesh).

  Arguments:
    shape-handle        - occt-handle for shape to export
    filename            - Path to output STL file
    linear-deflection   - Maximum distance between mesh and surface (mm)
    angular-deflection  - Maximum angular deviation (degrees)

  Returns: T on success

  Signals: occt-error on failure"
  (ensure-valid-handle shape-handle)

  (if *occt-available-p*
      (with-foreign-object (err-ptr :pointer)
        (let ((result-code (%occt-export-stl
                            (handle-ptr shape-handle)
                            filename
                            (coerce linear-deflection 'double-float)
                            (coerce angular-deflection 'double-float)
                            err-ptr)))
          (check-occt-result result-code "export-stl")
          t))

      (stub-export-stl shape-handle filename)))

(defun ffi-export-gltf (shape-handle filename
                        &key (linear-deflection 0.1)
                             (angular-deflection 0.5))
  "Export shape to glTF file (GL Transmission Format, binary .glb).

  glTF is optimized for web and real-time 3D applications.

  Arguments:
    shape-handle        - occt-handle for shape to export
    filename            - Path to output glTF file (.glb or .gltf)
    linear-deflection   - Maximum distance between mesh and surface (mm)
    angular-deflection  - Maximum angular deviation (radians)

  Returns: T on success

  Signals: occt-error on failure"
  (ensure-valid-handle shape-handle)

  (if *occt-available-p*
      (with-foreign-object (err-ptr :pointer)
        (let ((result-code (%occt-export-gltf
                            (handle-ptr shape-handle)
                            filename
                            (coerce linear-deflection 'double-float)
                            (coerce angular-deflection 'double-float)
                            err-ptr)))
          (check-occt-result result-code "export-gltf")
          t))

      (stub-export-gltf shape-handle filename)))

;;; ============================================================================
;;; Stub Implementations
;;; ============================================================================

(defun stub-export-step (h filename)
  "Stub implementation of STEP export"
  (format t "~&;; STUB: Exporting ~A to STEP file: ~A~%" (handle-ptr h) filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "ISO-10303-21;~%")
    (format out "HEADER;~%")
    (format out "FILE_DESCRIPTION(('CLAD stub export'),'2;1');~%")
    (format out "FILE_NAME('~A','~A',(''),(''),'','CLAD','');~%"
            filename (get-universal-time))
    (format out "FILE_SCHEMA(('AUTOMOTIVE_DESIGN'));~%")
    (format out "ENDSEC;~%")
    (format out "DATA;~%")
    (format out "/* Stub shape ~A */~%" (handle-ptr h))
    (format out "ENDSEC;~%")
    (format out "END-ISO-10303-21;~%"))
  t)

(defun stub-export-stl (h filename)
  "Stub implementation of STL export"
  (format t "~&;; STUB: Exporting ~A to STL file: ~A~%" (handle-ptr h) filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "solid CLAD-Stub~%")
    (format out "  facet normal 0 0 1~%")
    (format out "    outer loop~%")
    (format out "      vertex 0 0 0~%")
    (format out "      vertex 1 0 0~%")
    (format out "      vertex 0 1 0~%")
    (format out "    endloop~%")
    (format out "  endfacet~%")
    (format out "endsolid CLAD-Stub~%"))
  t)

(defun stub-export-gltf (h filename)
  "Stub implementation of glTF export - creates a simple colored cube"
  (format t "~&;; STUB: Exporting ~A to glTF file: ~A~%" (handle-ptr h) filename)
  ;; Create a minimal valid glTF file with a simple cube mesh
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "{~%")
    (format out "  \"asset\": { \"version\": \"2.0\", \"generator\": \"CLAD stub\" },~%")
    (format out "  \"scene\": 0,~%")
    (format out "  \"scenes\": [{ \"nodes\": [0] }],~%")
    (format out "  \"nodes\": [{ \"mesh\": 0 }],~%")
    (format out "  \"meshes\": [{~%")
    (format out "    \"primitives\": [{~%")
    (format out "      \"attributes\": { \"POSITION\": 0, \"NORMAL\": 1 },~%")
    (format out "      \"indices\": 2,~%")
    (format out "      \"material\": 0~%")
    (format out "    }]~%")
    (format out "  }],~%")
    (format out "  \"materials\": [{~%")
    (format out "    \"pbrMetallicRoughness\": {~%")
    (format out "      \"baseColorFactor\": [0.5, 0.7, 0.9, 1.0],~%")
    (format out "      \"metallicFactor\": 0.3,~%")
    (format out "      \"roughnessFactor\": 0.7~%")
    (format out "    }~%")
    (format out "  }],~%")
    (format out "  \"accessors\": [~%")
    ;; POSITION accessor
    (format out "    { \"bufferView\": 0, \"componentType\": 5126, \"count\": 24, \"type\": \"VEC3\", \"max\": [50,50,50], \"min\": [-50,-50,-50] },~%")
    ;; NORMAL accessor
    (format out "    { \"bufferView\": 1, \"componentType\": 5126, \"count\": 24, \"type\": \"VEC3\" },~%")
    ;; INDICES accessor
    (format out "    { \"bufferView\": 2, \"componentType\": 5123, \"count\": 36, \"type\": \"SCALAR\" }~%")
    (format out "  ],~%")
    (format out "  \"bufferViews\": [~%")
    (format out "    { \"buffer\": 0, \"byteOffset\": 0, \"byteLength\": 288 },~%")     ; positions (24 * 3 * 4 = 288)
    (format out "    { \"buffer\": 0, \"byteOffset\": 288, \"byteLength\": 288 },~%")   ; normals (24 * 3 * 4 = 288)
    (format out "    { \"buffer\": 0, \"byteOffset\": 576, \"byteLength\": 72 }~%")     ; indices (36 * 2 = 72)
    (format out "  ],~%")
    (format out "  \"buffers\": [~%")
    (format out "    { \"byteLength\": 648, \"uri\": \"data:application/octet-stream;base64,")
    ;; Embedded base64 cube data (positions, normals, indices for a simple cube)
    (format out "AAAAwAAAAMAAAADAAAA/wAAAAMAAAADAAAA/AAAAwAAAAMAAAADAAAA/AAAAwAAAADA")
    (format out "AAADAAAAwAAAAMAAAADAAAA/AAAAwAAAAMAAAADAAAA/wAAAAMAAAADAAAA/wAAAAMAA")
    (format out "AADAAAAwAAAAMAAAADAAAAwAAAA/AAAAPAAAAwAAAA/AAAAPwAAAA/AAAAPwAAAAMA")
    (format out "AAADAAAAwAAAA/AAAAPAAAAwAAAA/AAAAPwAAAA/AAAAPAAAAwAAAA/AAAADAAAAA")
    (format out "wAAAA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAA")
    (format out "AAIA/AACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAA")
    (format out "AAAAAAAAAQAAAAAAAAAAAAABAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAQAAAAAAA")
    (format out "AAAAgD8AAAAAAAAAAAAAAIAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACA")
    (format out "vwAAAAAAAAAAAACAvwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAAIAAAAAAAwAA")
    (format out "AAQABQAGAAcACAAJAAoACwAMAA0ADgAPABAAEQASABMAFAAVABYAFwA=\" }~%")
    (format out "  ]~%")
    (format out "}~%"))
  t)
