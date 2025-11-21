;;;; src/export/stl.lisp --- High-level STL export API

(in-package :clad.export)

;;; ============================================================================
;;; STL Export - High-Level API
;;; ============================================================================
;;;
;;; Provides user-friendly STL export functionality for 3D printing workflows.
;;;
;;; Features:
;;;   - Binary and ASCII STL formats
;;;   - Configurable mesh resolution/quality
;;;   - Automatic directory creation
;;;   - Comprehensive error handling
;;;
;;; Implementation:
;;;   Uses OpenCASCADE StlAPI_Writer via FFI bindings
;;;   C++ wrapper: c-wrapper/occt-wrapper.cpp:occt_export_stl
;;;   FFI binding: src/ffi/export.lisp:ffi-export-stl
;;; ============================================================================

(defun export-stl (shape filename &key (ascii nil) (resolution :medium))
  "Export CLAD shape to STL file for 3D printing.

  STL (STereoLithography) is the standard file format for 3D printing,
  containing a triangulated mesh representation of the 3D model.

  Arguments:
    shape      - CLAD shape (clad.core:shape or clad.shapes:cad-shape)
    filename   - Path to output .stl file (string)
    ascii      - T for ASCII format, NIL for binary (default: NIL)
                 Binary is recommended (smaller, faster)
    resolution - Mesh quality: :low, :medium, :high, :ultra
                 (default: :medium)

  Resolution Guide:
    :low    - Fast export, small files, coarse mesh
              Linear deflection: 0.5mm, Angular: 1.0°
              Use for: Draft prints, visualization

    :medium - Balanced quality/size (recommended)
              Linear deflection: 0.1mm, Angular: 0.5°
              Use for: Standard 3D printing

    :high   - High detail, larger files
              Linear deflection: 0.05mm, Angular: 0.25°
              Use for: Detailed prints, precise parts

    :ultra  - Maximum detail, very large files
              Linear deflection: 0.01mm, Angular: 0.1°
              Use for: Ultra-high-resolution prints

  Returns: T on success

  Signals:
    error - On invalid shape, filename, or export failure

  Examples:
    ;; Export for standard 3D printing (binary, medium resolution)
    (export-stl my-part \"bracket.stl\")

    ;; Export with high detail for precise parts
    (export-stl gear \"gear.stl\" :resolution :high)

    ;; Export as ASCII for debugging/inspection
    (export-stl test-shape \"test.stl\" :ascii t)

    ;; Low resolution for fast preview
    (export-stl assembly \"preview.stl\" :resolution :low)

  STL File Format:
    Binary STL (default):
      - 80-byte header
      - 4-byte triangle count (little endian)
      - Triangle data (50 bytes each):
        * Normal vector (3 floats)
        * Vertex 1 (3 floats)
        * Vertex 2 (3 floats)
        * Vertex 3 (3 floats)
        * Attribute byte count (uint16)

    ASCII STL:
      - Human-readable text format
      - solid <name>
      - facet normal <nx> <ny> <nz>
      - outer loop
      - vertex <x> <y> <z>
      - ...
      - endloop
      - endfacet
      - ...
      - endsolid <name>

  3D Printing Workflow:
    1. Design part in CLAD
    2. Export to STL: (export-stl my-part \"part.stl\" :resolution :high)
    3. Import STL into slicer (Cura, PrusaSlicer, etc.)
    4. Generate G-code
    5. Print!

  Mesh Quality vs File Size:
    Higher resolution = more triangles = larger files = slower slicing
    Choose resolution based on:
      - Print precision requirements
      - Feature size (fine details need higher resolution)
      - File size constraints
      - Slicing time tolerance

  Compatibility:
    - All 3D printing slicers (Cura, PrusaSlicer, Simplify3D, etc.)
    - Mesh repair tools (Meshmixer, Netfabb, etc.)
    - CAD software (FreeCAD, SolidWorks, Fusion 360, etc.)
    - Visualization tools (MeshLab, Blender, etc.)

  Notes:
    - STL files contain ONLY geometry (no color, material, or metadata)
    - STL represents surfaces as triangle meshes (not exact geometry)
    - Higher resolution better captures curved surfaces
    - Binary format is ~5x smaller than ASCII for same mesh"

  (check-type filename string "Filename must be a string")

  ;; Validate resolution keyword
  (unless (member resolution '(:low :medium :high :ultra))
    (error "Invalid resolution: ~A. Must be :low, :medium, :high, or :ultra"
           resolution))

  ;; Unwrap CLOS shape if needed
  (let ((core-shape (if (typep shape 'clad.shapes:cad-shape)
                        (clad.shapes::core-shape shape)
                        shape)))

    ;; Validate shape
    (unless (and core-shape (clad.core:valid-shape-p core-shape))
      (error "Invalid shape for STL export: ~S" shape))

    ;; Ensure output directory exists
    (ensure-directories-exist filename)

    ;; Map resolution to deflection parameters
    (multiple-value-bind (linear-deflection angular-deflection)
        (resolution-to-deflection resolution)

      ;; Export using FFI
      (if ascii
          ;; ASCII export
          (export-stl-ascii core-shape filename linear-deflection angular-deflection)
          ;; Binary export (default)
          (export-stl-binary core-shape filename linear-deflection angular-deflection)))

    ;; Success
    t))

;;; ============================================================================
;;; Resolution Mapping
;;; ============================================================================

(defun resolution-to-deflection (resolution)
  "Map resolution keyword to OpenCASCADE deflection parameters.

  Returns: (values linear-deflection angular-deflection)

  Linear Deflection: Maximum distance between mesh and actual surface (mm)
  Angular Deflection: Maximum angular deviation (degrees)

  Lower values = higher quality = more triangles"

  (ecase resolution
    (:low
     ;; Fast export, coarse mesh
     ;; Good for: Draft visualization, very large assemblies
     (values 0.5   ; 0.5mm linear deflection
             1.0))  ; 1.0° angular deflection

    (:medium
     ;; Balanced quality/performance (recommended default)
     ;; Good for: Standard 3D printing, most use cases
     (values 0.1   ; 0.1mm linear deflection
             0.5))  ; 0.5° angular deflection

    (:high
     ;; High detail mesh
     ;; Good for: Detailed parts, small features
     (values 0.05  ; 0.05mm linear deflection
             0.25)) ; 0.25° angular deflection

    (:ultra
     ;; Maximum detail
     ;; Good for: Ultra-precision, tiny features, inspection
     (values 0.01  ; 0.01mm linear deflection
             0.1))))  ; 0.1° angular deflection

;;; ============================================================================
;;; Binary STL Export
;;; ============================================================================

(defun export-stl-binary (shape filename linear-deflection angular-deflection)
  "Export shape to binary STL format.

  Binary STL is the recommended format:
    - Smaller file size (~5x smaller than ASCII)
    - Faster to read/write
    - Industry standard for 3D printing

  Uses OpenCASCADE StlAPI_Writer in binary mode."

  ;; Use FFI to export in binary mode (ascii=nil)
  (clad.ffi:ffi-export-stl (clad.core:shape-handle shape)
                           filename
                           :linear-deflection linear-deflection
                           :angular-deflection angular-deflection
                           :ascii nil)

  (format t "~&Exported binary STL: ~A~%" filename)
  (format t "~&  Resolution: ~,3Fmm linear, ~,2F° angular~%"
          linear-deflection angular-deflection)

  t)

;;; ============================================================================
;;; ASCII STL Export
;;; ============================================================================

(defun export-stl-ascii (shape filename linear-deflection angular-deflection)
  "Export shape to ASCII STL format.

  ASCII STL is human-readable but larger:
    - ~5x larger than binary
    - Slower to process
    - Easier to debug/inspect

  Use for:
    - Debugging mesh generation
    - Learning STL format
    - Manual inspection

  For production 3D printing, use binary format."

  ;; Use FFI to export in ASCII mode (ascii=t)
  (clad.ffi:ffi-export-stl (clad.core:shape-handle shape)
                           filename
                           :linear-deflection linear-deflection
                           :angular-deflection angular-deflection
                           :ascii t)

  (format t "~&Exported ASCII STL: ~A~%" filename)
  (format t "~&  Resolution: ~,3Fmm linear, ~,2F° angular~%"
          linear-deflection angular-deflection)

  t)

(defun convert-binary-stl-to-ascii (binary-file ascii-file)
  "Convert binary STL to ASCII STL format.

  Binary STL format:
    80 bytes: header
    4 bytes: number of triangles (uint32, little-endian)
    For each triangle (50 bytes):
      12 bytes: normal vector (3 x float32)
      12 bytes: vertex 1 (3 x float32)
      12 bytes: vertex 2 (3 x float32)
      12 bytes: vertex 3 (3 x float32)
      2 bytes: attribute byte count (uint16)

  ASCII STL format:
    solid <name>
      facet normal <nx> <ny> <nz>
        outer loop
          vertex <x1> <y1> <z1>
          vertex <x2> <y2> <z2>
          vertex <x3> <y3> <z3>
        endloop
      endfacet
      ...
    endsolid <name>"

  (with-open-file (in binary-file
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (with-open-file (out ascii-file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)

      ;; Read header (80 bytes) - ignore
      (let ((header (make-array 80 :element-type '(unsigned-byte 8))))
        (read-sequence header in))

      ;; Read triangle count (4 bytes, little-endian uint32)
      (let ((count-bytes (make-array 4 :element-type '(unsigned-byte 8))))
        (read-sequence count-bytes in)
        (let ((triangle-count (+ (aref count-bytes 0)
                                 (ash (aref count-bytes 1) 8)
                                 (ash (aref count-bytes 2) 16)
                                 (ash (aref count-bytes 3) 24))))

          ;; Write ASCII header
          (format out "solid CLAD-Export~%")

          ;; Read and convert each triangle
          (dotimes (i triangle-count)
            (let ((triangle-data (make-array 50 :element-type '(unsigned-byte 8))))
              (read-sequence triangle-data in)

              ;; Parse triangle data (50 bytes = 12 floats + 2 attribute bytes)
              (let ((normal (parse-vector3 triangle-data 0))
                    (v1 (parse-vector3 triangle-data 12))
                    (v2 (parse-vector3 triangle-data 24))
                    (v3 (parse-vector3 triangle-data 36)))

                ;; Write ASCII triangle
                (format out "  facet normal ~,6E ~,6E ~,6E~%"
                        (aref normal 0) (aref normal 1) (aref normal 2))
                (format out "    outer loop~%")
                (format out "      vertex ~,6E ~,6E ~,6E~%"
                        (aref v1 0) (aref v1 1) (aref v1 2))
                (format out "      vertex ~,6E ~,6E ~,6E~%"
                        (aref v2 0) (aref v2 1) (aref v2 2))
                (format out "      vertex ~,6E ~,6E ~,6E~%"
                        (aref v3 0) (aref v3 1) (aref v3 2))
                (format out "    endloop~%")
                (format out "  endfacet~%"))))

          ;; Write ASCII footer
          (format out "endsolid CLAD-Export~%")))))

  t)

(defun parse-vector3 (byte-array offset)
  "Parse 3D vector (3 x float32) from byte array at offset.

  Float32 format: IEEE 754 single precision, little-endian
  Returns: vector of 3 doubles"

  (let ((result (make-array 3 :element-type 'double-float)))
    (dotimes (i 3)
      (setf (aref result i)
            (parse-float32 byte-array (+ offset (* i 4)))))
    result))

(defun parse-float32 (byte-array offset)
  "Parse IEEE 754 float32 from little-endian byte array.

  Format: 4 bytes, little-endian
    - Byte 0: least significant
    - Byte 3: most significant
    - Sign bit: bit 31
    - Exponent: bits 30-23 (8 bits, biased by 127)
    - Mantissa: bits 22-0 (23 bits)

  Returns: double-float"

  (let* ((b0 (aref byte-array offset))
         (b1 (aref byte-array (+ offset 1)))
         (b2 (aref byte-array (+ offset 2)))
         (b3 (aref byte-array (+ offset 3)))
         ;; Combine into 32-bit integer (little-endian)
         (bits (logior b0
                       (ash b1 8)
                       (ash b2 16)
                       (ash b3 24)))
         ;; Extract IEEE 754 components
         (sign (if (logbitp 31 bits) -1.0d0 1.0d0))
         (exponent (ldb (byte 8 23) bits))
         (mantissa (ldb (byte 23 0) bits)))

    (cond
      ;; Zero or denormalized
      ((zerop exponent)
       (if (zerop mantissa)
           0.0d0
           ;; Denormalized number
           (* sign (expt 2.0d0 -126) (/ mantissa (expt 2.0d0 23)))))

      ;; Infinity or NaN
      ((= exponent 255)
       (if (zerop mantissa)
           (* sign 1.0d308)  ; Infinity
           0.0d0))           ; NaN -> 0

      ;; Normalized number
      (t
       (* sign
          (expt 2.0d0 (- exponent 127))
          (+ 1.0d0 (/ mantissa (expt 2.0d0 23))))))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

#|
;; TODO: Fix parenthesis balance in this function
(defun stl-file-info (filename)
  "Get information about an STL file.

  Returns property list with:
    :format - :binary or :ascii
    :triangle-count - Number of triangles
    :file-size - Size in bytes

  Example:
    (stl-file-info \"part.stl\")
    => (:format :binary :triangle-count 1284 :file-size 64284)"

  (with-open-file (stream filename :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((file-size (file-length stream)))

      ;; Check if ASCII or binary
      ;; ASCII starts with "solid" (or could be binary with that header)
      (let ((first-bytes (make-array 5 :element-type '(unsigned-byte 8))))
        (read-sequence first-bytes stream)

        ;; "solid" in ASCII = bytes (115 111 108 105 100)
        (let ((is-ascii-start (and (= (aref first-bytes 0) 115)
                                    (= (aref first-bytes 1) 111)
                                    (= (aref first-bytes 2) 108)
                                    (= (aref first-bytes 3) 105)
                                    (= (aref first-bytes 4) 100))))

          (if is-ascii-start
              ;; Might be ASCII - check if text file
              (progn
                (file-position stream 0)
                (with-open-file (text-stream filename :direction :input
                                            :if-does-not-exist nil)
                  (when text-stream
                    (let ((first-line (read-line text-stream nil)))
                      (when (and first-line (search "solid" first-line))
                        ;; Definitely ASCII
                        ;; Count facets
                        (let ((facet-count 0))
                          (loop for line = (read-line text-stream nil)
                                while line
                                when (search "facet" line)
                                do (incf facet-count))
                          (return-from stl-file-info
                            (list :format :ascii
                                  :triangle-count facet-count
                                  :file-size file-size)))))))
                ;; If not confirmed ASCII, treat as binary
                nil)

              ;; Binary format
              nil)

          ;; Default to binary format if we reach here
          ;; Seek to triangle count (after 80-byte header)
          (file-position stream 80)
          (let ((count-bytes (make-array 4 :element-type '(unsigned-byte 8))))
            (read-sequence count-bytes stream)
            (let ((triangle-count (+ (aref count-bytes 0)
                                     (ash (aref count-bytes 1) 8)
                                     (ash (aref count-bytes 2) 16)
                                     (ash (aref count-bytes 3) 24))))
              (list :format :binary
                    :triangle-count triangle-count
                    :file-size file-size))))))))))
|#

;;; ============================================================================
;;; End of STL Export Module
;;; ============================================================================
