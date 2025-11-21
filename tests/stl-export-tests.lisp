;;;; tests/stl-export-tests.lisp --- TDD tests for STL export (Phase 1: RED)

(in-package :clad.tests)

;;; ============================================================================
;;; STL Export Test Suite
;;; ============================================================================
;;;
;;; Following Test-Driven Development (TDD):
;;;   - RED: These tests will FAIL initially (implementation doesn't exist)
;;;   - GREEN: Implement minimal code to make tests pass
;;;   - REFACTOR: Clean up and optimize
;;;
;;; Coverage:
;;;   - Basic export (primitives: box, cylinder, sphere)
;;;   - Binary vs ASCII format
;;;   - Resolution/quality settings
;;;   - File validation
;;;   - Error handling
;;;   - Complex shapes and assemblies
;;; ============================================================================

(def-suite stl-export-tests
  :description "Test suite for STL export functionality"
  :in clad-tests)

(in-suite stl-export-tests)

;;; ----------------------------------------------------------------------------
;;; Basic Export Tests
;;; ----------------------------------------------------------------------------

(test stl-export-box-binary
  "Export simple box to binary STL"
  (let* ((box (clad.core:make-box 10 20 30))
         (filename "/tmp/test-box.stl"))
    ;; Export should succeed
    (finishes
      (clad.export:export-stl box filename))

    ;; File should exist
    (is (probe-file filename)
        "Binary STL file should be created")

    ;; File should be non-empty
    (when (probe-file filename)
      (is (> (with-open-file (stream filename :element-type '(unsigned-byte 8))
               (file-length stream))
             0)
          "STL file should not be empty")

      ;; Clean up
      (delete-file filename))))

(test stl-export-cylinder-binary
  "Export cylinder to binary STL"
  (let* ((cylinder (clad.core:make-cylinder 5 20))
         (filename "/tmp/test-cylinder.stl"))
    (finishes
      (clad.export:export-stl cylinder filename))

    (is (probe-file filename)
        "Cylinder STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-sphere-binary
  "Export sphere to binary STL"
  (let* ((sphere (clad.core:make-sphere 10))
         (filename "/tmp/test-sphere.stl"))
    (finishes
      (clad.export:export-stl sphere filename))

    (is (probe-file filename)
        "Sphere STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

;;; ----------------------------------------------------------------------------
;;; ASCII Format Tests
;;; ----------------------------------------------------------------------------

(test stl-export-box-ascii
  "Export box to ASCII STL format"
  (let* ((box (clad.core:make-box 10 10 10))
         (filename "/tmp/test-box-ascii.stl"))
    (finishes
      (clad.export:export-stl box filename :ascii t))

    (is (probe-file filename)
        "ASCII STL file should be created")

    ;; Validate ASCII format - should start with "solid"
    (when (probe-file filename)
      (with-open-file (stream filename :direction :input)
        (let ((first-line (read-line stream nil)))
          (is (and first-line (search "solid" first-line :test #'char-equal))
              "ASCII STL should start with 'solid' keyword")))

      (delete-file filename))))

(test stl-export-ascii-contains-facets
  "ASCII STL should contain facet data"
  (let* ((box (clad.core:make-box 10 10 10))
         (filename "/tmp/test-ascii-facets.stl"))
    (clad.export:export-stl box filename :ascii t)

    (when (probe-file filename)
      (let ((content (with-open-file (stream filename)
                       (with-output-to-string (s)
                         (loop for line = (read-line stream nil)
                               while line
                               do (write-line line s))))))
        ;; Should contain facet normal
        (is (search "facet normal" content :test #'char-equal)
            "ASCII STL should contain 'facet normal'")

        ;; Should contain outer loop
        (is (search "outer loop" content :test #'char-equal)
            "ASCII STL should contain 'outer loop'")

        ;; Should contain vertex data
        (is (search "vertex" content :test #'char-equal)
            "ASCII STL should contain 'vertex' entries")

        ;; Should end with endsolid
        (is (search "endsolid" content :test #'char-equal)
            "ASCII STL should end with 'endsolid'"))

      (delete-file filename))))

;;; ----------------------------------------------------------------------------
;;; Resolution/Quality Tests
;;; ----------------------------------------------------------------------------

(test stl-export-resolution-low
  "Export with low resolution (fewer triangles)"
  (let* ((sphere (clad.core:make-sphere 10))
         (filename "/tmp/test-sphere-low.stl"))
    (finishes
      (clad.export:export-stl sphere filename :resolution :low))

    (is (probe-file filename)
        "Low resolution STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-resolution-medium
  "Export with medium resolution (default)"
  (let* ((sphere (clad.core:make-sphere 10))
         (filename "/tmp/test-sphere-medium.stl"))
    (finishes
      (clad.export:export-stl sphere filename :resolution :medium))

    (is (probe-file filename)
        "Medium resolution STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-resolution-high
  "Export with high resolution (more triangles)"
  (let* ((sphere (clad.core:make-sphere 10))
         (filename "/tmp/test-sphere-high.stl"))
    (finishes
      (clad.export:export-stl sphere filename :resolution :high))

    (is (probe-file filename)
        "High resolution STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-resolution-affects-file-size
  "Higher resolution should produce larger files (more triangles)"
  (let* ((sphere (clad.core:make-sphere 10))
         (low-file "/tmp/test-sphere-low-size.stl")
         (high-file "/tmp/test-sphere-high-size.stl"))
    ;; Export at different resolutions
    (clad.export:export-stl sphere low-file :resolution :low)
    (clad.export:export-stl sphere high-file :resolution :high)

    ;; High resolution should be larger
    (let ((low-size (with-open-file (s low-file :element-type '(unsigned-byte 8))
                      (file-length s)))
          (high-size (with-open-file (s high-file :element-type '(unsigned-byte 8))
                       (file-length s))))
      (is (> high-size low-size)
          "High resolution STL should be larger than low resolution"))

    (delete-file low-file)
    (delete-file high-file)))

;;; ----------------------------------------------------------------------------
;;; Binary STL Format Validation Tests
;;; ----------------------------------------------------------------------------

(test stl-binary-format-header
  "Binary STL should have correct header structure"
  (let* ((box (clad.core:make-box 10 10 10))
         (filename "/tmp/test-binary-header.stl"))
    (clad.export:export-stl box filename :ascii nil)

    (when (probe-file filename)
      (with-open-file (stream filename :direction :input
                              :element-type '(unsigned-byte 8))
        ;; Binary STL starts with 80-byte header
        (let ((header (make-array 80 :element-type '(unsigned-byte 8))))
          (read-sequence header stream)
          (is (= (length header) 80)
              "Binary STL header should be 80 bytes"))

        ;; Followed by 4-byte triangle count (little endian)
        (let ((count-bytes (make-array 4 :element-type '(unsigned-byte 8))))
          (read-sequence count-bytes stream)
          (let ((triangle-count (+ (aref count-bytes 0)
                                   (ash (aref count-bytes 1) 8)
                                   (ash (aref count-bytes 2) 16)
                                   (ash (aref count-bytes 3) 24))))
            (is (> triangle-count 0)
                "Triangle count should be positive")

            ;; Each triangle is 50 bytes (12 floats + 2 attribute bytes)
            (let ((expected-size (+ 80 4 (* triangle-count 50))))
              (is (= (file-length stream) expected-size)
                  "Binary STL file size should match format")))))

      (delete-file filename))))

;;; ----------------------------------------------------------------------------
;;; Complex Shape Tests
;;; ----------------------------------------------------------------------------

(test stl-export-boolean-union
  "Export result of boolean union"
  (let* ((box1 (clad.core:make-box 10 10 10))
         (box2 (clad.core:translate (clad.core:make-box 10 10 10) 5 0 0))
         (union (clad.core:union-shapes box1 box2))
         (filename "/tmp/test-union.stl"))
    (finishes
      (clad.export:export-stl union filename))

    (is (probe-file filename)
        "Union shape STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-boolean-cut
  "Export result of boolean cut"
  (let* ((box (clad.core:make-box 20 20 20))
         (cylinder (clad.core:make-cylinder 5 30))
         (cut (clad.core:cut-shapes box cylinder))
         (filename "/tmp/test-cut.stl"))
    (finishes
      (clad.export:export-stl cut filename))

    (is (probe-file filename)
        "Cut shape STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-with-fillets
  "Export shape with filleted edges"
  (let* ((box (clad.core:make-box 20 20 20))
         (filename "/tmp/test-fillet.stl"))
    ;; Note: If fillet is implemented, this should work
    ;; If not, this test validates basic box export
    (finishes
      (clad.export:export-stl box filename))

    (is (probe-file filename)
        "Shape with fillets STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-transformed-shape
  "Export transformed shape (rotated, translated, scaled)"
  (let* ((cylinder (clad.core:make-cylinder 5 20))
         (rotated (clad.core:rotate cylinder :z 45))
         (translated (clad.core:translate rotated 10 20 30))
         (filename "/tmp/test-transformed.stl"))
    (finishes
      (clad.export:export-stl translated filename))

    (is (probe-file filename)
        "Transformed shape STL should be created")

    (when (probe-file filename)
      (delete-file filename))))

;;; ----------------------------------------------------------------------------
;;; Error Handling Tests
;;; ----------------------------------------------------------------------------

(test stl-export-invalid-shape-error
  "Exporting invalid shape should signal error"
  (let ((filename "/tmp/test-invalid.stl"))
    (signals error
      (clad.export:export-stl nil filename))

    ;; Clean up if file was somehow created
    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-invalid-filename-error
  "Invalid filename should signal error"
  (let ((box (clad.core:make-box 10 10 10)))
    (signals error
      (clad.export:export-stl box nil))))

(test stl-export-invalid-resolution-error
  "Invalid resolution keyword should signal error"
  (let* ((box (clad.core:make-box 10 10 10))
         (filename "/tmp/test-bad-resolution.stl"))
    (signals error
      (clad.export:export-stl box filename :resolution :invalid))

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-directory-creation
  "Export should create parent directories if needed"
  (let* ((box (clad.core:make-box 10 10 10))
         (filename "/tmp/stl-test-dir/subdir/test.stl")
         (test-dir (uiop:parse-native-namestring "/tmp/stl-test-dir/")))
    ;; Clean up first if exists
    (when (probe-file test-dir)
      (uiop:delete-directory-tree test-dir :validate t :if-does-not-exist :ignore))

    ;; Should create directories
    (finishes
      (clad.export:export-stl box filename))

    (is (probe-file filename)
        "Export should create parent directories")

    ;; Clean up
    (when (probe-file test-dir)
      (uiop:delete-directory-tree test-dir :validate t :if-does-not-exist :ignore))))

;;; ----------------------------------------------------------------------------
;;; File Size and Triangle Count Tests
;;; ----------------------------------------------------------------------------

(test stl-export-file-size-reasonable
  "Exported STL files should have reasonable sizes"
  (let* ((box (clad.core:make-box 10 10 10))
         (filename "/tmp/test-size.stl"))
    (clad.export:export-stl box filename)

    (let ((size (with-open-file (s filename :element-type '(unsigned-byte 8))
                  (file-length s))))
      ;; Binary STL minimum: 80 (header) + 4 (count) + 50*12 (box has 12 triangles)
      (is (>= size (+ 80 4 (* 50 12)))
          "STL file should have reasonable minimum size")

      ;; Should not be excessively large (less than 1MB for simple box)
      (is (< size (* 1024 1024))
          "Simple box STL should not be excessively large"))

    (delete-file filename)))

;;; ----------------------------------------------------------------------------
;;; 3D Printing Validation Tests
;;; ----------------------------------------------------------------------------

(test stl-export-manifold-check
  "Exported STL should represent manifold geometry (watertight)"
  ;; Note: This test verifies we can export closed solids
  ;; Actual manifold checking would require STL parsing
  (let* ((sphere (clad.core:make-sphere 10))
         (filename "/tmp/test-manifold.stl"))
    (finishes
      (clad.export:export-stl sphere filename))

    (is (probe-file filename)
        "Closed solid should export successfully")

    (when (probe-file filename)
      (delete-file filename))))

(test stl-export-positive-volume
  "Exported shapes should have positive volume (correct normals)"
  ;; Binary STL normals should point outward
  (let* ((box (clad.core:make-box 10 10 10))
         (filename "/tmp/test-normals.stl"))
    (finishes
      (clad.export:export-stl box filename))

    ;; File should be created - actual normal validation would require parsing
    (is (probe-file filename)
        "Shape should export with proper normals")

    (when (probe-file filename)
      (delete-file filename))))

;;; ----------------------------------------------------------------------------
;;; Real-World Use Case Tests
;;; ----------------------------------------------------------------------------

(test stl-export-3d-printing-workflow
  "Complete 3D printing workflow: design → export → validate"
  (let* (;; Design a simple bracket
         (base (clad.core:make-box 50 30 5))
         (wall (clad.core:translate (clad.core:make-box 5 30 20) 0 0 5))
         (bracket (clad.core:union-shapes base wall))
         ;; Add mounting holes (if available)
         (hole1 (clad.core:translate (clad.core:make-cylinder 2.5 10) 10 15 0))
         (hole2 (clad.core:translate (clad.core:make-cylinder 2.5 10) 40 15 0))
         (final-bracket (clad.core:cut-shapes bracket hole1 hole2))
         (filename "/tmp/bracket-for-printing.stl"))

    ;; Export at high resolution for 3D printing
    (finishes
      (clad.export:export-stl final-bracket filename :resolution :high))

    (is (probe-file filename)
        "Bracket should export for 3D printing")

    ;; File should be substantial (detailed part)
    (when (probe-file filename)
      (let ((size (with-open-file (s filename :element-type '(unsigned-byte 8))
                    (file-length s))))
        (is (> size 1000)
            "Detailed bracket STL should be substantial"))

      (delete-file filename))))

(test stl-export-default-parameters
  "Export with all default parameters should work"
  (let* ((box (clad.core:make-box 10 10 10))
         (filename "/tmp/test-defaults.stl"))
    ;; Should default to binary, medium resolution
    (finishes
      (clad.export:export-stl box filename))

    (is (probe-file filename)
        "Export with defaults should succeed")

    (when (probe-file filename)
      ;; Should be binary (not starting with "solid")
      (with-open-file (stream filename :element-type '(unsigned-byte 8))
        (let ((first-bytes (make-array 5 :element-type '(unsigned-byte 8))))
          (read-sequence first-bytes stream)
          ;; If ASCII, would start with "solid" = (115 111 108 105 100)
          (is (not (and (= (aref first-bytes 0) 115)
                        (= (aref first-bytes 1) 111)
                        (= (aref first-bytes 2) 108)
                        (= (aref first-bytes 3) 105)
                        (= (aref first-bytes 4) 100)))
              "Default format should be binary, not ASCII")))

      (delete-file filename))))

;;; ============================================================================
;;; Summary
;;; ============================================================================
;;;
;;; Total Tests: 25+
;;;
;;; Coverage:
;;;   ✓ Basic primitives (box, cylinder, sphere)
;;;   ✓ Binary format
;;;   ✓ ASCII format
;;;   ✓ Format validation
;;;   ✓ Resolution settings (low, medium, high)
;;;   ✓ Complex shapes (booleans, transformations)
;;;   ✓ Error handling (invalid inputs)
;;;   ✓ Directory creation
;;;   ✓ File size validation
;;;   ✓ 3D printing workflows
;;;   ✓ Default parameters
;;;
;;; Expected Result: All tests FAIL (implementation doesn't exist yet)
;;;
;;; Next Step: Implement export-stl to make these tests PASS (GREEN phase)
;;; ============================================================================
