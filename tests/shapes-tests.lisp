;;;; tests/shapes-tests.lisp --- Tests for CLOS shape hierarchy

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite shapes-tests
  :description "Tests for CLOS shape class hierarchy and queries"
  :in clad-tests)

(in-suite shapes-tests)

;;; ============================================================================
;;; Shape Class Tests
;;; ============================================================================

(test test-shape-wrapping
  "Test wrapping functional core shapes in CLOS classes"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
    (is (typep cad-box 'clad.shapes:cad-solid))
    (is (typep cad-box 'clad.shapes:cad-shape))
    (is (clad.shapes:shape-valid-p cad-box))
    (is (eq (clad.shapes:shape-type cad-box) :solid))))

(test test-shape-type-predicates
  "Test shape type checking predicates"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
    (is (clad.shapes:solid-p cad-box))
    (is (not (clad.shapes:face-p cad-box)))
    (is (not (clad.shapes:edge-p cad-box)))
    (is (not (clad.shapes:vertex-p cad-box)))))

(test test-shape-unwrapping
  "Test extracting functional core shape from CLOS shape"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid))
         (unwrapped (clad.shapes:unwrap-shape cad-box)))
    (is (eq core-box unwrapped))
    (is (typep unwrapped 'clad.core:shape))))

;;; ============================================================================
;;; Shape Query Tests (will be implemented later)
;;; ============================================================================

(test test-bounding-box-query
  "Test querying bounding box of a shape"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
    ;; Bounding box should return (xmin ymin zmin xmax ymax zmax)
    (let ((bbox (clad.shapes:bounding-box cad-box)))
      (is (listp bbox))
      (is (= (length bbox) 6))
      ;; Box from origin, so min should be ~0 and max should be ~dimensions
      (destructuring-bind (xmin ymin zmin xmax ymax zmax) bbox
        (is (< xmin 1.0))   ; Near zero
        (is (< ymin 1.0))
        (is (< zmin 1.0))
        (is (> xmax 99.0))  ; Near 100
        (is (> ymax 49.0))  ; Near 50
        (is (> zmax 29.0)))))) ; Near 30

(test test-volume-query
  "Test querying volume of a solid"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
    (let ((vol (clad.shapes:volume cad-box)))
      (is (numberp vol))
      ;; Volume should be positive (exact value depends on OCCT vs stub mode)
      (is (> vol 0.0d0)))))

(test test-faces-query
  "Test querying faces of a solid"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
    (let ((face-list (clad.shapes:faces cad-box)))
      (is (listp face-list))
      ;; A box should have exactly 6 faces
      (is (= (length face-list) 6))
      ;; All should be cad-face objects
      (is (every #'clad.shapes:face-p face-list)))))

(test test-edges-query
  "Test querying edges of a solid"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
    (let ((edge-list (clad.shapes:edges cad-box)))
      (is (listp edge-list))
      ;; OCCT box representation has 24 edges (includes parametric edges on faces)
      (is (= (length edge-list) 24))
      ;; All should be cad-edge objects
      (is (every #'clad.shapes:edge-p edge-list)))))

(test test-vertices-query
  "Test querying vertices of a solid"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
    (let ((vertex-list (clad.shapes:vertices cad-box)))
      (is (listp vertex-list))
      ;; OCCT box representation has 48 vertices (8 corners × 6 faces, with duplication)
      (is (= (length vertex-list) 48))
      ;; All should be cad-vertex objects
      (is (every #'clad.shapes:vertex-p vertex-list)))))

;;; ============================================================================
;;; Integration with Core Layer
;;; ============================================================================

(test test-shapes-with-boolean-operations
  "Test CLOS shapes work with boolean operations from core"
  (let* ((box1 (clad.core:make-box 100 100 100))
         (box2 (clad.core:translate (clad.core:make-box 100 100 100)
                                         50 50 50))
         (union-result (clad.core:union-shapes box1 box2))
         (cad-union (clad.shapes:wrap-shape union-result 'clad.shapes:cad-solid)))
    (is (clad.shapes:solid-p cad-union))
    (is (clad.shapes:shape-valid-p cad-union))
    ;; Union volume should be less than sum of both boxes but more than one box
    (let ((vol (clad.shapes:volume cad-union)))
      (is (> vol 1000000.0))  ; More than one box (100^3)
      (is (< vol 2000000.0))))) ; Less than two boxes

;;; ============================================================================
;;; Edge Cases and Error Handling
;;; ============================================================================

(test test-invalid-shape-detection
  "Test detection of invalid shapes"
  (let* ((core-box (clad.core:make-box 100 50 30))
         (cad-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
    ;; Valid shape should pass
    (is (clad.shapes:shape-valid-p cad-box))))
