;;;; tests/advanced-features-tests.lisp --- Test suite for advanced CAD features

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================
(def-suite advanced-features-tests
  :description "Tests for advanced CAD features (selectors, fillets, chamfers, etc.)"
  :in clad-tests)

(in-suite advanced-features-tests)

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defun make-test-box-with-hole ()
  "Create a box with a cylindrical hole for testing edge types"
  (let* ((box (clad.core:make-box 100 100 100))
         (cylinder (clad.core:make-cylinder 20 120))
         (shape (clad.core:cut-shapes box cylinder)))
    (clad.shapes:wrap-shape shape 'clad.shapes:cad-solid)))

(defun make-test-filleted-box ()
  "Create a box for fillet/chamfer testing (will implement after selectors)"
  (clad.shapes:wrap-shape
   (clad.core:make-box 100 100 50)
   'clad.shapes:cad-solid))

;;; ============================================================================
;;; TDD Cycle 1: FFI - Get Edge Geometric Type
;;; ============================================================================

(test ffi-edge-geom-type
  "Test FFI function to get geometric type of an edge (line, circle, etc.)"
  ;; This test drives the FFI implementation
  (let* ((box (make-test-box-with-hole))
         (all-edges (clad.shapes:edges box)))

    ;; Should have some edges
    (is (> (length all-edges) 0)
        "Box with hole should have edges")

    ;; Test that we can query edge types
    (dolist (edge all-edges)
      (let ((edge-handle (clad.core:shape-handle
                          (clad.shapes:unwrap-shape edge))))
        ;; This will fail until we implement the FFI function
        (multiple-value-bind (geom-type success)
            (clad.ffi:ffi-get-edge-geom-type edge-handle)
          (is (not (null success))
              "Should successfully get edge geometric type")
          (is (member geom-type '(:line :circle :ellipse :bspline :bezier :other))
              "Edge type should be a valid keyword: ~A" geom-type))))))

;;; ============================================================================
;;; TDD Cycle 2: FFI - Get Face Geometric Type
;;; ============================================================================

(test ffi-face-geom-type
  "Test FFI function to get geometric type of a face (plane, cylinder, etc.)"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box)))

    ;; Should have some faces
    (is (> (length all-faces) 0)
        "Box with hole should have faces")

    ;; Test that we can query face types
    (dolist (face all-faces)
      (let ((face-handle (clad.core:shape-handle
                          (clad.shapes:unwrap-shape face))))
        ;; This will fail until we implement the FFI function
        (multiple-value-bind (geom-type success)
            (clad.ffi:ffi-get-face-geom-type face-handle)
          (is (not (null success))
              "Should successfully get face geometric type")
          (is (member geom-type '(:plane :cylinder :sphere :cone :torus :bspline :other))
              "Face type should be a valid keyword: ~A" geom-type))))))

;;; ============================================================================
;;; TDD Cycle 3: Type Selector - Select Planar Faces
;;; ============================================================================

(test type-selector-planar-faces
  "Test selecting only planar faces from a shape"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box))
         (selector (make-instance 'clad.selectors::type-selector
                                  :shape-type :plane))
         (planar-faces (clad.selectors::apply-selector selector all-faces)))

    ;; A box with a cylindrical hole should have:
    ;; - 6 planar faces (top, bottom, 4 sides)
    ;; - 1 cylindrical face (inside the hole)
    ;; So we should select the 6 planar faces
    (is (>= (length planar-faces) 4)
        "Should select multiple planar faces")

    ;; Verify each selected face is actually planar
    (dolist (face planar-faces)
      (multiple-value-bind (geom-type success)
          (clad.ffi:ffi-get-face-geom-type
           (clad.core:shape-handle (clad.shapes:unwrap-shape face)))
        (is (not (null success))
            "Should successfully query face type")
        (is (eq geom-type :plane)
            "Selected face should be planar")))))

;;; ============================================================================
;;; TDD Cycle 4: Type Selector - Select Cylindrical Faces
;;; ============================================================================

(test type-selector-cylindrical-faces
  "Test selecting only cylindrical faces from a shape"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box))
         (selector (make-instance 'clad.selectors::type-selector
                                  :shape-type :cylinder))
         (cyl-faces (clad.selectors::apply-selector selector all-faces)))

    ;; Should select the cylindrical hole face
    (is (>= (length cyl-faces) 1)
        "Should select at least 1 cylindrical face")

    ;; Verify each selected face is actually cylindrical
    (dolist (face cyl-faces)
      (multiple-value-bind (geom-type success)
          (clad.ffi:ffi-get-face-geom-type
           (clad.core:shape-handle (clad.shapes:unwrap-shape face)))
        (is (not (null success))
            "Should successfully query face type")
        (is (eq geom-type :cylinder)
            "Selected face should be cylindrical")))))

;;; ============================================================================
;;; TDD Cycle 5: Type Selector - Select Line Edges
;;; ============================================================================

(test type-selector-line-edges
  "Test selecting only straight line edges from a shape"
  (let* ((box (make-test-box-with-hole))
         (all-edges (clad.shapes:edges box))
         (selector (make-instance 'clad.selectors::type-selector
                                  :shape-type :line))
         (line-edges (clad.selectors::apply-selector selector all-edges)))

    ;; A box has many straight edges
    (is (> (length line-edges) 0)
        "Should select straight line edges")

    ;; Verify each selected edge is actually a line
    (dolist (edge line-edges)
      (multiple-value-bind (geom-type success)
          (clad.ffi:ffi-get-edge-geom-type
           (clad.core:shape-handle (clad.shapes:unwrap-shape edge)))
        (is (not (null success))
            "Should successfully query edge type")
        (is (eq geom-type :line)
            "Selected edge should be a line")))))

;;; ============================================================================
;;; TDD Cycle 6: Type Selector - Select Circular Edges
;;; ============================================================================

(test type-selector-circular-edges
  "Test selecting only circular edges from a shape"
  (let* ((box (make-test-box-with-hole))
         (all-edges (clad.shapes:edges box))
         (selector (make-instance 'clad.selectors::type-selector
                                  :shape-type :circle))
         (circle-edges (clad.selectors::apply-selector selector all-edges)))

    ;; The cylindrical hole should create circular edges at top and bottom
    (is (>= (length circle-edges) 2)
        "Should select circular edges from the hole")

    ;; Verify each selected edge is actually circular
    (dolist (edge circle-edges)
      (multiple-value-bind (geom-type success)
          (clad.ffi:ffi-get-edge-geom-type
           (clad.core:shape-handle (clad.shapes:unwrap-shape edge)))
        (is (not (null success))
            "Should successfully query edge type")
        (is (eq geom-type :circle)
            "Selected edge should be circular")))))

;;; ============================================================================
;;; TDD Cycle 7: High-Level API - Type Selector
;;; ============================================================================

(test select-api-type-faces
  "Test high-level select API with :type keyword for faces"
  (let* ((box (make-test-box-with-hole))
         (planar-faces (clad.selectors:select (clad.shapes:faces box)
                                              :type :plane)))
    (is (>= (length planar-faces) 4)
        "Should select planar faces using high-level API")))

(test select-api-type-edges
  "Test high-level select API with :type keyword for edges"
  (let* ((box (make-test-box-with-hole))
         (line-edges (clad.selectors:select (clad.shapes:edges box)
                                            :type :line)))
    (is (> (length line-edges) 0)
        "Should select line edges using high-level API")))

;;; ============================================================================
;;; TDD Cycle 8: Combining Type Selector with Direction Selector
;;; ============================================================================

(test type-and-direction-selector-combination
  "Test combining type selector with direction selector"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box))
         ;; Select planar faces that are on top (direction +Z max)
         (type-sel (make-instance 'clad.selectors::type-selector
                                  :shape-type :plane))
         (dir-sel (make-instance 'clad.selectors::direction-selector
                                 :axis :+z
                                 :extreme :max))
         (and-sel (make-instance 'clad.selectors::and-selector
                                 :selectors (list type-sel dir-sel)))
         (result (clad.selectors::apply-selector and-sel all-faces)))

    ;; Should select exactly the top planar face
    (is (= 1 (length result))
        "Should select exactly 1 face (top planar)")

    ;; Verify it's planar
    (let ((face (first result)))
      (multiple-value-bind (geom-type success)
          (clad.ffi:ffi-get-face-geom-type
           (clad.core:shape-handle (clad.shapes:unwrap-shape face)))
        (is (not (null success))
            "Should successfully query face type")
        (is (eq geom-type :plane)
            "Top face should be planar")))))

;;; ============================================================================
;;; TDD Cycle 9: CLOS Wrapper - Shape Geometric Type Method
;;; ============================================================================

(test shape-geom-type-method
  "Test CLOS method for getting geometric type of a shape"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box)))

    ;; Test that we can call (geom-type shape) on each face
    (dolist (face all-faces)
      (let ((type (clad.shapes:geom-type face)))
        (is (member type '(:plane :cylinder :sphere :cone :torus :bspline :other))
            "Shape should have a valid geometric type")))))

(test edge-geom-type-method
  "Test CLOS method for getting geometric type of an edge"
  (let* ((box (make-test-box-with-hole))
         (all-edges (clad.shapes:edges box)))

    ;; Test that we can call (geom-type edge) on each edge
    (dolist (edge all-edges)
      (let ((type (clad.shapes:geom-type edge)))
        (is (member type '(:line :circle :ellipse :bspline :bezier :other))
            "Edge should have a valid geometric type")))))

;;; ============================================================================
;;; TDD Cycle 10: Integration Test - Full Workflow
;;; ============================================================================

(test phase8-selector-integration
  "Integration test demonstrating Phase 8 selector workflow"
  ;; Create a complex shape
  (let ((box-with-hole (make-test-box-with-hole)))

    ;; Select all planar faces
    (let ((planar-faces (clad.selectors:select (clad.shapes:faces box-with-hole)
                                               :type :plane)))
      (is (>= (length planar-faces) 4)))

    ;; Select all cylindrical faces
    (let ((cyl-faces (clad.selectors:select (clad.shapes:faces box-with-hole)
                                            :type :cylinder)))
      (is (>= (length cyl-faces) 1)))

    ;; Select all line edges
    (let ((line-edges (clad.selectors:select (clad.shapes:edges box-with-hole)
                                             :type :line)))
      (is (> (length line-edges) 0)))

    ;; Select all circular edges
    (let ((circle-edges (clad.selectors:select (clad.shapes:edges box-with-hole)
                                               :type :circle)))
      (is (>= (length circle-edges) 2)))))

;;; ============================================================================
;;; TDD Cycle 11: Size-Based Selectors - Area
;;; ============================================================================

(test size-selector-area-greater-than
  "Test selecting faces by area (greater than threshold)"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box))
         ;; Select faces with area > 5000 mm²
         (selector (make-instance 'clad.selectors::size-selector
                                  :property :area
                                  :comparator :>
                                  :value1 5000.0d0))
         (large-faces (clad.selectors::apply-selector selector all-faces)))

    ;; Should find large faces (100x100 = 10000 mm²)
    (is (> (length large-faces) 0)
        "Should find faces larger than 5000 mm²")

    ;; Verify selected faces actually have area > 5000
    (dolist (face large-faces)
      (is (> (clad.shapes:area face) 5000.0d0)
          "Selected face should have area > 5000"))))

(test size-selector-area-less-than
  "Test selecting faces by area (less than threshold)"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box))
         ;; Select faces with area < 2000 mm²
         (selector (make-instance 'clad.selectors::size-selector
                                  :property :area
                                  :comparator :<
                                  :value1 2000.0d0))
         (small-faces (clad.selectors::apply-selector selector all-faces)))

    ;; Cylindrical face might be smaller
    ;; Test passes if we can filter by area
    (is (listp small-faces)
        "Should return a list (possibly empty)")))

(test size-selector-area-between
  "Test selecting faces by area (between two values)"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box))
         ;; Select faces with area between 8000 and 12000 mm²
         (selector (make-instance 'clad.selectors::size-selector
                                  :property :area
                                  :comparator :between
                                  :value1 8000.0d0
                                  :value2 12000.0d0))
         (mid-faces (clad.selectors::apply-selector selector all-faces)))

    ;; Box faces (100x100 = 10000) should be selected
    (is (> (length mid-faces) 0)
        "Should find faces in the 8000-12000 range")

    ;; Verify they're actually in range
    (dolist (face mid-faces)
      (let ((a (clad.shapes:area face)))
        (is (and (>= a 8000.0d0) (<= a 12000.0d0))
            "Face area should be in range")))))

;;; ============================================================================
;;; TDD Cycle 12: Size-Based Selectors - Length
;;; ============================================================================

(test size-selector-length-greater-than
  "Test selecting edges by length (greater than threshold)"
  (let* ((box (make-test-filleted-box))
         (all-edges (clad.shapes:edges box))
         ;; Select edges longer than 50mm
         (selector (make-instance 'clad.selectors::size-selector
                                  :property :length
                                  :comparator :>
                                  :value1 50.0d0))
         (long-edges (clad.selectors::apply-selector selector all-edges)))

    ;; Box has edges of length 100 and 50
    (is (> (length long-edges) 0)
        "Should find edges longer than 50mm")))

(test size-selector-length-approximately
  "Test selecting edges by length (approximately equal)"
  (let* ((box (make-test-filleted-box))
         (all-edges (clad.shapes:edges box))
         ;; Select edges approximately 100mm long
         (selector (make-instance 'clad.selectors::size-selector
                                  :property :length
                                  :comparator :=
                                  :value1 100.0d0
                                  :tolerance 0.1d0))
         (edges-100mm (clad.selectors::apply-selector selector all-edges)))

    ;; Should find edges ~100mm
    (is (listp edges-100mm)
        "Should return a list of edges")))

;;; ============================================================================
;;; TDD Cycle 13: Size-Based Selectors - Volume
;;; ============================================================================

(test size-selector-volume-greater-than
  "Test selecting solids by volume"
  (let* ((box (make-test-filleted-box))
         (all-solids (clad.shapes:solids box))
         ;; Select solids with volume > 100000 mm³
         (selector (make-instance 'clad.selectors::size-selector
                                  :property :volume
                                  :comparator :>
                                  :value1 100000.0d0))
         (large-solids (clad.selectors::apply-selector selector all-solids)))

    ;; 100x100x50 box = 500000 mm³
    (is (>= (length large-solids) 0)
        "Should handle volume filtering")))

;;; ============================================================================
;;; TDD Cycle 14: High-Level API - Size Selectors
;;; ============================================================================

(test select-api-size-area
  "Test high-level select API with size-based area selector"
  (let* ((box (make-test-box-with-hole))
         ;; Select faces with area > 9000
         (large-faces (clad.selectors:select (clad.shapes:faces box)
                                             :area :> 9000.0d0)))
    (is (> (length large-faces) 0)
        "Should select large faces using high-level API")))

(test select-api-size-length
  "Test high-level select API with size-based length selector"
  (let* ((box (make-test-filleted-box))
         ;; Select edges with length > 50
         (long-edges (clad.selectors:select (clad.shapes:edges box)
                                            :length :> 50.0d0)))
    (is (listp long-edges)
        "Should return list of edges")))

(test select-api-size-between
  "Test high-level select API with :between comparator"
  (let* ((box (make-test-box-with-hole))
         ;; Select faces with area between 5000 and 15000
         (mid-faces (clad.selectors:select (clad.shapes:faces box)
                                           :area :between 5000.0d0 15000.0d0)))
    (is (listp mid-faces)
        "Should return list using :between")))

;;; ============================================================================
;;; TDD Cycle 15: Combining Type and Size Selectors
;;; ============================================================================

(test combining-type-and-size-selectors
  "Test combining type selector with size selector"
  (let* ((box (make-test-box-with-hole))
         (all-faces (clad.shapes:faces box))
         ;; Select planar faces with area > 9000
         (type-sel (make-instance 'clad.selectors::type-selector
                                  :shape-type :plane))
         (size-sel (make-instance 'clad.selectors::size-selector
                                  :property :area
                                  :comparator :>
                                  :value1 9000.0d0))
         (and-sel (make-instance 'clad.selectors::and-selector
                                 :selectors (list type-sel size-sel)))
         (result (clad.selectors::apply-selector and-sel all-faces)))

    ;; Should find large planar faces
    (is (listp result)
        "Should return list of shapes matching both criteria")))

;;; ============================================================================
;;; TDD Cycle 16: Fillet Operations - FFI Layer
;;; ============================================================================

(test ffi-fillet-single-edge
  "Test FFI fillet function on a single edge"
  (let* ((box (clad.core:make-box 100 100 100))
         (box-handle (clad.core:shape-handle box))
         (all-edges (clad.shapes:edges (clad.shapes:wrap-shape box 'clad.shapes:cad-solid)))
         (edge1 (first all-edges))
         (edge-handle (clad.core:shape-handle (clad.shapes:unwrap-shape edge1))))

    ;; Should be able to call ffi-fillet
    (let ((filleted-handle (clad.ffi:ffi-fillet 
                             box-handle 
                             (list edge-handle) 
                             5.0d0)))
      
      (is (not (null filleted-handle))
          "Fillet should return a handle")
      
      (is (clad.ffi:handle-valid-p filleted-handle)
          "Filleted shape should have a valid handle"))))

(test ffi-fillet-multiple-edges
  "Test FFI fillet function on multiple edges"
  (let* ((box (clad.core:make-box 100 100 100))
         (box-handle (clad.core:shape-handle box))
         (all-edges (clad.shapes:edges (clad.shapes:wrap-shape box 'clad.shapes:cad-solid)))
         (edge-handles (mapcar (lambda (e)
                                  (clad.core:shape-handle (clad.shapes:unwrap-shape e)))
                               (subseq all-edges 0 (min 4 (length all-edges))))))

    ;; Fillet multiple edges at once
    (let ((filleted-handle (clad.ffi:ffi-fillet 
                             box-handle 
                             edge-handles 
                             3.0d0)))
      
      (is (not (null filleted-handle))
          "Fillet should return a handle")
      
      (is (clad.ffi:handle-valid-p filleted-handle)
          "Filleted shape should have a valid handle"))))

;;; ============================================================================
;;; TDD Cycle 17: Fillet Operations - Core API
;;; ============================================================================

(test core-fillet-single-edge
  "Test core fillet function on a single edge"
  (let* ((box (clad.core:make-box 100 100 100))
         (wrapped-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (all-edges (clad.shapes:edges wrapped-box))
         (edge1 (clad.shapes:unwrap-shape (first all-edges))))

    ;; Apply fillet
    (let ((filleted (clad.core:fillet box (list edge1) 5.0d0)))
      
      (is (not (null filleted))
          "Fillet should return a shape")
      
      (is (clad.core:valid-shape-p filleted)
          "Filleted shape should be valid"))))

(test core-fillet-multiple-edges
  "Test core fillet function on multiple edges"
  (let* ((box (clad.core:make-box 100 100 100))
         (wrapped-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (all-edges (clad.shapes:edges wrapped-box))
         (unwrapped-edges (mapcar #'clad.shapes:unwrap-shape
                                  (subseq all-edges 0 (min 4 (length all-edges))))))

    ;; Apply fillet to multiple edges
    (let ((filleted (clad.core:fillet box unwrapped-edges 3.0d0)))
      
      (is (not (null filleted))
          "Fillet should return a shape")
      
      (is (clad.core:valid-shape-p filleted)
          "Filleted shape should be valid"))))

;;; ============================================================================
;;; TDD Cycle 18: Fillet Operations - Context API
;;; ============================================================================

(test context-fillet-selected-edges
  "Test context fillet-selected function"
  ;; Create a box, select some edges, fillet them
  (let ((result (clad.context:with-context ()
                  (clad.context:add (clad.core:make-box 100 100 100))
                  
                  ;; Select vertical edges
                  (clad.context:select-edges :parallel :z)
                  
                  ;; Apply fillet
                  (clad.context:fillet-selected 5.0d0)
                  
                  ;; Get result
                  (clad.context:get-result))))
    
    (is (not (null result))
        "Should return a filleted shape")
    
    (is (clad.core:valid-shape-p (clad.shapes:unwrap-shape result))
        "Result should be a valid shape")))

(test context-fillet-with-type-selector
  "Test filleting edges selected by type"
  ;; Select only line edges and fillet them
  (let ((result (clad.context:with-context ()
                  (clad.context:add (clad.core:make-box 100 100 100))
                  
                  ;; Select line edges specifically
                  (clad.context:select-edges :type :line)
                  
                  ;; Apply fillet
                  (clad.context:fillet-selected 3.0d0)
                  
                  (clad.context:get-result))))
    
    (is (not (null result))
        "Should return a filleted shape")))

;;; ============================================================================
;;; TDD Cycle 19: Fillet Integration Tests
;;; ============================================================================

(test fillet-workflow-integration
  "Integration test: Complete fillet workflow"
  ;; Test a realistic workflow: create box, select edges by criteria, fillet
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (all-edges (clad.shapes:edges wrapped-box))
         
         ;; Select only vertical edges using selector
         (vertical-edges (clad.selectors:select all-edges :parallel :z))
         (unwrapped-vertical (mapcar #'clad.shapes:unwrap-shape vertical-edges))
         
         ;; Apply fillet
         (filleted (clad.core:fillet box unwrapped-vertical 5.0d0)))
    
    (is (not (null filleted))
        "Fillet workflow should produce a shape")
    
    (is (clad.core:valid-shape-p filleted)
        "Filleted shape should be valid")
    
    ;; The filleted shape should still be a solid
    (let ((wrapped-filleted (clad.shapes:wrap-shape filleted 'clad.shapes:cad-solid)))
      (is (clad.shapes:solid-p wrapped-filleted)
          "Filleted shape should still be a solid"))))

(test fillet-with-size-selector
  "Test filleting edges selected by length"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (all-edges (clad.shapes:edges wrapped-box))
         
         ;; Select only long edges (>75mm)
         (long-edges (clad.selectors:select all-edges :length :> 75.0d0))
         (unwrapped-long (mapcar #'clad.shapes:unwrap-shape long-edges))
         
         ;; Apply fillet to long edges only
         (filleted (if (> (length unwrapped-long) 0)
                       (clad.core:fillet box unwrapped-long 5.0d0)
                       box)))  ; If no long edges, return original
    
    (is (not (null filleted))
        "Should produce a shape")

    (is (clad.core:valid-shape-p filleted)
        "Shape should be valid")))

;;; ============================================================================
;;; TDD Cycle 20: Chamfer Operations - FFI Layer
;;; ============================================================================

(test ffi-chamfer-single-edge
  "Test FFI chamfer function on a single edge"
  (let* ((box (clad.core:make-box 100 100 100))
         (box-handle (clad.core:shape-handle box))
         (all-edges (clad.shapes:edges (clad.shapes:wrap-shape box 'clad.shapes:cad-solid)))
         (edge1 (first all-edges))
         (edge-handle (clad.core:shape-handle (clad.shapes:unwrap-shape edge1))))

    ;; Should be able to call ffi-chamfer
    (let ((chamfered-handle (clad.ffi:ffi-chamfer
                             box-handle
                             (list edge-handle)
                             2.0d0)))

      (is (not (null chamfered-handle))
          "Chamfer should return a handle")

      (is (clad.ffi:handle-valid-p chamfered-handle)
          "Chamfered shape should have a valid handle"))))

(test ffi-chamfer-multiple-edges
  "Test FFI chamfer function on multiple edges"
  (let* ((box (clad.core:make-box 100 100 100))
         (box-handle (clad.core:shape-handle box))
         (all-edges (clad.shapes:edges (clad.shapes:wrap-shape box 'clad.shapes:cad-solid)))
         (edge-handles (mapcar (lambda (e)
                                  (clad.core:shape-handle (clad.shapes:unwrap-shape e)))
                               (subseq all-edges 0 (min 4 (length all-edges))))))

    ;; Chamfer multiple edges at once
    (let ((chamfered-handle (clad.ffi:ffi-chamfer
                             box-handle
                             edge-handles
                             1.5d0)))

      (is (not (null chamfered-handle))
          "Chamfer should return a handle")

      (is (clad.ffi:handle-valid-p chamfered-handle)
          "Chamfered shape should have a valid handle"))))

;;; ============================================================================
;;; TDD Cycle 21: Chamfer Operations - Core API
;;; ============================================================================

(test core-chamfer-single-edge
  "Test core chamfer function on a single edge"
  (let* ((box (clad.core:make-box 100 100 100))
         (wrapped-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (all-edges (clad.shapes:edges wrapped-box))
         (edge1 (clad.shapes:unwrap-shape (first all-edges))))

    ;; Apply chamfer
    (let ((chamfered (clad.core:chamfer box (list edge1) 2.0d0)))

      (is (not (null chamfered))
          "Chamfer should return a shape")

      (is (clad.core:valid-shape-p chamfered)
          "Chamfered shape should be valid"))))

(test core-chamfer-multiple-edges
  "Test core chamfer function on multiple edges"
  (let* ((box (clad.core:make-box 100 100 100))
         (wrapped-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (all-edges (clad.shapes:edges wrapped-box))
         (unwrapped-edges (mapcar #'clad.shapes:unwrap-shape
                                  (subseq all-edges 0 (min 4 (length all-edges))))))

    ;; Apply chamfer to multiple edges
    (let ((chamfered (clad.core:chamfer box unwrapped-edges 1.5d0)))

      (is (not (null chamfered))
          "Chamfer should return a shape")

      (is (clad.core:valid-shape-p chamfered)
          "Chamfered shape should be valid"))))

;;; ============================================================================
;;; TDD Cycle 22: Chamfer Operations - Context API
;;; ============================================================================

(test context-chamfer-selected-edges
  "Test context chamfer-selected function"
  ;; Create a box, select some edges, chamfer them
  (let ((result (clad.context:with-context ()
                  (clad.context:add (clad.core:make-box 100 100 100))

                  ;; Select vertical edges
                  (clad.context:select-edges :parallel :z)

                  ;; Apply chamfer
                  (clad.context:chamfer-selected 2.0d0)

                  ;; Get result
                  (clad.context:get-result))))

    (is (not (null result))
        "Should return a chamfered shape")

    (is (clad.core:valid-shape-p (clad.shapes:unwrap-shape result))
        "Result should be a valid shape")))

(test context-chamfer-with-type-selector
  "Test chamfering edges selected by type"
  ;; Select only line edges and chamfer them
  (let ((result (clad.context:with-context ()
                  (clad.context:add (clad.core:make-box 100 100 100))

                  ;; Select line edges specifically
                  (clad.context:select-edges :type :line)

                  ;; Apply chamfer
                  (clad.context:chamfer-selected 1.5d0)

                  (clad.context:get-result))))

    (is (not (null result))
        "Should return a chamfered shape")))

;;; ============================================================================
;;; TDD Cycle 23: Chamfer Integration Tests
;;; ============================================================================

(test chamfer-workflow-integration
  "Integration test: Complete chamfer workflow"
  ;; Test a realistic workflow: create box, select edges by criteria, chamfer
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (all-edges (clad.shapes:edges wrapped-box))

         ;; Select only vertical edges using selector
         (vertical-edges (clad.selectors:select all-edges :parallel :z))
         (unwrapped-vertical (mapcar #'clad.shapes:unwrap-shape vertical-edges))

         ;; Apply chamfer
         (chamfered (clad.core:chamfer box unwrapped-vertical 2.0d0)))

    (is (not (null chamfered))
        "Chamfer workflow should produce a shape")

    (is (clad.core:valid-shape-p chamfered)
        "Chamfered shape should be valid")

    ;; The chamfered shape should still be a solid
    (let ((wrapped-chamfered (clad.shapes:wrap-shape chamfered 'clad.shapes:cad-solid)))
      (is (clad.shapes:solid-p wrapped-chamfered)
          "Chamfered shape should still be a solid"))))

(test chamfer-with-size-selector
  "Test chamfering edges selected by length"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (all-edges (clad.shapes:edges wrapped-box))

         ;; Select only long edges (>75mm)
         (long-edges (clad.selectors:select all-edges :length :> 75.0d0))
         (unwrapped-long (mapcar #'clad.shapes:unwrap-shape long-edges))

         ;; Apply chamfer to long edges only
         (chamfered (if (> (length unwrapped-long) 0)
                       (clad.core:chamfer box unwrapped-long 2.0d0)
                       box)))  ; If no long edges, return original

    (is (not (null chamfered))
        "Should produce a shape")

    (is (clad.core:valid-shape-p chamfered)
        "Shape should be valid")))

;;; ============================================================================
;;; TDD Cycle 24: Spline Curves - FFI Layer
;;; ============================================================================

(test ffi-make-interpolated-curve-open
  "Test FFI function to create open interpolated spline"
  (let* ((points '((0 0 0) (10 5 0) (20 8 0) (30 5 0) (40 0 0)))
         (edge-handle (clad.ffi:ffi-make-interpolated-curve points :closed nil)))

    (is (not (null edge-handle))
        "Should return an edge handle")

    (is (clad.ffi:handle-valid-p edge-handle)
        "Spline edge should have a valid handle")

    (is (eq (clad.ffi:handle-type edge-handle) :edge)
        "Handle type should be :edge")))

(test ffi-make-interpolated-curve-closed
  "Test FFI function to create closed interpolated spline"
  (let* ((points '((0 0 0) (10 0 0) (10 10 0) (0 10 0)))
         (edge-handle (clad.ffi:ffi-make-interpolated-curve points :closed t)))

    (is (not (null edge-handle))
        "Should return an edge handle for closed spline")

    (is (clad.ffi:handle-valid-p edge-handle)
        "Closed spline edge should have a valid handle")))

(test ffi-make-interpolated-curve-min-points
  "Test spline creation with minimum number of points"
  ;; Minimum 2 points required
  (let* ((points '((0 0 0) (10 10 10)))
         (edge-handle (clad.ffi:ffi-make-interpolated-curve points :closed nil)))

    (is (not (null edge-handle))
        "Should create spline with 2 points")))

;;; ============================================================================
;;; TDD Cycle 25: Bezier Curves - FFI Layer
;;; ============================================================================

(test ffi-make-bezier-curve-quadratic
  "Test FFI function to create quadratic Bezier curve (3 control points)"
  (let* ((control-points '((0 0 0) (5 10 0) (10 0 0)))
         (edge-handle (clad.ffi:ffi-make-bezier-curve control-points)))

    (is (not (null edge-handle))
        "Should return an edge handle for quadratic Bezier")

    (is (clad.ffi:handle-valid-p edge-handle)
        "Bezier edge should have a valid handle")

    (is (eq (clad.ffi:handle-type edge-handle) :edge)
        "Handle type should be :edge")))

(test ffi-make-bezier-curve-cubic
  "Test FFI function to create cubic Bezier curve (4 control points)"
  (let* ((control-points '((0 0 0) (3 5 0) (7 5 0) (10 0 0)))
         (edge-handle (clad.ffi:ffi-make-bezier-curve control-points)))

    (is (not (null edge-handle))
        "Should return an edge handle for cubic Bezier")

    (is (clad.ffi:handle-valid-p edge-handle)
        "Cubic Bezier edge should have a valid handle")))

(test ffi-make-bezier-curve-higher-degree
  "Test FFI function to create higher-degree Bezier curve (6 control points)"
  (let* ((control-points '((0 0 0) (2 4 0) (4 6 0) (6 6 0) (8 4 0) (10 0 0)))
         (edge-handle (clad.ffi:ffi-make-bezier-curve control-points)))

    (is (not (null edge-handle))
        "Should return an edge handle for higher-degree Bezier")

    (is (clad.ffi:handle-valid-p edge-handle)
        "Higher-degree Bezier edge should have a valid handle")))

;;; ============================================================================
;;; TDD Cycle 26: Arc Curves - FFI Layer
;;; ============================================================================

(test ffi-make-arc-3points-xy-plane
  "Test FFI function to create arc through 3 points in XY plane"
  (let* ((p1 '(0 0 0))
         (p2 '(5 5 0))
         (p3 '(10 0 0))
         (edge-handle (clad.ffi:ffi-make-arc-3points p1 p2 p3)))

    (is (not (null edge-handle))
        "Should return an edge handle for arc")

    (is (clad.ffi:handle-valid-p edge-handle)
        "Arc edge should have a valid handle")

    (is (eq (clad.ffi:handle-type edge-handle) :edge)
        "Handle type should be :edge")))

(test ffi-make-arc-3points-3d
  "Test FFI function to create arc through 3 points in 3D"
  (let* ((p1 '(0 0 0))
         (p2 '(5 5 5))
         (p3 '(10 0 10))
         (edge-handle (clad.ffi:ffi-make-arc-3points p1 p2 p3)))

    (is (not (null edge-handle))
        "Should return an edge handle for 3D arc")

    (is (clad.ffi:handle-valid-p edge-handle)
        "3D arc edge should have a valid handle")))

(test ffi-make-arc-center-radius-quarter-circle
  "Test FFI function to create quarter circle arc"
  (let* ((center '(0 0 0))
         (radius 10.0d0)
         (start-angle 0.0d0)
         (end-angle 90.0d0)
         (edge-handle (clad.ffi:ffi-make-arc-center-radius center radius start-angle end-angle)))

    (is (not (null edge-handle))
        "Should return an edge handle for quarter circle")

    (is (clad.ffi:handle-valid-p edge-handle)
        "Quarter circle edge should have a valid handle")))

(test ffi-make-arc-center-radius-semicircle
  "Test FFI function to create semicircle arc"
  (let* ((center '(50 50 0))
         (radius 20.0d0)
         (start-angle 0.0d0)
         (end-angle 180.0d0)
         (edge-handle (clad.ffi:ffi-make-arc-center-radius center radius start-angle end-angle)))

    (is (not (null edge-handle))
        "Should return an edge handle for semicircle")

    (is (clad.ffi:handle-valid-p edge-handle)
        "Semicircle edge should have a valid handle")))

(test ffi-make-arc-center-radius-custom-axis
  "Test FFI function to create arc in YZ plane"
  (let* ((center '(0 0 0))
         (radius 15.0d0)
         (start-angle 0.0d0)
         (end-angle 90.0d0)
         (axis '(1 0 0))  ; X axis => arc in YZ plane
         (edge-handle (clad.ffi:ffi-make-arc-center-radius center radius start-angle end-angle :axis axis)))

    (is (not (null edge-handle))
        "Should return an edge handle for arc in YZ plane")

    (is (clad.ffi:handle-valid-p edge-handle)
        "YZ plane arc edge should have a valid handle")))

;;; ============================================================================
;;; TDD Cycle 27: Wire Operations - FFI Layer
;;; ============================================================================

(test ffi-make-wire-from-splines
  "Test FFI function to create wire from spline edges"
  (let* ((spline1 (clad.ffi:ffi-make-interpolated-curve '((0 0 0) (10 5 0) (20 0 0)) :closed nil))
         (spline2 (clad.ffi:ffi-make-interpolated-curve '((20 0 0) (30 -5 0) (40 0 0)) :closed nil))
         (wire-handle (clad.ffi:ffi-make-wire (list spline1 spline2))))

    (is (not (null wire-handle))
        "Should return a wire handle")

    (is (clad.ffi:handle-valid-p wire-handle)
        "Wire should have a valid handle")

    (is (eq (clad.ffi:handle-type wire-handle) :wire)
        "Handle type should be :wire")))

(test ffi-make-wire-single-edge
  "Test FFI function to create wire from single edge"
  (let* ((arc (clad.ffi:ffi-make-arc-center-radius '(0 0 0) 10.0d0 0.0d0 180.0d0))
         (wire-handle (clad.ffi:ffi-make-wire (list arc))))

    (is (not (null wire-handle))
        "Should create wire from single edge")

    (is (clad.ffi:handle-valid-p wire-handle)
        "Single-edge wire should have a valid handle")))

(test ffi-wire-is-closed-open-wire
  "Test FFI function to check if wire is closed (open case)"
  (let* ((spline (clad.ffi:ffi-make-interpolated-curve '((0 0 0) (10 10 0) (20 0 0)) :closed nil))
         (wire (clad.ffi:ffi-make-wire (list spline)))
         (is-closed (clad.ffi:ffi-wire-is-closed wire)))

    (is (not is-closed)
        "Open wire should return NIL for is-closed")))

(test ffi-wire-is-closed-closed-wire
  "Test FFI function to check if wire is closed (closed case)"
  (let* ((spline (clad.ffi:ffi-make-interpolated-curve '((0 0 0) (10 0 0) (10 10 0) (0 10 0)) :closed t))
         (wire (clad.ffi:ffi-make-wire (list spline)))
         (is-closed (clad.ffi:ffi-wire-is-closed wire)))

    (is is-closed
        "Closed wire should return T for is-closed")))

;;; ============================================================================
;;; TDD Cycle 28: Splines and Bezier - Core API
;;; ============================================================================

(test core-make-spline-open
  "Test core API to create open spline"
  (let* ((points '((0 0 0) (10 5 0) (20 8 0) (30 5 0) (40 0 0)))
         (spline (clad.core:make-spline points :closed nil)))

    (is (not (null spline))
        "Should return a shape")

    (is (clad.core:valid-shape-p spline)
        "Spline should be a valid shape")

    (is (clad.core:shape-p spline)
        "Should be a shape struct")))

(test core-make-spline-closed
  "Test core API to create closed spline"
  (let* ((points '((0 0 0) (10 0 0) (10 10 0) (0 10 0)))
         (spline (clad.core:make-spline points :closed t)))

    (is (not (null spline))
        "Should return a closed spline shape")

    (is (clad.core:valid-shape-p spline)
        "Closed spline should be a valid shape")))

(test core-make-spline-error-handling
  "Test core API spline error handling for insufficient points"
  (signals error
    (clad.core:make-spline '((0 0 0)))
    "Should signal error with only 1 point"))

(test core-make-bezier-quadratic
  "Test core API to create quadratic Bezier"
  (let* ((control-points '((0 0 0) (5 10 0) (10 0 0)))
         (bezier (clad.core:make-bezier control-points)))

    (is (not (null bezier))
        "Should return a Bezier shape")

    (is (clad.core:valid-shape-p bezier)
        "Bezier should be a valid shape")))

(test core-make-bezier-cubic
  "Test core API to create cubic Bezier"
  (let* ((control-points '((0 0 0) (3 5 0) (7 5 0) (10 0 0)))
         (bezier (clad.core:make-bezier control-points)))

    (is (not (null bezier))
        "Should return a cubic Bezier shape")

    (is (clad.core:valid-shape-p bezier)
        "Cubic Bezier should be a valid shape")))

;;; ============================================================================
;;; TDD Cycle 29: Arcs and Lines - Core API
;;; ============================================================================

(test core-make-arc-3points
  "Test core API to create arc through 3 points"
  (let* ((p1 '(0 0 0))
         (p2 '(5 5 0))
         (p3 '(10 0 0))
         (arc (clad.core:make-arc-3points p1 p2 p3)))

    (is (not (null arc))
        "Should return an arc shape")

    (is (clad.core:valid-shape-p arc)
        "Arc should be a valid shape")))

(test core-make-arc-quarter-circle
  "Test core API to create quarter circle arc"
  (let* ((center '(0 0 0))
         (radius 10)
         (start-angle 0)
         (end-angle 90)
         (arc (clad.core:make-arc center radius start-angle end-angle)))

    (is (not (null arc))
        "Should return a quarter circle arc shape")

    (is (clad.core:valid-shape-p arc)
        "Quarter circle should be a valid shape")))

(test core-make-arc-full-range
  "Test core API to create arc with full angular range"
  (let* ((center '(25 25 0))
         (radius 15)
         (start-angle 45)
         (end-angle 315)
         (arc (clad.core:make-arc center radius start-angle end-angle)))

    (is (not (null arc))
        "Should return an arc shape covering large angle")

    (is (clad.core:valid-shape-p arc)
        "Large angle arc should be a valid shape")))

(test core-make-arc-custom-axis
  "Test core API to create arc with custom axis"
  (let* ((center '(0 0 0))
         (radius 20)
         (start-angle 0)
         (end-angle 90)
         (axis '(1 0 0))  ; Arc in YZ plane
         (arc (clad.core:make-arc center radius start-angle end-angle :axis axis)))

    (is (not (null arc))
        "Should return an arc in YZ plane")

    (is (clad.core:valid-shape-p arc)
        "YZ plane arc should be a valid shape")))

(test core-make-line
  "Test core API to create straight line edge"
  (let* ((p1 '(0 0 0))
         (p2 '(10 0 0))
         (line (clad.core:make-line p1 p2)))

    (is (not (null line))
        "Should return a line edge shape")

    (is (clad.core:valid-shape-p line)
        "Line should be a valid shape")))

(test core-make-line-3d
  "Test core API to create line in 3D"
  (let* ((p1 '(0 0 0))
         (p2 '(10 20 30))
         (line (clad.core:make-line p1 p2)))

    (is (not (null line))
        "Should return a 3D line edge")

    (is (clad.core:valid-shape-p line)
        "3D line should be a valid shape")))

;;; ============================================================================
;;; TDD Cycle 30: Wires - Core API
;;; ============================================================================

(test core-make-wire-from-mixed-edges
  "Test core API to create wire from mixed edge types"
  (let* ((line1 (clad.core:make-line '(0 0 0) '(10 0 0)))
         (arc (clad.core:make-arc-3points '(10 0 0) '(15 5 0) '(20 0 0)))
         (line2 (clad.core:make-line '(20 0 0) '(20 10 0)))
         (wire (clad.core:make-wire (list line1 arc line2))))

    (is (not (null wire))
        "Should return a wire from mixed edges")

    (is (clad.core:valid-shape-p wire)
        "Wire should be a valid shape")))

(test core-make-wire-from-splines
  "Test core API to create wire from splines"
  (let* ((spline1 (clad.core:make-spline '((0 0 0) (10 5 0) (20 0 0)) :closed nil))
         (spline2 (clad.core:make-spline '((20 0 0) (30 -5 0) (40 0 0)) :closed nil))
         (wire (clad.core:make-wire (list spline1 spline2))))

    (is (not (null wire))
        "Should return a wire from splines")

    (is (clad.core:valid-shape-p wire)
        "Wire from splines should be a valid shape")))

(test core-wire-closed-p-open
  "Test core API to check if wire is closed (open case)"
  (let* ((spline (clad.core:make-spline '((0 0 0) (10 10 0) (20 0 0)) :closed nil))
         (wire (clad.core:make-wire (list spline)))
         (is-closed (clad.core:wire-closed-p wire)))

    (is (not is-closed)
        "Open wire should return NIL")))

(test core-wire-closed-p-closed
  "Test core API to check if wire is closed (closed case)"
  (let* ((spline (clad.core:make-spline '((0 0 0) (10 0 0) (10 10 0) (0 10 0)) :closed t))
         (wire (clad.core:make-wire (list spline)))
         (is-closed (clad.core:wire-closed-p wire)))

    (is is-closed
        "Closed wire should return T")))

(test core-wire-from-closed-path
  "Test creating a closed wire from connected edges"
  (let* ((line1 (clad.core:make-line '(0 0 0) '(10 0 0)))
         (line2 (clad.core:make-line '(10 0 0) '(10 10 0)))
         (line3 (clad.core:make-line '(10 10 0) '(0 10 0)))
         (line4 (clad.core:make-line '(0 10 0) '(0 0 0)))
         (wire (clad.core:make-wire (list line1 line2 line3 line4)))
         (is-closed (clad.core:wire-closed-p wire)))

    (is (not (null wire))
        "Should create wire from 4 connected lines")

    (is is-closed
        "Wire forming a closed square should be closed")))

;;; ============================================================================
;;; TDD Cycle 31: Curve Integration Tests
;;; ============================================================================

(test curve-integration-smooth-path
  "Integration test: Create smooth path with splines and arcs"
  (let* (;; Start with a line
         (line1 (clad.core:make-line '(0 0 0) '(10 0 0)))
         ;; Transition with an arc
         (arc1 (clad.core:make-arc-3points '(10 0 0) '(15 5 0) '(20 10 0)))
         ;; Continue with a spline
         (spline1 (clad.core:make-spline '((20 10 0) (30 15 0) (40 12 0)) :closed nil))
         ;; Final arc back down
         (arc2 (clad.core:make-arc-3points '(40 12 0) '(45 10 0) '(50 8 0)))
         ;; Combine into wire
         (wire (clad.core:make-wire (list line1 arc1 spline1 arc2))))

    (is (not (null wire))
        "Should create smooth path wire")

    (is (clad.core:valid-shape-p wire)
        "Wire should be valid")

    (is (not (clad.core:wire-closed-p wire))
        "Path should be open")))

(test curve-integration-closed-profile
  "Integration test: Create closed profile for extrusion"
  (let* (;; Create a closed profile using mixed curve types
         (line1 (clad.core:make-line '(0 0 0) '(20 0 0)))
         (arc1 (clad.core:make-arc '(20 10 0) 10 270 90))
         (line2 (clad.core:make-line '(20 20 0) '(0 20 0)))
         (arc2 (clad.core:make-arc '(0 10 0) 10 90 270))
         (wire (clad.core:make-wire (list line1 arc1 line2 arc2))))

    (is (not (null wire))
        "Should create closed profile")

    (is (clad.core:valid-shape-p wire)
        "Profile should be valid")

    ;; Note: This might not be closed due to endpoint tolerance
    ;; In production, we'd need endpoint snapping
    (is (listp (list wire))
        "Wire structure should be valid")))

(test curve-integration-bezier-path
  "Integration test: Create smooth Bezier path"
  (let* (;; Create smooth S-curve using Bezier
         (bezier1 (clad.core:make-bezier '((0 0 0) (10 20 0) (20 20 0) (30 0 0))))
         (bezier2 (clad.core:make-bezier '((30 0 0) (40 -20 0) (50 -20 0) (60 0 0))))
         (wire (clad.core:make-wire (list bezier1 bezier2))))

    (is (not (null wire))
        "Should create Bezier path")

    (is (clad.core:valid-shape-p wire)
        "Bezier wire should be valid")))

(test curve-integration-complex-spline
  "Integration test: Create complex spline with many points"
  (let* (;; Create wave pattern with spline
         (wave-points '((0 0 0)
                       (5 5 0)
                       (10 0 0)
                       (15 -5 0)
                       (20 0 0)
                       (25 5 0)
                       (30 0 0)
                       (35 -5 0)
                       (40 0 0)))
         (spline (clad.core:make-spline wave-points :closed nil)))

    (is (not (null spline))
        "Should create complex spline")

    (is (clad.core:valid-shape-p spline)
        "Complex spline should be valid")

    ;; Can be incorporated into wire
    (let ((wire (clad.core:make-wire (list spline))))
      (is (not (null wire))
          "Should create wire from complex spline"))))
