;;;; tests/core-tests.lisp --- Functional core tests

(in-package :clad.tests)

(in-suite clad-tests)

;;; ============================================================================
;;; Primitive Creation Tests
;;; ============================================================================

(test test-make-box
  "Test box primitive creation"
  (let ((box (clad.core:make-box 100 50 30)))
    (is (clad.core:shape-p box))
    (is (clad.core:valid-shape-p box))))

(test test-make-box-centered
  "Test centered box creation"
  (let ((box (clad.core:make-box 100 50 30 :center t)))
    (is (clad.core:shape-p box))
    (is (clad.core:valid-shape-p box))))

(test test-make-cylinder
  "Test cylinder primitive creation"
  (let ((cyl (clad.core:make-cylinder 10 50)))
    (is (clad.core:shape-p cyl))
    (is (clad.core:valid-shape-p cyl))))

(test test-make-cylinder-centered
  "Test centered cylinder creation"
  (let ((cyl (clad.core:make-cylinder 10 50 :center t)))
    (is (clad.core:shape-p cyl))
    (is (clad.core:valid-shape-p cyl))))

(test test-make-sphere
  "Test sphere primitive creation"
  (let ((sphere (clad.core:make-sphere 25)))
    (is (clad.core:shape-p sphere))
    (is (clad.core:valid-shape-p sphere))))

(test test-make-cone
  "Test cone primitive creation"
  (let ((cone (clad.core:make-cone 20 10 50)))
    (is (clad.core:shape-p cone))
    (is (clad.core:valid-shape-p cone))))

;;; ============================================================================
;;; Boolean Operation Tests
;;; ============================================================================

(test test-union-shapes
  "Test union operation"
  (let* ((box1 (clad.core:make-box 10 10 10))
         (box2 (clad.core:translate (clad.core:make-box 10 10 10) 5 0 0))
         (result (clad.core:union-shapes box1 box2)))
    (is (clad.core:shape-p result))
    (is (clad.core:valid-shape-p result))))

(test test-union-multiple
  "Test union of multiple shapes"
  (let ((result (clad.core:union-shapes
                 (clad.core:make-box 10 10 10)
                 (clad.core:translate (clad.core:make-box 5 5 5) 10 0 0)
                 (clad.core:translate (clad.core:make-box 5 5 5) -10 0 0))))
    (is (clad.core:shape-p result))
    (is (clad.core:valid-shape-p result))))

(test test-cut-shapes
  "Test cut (subtraction) operation"
  (let* ((box (clad.core:make-box 100 100 20))
         (hole (clad.core:translate (clad.core:make-cylinder 10 30) 50 50 0))
         (result (clad.core:cut-shapes box hole)))
    (is (clad.core:shape-p result))
    (is (clad.core:valid-shape-p result))))

(test test-cut-multiple
  "Test cut with multiple tool shapes"
  (let ((result (clad.core:cut-shapes
                 (clad.core:make-box 100 100 20)
                 (clad.core:translate (clad.core:make-cylinder 5 30) 20 20 0)
                 (clad.core:translate (clad.core:make-cylinder 5 30) 80 20 0)
                 (clad.core:translate (clad.core:make-cylinder 5 30) 20 80 0)
                 (clad.core:translate (clad.core:make-cylinder 5 30) 80 80 0))))
    (is (clad.core:shape-p result))
    (is (clad.core:valid-shape-p result))))

(test test-intersect-shapes
  "Test intersection operation"
  (let* ((box (clad.core:make-box 20 20 20))
         (sphere (clad.core:translate (clad.core:make-sphere 15) 10 10 10))
         (result (clad.core:intersect-shapes box sphere)))
    (is (clad.core:shape-p result))
    (is (clad.core:valid-shape-p result))))

;;; ============================================================================
;;; Transformation Tests
;;; ============================================================================

(test test-translate
  "Test translation transformation"
  (let* ((box (clad.core:make-box 10 10 10))
         (translated (clad.core:translate box 5 10 15)))
    (is (clad.core:shape-p translated))
    (is (clad.core:valid-shape-p translated))
    ;; Original should be unchanged
    (is (clad.core:valid-shape-p box))))

(test test-rotate-keyword-axis
  "Test rotation around keyword axis"
  (let* ((box (clad.core:make-box 10 10 10))
         (rotated-z (clad.core:rotate box :z 45))
         (rotated-x (clad.core:rotate box :x 90))
         (rotated-y (clad.core:rotate box :y 180)))
    (is (clad.core:valid-shape-p rotated-z))
    (is (clad.core:valid-shape-p rotated-x))
    (is (clad.core:valid-shape-p rotated-y))))

(test test-rotate-custom-axis
  "Test rotation around custom axis"
  (let* ((box (clad.core:make-box 10 10 10))
         (rotated (clad.core:rotate box '(1 1 0) 45)))
    (is (clad.core:valid-shape-p rotated))))

(test test-mirror
  "Test mirror transformation"
  (let* ((box (clad.core:make-box 10 10 10))
         (mirrored-z (clad.core:mirror box :z))
         (mirrored-x (clad.core:mirror box :x)))
    (is (clad.core:valid-shape-p mirrored-z))
    (is (clad.core:valid-shape-p mirrored-x))))

(test test-scale
  "Test scale transformation"
  (let* ((box (clad.core:make-box 10 10 10))
         (scaled-up (clad.core:scale-shape box 2.0))
         (scaled-down (clad.core:scale-shape box 0.5)))
    (is (clad.core:valid-shape-p scaled-up))
    (is (clad.core:valid-shape-p scaled-down))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(test test-complex-part
  "Test creating a complex part with multiple operations"
  (let* ((base (clad.core:make-box 100 100 20))
         (hole1 (clad.core:translate (clad.core:make-cylinder 5 25) 20 20 0))
         (hole2 (clad.core:translate (clad.core:make-cylinder 5 25) 80 20 0))
         (hole3 (clad.core:translate (clad.core:make-cylinder 5 25) 20 80 0))
         (hole4 (clad.core:translate (clad.core:make-cylinder 5 25) 80 80 0))
         (bracket (clad.core:cut-shapes base hole1 hole2 hole3 hole4)))
    (is (clad.core:shape-p bracket))
    (is (clad.core:valid-shape-p bracket))))

(test test-with-units-integration
  "Test shapes with unit conversion"
  (with-units :in
    (let ((box (clad.core:make-box (dim 4) (dim 2) (dim 1))))
      (is (clad.core:shape-p box))
      (is (clad.core:valid-shape-p box)))))
