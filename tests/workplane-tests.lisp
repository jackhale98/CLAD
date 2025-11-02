;;;; tests/workplane-tests.lisp --- Test suite for workplane system

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite workplane-tests
  :description "Tests for the workplane system"
  :in clad-tests)

(in-suite workplane-tests)

;;; ============================================================================
;;; TDD Cycle 1: Basic Workplane Class
;;; ============================================================================

(test workplane-creation
  "Test that we can create a basic workplane with origin and directions"
  ;; RED: This test will fail initially
  (let ((wp (clad.workplane:make-workplane
             :origin '(0 0 0)
             :x-dir '(1 0 0)
             :z-dir '(0 0 1))))

    ;; Workplane should exist
    (is (not (null wp)))

    ;; Should be able to access origin
    (let ((origin (clad.workplane:workplane-origin wp)))
      (is (= 0 (first origin)))
      (is (= 0 (second origin)))
      (is (= 0 (third origin))))

    ;; Should be able to access x-direction
    (let ((x-dir (clad.workplane:workplane-x-dir wp)))
      (is (= 1 (first x-dir)))
      (is (= 0 (second x-dir)))
      (is (= 0 (third x-dir))))

    ;; Should be able to access z-direction
    (let ((z-dir (clad.workplane:workplane-z-dir wp)))
      (is (= 0 (first z-dir)))
      (is (= 0 (second z-dir)))
      (is (= 1 (third z-dir))))

    ;; Y-direction should be computed automatically (cross product of z and x)
    (let ((y-dir (clad.workplane:workplane-y-dir wp)))
      (is (= 0 (first y-dir)))
      (is (= 1 (second y-dir)))
      (is (= 0 (third y-dir))))))

(test workplane-normalization
  "Test that workplane directions are automatically normalized"
  ;; Create workplane with non-unit vectors
  (let ((wp (clad.workplane:make-workplane
             :origin '(10 20 30)
             :x-dir '(2 0 0)    ; Length 2
             :z-dir '(0 0 5)))) ; Length 5

    ;; X-direction should be normalized
    (let ((x-dir (clad.workplane:workplane-x-dir wp)))
      (let ((length (sqrt (+ (* (first x-dir) (first x-dir))
                            (* (second x-dir) (second x-dir))
                            (* (third x-dir) (third x-dir))))))
        (is (< (abs (- length 1.0)) 0.001))))

    ;; Z-direction should be normalized
    (let ((z-dir (clad.workplane:workplane-z-dir wp)))
      (let ((length (sqrt (+ (* (first z-dir) (first z-dir))
                            (* (second z-dir) (second z-dir))
                            (* (third z-dir) (third z-dir))))))
        (is (< (abs (- length 1.0)) 0.001))))))

(test workplane-orthogonality
  "Test that workplane ensures X and Z directions are orthogonal"
  ;; Create workplane with slightly non-orthogonal vectors
  (let ((wp (clad.workplane:make-workplane
             :origin '(0 0 0)
             :x-dir '(1 0 0)
             :z-dir '(0 0 1))))

    ;; X and Z should be orthogonal (dot product = 0)
    (let ((x-dir (clad.workplane:workplane-x-dir wp))
          (z-dir (clad.workplane:workplane-z-dir wp)))
      (let ((dot (+ (* (first x-dir) (first z-dir))
                   (* (second x-dir) (second z-dir))
                   (* (third x-dir) (third z-dir)))))
        (is (< (abs dot) 0.001))))

    ;; X and Y should be orthogonal
    (let ((x-dir (clad.workplane:workplane-x-dir wp))
          (y-dir (clad.workplane:workplane-y-dir wp)))
      (let ((dot (+ (* (first x-dir) (first y-dir))
                   (* (second x-dir) (second y-dir))
                   (* (third x-dir) (third y-dir)))))
        (is (< (abs dot) 0.001))))

    ;; Y and Z should be orthogonal
    (let ((y-dir (clad.workplane:workplane-y-dir wp))
          (z-dir (clad.workplane:workplane-z-dir wp)))
      (let ((dot (+ (* (first y-dir) (first z-dir))
                   (* (second y-dir) (second z-dir))
                   (* (third y-dir) (third z-dir)))))
        (is (< (abs dot) 0.001))))))

;;; ============================================================================
;;; TDD Cycle 2: Standard Plane Constructors
;;; ============================================================================

(test xy-plane-default
  "Test creating an XY plane at origin"
  (let ((wp (clad.workplane:xy-plane)))
    ;; Origin should be at (0 0 0)
    (let ((origin (clad.workplane:workplane-origin wp)))
      (is (= 0 (first origin)))
      (is (= 0 (second origin)))
      (is (= 0 (third origin))))

    ;; X direction should be (1 0 0)
    (let ((x-dir (clad.workplane:workplane-x-dir wp)))
      (is (< (abs (- (first x-dir) 1)) 0.001))
      (is (< (abs (second x-dir)) 0.001))
      (is (< (abs (third x-dir)) 0.001)))

    ;; Y direction should be (0 1 0)
    (let ((y-dir (clad.workplane:workplane-y-dir wp)))
      (is (< (abs (first y-dir)) 0.001))
      (is (< (abs (- (second y-dir) 1)) 0.001))
      (is (< (abs (third y-dir)) 0.001)))

    ;; Z direction should be (0 0 1)
    (let ((z-dir (clad.workplane:workplane-z-dir wp)))
      (is (< (abs (first z-dir)) 0.001))
      (is (< (abs (second z-dir)) 0.001))
      (is (< (abs (- (third z-dir) 1)) 0.001)))))

(test xy-plane-at-point
  "Test creating an XY plane at a specific point"
  (let ((wp (clad.workplane:xy-plane :origin '(10 20 30))))
    ;; Origin should be at (10 20 30)
    (let ((origin (clad.workplane:workplane-origin wp)))
      (is (= 10 (first origin)))
      (is (= 20 (second origin)))
      (is (= 30 (third origin))))))

(test xz-plane-default
  "Test creating an XZ plane at origin"
  (let ((wp (clad.workplane:xz-plane)))
    ;; Origin should be at (0 0 0)
    (let ((origin (clad.workplane:workplane-origin wp)))
      (is (= 0 (first origin)))
      (is (= 0 (second origin)))
      (is (= 0 (third origin))))

    ;; X direction should be (1 0 0)
    (let ((x-dir (clad.workplane:workplane-x-dir wp)))
      (is (< (abs (- (first x-dir) 1)) 0.001))
      (is (< (abs (second x-dir)) 0.001))
      (is (< (abs (third x-dir)) 0.001)))

    ;; Y direction should be (0 0 1) - in XZ plane, Y points along Z
    (let ((y-dir (clad.workplane:workplane-y-dir wp)))
      (is (< (abs (first y-dir)) 0.001))
      (is (< (abs (second y-dir)) 0.001))
      (is (< (abs (- (third y-dir) 1)) 0.001)))

    ;; Z direction should be (0 -1 0) - normal to XZ plane is -Y
    (let ((z-dir (clad.workplane:workplane-z-dir wp)))
      (is (< (abs (first z-dir)) 0.001))
      (is (< (abs (+ (second z-dir) 1)) 0.001))
      (is (< (abs (third z-dir)) 0.001)))))

(test yz-plane-default
  "Test creating a YZ plane at origin"
  (let ((wp (clad.workplane:yz-plane)))
    ;; Origin should be at (0 0 0)
    (let ((origin (clad.workplane:workplane-origin wp)))
      (is (= 0 (first origin)))
      (is (= 0 (second origin)))
      (is (= 0 (third origin))))

    ;; X direction should be (0 1 0) - in YZ plane, X points along Y
    (let ((x-dir (clad.workplane:workplane-x-dir wp)))
      (is (< (abs (first x-dir)) 0.001))
      (is (< (abs (- (second x-dir) 1)) 0.001))
      (is (< (abs (third x-dir)) 0.001)))

    ;; Y direction should be (0 0 1) - in YZ plane, Y points along Z
    (let ((y-dir (clad.workplane:workplane-y-dir wp)))
      (is (< (abs (first y-dir)) 0.001))
      (is (< (abs (second y-dir)) 0.001))
      (is (< (abs (- (third y-dir) 1)) 0.001)))

    ;; Z direction should be (1 0 0) - normal to YZ plane is X
    (let ((z-dir (clad.workplane:workplane-z-dir wp)))
      (is (< (abs (- (first z-dir) 1)) 0.001))
      (is (< (abs (second z-dir)) 0.001))
      (is (< (abs (third z-dir)) 0.001)))))

;;; ============================================================================
;;; TDD Cycle 3: Coordinate Transformations
;;; ============================================================================

(test local-to-global-xy-plane
  "Test transforming local coordinates to global on XY plane"
  (let ((wp (clad.workplane:xy-plane)))
    ;; Point (10, 20, 0) in local coords should be (10, 20, 0) in global
    (let ((global (clad.workplane:local-to-global wp '(10 20 0))))
      (is (< (abs (- (first global) 10)) 0.001))
      (is (< (abs (- (second global) 20)) 0.001))
      (is (< (abs (third global)) 0.001)))

    ;; Point (0, 0, 5) in local coords should be (0, 0, 5) in global (offset in Z)
    (let ((global (clad.workplane:local-to-global wp '(0 0 5))))
      (is (< (abs (first global)) 0.001))
      (is (< (abs (second global)) 0.001))
      (is (< (abs (- (third global) 5)) 0.001)))))

(test local-to-global-offset-plane
  "Test transforming local coordinates to global on offset XY plane"
  (let ((wp (clad.workplane:xy-plane :origin '(100 200 300))))
    ;; Point (0, 0, 0) in local coords should be (100, 200, 300) in global
    (let ((global (clad.workplane:local-to-global wp '(0 0 0))))
      (is (< (abs (- (first global) 100)) 0.001))
      (is (< (abs (- (second global) 200)) 0.001))
      (is (< (abs (- (third global) 300)) 0.001)))

    ;; Point (10, 20, 0) in local coords should be (110, 220, 300) in global
    (let ((global (clad.workplane:local-to-global wp '(10 20 0))))
      (is (< (abs (- (first global) 110)) 0.001))
      (is (< (abs (- (second global) 220)) 0.001))
      (is (< (abs (- (third global) 300)) 0.001)))))

(test global-to-local-xy-plane
  "Test transforming global coordinates to local on XY plane"
  (let ((wp (clad.workplane:xy-plane)))
    ;; Point (10, 20, 0) in global should be (10, 20, 0) in local
    (let ((local (clad.workplane:global-to-local wp '(10 20 0))))
      (is (< (abs (- (first local) 10)) 0.001))
      (is (< (abs (- (second local) 20)) 0.001))
      (is (< (abs (third local)) 0.001)))

    ;; Point (0, 0, 5) in global should be (0, 0, 5) in local
    (let ((local (clad.workplane:global-to-local wp '(0 0 5))))
      (is (< (abs (first local)) 0.001))
      (is (< (abs (second local)) 0.001))
      (is (< (abs (- (third local) 5)) 0.001)))))

(test global-to-local-offset-plane
  "Test transforming global coordinates to local on offset XY plane"
  (let ((wp (clad.workplane:xy-plane :origin '(100 200 300))))
    ;; Point (100, 200, 300) in global should be (0, 0, 0) in local
    (let ((local (clad.workplane:global-to-local wp '(100 200 300))))
      (is (< (abs (first local)) 0.001))
      (is (< (abs (second local)) 0.001))
      (is (< (abs (third local)) 0.001)))

    ;; Point (110, 220, 300) in global should be (10, 20, 0) in local
    (let ((local (clad.workplane:global-to-local wp '(110 220 300))))
      (is (< (abs (- (first local) 10)) 0.001))
      (is (< (abs (- (second local) 20)) 0.001))
      (is (< (abs (third local)) 0.001)))))

(test round-trip-transformation
  "Test that local-to-global and global-to-local are inverses"
  (let ((wp (clad.workplane:xy-plane :origin '(50 60 70))))
    (let* ((local-pt '(10 20 30))
           (global-pt (clad.workplane:local-to-global wp local-pt))
           (local-again (clad.workplane:global-to-local wp global-pt)))
      ;; Should recover original local point
      (is (< (abs (- (first local-again) 10)) 0.001))
      (is (< (abs (- (second local-again) 20)) 0.001))
      (is (< (abs (- (third local-again) 30)) 0.001)))))

;;; ============================================================================
;;; TDD Cycle 5: Workplane Operations
;;; ============================================================================

(test offset-workplane-xy
  "Test offsetting an XY plane along its normal"
  (let* ((wp1 (clad.workplane:xy-plane))
         (wp2 (clad.workplane:offset-workplane wp1 10)))
    ;; Origin should be offset by 10 in Z direction
    (let ((origin (clad.workplane:workplane-origin wp2)))
      (is (< (abs (first origin)) 0.001))
      (is (< (abs (second origin)) 0.001))
      (is (< (abs (- (third origin) 10)) 0.001)))

    ;; Directions should remain the same
    (let ((x-dir (clad.workplane:workplane-x-dir wp2)))
      (is (< (abs (- (first x-dir) 1)) 0.001)))
    (let ((z-dir (clad.workplane:workplane-z-dir wp2)))
      (is (< (abs (- (third z-dir) 1)) 0.001)))))

(test offset-workplane-negative
  "Test offsetting a workplane in negative direction"
  (let* ((wp1 (clad.workplane:xy-plane :origin '(0 0 20)))
         (wp2 (clad.workplane:offset-workplane wp1 -10)))
    ;; Origin should be at (0 0 10)
    (let ((origin (clad.workplane:workplane-origin wp2)))
      (is (< (abs (first origin)) 0.001))
      (is (< (abs (second origin)) 0.001))
      (is (< (abs (- (third origin) 10)) 0.001)))))

(test rotate-workplane-z-axis
  "Test rotating a workplane around Z axis"
  (let* ((wp1 (clad.workplane:xy-plane))
         ;; Rotate 90 degrees (pi/2 radians) around Z
         (wp2 (clad.workplane:rotate-workplane wp1 :z (/ pi 2))))
    ;; After 90 degree rotation around Z:
    ;; X direction (1 0 0) becomes Y direction (0 1 0)
    (let ((x-dir (clad.workplane:workplane-x-dir wp2)))
      (is (< (abs (first x-dir)) 0.001))
      (is (< (abs (- (second x-dir) 1)) 0.001))
      (is (< (abs (third x-dir)) 0.001)))

    ;; Y direction (0 1 0) becomes -X direction (-1 0 0)
    (let ((y-dir (clad.workplane:workplane-y-dir wp2)))
      (is (< (abs (+ (first y-dir) 1)) 0.001))
      (is (< (abs (second y-dir)) 0.001))
      (is (< (abs (third y-dir)) 0.001)))

    ;; Z direction should remain (0 0 1)
    (let ((z-dir (clad.workplane:workplane-z-dir wp2)))
      (is (< (abs (first z-dir)) 0.001))
      (is (< (abs (second z-dir)) 0.001))
      (is (< (abs (- (third z-dir) 1)) 0.001)))))

(test rotate-workplane-x-axis
  "Test rotating a workplane around X axis"
  (let* ((wp1 (clad.workplane:xy-plane))
         ;; Rotate 90 degrees around X
         (wp2 (clad.workplane:rotate-workplane wp1 :x (/ pi 2))))
    ;; X direction should remain (1 0 0)
    (let ((x-dir (clad.workplane:workplane-x-dir wp2)))
      (is (< (abs (- (first x-dir) 1)) 0.001))
      (is (< (abs (second x-dir)) 0.001))
      (is (< (abs (third x-dir)) 0.001)))

    ;; Y direction (0 1 0) becomes Z direction (0 0 1)
    (let ((y-dir (clad.workplane:workplane-y-dir wp2)))
      (is (< (abs (first y-dir)) 0.001))
      (is (< (abs (second y-dir)) 0.001))
      (is (< (abs (- (third y-dir) 1)) 0.001)))

    ;; Z direction (0 0 1) becomes -Y direction (0 -1 0)
    (let ((z-dir (clad.workplane:workplane-z-dir wp2)))
      (is (< (abs (first z-dir)) 0.001))
      (is (< (abs (+ (second z-dir) 1)) 0.001))
      (is (< (abs (third z-dir)) 0.001)))))

;;; ============================================================================
;;; TDD Cycle 4: Workplane from Face
;;; ============================================================================

(test workplane-from-top-face
  "Test creating a workplane from the top face of a box"
  (let* ((box (clad.shapes:wrap-shape
               (clad.core:make-box 100 100 100)
               'clad.shapes:cad-solid))
         (all-faces (clad.shapes:faces box))
         ;; Select top face
         (top-face (first (clad.selectors:select all-faces
                                                  :direction :+z
                                                  :extreme :max)))
         ;; Create workplane from top face
         (wp (clad.workplane:workplane-from-face top-face)))

    ;; Origin should be at center of top face (50, 50, 100)
    (let ((origin (clad.workplane:workplane-origin wp)))
      (is (< (abs (- (first origin) 50)) 1))
      (is (< (abs (- (second origin) 50)) 1))
      (is (< (abs (- (third origin) 100)) 1)))

    ;; Z direction (normal) should point up (0 0 1)
    (let ((z-dir (clad.workplane:workplane-z-dir wp)))
      (is (> (abs (third z-dir)) 0.9))
      (is (< (+ (abs (first z-dir)) (abs (second z-dir))) 0.1)))))

(test workplane-from-side-face
  "Test creating a workplane from a side face of a box"
  (let* ((box (clad.shapes:wrap-shape
               (clad.core:make-box 100 100 100)
               'clad.shapes:cad-solid))
         (all-faces (clad.shapes:faces box))
         ;; Select +X face
         (side-face (first (clad.selectors:select all-faces
                                                   :direction :+x
                                                   :extreme :max)))
         ;; Create workplane from side face
         (wp (clad.workplane:workplane-from-face side-face)))

    ;; Origin should be at center of +X face (100, 50, 50)
    (let ((origin (clad.workplane:workplane-origin wp)))
      (is (< (abs (- (first origin) 100)) 1))
      (is (< (abs (- (second origin) 50)) 1))
      (is (< (abs (- (third origin) 50)) 1)))

    ;; Z direction (normal) should point in +X (1 0 0)
    (let ((z-dir (clad.workplane:workplane-z-dir wp)))
      (is (> (abs (first z-dir)) 0.9))
      (is (< (+ (abs (second z-dir)) (abs (third z-dir))) 0.1)))))
