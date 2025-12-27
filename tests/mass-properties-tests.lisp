;;;; tests/mass-properties-tests.lisp --- Tests for mass properties analysis

(in-package :clad.tests)

;;; ============================================================================
;;; Mass Properties Test Suite
;;; ============================================================================

(def-suite mass-properties-tests
  :description "Test suite for mass properties and material analysis"
  :in clad-tests)

(in-suite mass-properties-tests)

;;; ============================================================================
;;; Volume Tests
;;; ============================================================================

(test mass-props-volume-box
  "Calculate volume of a box"
  (let* ((box (clad.core:make-box 10 20 30))
         (props (clad.analysis:mass-properties box)))
    (is (numberp (getf props :volume)) "Volume should be a number")
    (is (< (abs (- (getf props :volume) 6000.0)) 1.0)
        "Box volume should be 10*20*30 = 6000")))

(test mass-props-volume-cylinder
  "Calculate volume of a cylinder"
  (let* ((cylinder (clad.core:make-cylinder 10 20))
         (props (clad.analysis:mass-properties cylinder))
         (expected-volume (* pi 100 20))) ; π*r²*h
    (is (numberp (getf props :volume)))
    (is (< (abs (- (getf props :volume) expected-volume)) 1.0)
        "Cylinder volume should be π*r²*h")))

(test mass-props-volume-sphere
  "Calculate volume of a sphere"
  (let* ((sphere (clad.core:make-sphere 15))
         (props (clad.analysis:mass-properties sphere))
         (expected-volume (* 4/3 pi 15 15 15))) ; 4/3*π*r³
    (is (numberp (getf props :volume)))
    (is (< (abs (- (getf props :volume) expected-volume)) 10.0)
        "Sphere volume should be 4/3*π*r³")))

(test mass-props-volume-union
  "Volume of boolean union"
  (let* ((box1 (clad.core:make-box 10 10 10))
         (box2 (clad.core:translate (clad.core:make-box 10 10 10) 5 0 0))
         (union (clad.core:union-shapes box1 box2))
         (props (clad.analysis:mass-properties union)))
    (is (numberp (getf props :volume)))
    ;; Two 10x10x10 boxes overlapping by 5 units
    ;; Volume = 2*1000 - 500 = 1500
    (is (< (abs (- (getf props :volume) 1500.0)) 10.0)
        "Union volume should account for overlap")))

;;; ============================================================================
;;; Surface Area Tests
;;; ============================================================================

(test mass-props-area-box
  "Calculate surface area of a box"
  (let* ((box (clad.core:make-box 10 20 30))
         (props (clad.analysis:mass-properties box))
         (expected-area (* 2 (+ (* 10 20) (* 10 30) (* 20 30)))))
    (is (numberp (getf props :surface-area)))
    (is (< (abs (- (getf props :surface-area) expected-area)) 1.0)
        "Box surface area = 2(lw + lh + wh)")))

(test mass-props-area-sphere
  "Calculate surface area of a sphere"
  (let* ((sphere (clad.core:make-sphere 10))
         (props (clad.analysis:mass-properties sphere))
         (expected-area (* 4 pi 100))) ; 4πr²
    (is (numberp (getf props :surface-area)))
    (is (< (abs (- (getf props :surface-area) expected-area)) 1.0)
        "Sphere surface area = 4πr²")))

;;; ============================================================================
;;; Center of Mass Tests
;;; ============================================================================

(test mass-props-center-box
  "Center of mass of a centered box"
  (let* ((box (clad.core:make-box 20 20 20))
         (props (clad.analysis:mass-properties box))
         (center (getf props :center-of-mass)))
    (is (listp center) "Center should be a list of coordinates")
    (is (= (length center) 3) "Center should have 3 coordinates")
    ;; Box centered on XY plane, so center at (0, 0, 10)
    (is (< (abs (first center)) 0.1) "X should be near 0")
    (is (< (abs (second center)) 0.1) "Y should be near 0")
    (is (< (abs (- (third center) 10.0)) 0.1) "Z should be near 10")))

(test mass-props-center-translated
  "Center of mass of a translated box"
  (let* ((box (clad.core:translate (clad.core:make-box 10 10 10) 50 60 70))
         (props (clad.analysis:mass-properties box))
         (center (getf props :center-of-mass)))
    (is (< (abs (- (first center) 50.0)) 1.0) "X center should be near 50")
    (is (< (abs (- (second center) 60.0)) 1.0) "Y center should be near 60")
    (is (< (abs (- (third center) 75.0)) 1.0) "Z center should be near 75")))

;;; ============================================================================
;;; Mass with Material Tests
;;; ============================================================================

(test mass-props-mass-default
  "Mass calculation with default density (1.0 g/cm³)"
  (let* ((box (clad.core:make-box 10 10 10)) ; 1000 mm³ = 1 cm³
         (props (clad.analysis:mass-properties box)))
    (is (numberp (getf props :mass)))
    ;; Default density 1.0 g/cm³, volume 1 cm³ = 1 gram
    (is (< (abs (- (getf props :mass) 1.0)) 0.1)
        "Mass with default density should be ~1g")))

(test mass-props-mass-aluminum
  "Mass calculation with aluminum"
  (let* ((box (clad.core:make-box 100 100 100)) ; 1,000,000 mm³ = 1000 cm³
         (props (clad.analysis:mass-properties box :material :aluminum)))
    (is (numberp (getf props :mass)))
    ;; Aluminum 2.70 g/cm³, volume 1000 cm³ = 2700 grams
    (is (< (abs (- (getf props :mass) 2700.0)) 10.0)
        "Aluminum mass should be ~2700g")))

(test mass-props-mass-steel
  "Mass calculation with steel"
  (let* ((box (clad.core:make-box 100 100 100)) ; 1000 cm³
         (props (clad.analysis:mass-properties box :material :steel)))
    (is (numberp (getf props :mass)))
    ;; Steel 7.87 g/cm³, volume 1000 cm³ = 7870 grams
    (is (< (abs (- (getf props :mass) 7870.0)) 50.0)
        "Steel mass should be ~7870g")))

(test mass-props-mass-custom-density
  "Mass calculation with custom density"
  (let* ((box (clad.core:make-box 100 100 100)) ; 1000 cm³
         (props (clad.analysis:mass-properties box :density 5.0)))
    (is (numberp (getf props :mass)))
    ;; Custom 5.0 g/cm³, volume 1000 cm³ = 5000 grams
    (is (< (abs (- (getf props :mass) 5000.0)) 10.0)
        "Custom density mass should be ~5000g")))

;;; ============================================================================
;;; Material Database Tests
;;; ============================================================================

(test material-database-aluminum
  "Aluminum material properties"
  (let ((mat (clad.analysis:get-material :aluminum)))
    (is (= (getf mat :density) 2.70) "Aluminum density 2.70 g/cm³")
    (is (string= (getf mat :name) "Aluminum 6061") "Material name")))

(test material-database-steel
  "Steel material properties"
  (let ((mat (clad.analysis:get-material :steel)))
    (is (= (getf mat :density) 7.87) "Steel density 7.87 g/cm³")
    (is (string= (getf mat :name) "Steel 1018") "Material name")))

(test material-database-stainless
  "Stainless steel material properties"
  (let ((mat (clad.analysis:get-material :stainless)))
    (is (= (getf mat :density) 8.00) "Stainless 304 density 8.00 g/cm³")))

(test material-database-abs
  "ABS plastic material properties"
  (let ((mat (clad.analysis:get-material :abs)))
    (is (= (getf mat :density) 1.05) "ABS density 1.05 g/cm³")))

(test material-database-pla
  "PLA plastic material properties"
  (let ((mat (clad.analysis:get-material :pla)))
    (is (= (getf mat :density) 1.24) "PLA density 1.24 g/cm³")))

(test material-database-list-all
  "List all available materials"
  (let ((materials (clad.analysis:list-materials)))
    (is (listp materials) "Should return a list")
    (is (>= (length materials) 5) "Should have at least 5 materials")
    (is (member :aluminum materials) "Should include aluminum")
    (is (member :steel materials) "Should include steel")))

;;; ============================================================================
;;; Moments of Inertia Tests
;;; ============================================================================

(test mass-props-inertia-box
  "Moments of inertia for a box"
  (let* ((box (clad.core:make-box 20 30 40))
         (props (clad.analysis:mass-properties box :material :aluminum))
         (inertia (getf props :inertia))
         (matrix (getf inertia :matrix)))
    (is (listp inertia) "Inertia should be a plist")
    (is (= (length matrix) 9) "Inertia tensor has 9 components")
    ;; Diagonal elements should be positive
    (is (> (nth 0 matrix) 0) "Ixx > 0")
    (is (> (nth 4 matrix) 0) "Iyy > 0")
    (is (> (nth 8 matrix) 0) "Izz > 0")))

(test mass-props-inertia-sphere
  "Moments of inertia for a sphere"
  (let* ((sphere (clad.core:make-sphere 10))
         (props (clad.analysis:mass-properties sphere :material :steel))
         (inertia (getf props :inertia))
         (matrix (getf inertia :matrix)))
    ;; For a sphere, Ixx = Iyy = Izz (using bounding-box approximation)
    ;; Note: bounding-box approximation treats sphere as a cube, so
    ;; these values will be equal since bounding box is symmetric
    (is (< (abs (- (nth 0 matrix) (nth 4 matrix))) 1.0)
        "Sphere should have Ixx ≈ Iyy")
    (is (< (abs (- (nth 4 matrix) (nth 8 matrix))) 1.0)
        "Sphere should have Iyy ≈ Izz")))

;;; ============================================================================
;;; Complex Shape Tests
;;; ============================================================================

(test mass-props-complex-shape
  "Mass properties of a complex shape"
  (let* ((base (clad.core:make-box 100 100 10))
         (boss (clad.core:translate (clad.core:make-cylinder 20 30) 0 0 10))
         (part (clad.core:union-shapes base boss))
         (props (clad.analysis:mass-properties part :material :aluminum)))
    (is (numberp (getf props :volume)))
    (is (numberp (getf props :mass)))
    (is (numberp (getf props :surface-area)))
    (is (listp (getf props :center-of-mass)))
    (is (> (getf props :volume) 100000.0) "Volume should be > base volume")))

(test mass-props-with-hole
  "Mass properties of a part with a hole"
  (let* ((block (clad.core:make-box 100 100 100))
         (hole (clad.core:make-cylinder 10 120))
         (part (clad.core:cut-shapes block hole))
         (props (clad.analysis:mass-properties part :material :steel)))
    (is (< (getf props :volume) 1000000.0)
        "Volume with hole should be less than solid block")))

;;; ============================================================================
;;; Convenience Function Tests
;;; ============================================================================

(test mass-props-simple-volume
  "Simple volume query function"
  (let ((box (clad.core:make-box 10 20 30)))
    (is (numberp (clad.analysis:volume box)))
    (is (< (abs (- (clad.analysis:volume box) 6000.0)) 1.0))))

(test mass-props-simple-mass
  "Simple mass query function"
  (let ((box (clad.core:make-box 100 100 100)))
    (is (numberp (clad.analysis:mass box :material :aluminum)))
    (is (< (abs (- (clad.analysis:mass box :material :aluminum) 2700.0)) 10.0))))

(test mass-props-simple-area
  "Simple surface area query function"
  (let ((box (clad.core:make-box 10 10 10)))
    (is (numberp (clad.analysis:surface-area box)))
    (is (< (abs (- (clad.analysis:surface-area box) 600.0)) 1.0))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(test mass-props-invalid-shape-error
  "Error on invalid shape"
  (signals error
    (clad.analysis:mass-properties nil)))

(test mass-props-invalid-material-error
  "Error on invalid material keyword"
  (let ((box (clad.core:make-box 10 10 10)))
    (signals error
      (clad.analysis:mass-properties box :material :nonexistent-material))))

(test mass-props-negative-density-error
  "Error on negative density"
  (let ((box (clad.core:make-box 10 10 10)))
    (signals error
      (clad.analysis:mass-properties box :density -1.0))))

;;; ============================================================================
;;; Assembly Mass Properties Tests
;;; ============================================================================

(test mass-props-multi-material
  "Calculate total mass for assembly with different materials"
  (let* ((aluminum-part (clad.core:make-box 100 100 10))
         (steel-part (clad.core:translate (clad.core:make-cylinder 20 50) 0 0 10))
         (al-props (clad.analysis:mass-properties aluminum-part :material :aluminum))
         (steel-props (clad.analysis:mass-properties steel-part :material :steel))
         (total-mass (+ (getf al-props :mass) (getf steel-props :mass))))
    (is (> total-mass 0) "Total assembly mass should be positive")
    (is (numberp total-mass))))

;;; ============================================================================
;;; Utility Tests
;;; ============================================================================

(test mass-props-compare-materials
  "Compare mass of same part in different materials"
  (let* ((part (clad.core:make-box 100 100 100))
         (al-mass (clad.analysis:mass part :material :aluminum))
         (steel-mass (clad.analysis:mass part :material :steel)))
    (is (> steel-mass al-mass)
        "Steel part should be heavier than aluminum")))

(test mass-props-format-output
  "Format mass properties for display"
  (let* ((box (clad.core:make-box 100 100 50))
         (props (clad.analysis:mass-properties box :material :aluminum)))
    ;; Test that all expected properties are present
    (is (getf props :volume))
    (is (getf props :surface-area))
    (is (getf props :mass))
    (is (getf props :center-of-mass))
    (is (getf props :inertia))
    (is (getf props :material-name))))

;;; ============================================================================
;;; End of Mass Properties Tests
;;; ============================================================================
