;;;; src/analysis/mass-properties.lisp --- Mass properties and material analysis

(in-package :clad.analysis)

;;; ============================================================================
;;; Material Database
;;; ============================================================================

(defparameter *material-database*
  '((:aluminum . (:name "Aluminum 6061" :density 2.70))
    (:steel . (:name "Steel 1018" :density 7.87))
    (:stainless . (:name "Stainless Steel 304" :density 8.00))
    (:abs . (:name "ABS Plastic" :density 1.05))
    (:pla . (:name "PLA Plastic" :density 1.24))
    (:brass . (:name "Brass" :density 8.50))
    (:copper . (:name "Copper" :density 8.96))
    (:titanium . (:name "Titanium Grade 5" :density 4.43))
    (:nylon . (:name "Nylon 6" :density 1.14))
    (:petg . (:name "PETG Plastic" :density 1.27)))
  "Material database with densities in g/cm³")

(defun get-material (material-keyword)
  "Get material properties from the database.

  Arguments:
    material-keyword - Material identifier (e.g., :aluminum, :steel)

  Returns:
    Property list with :name and :density

  Signals:
    error - If material not found

  Examples:
    (get-material :aluminum)
    => (:name \"Aluminum 6061\" :density 2.70)"

  (let ((material (cdr (assoc material-keyword *material-database*))))
    (unless material
      (error "Unknown material: ~A. Available materials: ~A"
             material-keyword
             (mapcar #'car *material-database*)))
    material))

(defun list-materials ()
  "List all available materials in the database.

  Returns:
    List of material keywords

  Example:
    (list-materials)
    => (:aluminum :steel :stainless :abs :pla ...)"

  (mapcar #'car *material-database*))

(defun define-material (keyword name density)
  "Add a custom material to the database.

  Arguments:
    keyword - Material identifier keyword
    name - Human-readable name
    density - Density in g/cm³

  Example:
    (define-material :custom-alloy \"Special Alloy\" 6.5)"

  (setf *material-database*
        (cons (cons keyword (list :name name :density density))
              (remove keyword *material-database* :key #'car))))

;;; ============================================================================
;;; Mass Properties Calculation
;;; ============================================================================

(defun mass-properties (shape &key (material nil) (density 1.0))
  "Calculate comprehensive mass properties for a shape.

  Arguments:
    shape - CLAD shape (clad.core:shape or clad.shapes:cad-shape)
    material - Material keyword (e.g., :aluminum, :steel) - optional
    density - Custom density in g/cm³ (used if material not specified)

  Returns:
    Property list containing:
      :volume - Volume in mm³
      :surface-area - Surface area in mm²
      :mass - Mass in grams
      :density - Density used (g/cm³)
      :center-of-mass - List (x y z) in mm
      :inertia - Inertia tensor (9 elements: Ixx Ixy Ixz Iyx Iyy Iyz Izx Izy Izz)
      :material-name - Material name string

  Examples:
    ;; With material
    (mass-properties box :material :aluminum)

    ;; With custom density
    (mass-properties cylinder :density 5.0)

    ;; Default density (1.0 g/cm³)
    (mass-properties sphere)

  Signals:
    error - On invalid shape or density"

  ;; Unwrap CLOS shape if needed
  (let ((core-shape (if (typep shape 'clad.shapes:cad-shape)
                        (clad.shapes::core-shape shape)
                        shape)))

    ;; Validate shape
    (unless (and core-shape (clad.core:valid-shape-p core-shape))
      (error "Invalid shape for mass properties: ~S" shape))

    ;; Determine density and material name
    (multiple-value-bind (actual-density material-name)
        (if material
            (let ((mat (get-material material)))
              (values (getf mat :density) (getf mat :name)))
            (progn
              (unless (and (numberp density) (> density 0))
                (error "Density must be a positive number, got: ~S" density))
              (values density "Custom Material")))

      ;; Calculate geometric properties
      (let* ((volume-mm3 (clad.ffi:ffi-get-volume (clad.core:shape-handle core-shape)))
             (area-mm2 (clad.ffi:ffi-get-area (clad.core:shape-handle core-shape)))
             ;; ffi-get-center-of-mass returns (values x y z), convert to list
             (com-coords (multiple-value-list
                          (clad.ffi:ffi-get-center-of-mass (clad.core:shape-handle core-shape))))
             ;; Convert volume from mm³ to cm³ (1 cm³ = 1000 mm³)
             (volume-cm3 (/ volume-mm3 1000.0))
             ;; Calculate mass: density (g/cm³) × volume (cm³) = mass (g)
             (mass-g (* actual-density volume-cm3))
             ;; Calculate moments of inertia (for now, placeholder - will implement properly)
             (inertia (calculate-inertia-tensor core-shape actual-density)))

        (list :volume volume-mm3
              :surface-area area-mm2
              :mass mass-g
              :density actual-density
              :center-of-mass com-coords
              :inertia inertia
              :material-name material-name)))))

(defun calculate-inertia-tensor (shape density)
  "Calculate the inertia tensor for a shape.

  For now, this is a simplified implementation.
  Returns a 9-element list representing the 3x3 inertia tensor.

  TODO: Implement proper inertia calculation using OpenCASCADE GProp_GProps"

  ;; Simplified calculation - get volume and center
  (let* ((volume-mm3 (clad.ffi:ffi-get-volume (clad.core:shape-handle shape)))
         (volume-cm3 (/ volume-mm3 1000.0))
         (mass-g (* density volume-cm3))
         ;; For a rough approximation, assume a bounding box
         ;; This is a placeholder - proper implementation would use GProp_GProps::MatrixOfInertia
         ;; bounding-box returns (xmin ymin zmin xmax ymax zmax)
         (bbox (clad.shapes:bounding-box shape))
         (xmin (nth 0 bbox))
         (ymin (nth 1 bbox))
         (zmin (nth 2 bbox))
         (xmax (nth 3 bbox))
         (ymax (nth 4 bbox))
         (zmax (nth 5 bbox))
         (dx (- xmax xmin))
         (dy (- ymax ymin))
         (dz (- zmax zmin))
         ;; Approximate as rectangular box inertia
         (ixx (* (/ mass-g 12.0) (+ (* dy dy) (* dz dz))))
         (iyy (* (/ mass-g 12.0) (+ (* dx dx) (* dz dz))))
         (izz (* (/ mass-g 12.0) (+ (* dx dx) (* dy dy)))))

    ;; Return 3x3 tensor (row-major order)
    (list ixx  0.0  0.0    ; Row 1: Ixx Ixy Ixz
          0.0  iyy  0.0    ; Row 2: Iyx Iyy Iyz
          0.0  0.0  izz))) ; Row 3: Izx Izy Izz

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun volume (shape)
  "Get the volume of a shape in mm³.

  Convenience wrapper around mass-properties.

  Example:
    (volume (clad.core:make-box 10 20 30))
    => 6000.0"

  (getf (mass-properties shape) :volume))

(defun surface-area (shape)
  "Get the surface area of a shape in mm².

  Convenience wrapper around mass-properties.

  Example:
    (surface-area (clad.core:make-box 10 10 10))
    => 600.0"

  (getf (mass-properties shape) :surface-area))

(defun mass (shape &key (material nil) (density 1.0))
  "Get the mass of a shape in grams.

  Convenience wrapper around mass-properties.

  Examples:
    (mass box :material :aluminum)
    (mass cylinder :density 5.0)"

  (getf (mass-properties shape :material material :density density) :mass))

(defun center-of-mass (shape)
  "Get the center of mass coordinates (x y z) in mm.

  Convenience wrapper around mass-properties.

  Example:
    (center-of-mass sphere)
    => (0.0 0.0 15.0)"

  (getf (mass-properties shape) :center-of-mass))

(defun inertia (shape &key (material nil) (density 1.0))
  "Get the inertia tensor of a shape.

  Convenience wrapper around mass-properties.

  Returns 9-element list (3x3 matrix in row-major order):
    (Ixx Ixy Ixz Iyx Iyy Iyz Izx Izy Izz)

  Example:
    (inertia part :material :steel)"

  (getf (mass-properties shape :material material :density density) :inertia))

;;; ============================================================================
;;; End of Mass Properties Module
;;; ============================================================================
