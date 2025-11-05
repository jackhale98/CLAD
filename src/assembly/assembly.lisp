;;;; assembly.lisp --- Assembly modeling with mates and constraints

(in-package #:clad.assembly)

;;; ==============================================================================
;;; Assembly Class (Week 9-10: Component System)
;;; ==============================================================================

(defclass assembly ()
  ((name :initarg :name
         :initform nil
         :accessor assembly-name
         :documentation "Assembly name (symbol)")
   (description :initarg :description
                :initform nil
                :accessor assembly-description
                :documentation "Assembly description string")
   (components :initform (make-hash-table :test 'eq)
               :accessor assembly-components
               :documentation "Hash table of components (name -> component)")
   (constraints :initform '()
                :accessor assembly-constraints
                :documentation "List of mate constraints")
   (parameters :initform (make-hash-table :test 'eq)
               :accessor assembly-parameters
               :documentation "Hash table of assembly parameters")
   (metadata :initarg :metadata
             :initform '()
             :accessor assembly-metadata
             :documentation "Assembly metadata (plist)"))
  (:documentation "Hierarchical assembly with components and mate constraints.

An assembly contains:
- Named components (parts or sub-assemblies)
- Mate constraints between components
- Parameters for parametric assemblies
- Metadata (project info, version, etc.)

Example:
  (let ((asm (make-assembly :name :motor-assembly)))
    (add-component asm :housing (make-box 100 100 50))
    (add-component asm :shaft (make-cylinder 10 120))
    ...)"))

;;; ==============================================================================
;;; Component Class (Week 9-10: Component System)
;;; ==============================================================================

(defclass component ()
  ((name :initarg :name
         :initform nil
         :accessor component-name
         :documentation "Component name (symbol)")
   (part :initarg :part
         :initform nil
         :accessor component-part
         :documentation "Part (shape) or sub-assembly")
   (assembly :initarg :assembly
             :initform nil
             :accessor component-assembly
             :documentation "Parent assembly")
   (position :initarg :position
             :initform '(0 0 0)
             :accessor component-position
             :documentation "Position (x y z) in mm")
   (rotation :initarg :rotation
             :initform '(:axis (0 0 1) :angle 0)
             :accessor component-rotation
             :documentation "Rotation as axis-angle (plist)")
   (fixed :initarg :fixed
          :initform nil
          :accessor component-fixed-p
          :documentation "Whether component is fixed (ground)")
   (quantity :initarg :quantity
             :initform 1
             :accessor component-quantity
             :documentation "Quantity for BOM")
   (metadata :initarg :metadata
             :initform '()
             :accessor component-metadata
             :documentation "Component metadata (plist: :part-number, :material, :vendor, etc.)"))
  (:documentation "Component in an assembly.

A component represents:
- A part (shape) or sub-assembly
- Position and orientation in parent assembly
- Metadata for BOM generation (part number, material, vendor, etc.)
- Quantity (for multiple identical parts)

Components can be nested (assembly as part) to create hierarchies.

Example:
  (let ((comp (make-instance 'component
                             :name :base-plate
                             :part (make-box 100 100 10)
                             :metadata '(:part-number \"BP-001\"
                                        :material \"6061-T6 Aluminum\"))))
    ...)"))

;;; ==============================================================================
;;; Constructor Functions
;;; ==============================================================================

(defun make-assembly (&key name description metadata)
  "Create a new assembly.

  Args:
    :name - Assembly name (symbol)
    :description - Optional description string
    :metadata - Optional metadata plist

  Returns: New assembly instance

  Example:
    (make-assembly :name :motor-assembly
                   :description \"DC motor assembly\"
                   :metadata '(:project \"Robot\" :version \"1.0\"))"
  (make-instance 'assembly
                 :name name
                 :description description
                 :metadata metadata))

(defun assembly-p (obj)
  "Test if object is an assembly.

  Args:
    obj - Object to test

  Returns: True if obj is an assembly, nil otherwise

  Example:
    (assembly-p (make-assembly :name :test)) => T
    (assembly-p \"not an assembly\") => NIL"
  (typep obj 'assembly))

;;; ==============================================================================
;;; Component Management
;;; ==============================================================================

(defun add-component (assembly name part &key (quantity 1) (fixed nil) (metadata '()))
  "Add a component to an assembly.

  Args:
    assembly - Assembly to add to
    name - Component name (symbol)
    part - Shape or sub-assembly
    :quantity - Quantity for BOM (default 1)
    :fixed - Whether component is fixed/ground (default nil)
    :metadata - Metadata plist (default empty)

  Returns: The assembly (for chaining)

  The component is created and stored in the assembly's component hash table.
  Initial position is (0 0 0) and rotation is identity.

  Example:
    (add-component asm :base (make-box 100 100 10)
                   :quantity 1
                   :fixed t
                   :metadata '(:part-number \"BP-001\"
                              :material \"Aluminum\"))"
  (let ((comp (make-instance 'component
                             :name name
                             :part part
                             :assembly assembly
                             :quantity quantity
                             :fixed fixed
                             :metadata metadata)))
    (setf (gethash name (assembly-components assembly)) comp))
  assembly)

(defun get-component (assembly name)
  "Get a component from an assembly by name.

  Args:
    assembly - Assembly to search
    name - Component name (symbol)

  Returns: Component if found, nil otherwise

  Example:
    (get-component asm :base-plate) => #<COMPONENT :BASE-PLATE>"
  (gethash name (assembly-components assembly)))

(defun remove-component (assembly name)
  "Remove a component from an assembly.

  Args:
    assembly - Assembly to remove from
    name - Component name (symbol)

  Returns: The assembly (for chaining)

  Example:
    (remove-component asm :old-part)"
  (remhash name (assembly-components assembly))
  assembly)

(defun list-components (assembly)
  "List all components in an assembly.

  Args:
    assembly - Assembly to list from

  Returns: List of component objects

  Example:
    (list-components asm) => (#<COMPONENT :PART1> #<COMPONENT :PART2>)"
  (let ((components '()))
    (maphash (lambda (name comp)
               (declare (ignore name))
               (push comp components))
             (assembly-components assembly))
    (nreverse components)))

;;; ==============================================================================
;;; Assembly Parameters
;;; ==============================================================================

(defun set-parameter (assembly name value)
  "Set a parameter in an assembly.

  Args:
    assembly - Assembly to modify
    name - Parameter name (symbol)
    value - Parameter value

  Returns: The value

  Parameters are used for parametric assemblies (e.g., spacing, counts).

  Example:
    (set-parameter asm :bolt-spacing 25)
    (set-parameter asm :bolt-count 4)"
  (setf (gethash name (assembly-parameters assembly)) value))

(defun get-parameter (assembly name &optional default)
  "Get a parameter from an assembly.

  Args:
    assembly - Assembly to query
    name - Parameter name (symbol)
    default - Default value if not found

  Returns: Parameter value or default

  Example:
    (get-parameter asm :bolt-spacing) => 25
    (get-parameter asm :missing :default-value) => :DEFAULT-VALUE"
  (gethash name (assembly-parameters assembly) default))

;;; ==============================================================================
;;; Component Transforms (Week 9-10)
;;; ==============================================================================

(defun set-component-position (component x y z)
  "Set component position.

  Args:
    component - Component to modify
    x, y, z - Position coordinates in mm

  Returns: The component (for chaining)

  Example:
    (set-component-position comp 10 20 30)"
  (setf (component-position component) (list x y z))
  component)

(defun set-component-rotation (component &key axis angle)
  "Set component rotation using axis-angle representation.

  Args:
    component - Component to modify
    :axis - Rotation axis as (x y z) list
    :angle - Rotation angle in degrees

  Returns: The component (for chaining)

  Example:
    (set-component-rotation comp :axis '(0 0 1) :angle 45)"
  (setf (component-rotation component)
        (list :axis axis :angle angle))
  component)

(defun component-transform (component)
  "Compute 4x4 homogeneous transformation matrix for component.

  Args:
    component - Component to compute transform for

  Returns: 4x4 transformation matrix (2D array)

  The transform combines position and rotation into a single 4x4 matrix
  suitable for OpenCascade transformations.

  Matrix format:
    | R11 R12 R13 Tx |
    | R21 R22 R23 Ty |
    | R31 R32 R33 Tz |
    |  0   0   0   1 |

  Where R is the 3x3 rotation matrix and T is the translation vector.

  Example:
    (component-transform comp) => #2A((1.0 0.0 0.0 10.0) ...)"
  (let* ((pos (component-position component))
         (rot (component-rotation component))
         (tx (first pos))
         (ty (second pos))
         (tz (third pos))
         (axis (getf rot :axis))
         (angle-deg (getf rot :angle))
         (angle-rad (* angle-deg (/ pi 180.0)))
         (transform (make-array '(4 4) :initial-element 0.0)))

    ;; If angle is zero, use identity rotation
    (if (zerop angle-deg)
        (progn
          ;; Identity rotation matrix
          (setf (aref transform 0 0) 1.0
                (aref transform 1 1) 1.0
                (aref transform 2 2) 1.0))
        ;; Compute rotation matrix using axis-angle (Rodrigues' formula)
        (let* ((ax (coerce (first axis) 'double-float))
               (ay (coerce (second axis) 'double-float))
               (az (coerce (third axis) 'double-float))
               ;; Normalize axis
               (len (sqrt (+ (* ax ax) (* ay ay) (* az az))))
               (nx (/ ax len))
               (ny (/ ay len))
               (nz (/ az len))
               (c (cos angle-rad))
               (s (sin angle-rad))
               (t_ (- 1.0 c)))
          ;; Rodrigues' rotation matrix
          (setf (aref transform 0 0) (+ c (* t_ nx nx))
                (aref transform 0 1) (- (* t_ nx ny) (* s nz))
                (aref transform 0 2) (+ (* t_ nx nz) (* s ny))

                (aref transform 1 0) (+ (* t_ nx ny) (* s nz))
                (aref transform 1 1) (+ c (* t_ ny ny))
                (aref transform 1 2) (- (* t_ ny nz) (* s nx))

                (aref transform 2 0) (- (* t_ nx nz) (* s ny))
                (aref transform 2 1) (+ (* t_ ny nz) (* s nx))
                (aref transform 2 2) (+ c (* t_ nz nz)))))

    ;; Set translation
    (setf (aref transform 0 3) (coerce tx 'double-float)
          (aref transform 1 3) (coerce ty 'double-float)
          (aref transform 2 3) (coerce tz 'double-float))

    ;; Set bottom row
    (setf (aref transform 3 0) 0.0
          (aref transform 3 1) 0.0
          (aref transform 3 2) 0.0
          (aref transform 3 3) 1.0)

    transform))
