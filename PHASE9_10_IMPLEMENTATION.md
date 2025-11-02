# CLAD Phase 9 & 10: Sketching and Assembly Implementation Plan
## Test-Driven Development with Common Lisp Advantages

**Version:** 1.0
**Date:** 2025-11-01
**Status:** Implementation Specification
**Methodology:** Test-Driven Development (TDD)

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Common Lisp Advantages](#common-lisp-advantages)
3. [Phase 9: 2D Sketching System](#phase-9-2d-sketching-system)
4. [Phase 10: Assembly System](#phase-10-assembly-system)
5. [TDD Implementation Strategy](#tdd-implementation-strategy)
6. [Testing Infrastructure](#testing-infrastructure)
7. [Implementation Timeline](#implementation-timeline)

---

## Executive Summary

### Goals

Implement production-ready 2D sketching and assembly systems using Test-Driven Development (TDD), with emphasis on leveraging Common Lisp's unique strengths over Python-based systems like CadQuery.

### Key Principles

1. **Test-First Development** - Write comprehensive tests before implementation
2. **CLOS-Based Architecture** - Extensible object system with multiple dispatch
3. **Macro-Powered DSL** - Compile-time optimization and symbolic computation
4. **Condition System** - Sophisticated error handling beyond exceptions
5. **Symbol-Based References** - No string parsing, full IDE support
6. **REPL-Driven** - Interactive development with instant feedback

### Comparison: CadQuery vs CLAD Approach

| Feature | CadQuery (Python) | CLAD (Common Lisp) |
|---------|------------------|-------------------|
| References | String-based `"part@face@>Z"` | Symbol-based `(@ref part :face :direction :+z)` |
| Constraints | Class hierarchy | CLOS with multiple dispatch |
| Solver | Numerical only | Symbolic + numerical (macros) |
| Error Handling | Exceptions | Condition system with restarts |
| DSL | Python decorators | Full macro system |
| Extensibility | Inheritance | Mixins, protocols, generic functions |
| IDE Support | Limited | Full (symbol completion, navigation) |

---

## Common Lisp Advantages

### 1. CLOS (Common Lisp Object System)

**Multiple Dispatch**:
```lisp
;; Python: Single dispatch only
class PointOnLineConstraint:
    def apply(self, point, line): ...

;; Lisp: Multiple dispatch - extensible by users!
(defgeneric apply-constraint (constraint entity1 entity2))

(defmethod apply-constraint ((c point-on-line) (p point) (l line))
  "Built-in implementation")

(defmethod apply-constraint ((c point-on-line) (p point) (l arc))
  "User can add new combinations!")
```

**Mixins & Protocols**:
```lisp
;; Define protocols that entities can opt-into
(defclass measurable-entity () ())
(defclass parametric-entity () ())

(defclass line (sketch-entity measurable-entity parametric-entity) ...)
```

### 2. Macros for Symbolic Computation

**Automatic Differentiation**:
```lisp
;; Python: Manual gradient computation
def error_func(x, y):
    return (x - target_x)**2 + (y - target_y)**2

def gradient_func(x, y):
    return [2*(x - target_x), 2*(y - target_y)]  # Hand-coded!

;; Lisp: Automatic via macros
(defmacro constraint-error ((x y) &body forms)
  `(list (compute-value ,@forms)
         (compute-gradient ',forms '(,x ,y))))  ; Compile-time!

;; Usage - gradient computed automatically!
(constraint-error (x y)
  (+ (expt (- x target-x) 2)
     (expt (- y target-y) 2)))
```

### 3. Condition System

**Restartable Errors**:
```lisp
;; Python: Exception stops execution
try:
    solver.solve()
except OverConstrainedError:
    # Must rebuild entire solve attempt
    pass

;; Lisp: Offer solutions without unwinding
(handler-bind
    ((over-constrained-error
      (lambda (c)
        (invoke-restart 'remove-weakest-constraint)
        (invoke-restart 'relax-tolerances)
        (invoke-restart 'use-approximate-solution))))
  (solve-constraints sketch))
```

### 4. Symbol-Based References

**No String Parsing**:
```lisp
;; CadQuery: String parsing at runtime
.constrain("housing@faces@>Z", "shaft@faces@<Z", "Plane")
  # Error-prone, no IDE help, fails at runtime

;; CLAD: Symbols checked at compile time
(:mate housing (@ref housing :face :direction :+z :extreme :max)
       shaft   (@ref shaft :face :direction :-z :extreme :min)
       :type :plane)
  ;; IDE autocomplete, compile-time checking, navigate to definition
```

### 5. Lisp-2 (Separate Function/Variable Namespace)

**No Name Conflicts**:
```lisp
;; Python: Must avoid conflicts
line = sketch.line(...)  # 'line' shadows 'line' function

;; Lisp: Function and variable namespaces are separate
(let ((line (make-line ...)))  ; Variable
  (line-length line))          ; Function - no conflict!
```

### 6. Reader Macros & Syntax Extension

**Domain-Specific Syntax**:
```lisp
;; Can extend Lisp syntax itself
(set-macro-character #\@
  (lambda (stream char)
    (list 'make-reference (read stream))))

;; Now can write:
#:bolt@thread-face  ; Expands to (make-reference 'bolt 'thread-face)
```

---

## Phase 9: 2D Sketching System

### Overview

Implement constraint-based 2D sketching on workplanes with a focus on:
- **Parametric** - Constraints drive geometry
- **Solver-based** - Automatic satisfaction of constraints
- **Extensible** - Users can define custom constraints
- **DSL-integrated** - Natural syntax in defpart

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│  DSL Layer: (defsketch name ... (:constraint ...) ...)  │
├─────────────────────────────────────────────────────────┤
│  Constraint System: CLOS hierarchy + solver             │
├─────────────────────────────────────────────────────────┤
│  Entity System: Points, lines, arcs, splines           │
├─────────────────────────────────────────────────────────┤
│  Solver Engine: Symbolic + numerical optimization      │
├─────────────────────────────────────────────────────────┤
│  OCCT 2D Geometry: Geom2d_* classes                    │
└─────────────────────────────────────────────────────────┘
```

### TDD Implementation: Sketch Entities

#### Test 1: Basic Point Entity
```lisp
;; tests/sketch/entity-tests.lisp

(deftest test-point-creation ()
  "Test creating a 2D point"
  (let ((p (make-point-2d 10.0 20.0)))
    (is (point-2d-p p))
    (is (= 10.0 (point-x p)))
    (is (= 20.0 (point-y p)))))

(deftest test-point-parameters ()
  "Points should have named parameters"
  (let ((p (make-point-2d 10.0 20.0 :name :p1)))
    (is (eq :p1 (entity-name p)))
    (is (= 2 (length (entity-parameters p))))  ; x and y
    (is (member :x (entity-parameters p) :key #'param-name))
    (is (member :y (entity-parameters p) :key #'param-name))))
```

#### Implementation: Point Entity
```lisp
;; src/sketch/entities.lisp

(defpackage :clad.sketch
  (:use :cl)
  (:export #:sketch-entity #:point-2d #:line-2d #:arc-2d
           #:make-point-2d #:make-line-2d #:make-arc-2d
           #:entity-name #:entity-parameters
           #:point-x #:point-y))

(in-package :clad.sketch)

(defclass sketch-parameter ()
  ((name :initarg :name :accessor param-name)
   (value :initarg :value :accessor param-value)
   (fixed :initarg :fixed :initform nil :accessor param-fixed-p)
   (min :initarg :min :initform nil :accessor param-min)
   (max :initarg :max :initform nil :accessor param-max))
  (:documentation "A parameter in a sketch entity"))

(defclass sketch-entity ()
  ((name :initarg :name :accessor entity-name)
   (parameters :initarg :parameters :accessor entity-parameters)
   (dependent-constraints :initform '() :accessor entity-constraints))
  (:documentation "Base class for all 2D sketch entities"))

(defclass point-2d (sketch-entity)
  ()
  (:documentation "2D point defined by x,y parameters"))

(defun make-point-2d (x y &key name (fixed nil))
  "Create a 2D point at (x, y)"
  (make-instance 'point-2d
    :name name
    :parameters (list
                 (make-instance 'sketch-parameter
                   :name :x :value (coerce x 'double-float) :fixed fixed)
                 (make-instance 'sketch-parameter
                   :name :y :value (coerce y 'double-float) :fixed fixed))))

(defun point-x (point)
  "Get x coordinate of point"
  (param-value (find :x (entity-parameters point) :key #'param-name)))

(defun point-y (point)
  "Get y coordinate of point"
  (param-value (find :y (entity-parameters point) :key #'param-name)))
```

#### Test 2: Line Entity
```lisp
(deftest test-line-from-points ()
  "Test creating a line from two points"
  (let* ((p1 (make-point-2d 0.0 0.0 :name :p1))
         (p2 (make-point-2d 10.0 0.0 :name :p2))
         (line (make-line-2d p1 p2 :name :line1)))
    (is (line-2d-p line))
    (is (eq p1 (line-start line)))
    (is (eq p2 (line-end line)))
    (is (= 10.0 (line-length line)))))

(deftest test-line-parametric-form ()
  "Line parameters should reference point parameters"
  (let* ((p1 (make-point-2d 0.0 0.0 :name :p1))
         (p2 (make-point-2d 10.0 0.0 :name :p2))
         (line (make-line-2d p1 p2)))
    ;; Line doesn't store coords - references points
    (is (= 0 (length (entity-parameters line))))
    ;; But can compute properties
    (is (= 10.0 (line-length line)))
    ;; Modify point - line updates
    (setf (param-value (find :x (entity-parameters p2) :key #'param-name)) 20.0)
    (is (= 20.0 (line-length line)))))
```

#### Implementation: Line Entity
```lisp
(defclass line-2d (sketch-entity)
  ((start-point :initarg :start :accessor line-start)
   (end-point :initarg :end :accessor line-end))
  (:documentation "2D line segment defined by two points"))

(defun make-line-2d (start-point end-point &key name)
  "Create a line from start to end point"
  (make-instance 'line-2d
    :name name
    :start start-point
    :end end-point
    :parameters '()))  ; No direct parameters - computed from points

(defgeneric line-length (line)
  (:documentation "Compute length of line"))

(defmethod line-length ((line line-2d))
  "Compute line length from endpoints"
  (let ((x1 (point-x (line-start line)))
        (y1 (point-y (line-start line)))
        (x2 (point-x (line-end line)))
        (y2 (point-y (line-end line))))
    (sqrt (+ (expt (- x2 x1) 2)
             (expt (- y2 y1) 2)))))
```

### TDD Implementation: Constraints

#### Test 3: Fixed Constraint
```lisp
;; tests/sketch/constraint-tests.lisp

(deftest test-fixed-constraint ()
  "Test fixing a point's position"
  (let* ((p (make-point-2d 10.0 20.0 :name :p1))
         (c (make-constraint 'fixed p :x 10.0 :y 20.0)))
    (is (constraint-p c))
    (is (= 0.0 (constraint-error c)))  ; Already satisfied
    ;; Move point - constraint violated
    (setf (param-value (find :x (entity-parameters p) :key #'param-name)) 15.0)
    (is (> (constraint-error c) 0.0))  ; Now violated
    ;; Solver should bring it back
    (apply-constraint c)
    (is (< (constraint-error c) 1e-6))))  ; Satisfied again

(deftest test-coincident-constraint ()
  "Test making two points coincident"
  (let* ((p1 (make-point-2d 0.0 0.0 :name :p1))
         (p2 (make-point-2d 10.0 10.0 :name :p2))
         (c (make-constraint 'coincident p1 p2)))
    (is (> (constraint-error c) 0.0))  ; Initially violated
    (apply-constraint c)
    (is (< (constraint-error c) 1e-6))  ; Now satisfied
    ;; Both points should be at same location
    (is (< (abs (- (point-x p1) (point-x p2))) 1e-6))
    (is (< (abs (- (point-y p1) (point-y p2))) 1e-6))))
```

#### Implementation: Constraint System
```lisp
;; src/sketch/constraints.lisp

(defpackage :clad.sketch.constraints
  (:use :cl :clad.sketch)
  (:export #:constraint #:make-constraint
           #:constraint-error #:apply-constraint
           #:fixed #:coincident #:distance #:horizontal #:vertical))

(in-package :clad.sketch.constraints)

(defclass constraint ()
  ((entities :initarg :entities :accessor constraint-entities)
   (type :initarg :type :accessor constraint-type)
   (parameters :initarg :parameters :accessor constraint-parameters)
   (weight :initarg :weight :initform 1.0 :accessor constraint-weight))
  (:documentation "Base class for geometric constraints"))

(defgeneric constraint-error (constraint)
  (:documentation "Compute error measure for constraint (0 = satisfied)"))

(defgeneric constraint-jacobian (constraint)
  (:documentation "Compute gradient of error with respect to parameters"))

(defgeneric apply-constraint (constraint)
  (:documentation "Apply constraint by modifying entity parameters"))

;; Fixed constraint: Point must be at specific location
(defclass fixed-constraint (constraint)
  ((target-x :initarg :x :accessor target-x)
   (target-y :initarg :y :accessor target-y))
  (:documentation "Fixes a point at a specific location"))

(defmethod constraint-error ((c fixed-constraint))
  "Error = distance from target position"
  (let* ((point (first (constraint-entities c)))
         (x (point-x point))
         (y (point-y point))
         (tx (target-x c))
         (ty (target-y c)))
    (+ (expt (- x tx) 2)
       (expt (- y ty) 2))))

(defmethod constraint-jacobian ((c fixed-constraint))
  "Gradient: [2*(x-tx), 2*(y-ty)]"
  (let* ((point (first (constraint-entities c)))
         (x (point-x point))
         (y (point-y point))
         (tx (target-x c))
         (ty (target-y c)))
    (list (* 2 (- x tx))
          (* 2 (- y ty)))))

;; Coincident constraint: Two points must be at same location
(defclass coincident-constraint (constraint)
  ()
  (:documentation "Makes two points coincident"))

(defmethod constraint-error ((c coincident-constraint))
  "Error = distance between points"
  (let* ((p1 (first (constraint-entities c)))
         (p2 (second (constraint-entities c)))
         (dx (- (point-x p1) (point-x p2)))
         (dy (- (point-y p1) (point-y p2))))
    (+ (expt dx 2) (expt dy 2))))

(defmethod constraint-jacobian ((c coincident-constraint))
  "Gradient with respect to both points"
  (let* ((p1 (first (constraint-entities c)))
         (p2 (second (constraint-entities c)))
         (dx (- (point-x p1) (point-x p2)))
         (dy (- (point-y p1) (point-y p2))))
    (list (* 2 dx)    ; ∂E/∂x1
          (* 2 dy)    ; ∂E/∂y1
          (* -2 dx)   ; ∂E/∂x2
          (* -2 dy)))) ; ∂E/∂y2
```

### TDD Implementation: Constraint Solver

#### Test 4: Simple Sketch Solving
```lisp
(deftest test-solve-simple-sketch ()
  "Test solving a sketch with constraints"
  (let* ((sketch (make-sketch :name :test))
         ;; Create entities
         (p1 (add-point sketch 0.0 0.0 :name :origin :fixed t))
         (p2 (add-point sketch 10.0 5.0 :name :p2))
         (line (add-line sketch p1 p2 :name :line1)))

    ;; Add constraints
    (add-constraint sketch 'horizontal line)  ; Line must be horizontal
    (add-constraint sketch 'distance p1 p2 :value 10.0)  ; Distance = 10

    ;; Solve
    (solve-sketch sketch)

    ;; Verify solution
    (is (< (abs (point-y p2)) 1e-6))  ; p2.y ≈ 0 (horizontal)
    (is (< (abs (- (line-length line) 10.0)) 1e-6))))  ; length ≈ 10

(deftest test-over-constrained-sketch ()
  "Test handling over-constrained sketches"
  (let* ((sketch (make-sketch))
         (p1 (add-point sketch 0.0 0.0 :fixed t))
         (p2 (add-point sketch 10.0 0.0 :fixed t)))

    ;; Conflicting constraint
    (add-constraint sketch 'distance p1 p2 :value 20.0)

    ;; Should signal condition
    (signals over-constrained-error
      (solve-sketch sketch))

    ;; Should offer restarts
    (handler-bind
        ((over-constrained-error
          (lambda (c)
            (is (find-restart 'relax-constraint))
            (is (find-restart 'remove-constraint))
            (invoke-restart 'remove-constraint))))
      (solve-sketch sketch))))
```

#### Implementation: Solver
```lisp
;; src/sketch/solver.lisp

(defpackage :clad.sketch.solver
  (:use :cl :clad.sketch :clad.sketch.constraints)
  (:export #:solve-sketch #:solver-options
           #:over-constrained-error #:under-constrained-error))

(in-package :clad.sketch.solver)

(define-condition over-constrained-error (error)
  ((sketch :initarg :sketch :reader error-sketch)
   (conflicting-constraints :initarg :constraints :reader error-constraints))
  (:report (lambda (condition stream)
             (format stream "Sketch is over-constrained. Conflicting constraints: ~A"
                     (error-constraints condition)))))

(defclass solver-options ()
  ((max-iterations :initarg :max-iterations :initform 100
                   :accessor solver-max-iterations)
   (tolerance :initarg :tolerance :initform 1e-6
              :accessor solver-tolerance)
   (method :initarg :method :initform :levenberg-marquardt
           :accessor solver-method))
  (:documentation "Options for constraint solver"))

(defun solve-sketch (sketch &optional (options (make-instance 'solver-options)))
  "Solve sketch constraints using nonlinear optimization"
  (let* ((constraints (sketch-constraints sketch))
         (parameters (collect-free-parameters sketch))
         (error-func (build-error-function constraints))
         (jacobian-func (build-jacobian-function constraints parameters)))

    ;; Check degrees of freedom
    (check-constraint-status sketch constraints parameters)

    ;; Run solver
    (multiple-value-bind (solution converged-p iterations final-error)
        (levenberg-marquardt error-func jacobian-func
                            (parameter-values parameters)
                            :max-iterations (solver-max-iterations options)
                            :tolerance (solver-tolerance options))

      ;; Update parameters with solution
      (update-parameters parameters solution)

      ;; Return results
      (values sketch converged-p iterations final-error))))

(defun check-constraint-status (sketch constraints parameters)
  "Check if sketch is properly constrained"
  (let ((n-params (length parameters))
        (n-constraints (length constraints)))
    (cond
      ((> n-constraints n-params)
       (restart-case
           (error 'over-constrained-error
                  :sketch sketch
                  :constraints constraints)
         (relax-constraint ()
           :report "Relax the weakest constraint"
           (relax-weakest-constraint constraints))
         (remove-constraint ()
           :report "Remove the most recently added constraint"
           (remove-last-constraint sketch))))

      ((< n-constraints (required-constraints n-params))
       (warn 'under-constrained-warning
             :sketch sketch
             :missing (- (required-constraints n-params) n-constraints))))))
```

### DSL Integration

#### Test 5: Sketch DSL
```lisp
(deftest test-sketch-dsl ()
  "Test declarative sketch syntax"
  (let ((profile
         (defsketch rectangular-profile ((width 50) (height 30))
           "A rectangular profile with rounded corners"

           ;; Define points
           (:point :p1 0 0 :fixed t)
           (:point :p2 width 0)
           (:point :p3 width height)
           (:point :p4 0 height)

           ;; Define lines
           (:line :bottom :p1 :p2)
           (:line :right :p2 :p3)
           (:line :top :p3 :p4)
           (:line :left :p4 :p1)

           ;; Constraints
           (:constraint :horizontal :bottom)
           (:constraint :vertical :right)
           (:constraint :horizontal :top)
           (:constraint :vertical :left)
           (:constraint :perpendicular :bottom :right)

           ;; Fillets at corners
           (:fillet :p2 :radius 5)
           (:fillet :p3 :radius 5))))

    ;; Should be a solved sketch
    (is (sketch-p profile))
    (is (sketch-solved-p profile))

    ;; Can use in 3D operations
    (let ((part (extrude-sketch profile 10)))
      (is (shape-p part)))))
```

#### Implementation: Sketch DSL
```lisp
;; src/sketch/dsl.lisp

(defmacro defsketch (name parameters &body body)
  "Define a parametric 2D sketch"
  (let ((docstring (when (stringp (first body)) (first body)))
        (forms (if (stringp (first body)) (rest body) body)))
    `(defun ,name ,parameters
       ,@(when docstring (list docstring))
       (let ((sketch (make-sketch :name ',name)))
         ,@(mapcar #'expand-sketch-form forms)
         (solve-sketch sketch)
         sketch))))

(defun expand-sketch-form (form)
  "Expand a sketch form into executable code"
  (case (first form)
    (:point
     (destructuring-bind (name x y &rest options) (rest form)
       `(add-point sketch ,x ,y :name ',name ,@options)))

    (:line
     (destructuring-bind (name start-point end-point) (rest form)
       `(add-line sketch
                  (find-entity sketch ',start-point)
                  (find-entity sketch ',end-point)
                  :name ',name)))

    (:arc
     (destructuring-bind (name start-point mid-point end-point) (rest form)
       `(add-arc sketch
                 (find-entity sketch ',start-point)
                 (find-entity sketch ',mid-point)
                 (find-entity sketch ',end-point)
                 :name ',name)))

    (:constraint
     (destructuring-bind (type &rest entities-and-params) (rest form)
       `(add-constraint sketch ',type
                        ,@(mapcar (lambda (e)
                                   (if (keywordp e)
                                       `',e  ; entity name
                                       e))  ; parameter value
                                 entities-and-params))))

    (:fillet
     (destructuring-bind (point &key radius) (rest form)
       `(add-fillet sketch (find-entity sketch ',point) ,radius)))))
```

---

## Phase 10: Assembly System

### Overview

Implement constraint-based assembly modeling with:
- **Component management** - Parts and subassemblies
- **Mate constraints** - Plane, axis, point relationships
- **BOM generation** - Automatic bill of materials
- **Parametric** - Assembly-level parameters

### TDD Implementation: Assembly Components

#### Test 6: Basic Assembly Creation
```lisp
;; tests/assembly/assembly-tests.lisp

(deftest test-create-assembly ()
  "Test creating an empty assembly"
  (let ((asm (make-assembly :name :test-assy)))
    (is (assembly-p asm))
    (is (eq :test-assy (assembly-name asm)))
    (is (zerop (length (assembly-components asm))))))

(deftest test-add-component ()
  "Test adding a component to assembly"
  (let* ((bracket (make-bracket :width 50))  ; from defpart
         (asm (make-assembly :name :test))
         (comp (add-component asm bracket :name :bracket-1)))
    (is (= 1 (length (assembly-components asm))))
    (is (eq :bracket-1 (component-name comp)))
    (is (eq bracket (component-part comp)))))

(deftest test-component-transform ()
  "Components should have position/orientation"
  (let* ((bracket (make-bracket))
         (asm (make-assembly))
         (comp (add-component asm bracket :name :b1
                             :position '(10 20 30)
                             :rotation '(:axis (0 0 1) :angle 45))))
    (is (equal '(10 20 30) (component-position comp)))
    (let ((mat (component-transform comp)))
      (is (= 4 (array-dimension mat 0)))  ; 4x4 matrix
      (is (= 4 (array-dimension mat 1))))))
```

#### Implementation: Assembly System
```lisp
;; src/assembly/assembly.lisp

(defpackage :clad.assembly
  (:use :cl :clad.shapes)
  (:export #:assembly #:component #:mate-constraint
           #:make-assembly #:add-component #:add-mate
           #:assembly-name #:assembly-components
           #:component-name #:component-part #:component-transform
           #:solve-assembly #:generate-bom))

(in-package :clad.assembly)

(defclass assembly ()
  ((name :initarg :name :accessor assembly-name)
   (components :initform (make-hash-table :test 'eq)
               :accessor assembly-components)
   (constraints :initform '()
                :accessor assembly-constraints)
   (parameters :initform (make-hash-table :test 'eq)
               :accessor assembly-parameters))
  (:documentation "Top-level assembly containing components and constraints"))

(defclass component ()
  ((name :initarg :name :accessor component-name)
   (part :initarg :part :accessor component-part)
   (assembly :initarg :assembly :accessor component-assembly)
   (position :initarg :position :initform '(0 0 0)
             :accessor component-position)
   (rotation :initarg :rotation :initform '(:axis (0 0 1) :angle 0)
             :accessor component-rotation)
   (transform :accessor component-transform)
   (fixed :initarg :fixed :initform nil :accessor component-fixed-p)
   (quantity :initarg :quantity :initform 1 :accessor component-quantity)
   (metadata :initarg :metadata :initform '()
             :accessor component-metadata))
  (:documentation "A part instance in an assembly"))

(defmethod initialize-instance :after ((comp component) &key)
  "Compute transform matrix from position/rotation"
  (setf (component-transform comp)
        (compute-transform (component-position comp)
                          (component-rotation comp))))

(defun make-assembly (&key name)
  "Create a new assembly"
  (make-instance 'assembly :name name))

(defun add-component (assembly part &key name position rotation fixed quantity metadata)
  "Add a component (part instance) to assembly"
  (let ((comp (make-instance 'component
                :name (or name (gensym "COMP"))
                :part part
                :assembly assembly
                :position (or position '(0 0 0))
                :rotation (or rotation '(:axis (0 0 1) :angle 0))
                :fixed fixed
                :quantity (or quantity 1)
                :metadata metadata)))
    (setf (gethash (component-name comp) (assembly-components assembly)) comp)
    comp))
```

#### Test 7: Mate Constraints
```lisp
(deftest test-plane-mate ()
  "Test mating two planar faces"
  (let* ((housing (make-housing))
         (cover (make-cover))
         (asm (make-assembly :name :housing-assy))
         (housing-comp (add-component asm housing :name :housing :fixed t))
         (cover-comp (add-component asm cover :name :cover)))

    ;; Mate top of housing to bottom of cover
    (add-mate asm
              :plane
              housing-comp (@ref housing :face :direction :+z :extreme :max)
              cover-comp (@ref cover :face :direction :-z :extreme :min))

    ;; Solve assembly
    (solve-assembly asm)

    ;; Verify cover is positioned correctly
    (let ((cover-pos (component-position cover-comp)))
      (is (= (third cover-pos)  ; z-coordinate
             (+ (housing-height housing) 0))))))  ; On top of housing

(deftest test-axis-mate ()
  "Test aligning two cylindrical features"
  (let* ((bracket (make-bracket))
         (bolt (make-bolt))
         (asm (make-assembly))
         (bracket-comp (add-component asm bracket :name :bracket :fixed t))
         (bolt-comp (add-component asm bolt :name :bolt)))

    ;; Align bolt axis with hole axis
    (add-mate asm
              :axis
              bracket-comp (@ref bracket :hole-axis)
              bolt-comp (@ref bolt :shaft-axis))

    (solve-assembly asm)

    ;; Bolt should be aligned with hole
    (let ((bolt-axis (get-axis bolt-comp))
          (hole-axis (get-axis bracket-comp :hole-axis)))
      (is (< (axis-angle bolt-axis hole-axis) 1e-6)))))
```

#### Implementation: Mate Constraints
```lisp
;; src/assembly/constraints.lisp

(defclass mate-constraint ()
  ((type :initarg :type :accessor mate-type)
   (component1 :initarg :comp1 :accessor mate-component1)
   (reference1 :initarg :ref1 :accessor mate-reference1)
   (component2 :initarg :comp2 :accessor mate-component2)
   (reference2 :initarg :ref2 :accessor mate-reference2)
   (offset :initarg :offset :initform 0.0 :accessor mate-offset)
   (flip :initarg :flip :initform nil :accessor mate-flip-p))
  (:documentation "Mate constraint between two components"))

(defgeneric mate-error (mate)
  (:documentation "Compute mate constraint violation"))

(defgeneric apply-mate (mate)
  (:documentation "Apply mate by adjusting component transforms"))

;; Plane mate: Two planar faces touch
(defmethod mate-error ((mate (eql :plane)) mate-constraint)
  "Error = distance + angle between planes"
  (let* ((plane1 (get-plane (mate-component1 mate) (mate-reference1 mate)))
         (plane2 (get-plane (mate-component2 mate) (mate-reference2 mate)))
         (dist-error (plane-distance plane1 plane2))
         (angle-error (plane-angle plane1 plane2)))
    (+ (* dist-error dist-error)
       (* angle-error angle-error))))

;; Axis mate: Two cylindrical axes align
(defmethod mate-error ((mate (eql :axis)) mate-constraint)
  "Error = distance between axes + angle between axes"
  (let* ((axis1 (get-axis (mate-component1 mate) (mate-reference1 mate)))
         (axis2 (get-axis (mate-component2 mate) (mate-reference2 mate)))
         (dist-error (axis-distance axis1 axis2))
         (angle-error (axis-angle axis1 axis2)))
    (+ (* dist-error dist-error)
       (* angle-error angle-error))))

;; Point mate: Two points coincide
(defmethod mate-error ((mate (eql :point)) mate-constraint)
  "Error = distance between points"
  (let* ((point1 (get-point (mate-component1 mate) (mate-reference1 mate)))
         (point2 (get-point (mate-component2 mate) (mate-reference2 mate))))
    (point-distance point1 point2)))

(defun solve-assembly (assembly &optional (options (make-instance 'solver-options)))
  "Solve assembly constraints by optimizing component transforms"
  (let* ((constraints (assembly-constraints assembly))
         (free-components (get-free-components assembly))
         (dof (compute-degrees-of-freedom free-components))
         (error-func (build-assembly-error-function constraints))
         (jacobian-func (build-assembly-jacobian-function constraints dof)))

    ;; Solve using nonlinear optimization
    (multiple-value-bind (solution converged-p iterations error)
        (levenberg-marquardt error-func jacobian-func
                            (get-component-transforms free-components)
                            :max-iterations (solver-max-iterations options)
                            :tolerance (solver-tolerance options))

      ;; Update component transforms
      (update-component-transforms free-components solution)

      (values assembly converged-p iterations error))))
```

### DSL Integration: Assembly

#### Test 8: Assembly DSL
```lisp
(deftest test-assembly-dsl ()
  "Test declarative assembly syntax"
  (let ((door-assy
         (defassembly door-assembly
             ((width 900) (height 2100) (panel-thickness 40))
           "Complete door assembly"

           ;; Components
           (:component :frame (door-frame :width width
                                         :height height
                                         :thickness panel-thickness)
             :fixed t
             :quantity 1
             :metadata (:material "Aluminum 6061-T6"
                       :finish "Anodized"
                       :supplier "Frame Co."))

           (:component :panel (door-panel :width (- width 20)
                                          :height (- height 20)
                                          :thickness panel-thickness)
             :quantity 1
             :metadata (:material "Oak"
                       :finish "Stained"))

           (:component :hinge (door-hinge :type :butt-hinge)
             :quantity 3
             :metadata (:material "Stainless Steel"
                       :part-number "HINGE-3IN-SS"))

           (:component :handle (door-handle :style :lever)
             :quantity 1
             :metadata (:material "Brushed Nickel"
                       :part-number "HANDLE-LVR-BN"))

           ;; Mates
           (:mate :frame (@ref frame :face :direction :+z :extreme :max)
                  :panel (@ref panel :face :direction :-z :extreme :min)
                  :type :plane
                  :offset 0)

           (:mate :frame (@ref frame :hinge-mount-1)
                  :hinge (@ref hinge :frame-face)
                  :type :plane)

           ;; Patterns
           (:pattern :linear
             :component :hinge
             :count 3
             :spacing (/ (- height 200) 2)
             :direction :z)

           ;; Derived dimensions
           (:dimension :panel-clearance
             :measure (min-distance (@ref frame :inner-face)
                                   (@ref panel :outer-face))
             :nominal 2.0
             :tolerance ±0.5
             :critical t))))

    (is (assembly-p door-assy))
    (is (= 6 (hash-table-count (assembly-components door-assy))))  ; frame + panel + 3 hinges + handle
    (is (solved-p door-assy))

    ;; Can generate BOM
    (let ((bom (generate-bom door-assy)))
      (is (= 4 (length bom)))  ; 4 unique part types
      (is (= 3 (getf (find :hinge bom :key #'bom-entry-name) :quantity))))))
```

#### Implementation: Assembly DSL
```lisp
;; src/assembly/dsl.lisp

(defmacro defassembly (name parameters &body body)
  "Define a parametric assembly"
  (let ((docstring (when (stringp (first body)) (first body)))
        (forms (if (stringp (first body)) (rest body) body)))
    `(defun ,name ,parameters
       ,@(when docstring (list docstring))
       (let ((assembly (make-assembly :name ',name)))
         ,@(mapcar #'expand-assembly-form forms)
         (solve-assembly assembly)
         assembly))))

(defun expand-assembly-form (form)
  "Expand an assembly form"
  (case (first form)
    (:component
     (destructuring-bind (name part-form &rest options) (rest form)
       `(add-component assembly ,part-form
                      :name ',name
                      ,@options)))

    (:mate
     (destructuring-bind (comp1 ref1 comp2 ref2 &rest options) (rest form)
       `(add-mate assembly
                  ,comp1 ,ref1
                  ,comp2 ,ref2
                  ,@options)))

    (:pattern
     (destructuring-bind (pattern-type &rest pattern-spec) (rest form)
       (expand-assembly-pattern assembly pattern-type pattern-spec)))

    (:dimension
     (destructuring-bind (name &rest dim-spec) (rest form)
       `(add-dimension assembly ',name ,@dim-spec)))))

(defmacro @ref (component &rest selector-spec)
  "Create a reference to a feature on a component"
  `(make-reference ',component ',selector-spec))
```

### BOM Generation

#### Test 9: Bill of Materials
```lisp
(deftest test-bom-generation ()
  "Test generating BOM from assembly"
  (let* ((asm (door-assembly 900 2100 40))
         (bom (generate-bom asm)))

    ;; Should have all unique parts
    (is (find :frame bom :key #'bom-entry-name))
    (is (find :panel bom :key #'bom-entry-name))
    (is (find :hinge bom :key #'bom-entry-name))

    ;; Should aggregate quantities
    (let ((hinge-entry (find :hinge bom :key #'bom-entry-name)))
      (is (= 3 (bom-entry-quantity hinge-entry))))

    ;; Should include metadata
    (let ((frame-entry (find :frame bom :key #'bom-entry-name)))
      (is (equal "Aluminum 6061-T6"
                 (getf (bom-entry-metadata frame-entry) :material))))))

(deftest test-nested-bom ()
  "Test BOM with subassemblies"
  (let* ((asm (cabinet-assembly))  ; Contains drawer subassemblies
         (bom (generate-bom asm :flatten t)))

    ;; Flattened BOM should show all parts
    (is (find :drawer-front bom :key #'bom-entry-name))

    ;; Quantities should be multiplied through levels
    (let ((screw-entry (find :wood-screw bom :key #'bom-entry-name)))
      (is (= 120 (bom-entry-quantity screw-entry))))))  ; 4 drawers * 30 screws/drawer
```

#### Implementation: BOM Generation
```lisp
;; src/assembly/bom.lisp

(defclass bom-entry ()
  ((name :initarg :name :accessor bom-entry-name)
   (part :initarg :part :accessor bom-entry-part)
   (quantity :initarg :quantity :accessor bom-entry-quantity)
   (metadata :initarg :metadata :accessor bom-entry-metadata)
   (unit-cost :initarg :unit-cost :initform nil :accessor bom-entry-unit-cost)
   (total-cost :accessor bom-entry-total-cost))
  (:documentation "Entry in bill of materials"))

(defmethod initialize-instance :after ((entry bom-entry) &key)
  "Compute total cost"
  (when (bom-entry-unit-cost entry)
    (setf (bom-entry-total-cost entry)
          (* (bom-entry-quantity entry)
             (bom-entry-unit-cost entry)))))

(defun generate-bom (assembly &key (flatten nil) (format :list))
  "Generate bill of materials from assembly

  Options:
    :flatten - If T, expand subassemblies into flat list
    :format  - :list, :csv, :json, :html, :lisp"
  (let ((entries (make-hash-table :test 'eq)))

    ;; Collect all components
    (collect-bom-entries assembly entries 1 flatten)

    ;; Convert to list
    (let ((bom-list (hash-table-values entries)))
      ;; Format output
      (ecase format
        (:list bom-list)
        (:csv (format-bom-csv bom-list))
        (:json (format-bom-json bom-list))
        (:html (format-bom-html bom-list))
        (:lisp bom-list)))))

(defun collect-bom-entries (assembly entries multiplier flatten)
  "Recursively collect BOM entries"
  (loop for component being the hash-values of (assembly-components assembly)
        do (let ((part (component-part component))
                 (qty (* (component-quantity component) multiplier)))

             (cond
               ;; Part is an assembly
               ((assembly-p part)
                (if flatten
                    ;; Recurse into subassembly
                    (collect-bom-entries part entries qty flatten)
                    ;; Add subassembly as single entry
                    (add-bom-entry entries component qty)))

               ;; Regular part
               (t
                (add-bom-entry entries component qty))))))

(defun add-bom-entry (entries component quantity)
  "Add or update BOM entry"
  (let ((name (component-name component)))
    (if (gethash name entries)
        ;; Update quantity
        (incf (bom-entry-quantity (gethash name entries)) quantity)
        ;; Create new entry
        (setf (gethash name entries)
              (make-instance 'bom-entry
                :name name
                :part (component-part component)
                :quantity quantity
                :metadata (component-metadata component))))))
```

---

## TDD Implementation Strategy

### Phase 9: 2D Sketching (8 weeks)

#### Week 1-2: Entity System
- **Tests First**: Point, line, arc, spline entities
- **Implementation**: Basic CLOS classes
- **Verification**: All entity tests pass

#### Week 3-4: Constraint System
- **Tests First**: Fixed, coincident, distance, horizontal, vertical, parallel, perpendicular, tangent, equal-length, equal-radius constraints
- **Implementation**: Constraint classes with error/jacobian methods
- **Verification**: Each constraint type tested individually

#### Week 5-6: Solver
- **Tests First**: Simple sketches (3-4 entities), complex sketches (10+ entities), over-constrained, under-constrained
- **Implementation**: Levenberg-Marquardt solver with condition system
- **Verification**: All sketches solve correctly, conditions handled properly

#### Week 7-8: DSL & Integration
- **Tests First**: `defsketch` macro, extrude-sketch, revolve-sketch, integration with defpart
- **Implementation**: DSL macros, 3D operations from sketches
- **Verification**: End-to-end examples work

### Phase 10: Assembly (8 weeks)

#### Week 9-10: Component System
- **Tests First**: Assembly creation, add component, component transforms, nested assemblies
- **Implementation**: Assembly and component classes
- **Verification**: Component management works

#### Week 11-12: Mate Constraints
- **Tests First**: Plane mate, axis mate, point mate, angle mate, tangent mate
- **Implementation**: Mate constraint types and solver
- **Verification**: All mate types tested

#### Week 13-14: Assembly Solver
- **Tests First**: Simple assemblies (2-3 parts), complex assemblies (10+ parts), kinematic chains
- **Implementation**: Assembly constraint solver
- **Verification**: All assemblies solve correctly

#### Week 15-16: DSL & BOM
- **Tests First**: `defassembly` macro, BOM generation, nested BOMs, CSV/JSON export
- **Implementation**: Assembly DSL and BOM system
- **Verification**: Complete workflow tested

---

## Testing Infrastructure

### Test Organization

```
tests/
├── sketch/
│   ├── entity-tests.lisp       ; Test 1-2: Entities
│   ├── constraint-tests.lisp   ; Test 3: Constraints
│   ├── solver-tests.lisp       ; Test 4: Solver
│   └── dsl-tests.lisp          ; Test 5: DSL
├── assembly/
│   ├── assembly-tests.lisp     ; Test 6: Assembly basics
│   ├── mate-tests.lisp         ; Test 7: Mate constraints
│   ├── dsl-tests.lisp          ; Test 8: Assembly DSL
│   └── bom-tests.lisp          ; Test 9: BOM generation
└── integration/
    ├── sketch-to-3d-tests.lisp ; Extrude/revolve from sketch
    └── assembly-workflow-tests.lisp ; Complete workflows
```

### Test Utilities

```lisp
;; tests/test-utils.lisp

(defpackage :clad.test-utils
  (:use :cl :fiveam :clad.sketch :clad.assembly)
  (:export #:with-test-sketch #:with-test-assembly
           #:assert-solved #:assert-coincident #:assert-parallel
           #:make-test-bracket #:make-test-housing))

(defmacro with-test-sketch (&body body)
  "Create a temporary sketch for testing"
  `(let ((sketch (make-sketch :name :test)))
     ,@body))

(defun assert-solved (sketch &optional (tolerance 1e-6))
  "Assert that sketch is solved within tolerance"
  (is (< (sketch-total-error sketch) tolerance)
      "Sketch not solved: error = ~A" (sketch-total-error sketch)))

(defun assert-coincident (point1 point2 &optional (tolerance 1e-6))
  "Assert that two points are coincident"
  (let ((dist (point-distance point1 point2)))
    (is (< dist tolerance)
        "Points not coincident: distance = ~A" dist)))
```

### Continuous Integration

```yaml
# .github/workflows/test.yml

name: CLAD Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Install SBCL
        run: sudo apt-get install sbcl

      - name: Install Quicklisp
        run: curl -O https://beta.quicklisp.org/quicklisp.lisp && sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit

      - name: Install OCCT
        run: sudo apt-get install libocct-dev

      - name: Build C wrapper
        run: cd c-wrapper && ./build.sh

      - name: Run Phase 9 tests
        run: sbcl --eval "(ql:quickload :clad)" --eval "(asdf:test-system :clad/sketch)" --quit

      - name: Run Phase 10 tests
        run: sbcl --eval "(ql:quickload :clad)" --eval "(asdf:test-system :clad/assembly)" --quit

      - name: Generate coverage report
        run: sbcl --eval "(ql:quickload :sb-cover)" --eval "(sb-cover:report)" --quit
```

---

## Implementation Timeline

### Phase 9: 2D Sketching System (8 weeks)

| Week | Focus | Deliverables | Tests |
|------|-------|-------------|-------|
| 1 | Entity System | Point, Line classes | 10 tests |
| 2 | Entity System | Arc, Spline, Circle | 15 tests |
| 3 | Constraints | Fixed, Coincident, Distance | 12 tests |
| 4 | Constraints | Angle, Parallel, Perpendicular | 15 tests |
| 5 | Solver | Basic solver implementation | 20 tests |
| 6 | Solver | Advanced features, conditions | 15 tests |
| 7 | DSL | defsketch macro | 10 tests |
| 8 | Integration | Extrude, revolve, examples | 20 tests |
| **Total** | | **Complete sketch system** | **117 tests** |

### Phase 10: Assembly System (8 weeks)

| Week | Focus | Deliverables | Tests |
|------|-------|-------------|-------|
| 9 | Components | Assembly, Component classes | 12 tests |
| 10 | Components | Transforms, nesting | 15 tests |
| 11 | Mates | Plane, Axis, Point mates | 18 tests |
| 12 | Mates | Advanced mates, patterns | 12 tests |
| 13 | Solver | Assembly solver | 20 tests |
| 14 | Solver | Kinematic chains, conditions | 15 tests |
| 15 | DSL & BOM | defassembly, BOM generation | 15 tests |
| 16 | Integration | Complete workflows, examples | 25 tests |
| **Total** | | **Complete assembly system** | **132 tests** |

### Combined: 16 weeks, 249 tests

---

## Conclusion

This TDD-based implementation plan for Phase 9 and 10 systematically builds on CLAD's existing architecture while fully leveraging Common Lisp's unique advantages:

**Key Differentiators from CadQuery**:

1. **CLOS Multiple Dispatch** - Extensible constraint system
2. **Macro-Based Symbolic Computation** - Automatic differentiation for solver
3. **Condition System** - Sophisticated error handling with restarts
4. **Symbol-Based References** - Compile-time checking, full IDE support
5. **Lisp-2 Namespace** - No naming conflicts
6. **True Parametrics** - Deep integration with existing parameter system

**TDD Benefits**:

- **Confidence** - Comprehensive test coverage ensures correctness
- **Documentation** - Tests serve as executable specifications
- **Regression Prevention** - Catch breakage immediately
- **Design Feedback** - Tests drive clean API design
- **Refactoring Safety** - Change implementation with confidence

**Next Steps**:

1. Review and approve this specification
2. Set up Phase 9 testing infrastructure
3. Begin Week 1: Point and Line entity tests
4. Implement to pass tests
5. Proceed iteratively through 16-week timeline

The result will be a production-ready sketching and assembly system that showcases Common Lisp's strengths while maintaining CLAD's core philosophy of simplicity and power.

---

**Ready to begin Phase 9 implementation? Let's start with Test 1!**
