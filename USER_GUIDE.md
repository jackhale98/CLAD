# CLAD User Guide

A comprehensive guide to designing parametric CAD models with CLAD.

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Core Concepts](#core-concepts)
4. [The defpart DSL](#the-defpart-dsl)
5. [Selectors](#selectors)
6. [Primitives and Shapes](#primitives-and-shapes)
7. [Boolean Operations](#boolean-operations)
8. [Transformations](#transformations)
9. [Patterns](#patterns)
10. [Edge Operations](#edge-operations)
11. [Advanced Features](#advanced-features)
12. [2D Sketching](#2d-sketching)
13. [Assemblies](#assemblies)
14. [Viewing and Export](#viewing-and-export)
15. [Best Practices](#best-practices)

---

## Introduction

CLAD is a code-first CAD system that lets you design 3D parts using Common Lisp. Unlike traditional CAD software where you click and drag, in CLAD you write code that describes your design. This approach offers several advantages:

- **Parametric by default**: Change a dimension, instantly update the entire part
- **Version control**: Your designs are text files - use git like any code project
- **Programmatic generation**: Create variations, families of parts, or generative designs
- **Reusable components**: Define custom features and reuse them across projects
- **Precise**: Specify exact dimensions, no manual alignment needed

### Philosophy

CLAD embraces a **declarative** approach using the `defpart` macro. Rather than imperatively describing a sequence of operations, you declare what your part *is*:

```lisp
(clad.dsl:defpart mounting-bracket ((width 100) (thickness 8))
  "A mounting bracket"
  (:body (clad.core:make-box width 50 thickness))
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:make-box 20 20 30))))
```

This reads naturally: "A mounting bracket has a body (a box), and on its top face, we add another box."

---

## Getting Started

### Installation

First, ensure you have the prerequisites installed:

**Ubuntu/Debian:**
```bash
sudo apt-get install libocct-foundation-dev \
                     libocct-modeling-data-dev \
                     libocct-modeling-algorithms-dev \
                     libocct-data-exchange-dev \
                     sbcl
```

**macOS:**
```bash
brew install opencascade sbcl
```

Install Quicklisp if you haven't:
```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
```

Build the C++ wrapper:
```bash
cd clad/c-wrapper
mkdir build && cd build
cmake ..
make
```

### First Steps

Start a Common Lisp REPL and load CLAD:

```lisp
(ql:quickload :clad)
;; or
(asdf:load-system :clad)
```

Create your first part:

```lisp
;; Define a simple box
(clad.dsl:defpart simple-box ((width 50) (height 30) (depth 20))
  "A simple parametric box"
  (:body (clad.core:make-box width height depth)))

;; Create an instance
(defparameter *my-box* (simple-box))

;; View it in the browser
(clad:view *my-box* :name "my-box")
```

This opens a web viewer at `http://localhost:8080` showing your part in 3D.

---

## Core Concepts

### Parts are Functions

When you use `defpart`, you're defining a function that creates a CAD part:

```lisp
(clad.dsl:defpart widget ((size 100))
  (:body (clad.core:make-box size size 20)))

;; This creates a function called 'widget'
(widget)              ; Creates a 100x100x20 box
(widget :size 150)    ; Creates a 150x150x20 box
```

### Parameters and Defaults

Parameters have default values and support keyword arguments:

```lisp
(clad.dsl:defpart plate
    ((width 100)      ; Default: 100
     (height 80)      ; Default: 80
     (thickness 10))  ; Default: 10
  "A parametric plate"
  (:body (clad.core:make-box width height thickness)))

;; All these work:
(plate)                                    ; Uses all defaults
(plate :width 120)                         ; Override one parameter
(plate :width 120 :height 90 :thickness 15) ; Override multiple
```

### Shapes and the CAD Kernel

CLAD uses OpenCASCADE as its geometry kernel. All shapes are:
- **Precise**: Real B-Rep (Boundary Representation) solids, not meshes
- **Manufacturable**: Can be exported to STEP for CNC machining, 3D printing, etc.
- **Queryable**: Get properties like volume, bounding box, faces, edges

---

## The defpart DSL

The `defpart` macro is the main way to define parts in CLAD.

### Basic Structure

```lisp
(clad.dsl:defpart part-name
    ((parameter1 default1)
     (parameter2 default2)
     ...)
  "Documentation string"

  ;; Body: the base shape
  (:body <shape-expression>)

  ;; Operations (optional, can have multiple)
  (:on-face <selectors> <operation>)
  (:on-edge <selectors> <operation>)
  (:circular-pattern <options> <operation>)
  ;; ... more operations
  )
```

### The Body

Every `defpart` must have a `:body` - the base shape:

```lisp
(clad.dsl:defpart cube ((size 50))
  (:body (clad.core:make-box size size size)))
```

The body can be any shape expression:
```lisp
(:body (clad.core:make-cylinder 20 50))
(:body (clad.core:make-sphere 30))
(:body (clad.core:union
         (clad.core:make-box 100 100 10)
         (clad.core:translate (clad.core:make-cylinder 15 20) 50 50 10)))
```

### Operations

After the body, you describe operations on it:

**Face operations** - work on faces:
```lisp
(:on-face :direction :+z :extreme :max
  (:add (clad.core:make-cylinder 10 20))   ; Add material
  (:cut (clad.core:make-cylinder 5 30)))   ; Remove material
```

**Edge operations** - work on edges:
```lisp
(:on-edge :parallel :z
  (:fillet 5.0d0)      ; Round edges
  (:chamfer 2.0d0))    ; Bevel edges
```

**Patterns** - repeat features:
```lisp
(:circular-pattern :count 8 :radius 40
  (:cut (clad.core:make-cylinder 3 15)))
```

---

## Selectors

Selectors are how you choose which faces or edges to operate on. This is one of CLAD's most powerful features.

### Face Selectors

**Direction-based selection:**

Select faces by which direction they point:

```lisp
;; Select top face (pointing up in +Z)
(:on-face :direction :+z :extreme :max
  ...)

;; Select bottom face (pointing down in -Z)
(:on-face :direction :-z :extreme :min
  ...)

;; Select front face (pointing forward in +Y)
(:on-face :direction :+y :extreme :max
  ...)

;; All six directions available: :+x, :-x, :+y, :-y, :+z, :-z
```

The `:extreme` keyword specifies which face to pick when multiple faces point the same direction:
- `:max` - the furthest face in that direction
- `:min` - the nearest face in that direction

**Type-based selection:**

Select by geometry type:

```lisp
;; Select all flat faces
(:on-face :type :planar
  ...)

;; Select all cylindrical faces
(:on-face :type :cylindrical
  ...)

;; Select all spherical faces
(:on-face :type :spherical
  ...)
```

### Edge Selectors

**Parallel selection:**

Select edges parallel to an axis:

```lisp
;; Select all vertical edges (parallel to Z axis)
(:on-edge :parallel :z
  (:fillet 3.0d0))

;; Select edges along X
(:on-edge :parallel :x
  (:chamfer 2.0d0))
```

**Type-based selection:**

```lisp
;; Select straight edges
(:on-edge :type :line
  (:fillet 2.0d0))

;; Select circular edges
(:on-edge :type :circle
  (:chamfer 1.0d0))
```

**Combining selectors:**

You can combine multiple selector criteria (they work as AND):

```lisp
;; Select straight edges that are parallel to X
(:on-edge :type :line :parallel :x
  (:fillet 4.0d0))

;; Select planar faces pointing up
(:on-face :type :planar :direction :+z :extreme :max
  (:add (clad.core:make-cylinder 10 20)))
```

### Practical Selector Examples

```lisp
(clad.dsl:defpart selector-demo ((size 80))
  "Demonstrates various selectors"

  ;; Start with a box
  (:body (clad.core:make-box size size 30))

  ;; Add a cylinder on top
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder 20 15)
            (/ size 2) (/ size 2) 30)))

  ;; Fillet the vertical edges of the base box
  (:on-edge :parallel :z
    (:fillet 4.0d0))

  ;; Chamfer the circular top edge of the cylinder
  (:on-edge :type :circle
    (:chamfer 1.0d0)))
```

---

## Primitives and Shapes

### Box

Creates a rectangular box:

```lisp
(clad.core:make-box width height depth)
```

The box is **centered** on the XY plane and starts at Z=0:
- X range: `[- width/2, +width/2]`
- Y range: `[-height/2, +height/2]`
- Z range: `[0, depth]`

Example:
```lisp
(clad.core:make-box 100 80 20)
;; Creates: 100mm wide, 80mm deep, 20mm tall
;; Centered at origin in XY, bottom at Z=0
```

### Cylinder

Creates a cylinder:

```lisp
(clad.core:make-cylinder radius height)
```

The cylinder is **centered** on the XY plane and extends upward:
- Centered at (0, 0, 0)
- Extends from Z=0 to Z=height

Example:
```lisp
(clad.core:make-cylinder 15 50)
;; Creates: 30mm diameter, 50mm tall cylinder
;; Centered at origin, extends from Z=0 to Z=50
```

### Sphere

Creates a sphere:

```lisp
(clad.core:make-sphere radius)
```

The sphere is **centered** at the origin:

Example:
```lisp
(clad.core:make-sphere 25)
;; Creates: 50mm diameter sphere centered at origin
```

### Cone

Creates a cone or truncated cone:

```lisp
(clad.core:make-cone radius1 radius2 height)
```

- `radius1`: radius at bottom (Z=0)
- `radius2`: radius at top (Z=height)
- Centered on XY plane

Example:
```lisp
(clad.core:make-cone 20 10 50)   ; Tapered cone
(clad.core:make-cone 20 0 50)    ; Cone to point
```

---

## Boolean Operations

Combine shapes using boolean operations.

### Union (Addition)

Combine two or more shapes:

```lisp
(clad.core:union shape1 shape2 shape3 ...)
```

Example - L-bracket:
```lisp
(clad.core:union
  (clad.core:make-box 100 100 10)              ; Base
  (clad.core:translate
    (clad.core:make-box 100 10 50)             ; Vertical
    0 45 10))
```

### Cut (Subtraction)

Remove one shape from another:

```lisp
(clad.core:cut base-shape shape-to-remove ...)
```

Example - box with hole:
```lisp
(clad.core:cut
  (clad.core:make-box 100 100 20)              ; Base box
  (clad.core:translate
    (clad.core:make-cylinder 10 30)            ; Hole
    0 0 -5))                                   ; Centered, through part
```

### Intersection

Keep only the overlapping volume:

```lisp
(clad.core:intersect shape1 shape2 ...)
```

Example - rounded box (box ∩ sphere):
```lisp
(clad.core:intersect
  (clad.core:make-box 80 80 80)
  (clad.core:make-sphere 60))
```

### In defpart

In `defpart`, use `:add` and `:cut` on selected faces:

```lisp
(clad.dsl:defpart plate-with-boss ((size 100))
  (:body (clad.core:make-box size size 10))

  ;; Add a boss on top
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder 20 15)
            (/ size 2) (/ size 2) 10)))

  ;; Cut a hole through the boss
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder 5 30)
            (/ size 2) (/ size 2) 0))))
```

---

## Transformations

Move, rotate, mirror, and scale shapes.

### Translate (Move)

Move a shape in space:

```lisp
(clad.core:translate shape dx dy dz)
```

Example:
```lisp
;; Move a cylinder to position (50, 50, 10)
(clad.core:translate
  (clad.core:make-cylinder 10 20)
  50 50 10)
```

### Rotate

Rotate a shape around an axis:

```lisp
(clad.core:rotate shape :axis axis-keyword :angle degrees)
```

Axis keywords: `:x`, `:y`, `:z`

Example:
```lisp
;; Rotate a box 45° around Z axis
(clad.core:rotate
  (clad.core:make-box 100 50 20)
  :axis :z
  :angle 45)

;; Tilt a cylinder 30° around X axis
(clad.core:rotate
  (clad.core:make-cylinder 10 50)
  :axis :x
  :angle 30)
```

Custom rotation axis:
```lisp
(clad.core:rotate shape
  :axis '(1 1 0)       ; Vector defining axis
  :angle 45
  :origin '(0 0 0))    ; Point on axis
```

### Mirror

Mirror a shape across a plane:

```lisp
(clad.core:mirror shape :plane plane-keyword)
```

Plane keywords: `:xy`, `:xz`, `:yz`

Example:
```lisp
;; Mirror across XY plane (flip in Z)
(clad.core:mirror
  (clad.core:translate (clad.core:make-cylinder 10 50) 20 0 0)
  :plane :xy)
```

### Scale

Scale a shape uniformly or non-uniformly:

```lisp
(clad.core:scale shape factor)              ; Uniform
(clad.core:scale shape factor-x factor-y factor-z)  ; Non-uniform
```

Example:
```lisp
;; Make a shape twice as big
(clad.core:scale (clad.core:make-box 50 50 50) 2)

;; Stretch in Z only
(clad.core:scale (clad.core:make-cylinder 20 30) 1 1 2)
```

---

## Patterns

Patterns let you repeat features efficiently.

### Circular Pattern

Arrange features in a circle:

```lisp
(:circular-pattern
    :count <number>           ; Number of instances
    :radius <distance>        ; Radius of circle
    :center-x <x>             ; Center X coordinate
    :center-y <y>             ; Center Y coordinate
    :angle-start <degrees>    ; Optional: start angle (default: 0)
    :angle-end <degrees>      ; Optional: end angle (default: 360)
  <operation>)
```

Example - bolt circle:
```lisp
(clad.dsl:defpart bolt-circle-plate ((diameter 120) (hole-count 8))
  "Plate with bolt circle"
  (:body (clad.core:make-box diameter diameter 10))

  ;; Create 8 holes in a circle
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count hole-count
        :radius (* diameter 0.35)
        :center-x (/ diameter 2)
        :center-y (/ diameter 2)
      (:cut (clad.core:make-cylinder 4 20)))))
```

Partial circle:
```lisp
;; Holes only on the top half (semicircle)
(:circular-pattern
    :count 5
    :radius 40
    :center-x 50
    :center-y 50
    :angle-start 0
    :angle-end 180
  (:cut (clad.core:make-cylinder 3 15)))
```

### Linear Pattern

Repeat features in a line:

```lisp
(:linear-pattern
    :count-x <number>         ; Number in X direction
    :count-y <number>         ; Number in Y direction (optional)
    :spacing-x <distance>     ; Spacing in X
    :spacing-y <distance>     ; Spacing in Y (if count-y used)
    :offset-x <distance>      ; Optional: start offset X
    :offset-y <distance>      ; Optional: start offset Y
  <operation>)
```

Example - row of holes:
```lisp
(clad.dsl:defpart perforated-bar ((length 200))
  "Bar with row of holes"
  (:body (clad.core:make-box length 40 10))

  (:on-face :direction :+z :extreme :max
    (:linear-pattern
        :count-x 8
        :spacing-x 20
        :offset-x 20          ; Start 20mm from edge
      (:cut (clad.core:make-cylinder 4 15)))))
```

2D linear pattern:
```lisp
;; Grid of 3x4 holes
(:linear-pattern
    :count-x 3
    :count-y 4
    :spacing-x 30
    :spacing-y 25
    :offset-x 20
    :offset-y 20
  (:cut (clad.core:make-cylinder 3 15)))
```

### Grid Pattern

Rectangular grid of features:

```lisp
(:grid-pattern
    :count-x <number>
    :count-y <number>
    :spacing-x <distance>
    :spacing-y <distance>
    :offset-x <distance>      ; Optional
    :offset-y <distance>      ; Optional
  <operation>)
```

Example - ventilation grid:
```lisp
(clad.dsl:defpart vent-cover ((width 150) (height 100))
  "Cover with ventilation grid"
  (:body (clad.core:make-box width height 3))

  (:on-face :direction :+z :extreme :max
    (:grid-pattern
        :count-x 12
        :count-y 8
        :spacing-x 12
        :spacing-y 12
        :offset-x 10
        :offset-y 10
      (:cut (clad.core:make-cylinder 2 5)))))
```

---

## Edge Operations

Round or bevel edges for aesthetics and to reduce stress concentrations.

### Fillet (Round)

Create rounded edges:

```lisp
(:on-edge <selectors>
  (:fillet radius))
```

The radius is a double-float (note the `d0` suffix):

Example:
```lisp
(clad.dsl:defpart rounded-box ((size 80) (fillet-radius 5.0d0))
  (:body (clad.core:make-box size size 30))

  ;; Fillet all vertical edges
  (:on-edge :parallel :z
    (:fillet fillet-radius)))
```

Multiple fillets:
```lisp
(clad.dsl:defpart multi-fillet ((size 100))
  (:body (clad.core:make-box size size 40))

  ;; Large fillet on vertical edges
  (:on-edge :parallel :z
    (:fillet 8.0d0))

  ;; Small fillet on horizontal edges
  (:on-edge :parallel :x
    (:fillet 2.0d0))

  (:on-edge :parallel :y
    (:fillet 2.0d0)))
```

### Chamfer (Bevel)

Create beveled edges:

```lisp
(:on-edge <selectors>
  (:chamfer distance))
```

Example:
```lisp
(clad.dsl:defpart chamfered-part ((size 60))
  (:body (clad.core:make-box size size size))

  ;; Chamfer all vertical edges
  (:on-edge :parallel :z
    (:chamfer 3.0d0)))
```

### Selective Edge Operations

Use selectors to apply operations to specific edges:

```lisp
(clad.dsl:defpart selective-finishing ((size 100))
  (:body (clad.core:make-box size size 20))

  ;; Add a cylindrical boss
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder 25 15)
            (/ size 2) (/ size 2) 20)))

  ;; Fillet only the straight edges
  (:on-edge :type :line
    (:fillet 3.0d0))

  ;; Chamfer only the circular edges
  (:on-edge :type :circle
    (:chamfer 1.5d0)))
```

---

## Advanced Features

CLAD supports advanced CAD operations like lofts, sweeps, and pipes.

### Loft

Create a smooth transition between 2D profiles:

```lisp
(:on-face <selectors>
  (:loft :sections (list wire1 wire2 wire3 ...)
         :solid t              ; Create solid (true) or shell (false)
         :ruled nil))          ; Straight (true) or smooth (false) interpolation
```

You need to create wire profiles using curve functions:

```lisp
(clad.core:make-circle-wire '(x y z) radius)
(clad.core:make-rectangle-wire '(x y z) width height)
```

Example - vase shape:
```lisp
(clad.dsl:defpart vase ((height 100) (base-radius 30) (top-radius 20))
  "A vase created by lofting"
  (:body (clad.core:make-box 1 1 1))  ; Dummy body

  (:on-face :direction :+z :extreme :max
    (:loft :sections (list
                      (clad.core:make-circle-wire '(0 0 0) base-radius)
                      (clad.core:make-circle-wire '(0 0 (* height 0.33)) 35)
                      (clad.core:make-circle-wire '(0 0 (* height 0.66)) 25)
                      (clad.core:make-circle-wire `(0 0 ,height) top-radius))
           :solid t
           :ruled nil)))
```

### Sweep

Sweep a 2D profile along a 3D path:

```lisp
(:on-face <selectors>
  (:sweep :profile <wire>
          :path <wire>))
```

Example - tube along curved path:
```lisp
(clad.dsl:defpart curved-tube ((radius 5))
  "Tube swept along spline"
  (:body (clad.core:make-box 1 1 1))

  (:on-face :direction :+z :extreme :max
    (:sweep :profile (clad.core:make-circle-wire '(0 0 0) radius
                                                  :axis '(1 0 0))
            :path (clad.core:make-spline
                    '((0 0 0) (20 10 15) (40 5 30) (60 0 40))
                    :closed nil))))
```

### Pipe

Create a tube along a path (simplified sweep):

```lisp
(:on-face <selectors>
  (:pipe :path <wire>
         :radius <number>))
```

Example - handle:
```lisp
(clad.dsl:defpart handle ()
  "Handle created with pipe"
  (:body (clad.core:make-box 80 30 10))

  (:on-face :direction :+z :extreme :max
    (:pipe :path (clad.core:make-spline
                   '((10 15 10) (20 25 25) (40 25 25) (60 25 25) (70 15 10))
                   :closed nil)
           :radius 6)))
```

### Splines and Curves

Create smooth curves through points:

```lisp
(clad.core:make-spline points &key closed)
```

- `points`: List of `'(x y z)` coordinates
- `:closed`: Whether to close the curve (default: `nil`)

Example:
```lisp
;; Open spline
(clad.core:make-spline '((0 0 0) (10 20 5) (30 15 10) (50 0 15))
                       :closed nil)

;; Closed spline (connects back to start)
(clad.core:make-spline '((0 0 10) (20 20 10) (20 -20 10) (-20 -20 10))
                       :closed t)
```

---

## 2D Sketching

CLAD includes a parametric 2D sketching system with constraint solving (similar to commercial CAD).

### Creating Sketches

```lisp
(clad.sketch:make-sketch &key name)
```

### Sketch Entities

**Points:**
```lisp
(clad.sketch:make-point-2d x y &key name fixed)
```

**Lines:**
```lisp
(clad.sketch:make-line-2d start-point end-point &key name)
```

**Arcs:**
```lisp
(clad.sketch:make-arc-2d center-point start-point end-point &key name)
```

**Circles:**
```lisp
(clad.sketch:make-circle-2d center-point radius &key name)
```

### Constraints

Add geometric constraints to control the sketch:

**Distance constraint:**
```lisp
(clad.sketch.constraints:make-distance-constraint entity1 entity2 distance)
```

**Horizontal/Vertical:**
```lisp
(clad.sketch.constraints:make-horizontal-constraint line)
(clad.sketch.constraints:make-vertical-constraint line)
```

**Coincident:**
```lisp
(clad.sketch.constraints:make-coincident-constraint point1 point2)
```

**Parallel/Perpendicular:**
```lisp
(clad.sketch.constraints:make-parallel-constraint line1 line2)
(clad.sketch.constraints:make-perpendicular-constraint line1 line2)
```

### Example - Constrained Rectangle

```lisp
;; Create a sketch
(defparameter *sketch* (clad.sketch:make-sketch :name "Rectangle"))

;; Create points
(defparameter *p1* (clad.sketch:make-point-2d 0.0d0 0.0d0 :name "P1" :fixed t))
(defparameter *p2* (clad.sketch:make-point-2d 100.0d0 0.0d0 :name "P2"))
(defparameter *p3* (clad.sketch:make-point-2d 100.0d0 50.0d0 :name "P3"))
(defparameter *p4* (clad.sketch:make-point-2d 0.0d0 50.0d0 :name "P4"))

;; Create lines
(defparameter *l1* (clad.sketch:make-line-2d *p1* *p2* :name "Bottom"))
(defparameter *l2* (clad.sketch:make-line-2d *p2* *p3* :name "Right"))
(defparameter *l3* (clad.sketch:make-line-2d *p3* *p4* :name "Top"))
(defparameter *l4* (clad.sketch:make-line-2d *p4* *p1* :name "Left"))

;; Add to sketch
(clad.sketch:add-entity *sketch* *p1*)
(clad.sketch:add-entity *sketch* *p2*)
(clad.sketch:add-entity *sketch* *p3*)
(clad.sketch:add-entity *sketch* *p4*)
(clad.sketch:add-entity *sketch* *l1*)
(clad.sketch:add-entity *sketch* *l2*)
(clad.sketch:add-entity *sketch* *l3*)
(clad.sketch:add-entity *sketch* *l4*)

;; Add constraints
(clad.sketch:add-constraint *sketch*
  (clad.sketch.constraints:make-horizontal-constraint *l1*))
(clad.sketch:add-constraint *sketch*
  (clad.sketch.constraints:make-horizontal-constraint *l3*))
(clad.sketch:add-constraint *sketch*
  (clad.sketch.constraints:make-vertical-constraint *l2*))
(clad.sketch:add-constraint *sketch*
  (clad.sketch.constraints:make-vertical-constraint *l4*))
(clad.sketch:add-constraint *sketch*
  (clad.sketch.constraints:make-distance-constraint *p1* *p2* 100.0d0))
(clad.sketch:add-constraint *sketch*
  (clad.sketch.constraints:make-distance-constraint *p2* *p3* 50.0d0))

;; Solve constraints
(clad.sketch:solve-sketch *sketch*)

;; Convert to 3D wire
(defparameter *wire* (clad.sketch.conversion:sketch-to-wire *sketch*))

;; Extrude to 3D
(defparameter *part* (clad.core:extrude *wire* 20))
```

---

## Assemblies

Build assemblies of multiple parts with mating constraints. CLAD provides both a declarative DSL (`defassembly`) and a lower-level API.

### The defassembly DSL (Recommended)

Define parametric assemblies using `defassembly`:

```lisp
(clad.assembly.dsl:defassembly assembly-name
    ((parameter1 default1)
     (parameter2 default2)
     ...)
  "Documentation string"

  ;; Define components
  (:component :name part-expression
              :quantity number        ; optional, default 1
              :fixed boolean         ; optional, default nil
              :metadata plist)       ; optional

  ;; Define mates between components
  (:mate type
         component1 reference1
         component2 reference2
         options...)

  ;; Set assembly parameters
  (:parameter name value))
```

### Example Assembly

```lisp
;; First define your parts
(clad.dsl:defpart base-plate ((size 150))
  "Base mounting plate"
  (:body (clad.core:make-box size size 10))
  (:on-face :direction :+z :extreme :max
    (:circular-pattern
        :count 4
        :radius (* size 0.35)
        :center-x (/ size 2)
        :center-y (/ size 2)
      (:cut (clad.core:make-cylinder 3 15)))))

(clad.dsl:defpart mounting-bracket ((width 50) (height 60))
  "L-bracket for mounting"
  (:body (clad.core:make-box width 10 height)))

(clad.dsl:defpart bolt ((diameter 6) (length 20))
  "Standard bolt"
  (:body (clad.core:make-cylinder (/ diameter 2) length)))

;; Now define the assembly
(clad.assembly.dsl:defassembly bracket-assembly
    ((base-size 150)
     (bracket-width 50)
     (bracket-height 60)
     (bolt-count 4))
  "Base plate with two mounting brackets"

  ;; Base - fixed in place
  (:component :base (base-plate :size base-size)
              :fixed t
              :metadata '(:part-number "BASE-001"
                         :material "Aluminum"))

  ;; Two brackets
  (:component :bracket-left (mounting-bracket :width bracket-width
                                             :height bracket-height)
              :metadata '(:part-number "BRKT-001"
                         :material "Aluminum"))

  (:component :bracket-right (mounting-bracket :width bracket-width
                                              :height bracket-height)
              :metadata '(:part-number "BRKT-001"
                         :material "Aluminum"))

  ;; Bolts (4 total)
  (:component :bolt (bolt :diameter 6 :length 20)
              :quantity bolt-count
              :metadata '(:part-number "M6-20"
                         :material "Steel"))

  ;; Mate constraints
  (:mate :coincident
         :base :face-top
         :bracket-left :face-bottom)

  (:mate :distance
         :base :edge-left
         :bracket-left :edge-center
         :offset 20.0)

  (:mate :coincident
         :base :face-top
         :bracket-right :face-bottom)

  (:mate :distance
         :base :edge-right
         :bracket-right :edge-center
         :offset 20.0))

;; Create the assembly
(defparameter *my-assembly* (bracket-assembly))

;; View it
(clad:view *my-assembly* :name "bracket-assembly")

;; Generate Bill of Materials
(clad.assembly:generate-bom *my-assembly*)
```

### Mate Types

**Coincident** - Align two faces:
```lisp
(:mate :coincident
       component1 face-reference1
       component2 face-reference2)
```

**Concentric** - Align two cylindrical/circular features:
```lisp
(:mate :concentric
       component1 axis-reference1
       component2 axis-reference2)
```

**Distance** - Offset two faces by a distance:
```lisp
(:mate :distance
       component1 face-reference1
       component2 face-reference2
       :offset distance-value)
```

**Parallel** - Make two faces parallel:
```lisp
(:mate :parallel
       component1 face-reference1
       component2 face-reference2)
```

### Component Options

**:quantity** - Create multiple instances:
```lisp
(:component :bolt (bolt :diameter 6 :length 20)
            :quantity 8)  ; Creates 8 bolts
```

**:fixed** - Fix component in space (typically the base):
```lisp
(:component :base (base-plate)
            :fixed t)  ; This part won't move
```

**:metadata** - Attach manufacturing/BOM data:
```lisp
(:component :housing (motor-housing)
            :metadata '(:part-number "HOUSING-001"
                       :material "ABS Plastic"
                       :supplier "Acme Parts Co"
                       :cost 12.50))
```

### Lower-Level Assembly API

For more control, use the imperative API:

```lisp
;; Create assembly
(defparameter *asm* (clad.assembly:make-assembly :name "My Assembly"))

;; Add components
(clad.assembly:add-component *asm* :base (base-plate))
(clad.assembly:add-component *asm* :bracket (bracket) :quantity 2)

;; Add mates
(clad.assembly:add-mate *asm* :coincident
                        :base :top-face
                        :bracket :bottom-face)

;; Solve constraints
(clad.assembly:solve-assembly *asm*)

;; Generate BOM
(clad.assembly:generate-bom *asm*)
```

---

## Viewing and Export

### Web Viewer

View parts in your browser:

```lisp
(clad:view part &key name port)
```

- `name`: Name for the model (used in URL and filename)
- `port`: Web server port (default: 8080)

Example:
```lisp
(defparameter *my-part* (mounting-plate :width 120))
(clad:view *my-part* :name "my-mounting-plate")
;; Opens browser at: http://localhost:8080/?model=/models/my-mounting-plate.glb
```

The viewer:
- Shows 3D model with rotation, zoom, pan
- Auto-updates when you regenerate the part (re-evaluate the defpart)
- Uses WebGL (works in all modern browsers)

### STEP Export

Export to STEP format for CAM, other CAD software, or manufacturing:

```lisp
(clad.export:export-step part filepath)
```

Example:
```lisp
(defparameter *final-design* (complex-part :size 200))
(clad.export:export-step *final-design* "output/final-design.step")
```

STEP files can be opened in:
- FreeCAD
- SolidWorks
- Fusion 360
- OnShape
- CAM software for CNC machining

---

## Best Practices

### 1. Use Meaningful Parameter Names

**Good:**
```lisp
(clad.dsl:defpart mounting-bracket
    ((base-width 100)
     (base-thickness 8)
     (bracket-height 60)
     (hole-diameter 6))
  ...)
```

**Bad:**
```lisp
(clad.dsl:defpart part1
    ((w 100) (t 8) (h 60) (d 6))
  ...)
```

### 2. Add Documentation

```lisp
(clad.dsl:defpart servo-mount
    ((servo-width 40)
     (servo-length 20)
     (mount-thickness 5))
  "Mount for 9g servo motor. Fits standard 40x20mm servos."
  ...)
```

### 3. Use Centered Primitives

Primitives are centered - take advantage of this:

```lisp
;; Good - simple, centered
(:body (clad.core:make-box width height thickness))

;; Don't do this - unnecessarily complex
(:body (clad.core:translate
         (clad.core:make-box width height thickness)
         (/ width 2) (/ height 2) 0))
```

### 4. Group Related Parameters

```lisp
(clad.dsl:defpart electronics-box
    ;; Overall dimensions
    ((width 150)
     (height 100)
     (depth 50)
     ;; Wall thickness
     (wall-thickness 3)
     ;; Mounting holes
     (mount-hole-diameter 4)
     (mount-hole-inset 10))
  ...)
```

### 5. Use Variables for Derived Dimensions

```lisp
(clad.dsl:defpart smart-bracket
    ((base-size 100)
     (thickness 10))
  "Bracket with derived dimensions"

  ;; Base
  (:body (clad.core:make-box base-size base-size thickness))

  ;; Boss - automatically centered and sized proportionally
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder (* base-size 0.2)  ; 20% of base
                                     (* thickness 1.5))  ; 1.5x thickness
            (/ base-size 2) (/ base-size 2) thickness))))
```

### 6. Test with Different Parameters

```lisp
;; Define part
(clad.dsl:defpart widget ((size 50)) ...)

;; Test various sizes
(clad:view (widget :size 30) :name "small")
(clad:view (widget :size 100) :name "large")
(clad:view (widget :size 200) :name "xlarge")
```

### 7. Use Selectors, Don't Hardcode

**Good - robust to changes:**
```lisp
(:on-face :direction :+z :extreme :max
  (:add (clad.core:make-cylinder 10 20)))
```

**Bad - breaks if base geometry changes:**
```lisp
;; Hardcoded position - fragile!
(:body (clad.core:union
         (clad.core:make-box 100 100 10)
         (clad.core:translate (clad.core:make-cylinder 10 20) 50 50 10)))
```

### 8. Organize Complex Parts

For complex parts, consider breaking into separate defparts:

```lisp
(clad.dsl:defpart base-plate ((size 200))
  (:body (clad.core:make-box size size 10)))

(clad.dsl:defpart mounting-boss ((diameter 40) (height 30))
  (:body (clad.core:make-cylinder (/ diameter 2) height)))

(clad.dsl:defpart assembly ((size 200) (boss-size 40))
  "Complete assembly"
  (:body (base-plate :size size))
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (mounting-boss :diameter boss-size :height 30)
            (/ size 2) (/ size 2) 10))))
```

### 9. Comment Non-Obvious Operations

```lisp
(clad.dsl:defpart complex-part ((size 100))
  (:body (clad.core:make-box size size 20))

  ;; Need extra thickness here to accommodate M6 nut (8mm + 2mm clearance)
  (:on-face :direction :-z :extreme :min
    (:add (clad.core:translate
            (clad.core:make-cylinder 10 10)
            25 25 -10))))
```

### 10. Use Auto-Rebuild for Iterative Design

```lisp
;; In your design file: my-part.lisp
(clad.dsl:defpart my-part ((size 50))
  (:body (clad.core:make-box size size 20)))

(clad:view (my-part) :name "my-part")
(clad.auto-rebuild:start-watching "my-part.lisp")

;; Now edit my-part.lisp and save - it rebuilds automatically!
```

---

## Troubleshooting

### Common Issues

**"Library not found" error:**
- Make sure OpenCASCADE is installed
- Build the C wrapper: `cd c-wrapper/build && cmake .. && make`

**Part doesn't appear in viewer:**
- Check that the viewer URL is correct (http://localhost:8080)
- Make sure the part name in `:name` doesn't have special characters
- Try refreshing the browser

**"Shape is invalid" error:**
- Check fillet/chamfer radii aren't too large for the geometry
- Ensure boolean operations have overlapping volumes
- Try simpler geometry first to isolate the issue

**Fillets/Chamfers fail:**
- Radius too large for the edge - reduce it
- Multiple fillets competing - apply them in separate `:on-edge` blocks
- Edge selection is empty - check your selectors

**Parameters don't update:**
- Make sure you're re-evaluating the `defpart` definition (C-c C-c in Emacs)
- Create a new instance: `(setf *my-part* (my-part :size 100))`
- Refresh the viewer

---

## Examples

See the `examples/` directory for comprehensive tutorials:

- `01-basic-dsl.lisp` - Basics of defpart
- `02-patterns.lisp` - Pattern operations
- `03-fillets-chamfers.lisp` - Edge finishing
- `04-advanced-features.lisp` - Loft, sweep, pipe
- `05-assemblies.lisp` - Building assemblies
- `06-advanced-selectors.lisp` - Selector techniques
- `06-sketches.lisp` - 2D parametric sketching

Load any example:
```lisp
(load "examples/01-basic-dsl.lisp")
```

---

## Next Steps

- Explore the examples
- Build your own parametric parts
- Share designs with the community
- Check out the [SELECTOR_REFERENCE.md](SELECTOR_REFERENCE.md) for advanced selection techniques

Happy designing!
