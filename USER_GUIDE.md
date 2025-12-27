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
12. [Tolerancing and GD&T](#tolerancing-and-gdt)
13. [2D Sketching](#2d-sketching)
14. [Assemblies](#assemblies)
15. [Viewing and Export](#viewing-and-export)
16. [Best Practices](#best-practices)

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

**Boolean combinators:**

Combine selectors using logical operators:

```lisp
;; AND - match ALL criteria
(:on-face :and :type :plane :direction :+z
  (:add (clad.core:make-cylinder 10 20)))

;; OR - match ANY criteria
(:on-edge :or :parallel :x :parallel :z
  (:fillet 4.0d0))

;; NOT - exclude matches
(:on-face :not :type :cylinder
  (:chamfer 2.0d0))

;; Nested combinators
(:on-edge :and :type :line
               :not :parallel :z
  (:fillet 3.0d0))
```

**Position-based selectors:**

Select by coordinate position:

```lisp
;; Select faces at specific Z height
(:on-face :at-z 50.0 :tolerance 0.1
  (:cut (clad.core:make-cylinder 5 10)))

;; Select faces within Z range
(:on-face :between-z 10.0 40.0
  (:add ...))

;; Select faces within bounding box
(:on-face :within-box '(-10 -10 0) '(10 10 50)
  (:chamfer 1.0))

;; Select faces near a point
(:on-face :near-point '(50 50 20) :radius 25.0
  (:fillet 5.0))
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

## Face-Plane Operations

CLAD provides a lightweight workplane system for creating features directly on faces with automatic centering and alignment.

### The :on-face-plane Context

Establish a local 2D coordinate system on a selected face:

```lisp
(:on-face-plane <selector-spec>
  <2D operations>)
```

The face's center becomes the origin (0, 0), and the face normal becomes the Z-axis.

### Basic 2D Operations

**Cut circular holes:**
```lisp
(:on-face-plane :direction :+z :extreme :max
  (:cut-circle radius :depth depth))
```

**Add circular bosses:**
```lisp
(:on-face-plane :direction :+z :extreme :max
  (:add-circle radius :height height))
```

**Cut rectangular pockets:**
```lisp
(:on-face-plane :direction :+z :extreme :max
  (:cut-rectangle width height :depth depth))
```

**Add rectangular bosses:**
```lisp
(:on-face-plane :direction :+z :extreme :max
  (:add-rectangle width height :height height))
```

### Patterns on Faces

Patterns work seamlessly in face-plane context:

**Grid pattern of holes:**
```lisp
(clad.dsl:defpart perforated-plate ((size 150))
  "Plate with grid of holes"
  (:body (clad.core:make-box size size 10))

  (:on-face-plane :direction :+z :extreme :max
    (:grid-pattern :x-count 10 :y-count 10
                   :x-spacing 12 :y-spacing 12
      (:cut-circle 2 :depth 8))))
```

**Circular bolt pattern:**
```lisp
(clad.dsl:defpart flange ((diameter 120))
  "Flange with bolt circle"
  (:body (clad.core:make-cylinder (/ diameter 2) 15))

  (:on-face-plane :direction :+z :extreme :max
    (:circular-pattern :count 6 :radius (* diameter 0.35)
      (:cut-circle 4 :depth 20))))
```

### Example: Servo Mounting Bracket

```lisp
(clad.dsl:defpart servo-mount
    ((base-width 80)
     (base-depth 60)
     (base-thickness 6)
     (riser-height 25)
     (servo-spacing 48))
  "Professional servo motor mounting bracket"

  ;; Base plate
  (:body (clad.core:make-box base-width base-depth base-thickness))

  ;; Raised platform
  (:body (clad.core:translate
          (clad.core:make-box 55 45 riser-height)
          0 0 base-thickness))

  ;; Cable management slot on back face
  (:on-face-plane :direction :-y :extreme :min
    (:cut-rectangle 8 4 :depth 3))

  ;; Servo mounting holes on top
  (:on-face-plane :direction :+z :extreme :max
    (:grid-pattern :x-count 2 :y-count 2
                   :x-spacing servo-spacing :y-spacing 10
      (:cut-circle 1.5 :depth 33))))  ; M3 holes
```

### Advantages of Face-Plane Operations

1. **Automatic centering** - No manual position calculations
2. **Face-relative coordinates** - Features move with the face
3. **Clean syntax** - More readable than manual translation
4. **Pattern-friendly** - Grid and circular patterns "just work"

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

## Tolerancing and GD&T

CLAD includes comprehensive support for Geometric Dimensioning and Tolerancing (GD&T) per ASME Y14.5, enabling production-ready CAD models with manufacturing specifications.

### Overview

GD&T support in CLAD includes:
- **Datum Features** - Define measurement reference frames (A, B, C)
- **Form Tolerances** - Control shape (flatness, straightness, circularity, cylindricity)
- **Orientation Tolerances** - Control orientation (perpendicularity, parallelism, angularity)
- **Location Tolerances** - Control position (position, concentricity, symmetry)
- **Profile Tolerances** - Control surface and line profiles
- **Runout Tolerances** - Control surface variation during rotation
- **STEP Export** - Export parts with Product Manufacturing Information (PMI)

### Defining Datums

Datums establish a coordinate reference system for measurement. Define datums on faces using the `:datum` form:

```lisp
(clad.dsl:defpart machined-block ((size 100))
  "Block with datum reference frame"
  (:body (clad.core:make-box size size (* size 0.3)))

  ;; Primary datum - largest flattest surface (bottom face)
  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Secondary datum - perpendicular to primary
  (:datum "B" :on-face :direction :+x :extreme :max)

  ;; Tertiary datum - completes the 3-2-1 datum scheme
  (:datum "C" :on-face :direction :+y :extreme :max))
```

**Datum with material condition modifier:**
```lisp
;; Datum at Maximum Material Condition (MMC)
(:datum "D" :on-face :direction :+z :extreme :max :mmc t)
```

### Form Tolerances

Form tolerances control the shape of features without reference to datums.

**Flatness** - Surface must lie within two parallel planes:
```lisp
(clad.dsl:defpart flat-plate ((width 150))
  (:body (clad.core:make-box width width 10))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Top surface flatness within 0.05mm
  (:flatness :on-face :direction :+z :extreme :max
             :tolerance 0.05))
```

**Straightness** - Line elements must be straight within tolerance zone:
```lisp
(:straightness :on-edge :parallel :z
               :tolerance 0.02)
```

**Circularity** - Circular cross-sections must be within tolerance:
```lisp
(:circularity :on-face :type :cylindrical
              :tolerance 0.01)
```

**Cylindricity** - Cylindrical surface must be within coaxial tolerance zone:
```lisp
(clad.dsl:defpart precision-shaft ((diameter 25) (length 100))
  (:body (clad.core:make-cylinder (/ diameter 2) length))

  ;; Entire cylindrical surface within 0.02mm
  (:cylindricity :on-face :type :cylindrical
                 :tolerance 0.02))
```

### Orientation Tolerances

Orientation tolerances control the relationship between features and datums.

**Perpendicularity** - Feature must be perpendicular to datum:
```lisp
(clad.dsl:defpart mounting-block ((size 80))
  (:body (clad.core:make-box size size 30))

  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Side face perpendicular to datum A within 0.1mm
  (:perpendicularity :on-face :direction :+x :extreme :max
                     :tolerance 0.1
                     :datum-ref "A"))
```

**Parallelism** - Feature must be parallel to datum:
```lisp
(:parallelism :on-face :direction :+z :extreme :max
              :tolerance 0.05
              :datum-ref "A")
```

**Angularity** - Feature must be at specified angle to datum:
```lisp
(:angularity :on-face :direction :+x :extreme :max
             :tolerance 0.08
             :datum-ref "A"
             :angle 45.0)  ; 45 degrees
```

### Location Tolerances

Location tolerances control the position of features relative to datums.

**Position** - Feature location must be within tolerance zone from true position:
```lisp
(clad.dsl:defpart bolt-hole-plate ((size 100))
  (:body (clad.core:make-box size size 10))

  (:datum "A" :on-face :direction :-z :extreme :min)
  (:datum "B" :on-face :direction :+x :extreme :max)
  (:datum "C" :on-face :direction :+y :extreme :max)

  ;; Centered hole
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder 5 15)
            (/ size 2) (/ size 2) 0)))

  ;; Position tolerance: hole center within ⌀0.2mm at true position
  (:position :on-face :type :cylindrical
             :tolerance 0.2
             :datum-refs ("A" "B" "C")))
```

**Position with MMC (Maximum Material Condition):**
```lisp
(:position :on-face :type :cylindrical
           :tolerance 0.5
           :datum-refs ("A" "B" "C")
           :mmc t)  ; Allows bonus tolerance
```

**Concentricity** - Feature axis must align with datum axis:
```lisp
(:concentricity :on-face :type :cylindrical
                :tolerance 0.05
                :datum-ref "A")
```

**Symmetry** - Feature must be symmetrical about datum plane:
```lisp
(:symmetry :on-face :direction :+x :extreme :max
           :tolerance 0.1
           :datum-ref "A")
```

### Profile Tolerances

Profile tolerances define a 3D tolerance zone around a nominal surface or 2D zone for line profiles.

**Profile of a Surface** - 3D tolerance zone around nominal surface:
```lisp
(clad.dsl:defpart contoured-part ((width 100))
  (:body (clad.core:make-box width width 25))

  (:datum "A" :on-face :direction :-z :extreme :min)
  (:datum "B" :on-face :direction :+x :extreme :max)

  ;; Contoured top surface profile
  (:profile-surface :on-face :direction :+z :extreme :max
                    :tolerance 0.1
                    :datum-refs ("A" "B")
                    :bilateral t))  ; Equal +/- from nominal
```

**Unilateral profile tolerance:**
```lisp
;; Tolerance only outside nominal surface
(:profile-surface :on-face :direction :+z :extreme :max
                  :tolerance 0.15
                  :datum-refs ("A" "B")
                  :bilateral nil)
```

**Profile of a Line** - 2D tolerance zone in cutting plane:
```lisp
(:profile-line :on-face :direction :+y :extreme :max
               :tolerance 0.08
               :datum-refs ("A")
               :bilateral t)
```

### Runout Tolerances

Runout tolerances control surface variation when a part rotates about a datum axis.

**Circular Runout** - Full Indicator Movement (FIM) at individual circular elements:
```lisp
(clad.dsl:defpart rotating-shaft ((diameter 30) (length 120))
  (:body (clad.core:make-cylinder (/ diameter 2) length))

  ;; Datum axis from one end face
  (:datum "A" :on-face :direction :-z :extreme :min)

  ;; Circular runout of cylindrical surface
  (:circular-runout :on-face :type :cylindrical
                    :tolerance 0.05
                    :datum-ref "A"))
```

**Total Runout** - Composite surface variation (combines circularity, straightness, coaxiality):
```lisp
(clad.dsl:defpart precision-spindle ((diameter 25) (length 100))
  (:body (clad.core:make-cylinder (/ diameter 2) length))

  (:datum "A" :on-face :direction :-z :extreme :min)
  (:datum "B" :on-face :direction :+z :extreme :max)

  ;; Total runout relative to datum axis A-B
  (:total-runout :on-face :type :cylindrical
                 :tolerance 0.02
                 :datum-ref "A"))
```

### Complete Example: Production Part

Here's a complete example demonstrating datums and multiple GD&T callouts:

```lisp
(clad.dsl:defpart production-mount
    ((base-width 150)
     (base-height 100)
     (base-thickness 12)
     (boss-diameter 40)
     (boss-height 25)
     (hole-diameter 8))
  "Production mounting bracket with full GD&T specification"

  ;; Base plate
  (:body (clad.core:make-box base-width base-height base-thickness))

  ;; Central mounting boss
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder (/ boss-diameter 2) boss-height)
            (/ base-width 2) (/ base-height 2) base-thickness)))

  ;; Through hole in boss
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) 50)
            (/ base-width 2) (/ base-height 2) 0)))

  ;; Datum Reference Frame (A-B-C)
  (:datum "A" :on-face :direction :-z :extreme :min)  ; Primary: bottom face
  (:datum "B" :on-face :direction :+x :extreme :max)  ; Secondary: right face
  (:datum "C" :on-face :direction :+y :extreme :max)  ; Tertiary: back face

  ;; Form tolerance: Bottom face flatness
  (:flatness :on-face :direction :-z :extreme :min
             :tolerance 0.05)

  ;; Orientation: Top face perpendicular to datum A
  (:perpendicularity :on-face :direction :+z :extreme :max
                     :tolerance 0.1
                     :datum-ref "A")

  ;; Orientation: Side face parallel to datum B
  (:parallelism :on-face :direction :-x :extreme :min
                :tolerance 0.08
                :datum-ref "B")

  ;; Location: Boss position relative to datum frame
  (:position :on-face :type :cylindrical
             :tolerance 0.2
             :datum-refs ("A" "B" "C")
             :mmc t)

  ;; Profile: Boss top surface profile
  (:profile-surface :on-face :direction :+z :extreme :max
                    :tolerance 0.15
                    :datum-refs ("A")
                    :bilateral t)

  ;; Runout: Boss cylindrical surface
  (:circular-runout :on-face :type :cylindrical
                    :tolerance 0.1
                    :datum-ref "A"))
```

### Exporting with GD&T

When you export to STEP format, all datum and tolerance information is included as Product Manufacturing Information (PMI):

```lisp
(defparameter *production-part* (production-mount))
(clad.export:export-step *production-part* "output/production-mount.step")
```

The exported STEP file includes:
- Geometry (solid model)
- Datum feature definitions
- Geometric tolerance callouts
- Material condition modifiers
- Datum reference frames

These can be viewed in CAD software that supports STEP AP242 with PMI (FreeCAD, SolidWorks, etc.).

### Best Practices for GD&T

**1. Establish a proper datum reference frame:**
- Primary datum (A): Typically the largest, most stable surface
- Secondary datum (B): Perpendicular to primary, second most important
- Tertiary datum (C): Completes the 3-2-1 datum scheme

**2. Use form tolerances before orientation/location:**
- Control form first (flatness, straightness)
- Then control orientation (perpendicularity, parallelism)
- Finally control location (position)

**3. Choose appropriate tolerance values:**
- Consider manufacturing capabilities
- Tighter tolerances = higher cost
- Use tolerance analysis for critical dimensions

**4. Leverage material condition modifiers (MMC/LMC):**
- MMC allows bonus tolerance as features depart from maximum material
- Useful for hole patterns with fasteners
- Can reduce manufacturing cost

**5. Document intent:**
- Add comments explaining why specific tolerances are needed
- Reference relevant standards (ASME Y14.5, ISO 1101)

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
(clad.sketch.solver:solve-sketch *sketch*)
```

### Sketch to 3D Conversion

CLAD provides full support for converting 2D sketches to 3D solids.

**Convert to Face:**
```lisp
;; Convert closed sketch to a 3D face
(clad.sketch:sketch-to-face sketch)
```

**Extrude a Sketch:**
```lisp
;; Extrude sketch to create a solid
(clad.sketch:extrude-sketch sketch distance)

;; With custom direction
(clad.sketch:extrude-sketch sketch distance :direction '(0 0 1))

;; On a different plane
(let ((plane (clad.sketch:make-sketch-plane :type :yz)))
  (clad.sketch:extrude-sketch sketch 20 :plane plane))
```

**Revolve a Sketch:**
```lisp
;; Full revolution around Y axis (default)
(clad.sketch:revolve-sketch sketch)

;; Partial revolution (90 degrees)
(clad.sketch:revolve-sketch sketch :angle (/ pi 2))

;; Revolution around Z axis
(clad.sketch:revolve-sketch sketch :axis-direction '(0 0 1))

;; Revolution around arbitrary axis
(clad.sketch:revolve-sketch sketch
  :axis-point '(10 0 0)
  :axis-direction '(0 1 0)
  :angle (* 2 pi))
```

### Complete Example - Extruded Profile

```lisp
;; Create a sketch with a circular profile
(let* ((sketch (clad.sketch:make-sketch))
       (center (clad.sketch:make-point-2d 0 0))
       (circle (clad.sketch:make-circle-2d center 15.0)))
  ;; Add circle to sketch
  (clad.sketch:add-entity sketch circle)
  ;; Extrude to create a cylinder
  (clad.sketch:extrude-sketch sketch 30.0))
```

### Complete Example - Revolved Profile (Torus)

```lisp
;; Create a torus by revolving a circle
(let* ((sketch (clad.sketch:make-sketch))
       ;; Circle offset from axis
       (center (clad.sketch:make-point-2d 30 0))
       (circle (clad.sketch:make-circle-2d center 5.0)))
  (clad.sketch:add-entity sketch circle)
  ;; Full revolution creates a torus
  (clad.sketch:revolve-sketch sketch :axis-direction '(0 1 0)))
```

### Sketch Planes

Define the plane on which your sketch lives:

```lisp
;; Standard planes
(clad.sketch:make-sketch-plane :type :xy)  ; XY plane at Z=0 (default)
(clad.sketch:make-sketch-plane :type :yz)  ; YZ plane at X=0
(clad.sketch:make-sketch-plane :type :xz)  ; XZ plane at Y=0

;; With offset origin
(clad.sketch:make-sketch-plane :type :xy :origin '(0 0 50))
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

### STL Export (3D Printing)

Export to STL format for 3D printing:

```lisp
(clad.export:export-stl part filepath &key (ascii nil) (resolution :medium))
```

STL (STereoLithography) is the standard format for 3D printing, containing a triangulated mesh representation of your model.

**Resolution options:**
- `:low` - Fast export, coarse mesh (0.5mm linear, 1.0° angular)
- `:medium` - Balanced quality/size (0.1mm linear, 0.5° angular) - **recommended**
- `:high` - High detail (0.05mm linear, 0.25° angular)
- `:ultra` - Maximum detail (0.01mm linear, 0.1° angular)

**Format options:**
- Binary STL (default, `:ascii nil`) - Smaller files, faster processing
- ASCII STL (`:ascii t`) - Human-readable, easier to debug

**Examples:**

```lisp
;; Standard 3D printing export (binary, medium resolution)
(defparameter *bracket* (mounting-bracket :width 80))
(clad.export:export-stl *bracket* "output/bracket.stl")

;; High detail for precise parts
(defparameter *gear* (spur-gear :teeth 20))
(clad.export:export-stl *gear* "output/gear.stl" :resolution :high)

;; ASCII format for debugging
(clad.export:export-stl *test-part* "debug/test.stl" :ascii t :resolution :low)

;; Low resolution for quick preview
(clad.export:export-stl *assembly* "preview.stl" :resolution :low)
```

**3D Printing Workflow:**

1. Design part in CLAD
2. Export to STL: `(clad.export:export-stl my-part "part.stl" :resolution :high)`
3. Import STL into slicer software (Cura, PrusaSlicer, etc.)
4. Generate G-code for your printer
5. Print!

**Resolution Guidelines:**

Choose resolution based on your needs:
- **:low** - Draft prints, large models, visualization (fast, small files)
- **:medium** - Most 3D printing applications (balanced)
- **:high** - Fine details, small features, precision parts (larger files, slower slicing)
- **:ultra** - Ultra-high-resolution prints, inspection (very large files)

**Compatibility:**

STL files work with all 3D printing software:
- Slicers: Cura, PrusaSlicer, Simplify3D, etc.
- Mesh tools: Meshmixer, Netfabb, etc.
- CAD: FreeCAD, SolidWorks, Fusion 360, etc.
- Viewers: MeshLab, Blender, etc.

**Notes:**
- Higher resolution = more triangles = larger files = slower slicing
- STL files contain only geometry (no color, material metadata)
- STL represents surfaces as triangle meshes (not exact B-Rep geometry)
- Binary format is ~5x smaller than ASCII

---

### Mass Properties Analysis

Calculate mass, volume, and other engineering properties for parts:

```lisp
(clad.analysis:mass-properties shape &key (material nil) (density 1.0))
```

Mass properties are essential for engineering analysis, weight estimation, and BOM generation. CLAD calculates exact properties using OpenCASCADE's geometry engine (not approximate mesh-based methods).

**Returned properties:**
- `:volume` - Volume in mm³
- `:surface-area` - Surface area in mm²
- `:mass` - Mass in grams
- `:density` - Density used (g/cm³)
- `:center-of-mass` - Center of mass coordinates `(x y z)` in mm
- `:inertia` - Moment of inertia tensor (3×3 matrix)
- `:material-name` - Material name string

**Examples:**

```lisp
;; Basic volume and mass calculation
(defparameter *box* (clad.core:make-box 100 50 20))
(defparameter *props* (clad.analysis:mass-properties *box* :material :aluminum))

(getf *props* :volume)        ; => 100000.0 (mm³)
(getf *props* :mass)           ; => 270.0 (grams)
(getf *props* :material-name)  ; => "Aluminum 6061"

;; Custom density
(clad.analysis:mass-properties *box* :density 5.0)  ; 5.0 g/cm³

;; Different materials
(clad.analysis:mass-properties *part* :material :steel)
(clad.analysis:mass-properties *part* :material :pla)
```

**Built-in Material Database:**

CLAD includes common engineering materials (densities in g/cm³):

| Material | Keyword | Density (g/cm³) |
|----------|---------|-----------------|
| Aluminum 6061 | `:aluminum` | 2.70 |
| Steel 1018 | `:steel` | 7.87 |
| Stainless Steel 304 | `:stainless` | 8.00 |
| ABS Plastic | `:abs` | 1.05 |
| PLA Plastic | `:pla` | 1.24 |
| Brass | `:brass` | 8.50 |
| Copper | `:copper` | 8.96 |
| Titanium Grade 5 | `:titanium` | 4.43 |
| Nylon 6 | `:nylon` | 1.14 |
| PETG Plastic | `:petg` | 1.27 |

List all materials:
```lisp
(clad.analysis:list-materials)
; => (:aluminum :steel :stainless :abs :pla :brass :copper :titanium :nylon :petg)
```

**Convenience Functions:**

For quick queries, use these wrappers:

```lisp
;; Volume only
(clad.analysis:volume *part*)  ; mm³

;; Surface area only
(clad.analysis:surface-area *part*)  ; mm²

;; Mass only
(clad.analysis:mass *part* :material :steel)  ; grams

;; Center of mass
(clad.analysis:center-of-mass *part*)  ; (x y z)

;; Inertia tensor
(clad.analysis:inertia *part* :material :aluminum)  ; 3×3 matrix
```

**Assembly Mass Properties:**

Calculate total mass for multi-material assemblies:

```lisp
(defparameter *aluminum-base* (clad.core:make-box 100 100 10))
(defparameter *steel-bracket* (clad.core:make-box 50 50 5))

(let ((base-props (clad.analysis:mass-properties *aluminum-base* :material :aluminum))
      (bracket-props (clad.analysis:mass-properties *steel-bracket* :material :steel)))
  (+ (getf base-props :mass) (getf bracket-props :mass)))
; => Total assembly mass in grams
```

**Custom Materials:**

Define custom materials for your application:

```lisp
(clad.analysis:define-material :custom-alloy "Special Alloy XJ-7" 6.5)
(clad.analysis:mass-properties *part* :material :custom-alloy)
```

**Practical Example - Weight Estimation:**

```lisp
(clad.dsl:defpart drone-frame ((arm-length 300) (tube-diameter 10))
  "Quadcopter frame"
  (:body (clad.core:make-box 100 100 10))  ; Center plate

  ;; Four arms
  (:on-face :direction :+z :extreme :max
    (:grid-pattern :count-x 2 :count-y 2
                   :spacing-x 90 :spacing-y 90
      (:add (clad.core:make-cylinder (/ tube-diameter 2) arm-length)))))

;; Calculate frame weight
(defparameter *frame* (drone-frame))
(defparameter *frame-props*
  (clad.analysis:mass-properties *frame* :material :aluminum))

(format t "Frame weight: ~,1f grams~%" (getf *frame-props* :mass))
(format t "Frame volume: ~,1f cm³~%" (/ (getf *frame-props* :volume) 1000.0))
(format t "Center of mass: ~A~%" (getf *frame-props* :center-of-mass))
```

**Engineering Applications:**

1. **Weight Budgets** - Ensure parts meet weight requirements
2. **Material Selection** - Compare weight vs. strength trade-offs
3. **Center of Mass** - Balance analysis for rotating parts
4. **Inertia Calculations** - Dynamics and motion analysis
5. **Cost Estimation** - Calculate material cost from volume and density
6. **BOM Generation** - Accurate weight data for assemblies

**Unit Conversions:**

Mass properties use metric units:
- Volume: mm³ (cubic millimeters)
- Mass: grams
- Density: g/cm³
- Length: mm (millimeters)

Common conversions:
```lisp
;; mm³ to cm³
(/ volume-mm3 1000.0)

;; grams to kilograms
(/ mass-g 1000.0)

;; grams to pounds
(/ mass-g 453.592)
```

---

### Thread Modeling

Create standard threaded features (bolts, holes, nuts) for mechanical assemblies. CLAD includes a database of common thread standards and functions for thread creation.

**Thread Types Supported:**
- ISO Metric (M3, M6, M8, M10, etc.)
- ISO Metric Fine (M8x1.0, M10x1.25, etc.)
- Unified (UNC/UNF: 1/4-20, #10-32, etc.)

**Basic Thread Creation:**

```lisp
(clad.features:make-external-thread designation &key length (cosmetic nil))
(clad.features:make-internal-thread designation &key depth (cosmetic nil))
```

**Examples:**

```lisp
;; External thread (bolt, stud)
(defparameter *m6-bolt*
  (clad.features:make-external-thread :m6 :length 30))

;; Internal thread (threaded hole)
(defparameter *m8-hole*
  (clad.features:make-internal-thread :m8 :depth 25))

;; Unified thread
(defparameter *quarter-20-bolt*
  (clad.features:make-external-thread :1/4-20 :length 25.4))  ; 1 inch
```

**Thread Database:**

List available thread standards:

```lisp
(clad.features:list-thread-specs)
; => (:m3 :m6 :m8 :m10 :m8x1.0 :m10x1.25 :1/4-20)
```

Get thread parameters:

```lisp
(clad.features:get-thread-spec :m6)
; => (:major-diameter 6.0 :pitch 1.0 :standard "ISO Metric")

(clad.features:get-thread-spec :m8x1.0)
; => (:major-diameter 8.0 :pitch 1.0 :standard "ISO Metric Fine")
```

**Thread Calculations:**

Calculate minor diameter and tap drill size:

```lisp
;; Minor diameter (root of thread)
(clad.features:thread-minor-diameter :m6)
; => 4.917 (mm)

;; Recommended tap drill size
(clad.features:tap-drill-size :m6)
; => 5.0 (mm)

(clad.features:tap-drill-size :m8)
; => 6.75 (mm)
```

**Practical Example - Threaded Part:**

```lisp
(clad.dsl:defpart threaded-standoff
    ((body-diameter 10)
     (body-length 30)
     (thread-length 8))
  "Standoff with M4 external threads on both ends"

  ;; Main body
  (:body (clad.core:make-cylinder (/ body-diameter 2) body-length))

  ;; Top thread (M4 external)
  (:on-face :direction :+z :extreme :max
    (:add (clad.features:make-external-thread :m4 :length thread-length)))

  ;; Bottom thread (M4 external)
  (:on-face :direction :-z :extreme :min
    (:add (clad.core:translate
            (clad.features:make-external-thread :m4 :length thread-length)
            0 0 (- thread-length)))))

;; Create and export
(defparameter *standoff* (threaded-standoff))
(clad:view *standoff* :name "standoff")
```

**Example - Part with Threaded Holes:**

```lisp
(clad.dsl:defpart mounting-plate-threaded
    ((width 100)
     (height 80)
     (thickness 10)
     (hole-spacing 70))
  "Mounting plate with M6 threaded holes"

  ;; Base plate
  (:body (clad.core:make-box width height thickness))

  ;; Drill and tap M6 holes in corners
  (:on-face :direction :+z :extreme :max
    (:grid-pattern :count-x 2 :count-y 2
                   :spacing-x hole-spacing
                   :spacing-y (- height 20)
      (:cut (clad.core:translate
              (clad.features:make-internal-thread :m6 :depth thickness)
              0 0 0)))))
```

**Custom Thread Standards:**

Define custom thread specifications:

```lisp
(clad.features:define-thread-spec :m12x1.5 12.0 1.5 :standard "ISO Metric Fine")
(defparameter *custom-thread*
  (clad.features:make-external-thread :m12x1.5 :length 40))
```

**Thread Operations:**

Add threads to existing geometry:

```lisp
;; Add external thread to existing cylinder
(defparameter *cylinder* (clad.core:make-cylinder 3 20))
(defparameter *threaded-rod*
  (clad.features:add-external-thread *cylinder* :m6))

;; Cut internal thread into part
(defparameter *block* (clad.core:make-box 30 30 15))
(defparameter *threaded-block*
  (clad.features:cut-internal-thread *block* :m6 15 15 0))
```

**Common Thread Standards:**

| Designation | Major Dia (mm) | Pitch (mm) | Standard |
|-------------|----------------|------------|----------|
| M3 | 3.0 | 0.5 | ISO Metric |
| M6 | 6.0 | 1.0 | ISO Metric |
| M8 | 8.0 | 1.25 | ISO Metric |
| M10 | 10.0 | 1.5 | ISO Metric |
| M8x1.0 | 8.0 | 1.0 | ISO Metric Fine |
| M10x1.25 | 10.0 | 1.25 | ISO Metric Fine |
| 1/4-20 | 6.35 | 1.27 | UNC |

**Engineering Notes:**

1. **Cosmetic vs. Detailed**: Current implementation uses cosmetic representation (simplified geometry). Suitable for assemblies where exact thread form isn't critical.

2. **Tap Drill Sizing**: Use `tap-drill-size` function to determine correct drill diameter for tapped holes.

3. **Thread Engagement**: For blind holes, make depth at least 1.5× the major diameter for full strength.

4. **Clearance Holes**: For bolts passing through, use major diameter + 0.5mm clearance (e.g., 6.5mm for M6).

**Assembly Integration:**

```lisp
;; Bolt and nut assembly
(clad.assembly.dsl:defassembly bolted-joint ()
  "Simple bolted connection"

  (:component :plate1 (mounting-plate-threaded)
              :fixed t
              :metadata '(:material "Aluminum"))

  (:component :bolt (clad.features:make-external-thread :m6 :length 40)
              :quantity 4
              :metadata '(:part-number "M6-40" :material "Steel"))

  (:mate :concentric
         :plate1 :hole-1
         :bolt :axis))
```

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
- `06-advanced-selectors-showcase.lisp` - Boolean combinators, position selectors, face-plane operations
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
