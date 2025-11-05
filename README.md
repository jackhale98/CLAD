# CLAD - Common Lisp CAD

A powerful, code-first CAD system built in Common Lisp with OpenCASCADE Technology. Design parametric 3D parts using an expressive DSL, visualize them in real-time, and export to industry-standard formats.

## Features

- **Declarative DSL**: Define parametric parts with `defpart` - clean, readable, and powerful
- **Smart Selectors**: Intelligently select faces and edges by direction, geometry type, or custom predicates
- **Pattern Operations**: Create circular, linear, and grid patterns with ease
- **Advanced Geometry**: Fillets, chamfers, lofts, sweeps, and pipes
- **2D Sketching**: Parametric sketch system with constraint solving
- **Assemblies**: Build complex assemblies with mate constraints
- **Live Preview**: Integrated web-based 3D viewer with real-time updates
- **Auto-Rebuild**: Automatic part regeneration on file save
- **STEP Export**: Export to industry-standard STEP format

## Quick Start

### Installation

#### Prerequisites

**Ubuntu/Debian:**
```bash
# Install OpenCASCADE and SBCL
sudo apt-get install libocct-foundation-dev \
                     libocct-modeling-data-dev \
                     libocct-modeling-algorithms-dev \
                     libocct-data-exchange-dev \
                     sbcl

# Install Quicklisp (if not already installed)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
```

**macOS:**
```bash
brew install opencascade sbcl
```

#### Load CLAD

```lisp
(ql:quickload :clad)
```

### Your First Part

```lisp
;; Load the system
(asdf:load-system :clad)

;; Define a parametric mounting plate
(clad.dsl:defpart mounting-plate
    ((width 100)
     (height 80)
     (thickness 10)
     (hole-diameter 6)
     (fillet-radius 3.0d0))
  "A mounting plate with holes and fillets"

  ;; Create the base plate
  (:body (clad.core:make-box width height thickness))

  ;; Add mounting holes in corners
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* thickness 2))
            10 10 0)))

  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate
            (clad.core:make-cylinder (/ hole-diameter 2) (* thickness 2))
            (- width 10) (- height 10) 0)))

  ;; Fillet all vertical edges
  (:on-edge :parallel :z
    (:fillet fillet-radius)))

;; Create the part
(defparameter *my-plate* (mounting-plate))

;; View it in the browser
(clad:view *my-plate* :name "mounting-plate")
;; Opens at http://localhost:8080

;; Export to STEP
(clad.export:export-step *my-plate* "mounting-plate.step")

;; Try different parameters
(defparameter *large-plate* (mounting-plate :width 150 :height 120))
```

## Core Concepts

### The defpart DSL

`defpart` is the heart of CLAD. It defines parametric, regeneratable parts:

```lisp
(clad.dsl:defpart part-name
    ((param1 default1) (param2 default2) ...)
  "Documentation string"

  ;; Body: the base shape
  (:body <shape>)

  ;; Operations on faces
  (:on-face <selectors>
    <operations>)

  ;; Operations on edges
  (:on-edge <selectors>
    <operations>))
```

### Selectors

Select faces and edges intelligently:

```lisp
;; Select by direction
(:on-face :direction :+z :extreme :max    ; Top face
  ...)

(:on-face :direction :-z :extreme :min    ; Bottom face
  ...)

;; Select edges parallel to an axis
(:on-edge :parallel :z                    ; Vertical edges
  ...)

;; Select by geometry type
(:on-edge :type :line                     ; Straight edges
  ...)

(:on-face :type :cylindrical              ; Curved faces
  ...)

;; Combine selectors
(:on-edge :type :line :parallel :x        ; Straight edges along X
  ...)
```

### Operations

**Boolean Operations:**
```lisp
(:on-face :direction :+z :extreme :max
  (:add <shape>)      ; Add material
  (:cut <shape>))     ; Remove material
```

**Edge Operations:**
```lisp
(:on-edge :parallel :z
  (:fillet 5.0d0)     ; Round edges
  (:chamfer 2.0d0))   ; Bevel edges
```

**Patterns:**
```lisp
;; Circular pattern
(:circular-pattern
    :count 8
    :radius 40
    :center-x 50
    :center-y 50
  (:cut (clad.core:make-cylinder 3 20)))

;; Linear pattern
(:linear-pattern
    :count-x 3
    :count-y 2
    :spacing-x 30
    :spacing-y 20
  (:add (clad.core:make-box 5 5 10)))

;; Grid pattern
(:grid-pattern
    :count-x 4
    :count-y 3
    :spacing-x 25
    :spacing-y 25
  (:cut (clad.core:make-cylinder 2 15)))
```

### Primitive Shapes

```lisp
;; Box (centered on XY plane, starts at Z=0)
(clad.core:make-box width height depth)

;; Cylinder (centered, extends upward)
(clad.core:make-cylinder radius height)

;; Sphere (centered at origin)
(clad.core:make-sphere radius)

;; Cone
(clad.core:make-cone radius1 radius2 height)
```

### Transformations

```lisp
;; Translate (move)
(clad.core:translate shape dx dy dz)

;; Rotate around axis
(clad.core:rotate shape :axis :z :angle 45)

;; Mirror across plane
(clad.core:mirror shape :plane :xy)

;; Scale
(clad.core:scale shape factor)
```

### Advanced Features

**Loft between profiles:**
```lisp
(:loft :sections (list
         (clad.core:make-circle-wire '(0 0 0) 20)
         (clad.core:make-circle-wire '(0 0 50) 10))
       :solid t
       :ruled nil)
```

**Sweep profile along path:**
```lisp
(:sweep :profile (clad.core:make-circle-wire '(0 0 0) 5)
        :path (clad.core:make-spline '((0 0 0) (10 10 20) (20 0 40))))
```

**Pipe along path:**
```lisp
(:pipe :path (clad.core:make-spline '((0 0 0) (20 20 30)))
       :radius 3)
```

## Examples

The `examples/` directory contains comprehensive tutorials:

- **01-basic-dsl.lisp** - Introduction to defpart and basic shapes
- **02-patterns.lisp** - Circular, linear, and grid patterns
- **03-fillets-chamfers.lisp** - Edge finishing operations
- **04-advanced-features.lisp** - Lofts, sweeps, and pipes
- **05-assemblies.lisp** - Building assemblies with constraints
- **06-advanced-selectors.lisp** - Advanced face and edge selection
- **06-sketches.lisp** - 2D parametric sketching

Run any example:
```lisp
(load "examples/01-basic-dsl.lisp")
```

## Documentation

- **[USER_GUIDE.md](USER_GUIDE.md)** - Comprehensive tutorial and reference
- **[SELECTOR_REFERENCE.md](SELECTOR_REFERENCE.md)** - Complete selector system documentation
- **[examples/README.md](examples/README.md)** - Example code guide

## Development

### Building the C Wrapper

CLAD uses a C++ wrapper to interface with OpenCASCADE:

```bash
cd c-wrapper
mkdir build && cd build
cmake ..
make
```

### Running Tests

```lisp
(asdf:test-system :clad)
```

Or from command line:
```bash
sbcl --eval "(asdf:test-system :clad)" --quit
```

### Project Structure

```
clad/
├── src/
│   ├── ffi/          # OpenCASCADE bindings
│   ├── core/         # Functional core (primitives, booleans, transforms)
│   ├── units/        # Unit system with conversions
│   ├── shapes/       # CLOS shape wrappers
│   ├── selectors/    # Face and edge selection system
│   ├── workplane/    # Workplane support
│   ├── context/      # Context API for imperative usage
│   ├── dsl/          # defpart DSL
│   ├── sketch/       # 2D sketching with constraints
│   ├── assembly/     # Assembly system
│   ├── export/       # STEP export
│   ├── viewer/       # Web-based viewer
│   └── auto-rebuild/ # Automatic regeneration
├── tests/            # Comprehensive test suite
├── examples/         # Tutorial examples
└── c-wrapper/        # C++ wrapper for OpenCASCADE
```

## Interactive Workflow

CLAD supports an interactive, REPL-driven workflow:

```lisp
;; 1. Start a REPL and load CLAD
(ql:quickload :clad)

;; 2. Define your part
(clad.dsl:defpart my-widget ((size 50))
  (:body (clad.core:make-box size size 10)))

;; 3. View it
(clad:view (my-widget) :name "widget")

;; 4. Edit the defpart definition, re-evaluate (C-c C-c in Emacs)
;; The viewer updates automatically!

;; 5. Export when ready
(clad.export:export-step (my-widget :size 75) "widget.step")
```

For file-watching auto-rebuild:
```lisp
(clad.auto-rebuild:start-watching "my-design.lisp")
;; Edit and save the file - part rebuilds automatically!
```

## Comparison to Other CAD Systems

| Feature | CLAD | OpenSCAD | CadQuery |
|---------|------|----------|----------|
| Language | Common Lisp | Custom DSL | Python |
| Kernel | OpenCASCADE | CGAL | OpenCASCADE |
| REPL-Driven | ✓ | ✗ | ✓ |
| Parametric | ✓ | ✓ | ✓ |
| Assemblies | ✓ | ✗ | ✓ |
| Sketching | ✓ | ✗ | ✓ |
| Live Viewer | ✓ | ✓ | ✓ |

## Why CLAD?

**Code-First Design**: Your CAD models are programs. Version control them with git, generate them programmatically, and leverage the full power of Common Lisp.

**Interactive Development**: The REPL workflow means instant feedback. Change a parameter, re-evaluate, see results immediately.

**Powerful DSL**: The `defpart` macro creates readable, maintainable designs. Selectors eliminate tedious manual edge/face selection.

**Production Ready**: Built on OpenCASCADE (used by FreeCAD, Salome), export to STEP for manufacturing.

## Community

- Report issues on GitHub
- Share designs and techniques
- Contribute examples and documentation

## License

MIT License - see LICENSE file

## Acknowledgments

- Built on [OpenCASCADE Technology](https://www.opencascade.com/)
- Inspired by [CadQuery](https://cadquery.readthedocs.io/) and [OpenSCAD](https://openscad.org/)
- Common Lisp community for excellent libraries (CFFI, Alexandria, etc.)
