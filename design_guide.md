# CLAD Design Guide
## A Source-Code Based CAD Design System in Common Lisp

**Version:** 1.1  
**Date:** 2025-10-30  
**Status:** Design Specification  
**Changes:** Added unit inheritance system and tolerance analysis integration

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [System Architecture](#system-architecture)
3. [Core Components](#core-components)
4. [Units System](#units-system)
5. [Tolerance Analysis](#tolerance-analysis)
6. [Implementation Phases](#implementation-phases)
7. [API Design](#api-design)
8. [Extension Mechanisms](#extension-mechanisms)
9. [Development Workflow](#development-workflow)
10. [Testing Strategy](#testing-strategy)
11. [Future Roadmap](#future-roadmap)

---

## Executive Summary

### Vision

CLAD is a parametric, source-code-based CAD system built on Common Lisp and OpenCASCADE Technology (OCCT). It combines the expressiveness of Lisp with the industrial-strength geometry kernel of OCCT to create a powerful, extensible platform for mechanical design.

### Key Design Goals

1. **Ergonomic DSL**: Declarative syntax that reads like natural design intent
2. **REPL-Driven Development**: Instant feedback loop with auto-rebuilding
3. **Parametric by Default**: All designs are inherently parametric
4. **Extensible**: User-defined features and reusable components
5. **Industrial Grade**: STEP export compatible with professional CAD tools
6. **Interactive**: Web-based 3D visualization during development
7. **Tolerance-Aware**: Optional integrated tolerance analysis without added complexity

### Core Principles

- **Functional Foundation**: Pure functions at the base layer enable composition and testing
- **Contextual Convenience**: Stateful contexts for ergonomics without sacrificing clarity
- **Declarative DSL**: Macros provide high-level syntax that compiles to functional core
- **Safety First**: C++ exceptions properly wrapped, memory automatically managed
- **Incremental Complexity**: Simple things simple, complex things possible
- **Progressive Enhancement**: Basic parts need no tolerance/unit specification; add when needed

---

## System Architecture

### Layered Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Layer 6: Tolerance Analysis (Optional)                     │
│  - Tolerance chain definitions                              │
│  - Worst-case, RSS, Monte Carlo analysis                   │
│  - Cross-part dimension tracking                            │
│  - Sensitivity analysis                                     │
├─────────────────────────────────────────────────────────────┤
│  Layer 5: DSL (Declarative Macros)                          │
│  - defpart macro with declarative syntax                    │
│  - Auto-rebuild integration                                 │
│  - High-level feature definitions                           │
│  - Optional inline tolerance specification                  │
├─────────────────────────────────────────────────────────────┤
│  Layer 4: Context API (Stateful Convenience)               │
│  - with-context for sequential operations                   │
│  - Workplane management                                     │
│  - Selection stack operations                               │
│  - Unit context management                                  │
├─────────────────────────────────────────────────────────────┤
│  Layer 3: CLOS Wrapper (Object-Oriented Interface)         │
│  - shape, edge, face, solid classes                        │
│  - Generic functions for operations                         │
│  - Selector system                                          │
│  - Unit conversion with inheritance                         │
├─────────────────────────────────────────────────────────────┤
│  Layer 2: Functional Core (Pure Functions)                 │
│  - make-box, make-cylinder, etc.                           │
│  - Boolean operations (union, cut, intersect)              │
│  - Transformations (translate, rotate, mirror)             │
│  - Returns immutable values                                 │
├─────────────────────────────────────────────────────────────┤
│  Layer 1: CFFI Bindings (Foreign Interface)                │
│  - Direct OCCT function wrappers                           │
│  - Exception handling boundary                              │
│  - Memory management (Handle<> integration)                 │
├─────────────────────────────────────────────────────────────┤
│  Layer 0: OpenCASCADE Technology (C++ Kernel)              │
│  - B-rep geometry kernel                                    │
│  - Boolean operations                                       │
│  - STEP/IGES import/export                                 │
└─────────────────────────────────────────────────────────────┘
```

### Data Flow

```
User DSL Code
    ↓
  Macro Expansion (compile time)
    ↓
  Unit Resolution (compile/runtime)
    ↓
  CLOS Method Dispatch (runtime)
    ↓
  Functional Core Operations
    ↓
  CFFI Foreign Calls
    ↓
  OCCT C++ Kernel
    ↓
  STEP File / Mesh / Visualization
    ↓
  Optional: Tolerance Analysis
```

### Package Structure

```lisp
;; Package hierarchy
clad.ffi          ; CFFI bindings, low-level
clad.core         ; Functional core API
clad.shapes       ; CLOS shape classes
clad.selectors    ; Selection system
clad.workplane    ; Coordinate systems
clad.units        ; Unit conversion with inheritance
clad.tolerance    ; Tolerance specifications
clad.metrology    ; Dimension measurement
clad.analysis     ; Tolerance analysis engine
clad.context      ; Context-based API
clad.dsl          ; High-level DSL macros
clad.auto-rebuild ; REPL integration
clad.viewer       ; Web viewer
clad.export       ; STEP/STL export
clad             ; Main user package (re-exports)
```

---

## Core Components

### 1. CFFI Bindings Layer

#### Design Philosophy

- **Exception Safety**: All OCCT calls wrapped in try-catch at C++ boundary
- **Memory Management**: Integrate with OCCT's Handle<> reference counting
- **Error Propagation**: Convert C++ exceptions to Lisp conditions

### 2-5. Other Core Components

*(Functional Core, CLOS Shapes, Selectors, Workplanes remain as in original guide)*

---

## Units System

### Design Philosophy

**Key Principle**: Unit management through inheritance with dynamic scoping

- **Zero Boilerplate**: Simple parts need no unit specification
- **Natural Inheritance**: File → Part → Operation → Explicit
- **Zero Runtime Cost**: Compile-time conversion when possible
- **Mixed Units**: Support mixed-unit assemblies naturally
- **Traceability**: Every dimension remembers its source units

### Architecture

```lisp
(defpackage :clad.units
  (:use :cl)
  (:export #:*default-units* #:*file-units* 
           #:with-units #:dim #:convert-units
           #:effective-units #:define-unit))
```

### Four Levels of Unit Context

```lisp
;; Level 1: Global system default
(defvar *default-units* :mm
  "System-wide default units (typically :mm or :in)")

;; Level 2: File-level override
(defvar *file-units* nil
  "File-level unit override. If nil, uses *default-units*")

;; Level 3: Part-level (set in defpart :units keyword)
;; Level 4: Operation-level (explicit unit in dim call)

(defun effective-units ()
  "Get the currently effective units from the context stack"
  (or *file-units* *default-units*))
```

### Unit Resolution Order

```
Explicit (dim 10 :in) → with-units block → Part :units → *file-units* → *default-units*
      ↑ Highest Priority                                        Lowest Priority ↓
```

### Dimension Macro with Inheritance

```lisp
(defmacro dim (value &optional (unit nil unit-provided-p) 
                    &key tol fit grade)
  "Dimensional value with unit inheritance and optional tolerance.
  
  Unit resolution:
    1. Explicit unit provided: (dim 10 :in)
    2. Current dynamic context from with-units
    3. Part-level :units specification
    4. File-level *file-units*
    5. Global *default-units*
  
  Examples:
    (dim 10)              ; Uses inherited units
    (dim 10 :in)          ; Explicit override
    (dim 10 :mm :tol ±0.1); With tolerance
    (dim 30 :mm :fit :H7) ; ISO fit
  
  Returns: toleranced-dimension object (stored in mm internally)"
  
  (let ((source-unit (cond
                       unit-provided-p unit
                       t '(effective-units))))
    `(make-toleranced-dimension
      :nominal ,(if (keywordp source-unit)
                    `(coerce ,(convert-units value source-unit :mm) 
                            'double-float)
                    `(coerce (convert-units ,value ,source-unit :mm) 
                            'double-float))
      :source-value ,value
      :source-units ',source-unit
      :tolerance ,(expand-tolerance-spec tol fit grade source-unit value))))
```

### Usage Patterns

#### Pattern 1: Simple Parts

```lisp
;; Uses global default (*default-units* = :mm)
(defpart simple-bracket ()
  (:body
    (box :width (dim 100)      ; 100mm
         :height (dim 50))))   ; 50mm
```

#### Pattern 2: File-Level Units

```lisp
;; At top of file
(setf *file-units* :in)

(defpart us-part ()
  (:body
    (box :width (dim 4)        ; 4 inches
         :height (dim 2))))    ; 2 inches
```

#### Pattern 3: Part-Level Override

```lisp
(setf *file-units* :in)

(defpart metric-insert ()
  :units :mm                   ; Override for this part
  (:body
    (cylinder :diameter (dim 30))))  ; 30mm
```

#### Pattern 4: Mixed Units

```lisp
(defpart hybrid-part ()
  :units :mm
  (:body
    (box :width (dim 100)))    ; mm
  
  (with-units :in
    (:feature :cut
      (cylinder :diameter (dim 0.25)))) ; inches
  
  (:feature :cut
    (cylinder :diameter (dim 10))))    ; back to mm
```

### Toleranced Dimension Data Structure

```lisp
(defclass toleranced-dimension ()
  ((nominal :initarg :nominal
            :type double-float
            :documentation "Nominal dimension in mm (OCCT native)")
   (source-value :initarg :source-value
                 :documentation "Original value as entered")
   (source-units :initarg :source-units
                 :documentation "Units entered in")
   (tolerance :initarg :tolerance :initform nil
              :documentation "Tolerance specification")
   (display-units :initarg :display-units :initform nil
                  :documentation "Preferred display units")))
```

---

## Tolerance Analysis

### Design Philosophy

**Key Principles**:
- **Optional**: Basic parts need no tolerance specification
- **Progressive**: Add tolerances only where needed
- **Inline**: Tolerances live with dimensions
- **Derived Dimensions**: Support dimensions measured from finished geometry
- **Standards-Based**: Built-in ISO/ASME standards
- **Analysis-Ready**: Automatic chain construction

### Inline Tolerance Specification

```lisp
;; Simple - no tolerance
(dim 100)

;; Explicit bilateral
(dim 30 :mm :tol ±0.05)

;; Explicit unilateral
(dim 30 :mm :tol (+0.025 0))

;; ISO fit lookup
(dim 30 :mm :fit :H7)

;; Use standard from context
(with-tolerance-standard :iso-2768-fine
  (dim 100 :mm))  ; Gets ±0.15
```

### Tolerance Standards

```lisp
(defvar *tolerance-standard* :iso-2768-medium
  "Default tolerance standard")

;; Built-in standards
(define-tolerance-standard :iso-2768-fine
  (:linear-dimensions
    ((<= dim 3)     ±0.05)
    ((<= dim 6)     ±0.05)
    ((<= dim 30)    ±0.1)
    ((<= dim 120)   ±0.15)
    ((<= dim 400)   ±0.2)
    ((<= dim 1000)  ±0.3)
    ((<= dim 2000)  ±0.5)))
```

### Part with Tolerances

```lisp
(defpart precision-housing ((bore-dia 30))
  :units :mm
  :tolerance-standard :iso-2768-fine
  
  (:body
    (box :width (dim 80)       ; Gets ±0.15 from standard
         :height (dim 80)))
  
  (:on-face ">Z"
    (:feature :cut
      (cylinder :diameter (dim bore-dia :fit :H7)  ; 30 H7
                :depth (dim 25 :tol ±0.05))
      :name :bearing-bore)))
```

### Derived/Mate Dimensions

```lisp
(defpart shaft ()
  :units :mm
  
  (:body
    (cylinder :diameter (dim 30) :depth (dim 150)))
  
  (:on-solid
    (:section :from 40 :to 65
      :name :bearing-seat
      :diameter (dim 30 :fit :h6)))  ; Shaft fit
  
  ;; DERIVED DIMENSION: Measured from finished geometry
  (:dimension :bearing-seat-position
    "Distance from left face to bearing seat center"
    :measure (distance 
               (face :direction :-z :extreme :min)
               (section-center :bearing-seat))
    :nominal (dim 52.5)
    :tolerance ±0.1
    :critical t
    :note "Critical for bearing preload"))
```

### Tolerance Chains

```lisp
(deftolerance-chain bearing-preload-stack
  "Stack-up controlling bearing preload"
  :display-units :mm
  
  (:parts
    (housing precision-housing)
    (shaft precision-shaft)
    (bearing :standard "6006")
    (spacer bearing-spacer))
  
  (:chain
    (:start housing :bearing-seat-position)  ; Derived
    (:add spacer :thickness)                 ; Constructed
    (:add bearing :width)                    ; Fixed
    (:subtract shaft :bearing-seat-position) ; Derived
    
    (:result :preload
      :target (dim 0.01)
      :min (dim 0.000)
      :max (dim 0.050)))
  
  (:analysis :worst-case :rss :monte-carlo))
```

### Analysis Methods

```lisp
;; Worst-case analysis
(defun worst-case (chain)
  "Compute min/max using worst-case stack-up")

;; Statistical RSS
(defun rss-analysis (chain &key (confidence 0.99))
  "Statistical analysis assuming normal distributions")

;; Monte Carlo simulation
(defun monte-carlo (chain &key (samples 10000))
  "Random sampling tolerance analysis")

;; Sensitivity analysis
(defun sensitivity-analysis (chain)
  "Determine which dimensions contribute most to variance")
```

### Analysis Output

```lisp
CL-USER> (analyze-chain 'bearing-preload-stack)

╔═══════════════════════════════════════════════╗
║  Tolerance Analysis: BEARING-PRELOAD-STACK   ║
╚═══════════════════════════════════════════════╝

Chain Components:
  housing:bearing-seat-position   +15.000mm ±0.050mm
+ spacer:thickness                +10.000mm ±0.200mm
+ bearing:width                   + 9.000mm (fixed)
- shaft:bearing-seat-position     -52.500mm ±0.100mm
───────────────────────────────────────────────────
= preload                           0.010mm target

WORST-CASE ANALYSIS:
  Min: -0.350mm ❌ INTERFERENCE!
  Max: +0.350mm

RSS ANALYSIS (99% confidence):
  Mean: 0.010mm
  Range: -0.18mm to +0.20mm
  P(interference): 18.2% ⚠️

MONTE CARLO (10,000 samples):
  μ = 0.011mm, σ = 0.073mm
  P(interference): 17.8% ⚠️

SENSITIVITY ANALYSIS:
  1. spacer:thickness           67.3% ████████████
  2. shaft:bearing-seat         23.5% █████
  3. housing:bearing-seat        9.2% ██

RECOMMENDATIONS:
  ⚠️  Current design: 18% interference probability
  1. Tighten spacer:thickness to ±0.1mm
  2. Consider selective assembly
```

---

## Implementation Phases

### Phase 1: Foundation (Weeks 1-3)

**Goal**: Basic OCCT integration with units

**Tasks**:
1. CFFI bindings with exception handling
2. Functional core: primitives, booleans
3. **Basic units**: `dim` macro with compile-time conversion
4. STEP export
5. Comprehensive tests

### Phase 2-4: Object System, Selectors, Workplanes (Weeks 4-8)

**Tasks include**:
- CLOS wrapper
- **Unit inheritance**: File and part-level contexts
- Selector system
- Workplane system

### Phase 5: DSL & Basic Tolerances (Weeks 9-10)

**Goal**: Declarative DSL with optional tolerances

**Tasks**:
1. Complete unit system
2. `with-units` macro
3. **Tolerance data structures**
4. **ISO 2768 standards**
5. **ISO fit lookup tables**
6. `defpart` macro with :units and :tolerance-standard

### Phase 6-7: Auto-Rebuild & Visualization (Weeks 11-13)

*(Same as original guide)*

### Phase 8: Advanced Features (Weeks 14-16)

**Tasks include**:
- Fillets, chamfers, splines
- **Derived dimensions**: Metrology system
- **GD&T support**: Basic position, perpendicularity
- Example library

### Phase 9: Tolerance Analysis (Weeks 17-19)

**Goal**: Complete tolerance analysis

**Tasks**:
1. Tolerance chain data structures
2. Dimension tracking across parts
3. Worst-case engine
4. RSS analysis
5. Monte Carlo simulation
6. Sensitivity analysis
7. Report generation
8. Viewer integration
9. Optimization suggestions

---

## API Design

### API Layers Summary

**Layer 1 (FFI)**: `ffi-make-box`  
**Layer 2 (Functional)**: `make-box`, `union`  
**Layer 3 (CLOS)**: `(faces shape)`, `(volume solid)`  
**Layer 4 (Context)**: `(with-units :in ...)`, `(add-solid ctx ...)`  
**Layer 5 (DSL)**: `(defpart name ... :units :mm)`  
**Layer 6 (Analysis)**: `(deftolerance-chain ...)`, `(analyze-chain name)`

### Complete Workflow Example

```lisp
;; 1. Simple - no tolerances
(defpart bracket-v1 ()
  (:body (box :width (dim 100))))

;; 2. Add tolerance standard
(defpart bracket-v2 ()
  :tolerance-standard :iso-2768-fine
  (:body (box :width (dim 100)))
  (:feature :cut (cylinder :d (dim 6 :fit :H7))))

;; 3. Add derived dimensions
(defpart bracket-v3 ()
  :tolerance-standard :iso-2768-fine
  (:body (box :width (dim 100)))
  (:feature :cut (cylinder :d (dim 6 :fit :H7)) :name :hole)
  (:dimension :hole-to-edge
    :measure (min-distance :hole (edges :extreme :max))
    :nominal (dim 20)
    :critical t))

;; 4. Tolerance analysis
(deftolerance-chain assembly
  (:parts (bracket bracket-v3) (spacer ...))
  (:chain ...)
  (:analysis :monte-carlo))
```

---

## Development Workflow

### Progressive Enhancement Pattern

```lisp
;; STAGE 1: Prototype - no tolerances
(defpart prototype ()
  (:body (box :width (dim 100))))

;; STAGE 2: Add tolerance standard
(defpart production-v1 ()
  :tolerance-standard :iso-2768-medium
  (:body (box :width (dim 100))))

;; STAGE 3: Tighten critical dimensions
(defpart production-v2 ()
  :tolerance-standard :iso-2768-medium
  (:body (box :width (dim 100 :tol ±0.1))))  ; Explicit

;; STAGE 4: Add ISO fits
(defpart production-v3 ()
  :tolerance-standard :iso-2768-medium
  (:body (box :width (dim 100 :tol ±0.1)))
  (:feature :cut (cylinder :d (dim 6 :fit :H7))))

;; STAGE 5: Full analysis
(defpart production-final ()
  :tolerance-standard :iso-2768-medium
  (:body (box :width (dim 100 :tol ±0.1)))
  (:feature :cut (cylinder :d (dim 6 :fit :H7)) :name :hole)
  (:dimension :hole-position
    :measure (distance :hole (edge :extreme :min))
    :nominal (dim 20)
    :tolerance ±0.2))
```

---

## Conclusion

This enhanced design integrates units and tolerance analysis without compromising CLAD's core vision. The system maintains its fundamental principles:

**Progressive Complexity**:
```lisp
;; Simple: Just works
(dim 100)

;; Intermediate: Add what you need
(dim 100 :tol ±0.1)

;; Advanced: Full capability
(dim 30 :mm :fit :H7)
```

**Optional Enhancement**:
- Tolerances completely optional
- Units have smart defaults
- Analysis separate from parts
- Each layer adds capability without complexity

**Workflow Preservation**:
- REPL-driven development unchanged
- Auto-rebuild works with tolerances
- Visualization enhanced with tolerance info
- Export works with or without tolerances

The system scales from quick prototypes to production parts with full tolerance analysis, maintaining the source-code-as-CAD-model philosophy throughout.

---

**Next Steps**: Begin Phase 1, including basic units system with compile-time conversion.
