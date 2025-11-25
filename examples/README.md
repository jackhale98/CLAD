# CLAD DSL Examples

This directory contains comprehensive examples demonstrating the CLAD DSL (Domain-Specific Language) for parametric CAD modeling.

## Quick Start

Load any example file in your Common Lisp REPL:

```lisp
(load "examples/01-basic-dsl.lisp")
```

Each example file is self-contained and will run demonstrations automatically when loaded.

## Example Files

### 01-basic-dsl.lisp - Fundamentals
**What you'll learn:**
- Defining parametric parts with `defpart`
- Using parameters for configurability
- Boolean operations (`:add`, `:cut`)
- Face selection (`:on-face`, `:direction`, `:extreme`)

**Examples:**
1. Simple parametric box with width/height/depth parameters
2. Box with centered through-hole (boolean subtraction)
3. Base with boss and hole (multiple operations)
4. Bracket with tabs (face selection in all directions)

**Run:**
```lisp
(load "examples/01-basic-dsl.lisp")
(clad:view (simple-box) :name "simple-box")
```

### 02-patterns.lisp - Repetitive Features
**What you'll learn:**
- Linear patterns (`:linear-pattern`)
- Circular patterns (`:circular-pattern`)
- Partial arc patterns
- Combining multiple patterns
- Grid patterns using multiple linear patterns

**Examples:**
1. Flange with linearly spaced bolt holes
2. Circular mounting plate with radial holes
3. Arc bracket with partial circular pattern (90°)
4. Combined linear and circular patterns
5. 2D grid using multiple separate linear patterns

**Note:** Nested patterns are not supported - use multiple separate patterns instead.

**Run:**
```lisp
(load "examples/02-patterns.lisp")
(clad:view (mounting-plate-circular) :name "circular-plate")
```

### 03-fillets-chamfers.lisp - Edge Finishing
**What you'll learn:**
- Applying fillets with context API
- Selecting edges (`:parallel`, `:type`)
- Chamfering edges
- Combining fillets and chamfers
- Strategic edge finishing

**Examples:**
1. Simple fillet on vertical edges
2. Selective filleting (top edges only)
3. Chamfering vertical edges
4. Mixed fillets and chamfers
5. Practical filleted mounting bracket

**Run:**
```lisp
(load "examples/03-fillets-chamfers.lisp")
;; Returns multiple parts (part1-5)
(clad:view part1 :name "simple-fillet")
```

### 04-advanced-features.lisp - Complex Geometry
**What you'll learn:**
- Lofting between profiles (`make-loft`)
- Sweeping along paths (`make-sweep`)
- Pipe creation (`make-pipe`)
- Multi-section lofts
- Combining advanced features with booleans

**Examples:**
1. Simple loft from square to circle
2. Multi-section loft through 4 profiles
3. Sweep circular profile along spline path
4. Pipe following curved spline
5. Hollow funnel using two lofts
6. Swept ergonomic handle
7. HVAC transition duct (rectangle to circle)

**Run:**
```lisp
(load "examples/04-advanced-features.lisp")
(clad:view part5 :name "funnel")
```

### 05-assemblies.lisp - Multi-Part Assemblies
**What you'll learn:**
- Creating assemblies (`make-assembly`)
- Adding components with positioning
- Mate constraints (coincident, concentric, distance, parallel)
- Nested sub-assemblies
- Component metadata for BOM generation
- Parametric assemblies

**Examples:**
1. Simple two-part assembly
2. Assembly with mate constraints
3. Nested multi-level assembly
4. Multiple constraint types
5. Assembly with metadata for BOM
6. Parametric assembly with adjustable parameters

**Run:**
```lisp
(load "examples/05-assemblies.lisp")
;; Returns multiple assemblies (assy1-6)
```

### 06-advanced-selectors.lisp - Advanced Selector System
**What you'll learn:**
- Direction-based selection (`:direction`)
- Extreme position selection (`:extreme`)
- Type-based edge selection (`:type`)
- Parallel edge selection (`:parallel`)
- Combined selectors (AND logic)
- Strategic feature placement

**Examples:**
1. Direction and extreme selectors (top/bottom faces)
2. Type-based selection (line vs circle edges)
3. Parallel edge selection (X, Y, Z axes)
4. Combined selectors for precision
5. Real-world application examples

**Run:**
```lisp
(load "examples/06-advanced-selectors.lisp")
;; Returns multiple parts (part1-4)
(clad:view part1 :name "direction-extreme")
```

### 06-advanced-selectors-showcase.lisp - Comprehensive Selector Showcase
**What you'll learn:**
- Boolean combinators (`:and`, `:or`, `:not`)
- Position-based selectors (`:at-z`, `:between-x`, etc.)
- Face-plane operations (`:on-face-plane`)
- 2D operations on faces (`:cut-circle`, `:add-rectangle`)
- Pattern integration with face-planes (`:grid-pattern`, `:circular-pattern`)
- Debugging and inspection tools

**Examples:**
1. Servo mounting bracket with face-plane operations
2. Interactive demonstration function
3. Boolean combinator examples
4. Position-based selection showcase
5. Real-world robotics mounting bracket design

**Features demonstrated:**
- Phase 1: Boolean combinators (AND/OR/NOT)
- Phase 2: Position-based selectors
- Phase 3: Debugging tools (inspect-selection)
- Phase 4: Face-plane operations with patterns

**Run:**
```lisp
(load "examples/06-advanced-selectors-showcase.lisp")
(clad:view (servo-mounting-bracket) :name "servo-bracket")
(demonstrate-advanced-selectors)  ; Interactive demo
```

### 07-threads.lisp - Thread Modeling Tutorial
**What you'll learn:**
- Thread database overview (119 specifications)
- ISO Metric threads (coarse and fine pitch)
- UNC and UNF imperial threads
- External threads (bolts, studs)
- Internal threads (threaded holes)
- Thread DSL integration in defpart
- Left-handed and lead-in/lead-out threads
- Thread utilities (tap drill, minor diameter)
- Low-level thread API

**Examples:**
1. Simple threaded shaft with M6 thread
2. Block with M8 threaded hole
3. Parametric hex bolt
4. Parametric hex nut
5. Left-handed thread
6. Thread with smooth engagement (lead-in/out)
7. Dual-threaded shaft (M8 and M10)
8. UNC thread (1/4-20)
9. UNF thread (1/4-28)

**Run:**
```lisp
(load "examples/07-threads.lisp")
(view-thread-examples)  ; View all in browser
```

## Common Patterns

### Defining a Parametric Part

```lisp
(clad.dsl:defpart my-part
    ((width 100)    ; Default values
     (height 50)
     (depth 30))
  "Description of the part"
  (:body (clad.core:make-box width height depth))

  ;; Add features on selected faces
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:make-cylinder 10 (* depth 1.2)))))

;; Use with defaults
(my-part)

;; Override parameters
(my-part 200 75 40)
```

### Face Selection

```lisp
:direction :+z  ; Faces pointing in +Z direction
:direction :-x  ; Faces pointing in -X direction
:extreme :max   ; The furthest face in that direction
:extreme :min   ; The nearest face in that direction
```

### Pattern Operations

```lisp
;; Linear pattern
(:linear-pattern
    :count 5
    :spacing 20
    :direction-x 1    ; Unit vector for direction
    :direction-y 0
    :start-x 10       ; Starting position
    :start-y 10
  (:cut (clad.core:make-cylinder 3 15)))

;; Circular pattern
(:circular-pattern
    :count 8
    :radius 40
    :center-x 50
    :center-y 50
    :angle-start 0    ; Optional, defaults to 0
    :angle-end 360    ; Optional, defaults to 360
  (:add (clad.core:make-cylinder 5 20)))
```

### Context API Operations

```lisp
(clad.context:with-context ()
  ;; Create base shape
  (clad.context:add (clad.core:make-box 100 100 50))

  ;; Select edges by parallel direction
  (clad.context:select-edges :parallel :z)

  ;; Apply fillet
  (clad.context:fillet-selected 5.0d0)

  ;; Select edges by type
  (clad.context:select-edges :type :line)

  ;; Get result
  (clad.context:get-result))
```

### Advanced Selectors

```lisp
;; Direction + extreme position
(clad.context:select-faces :direction :+z :extreme :max)

;; Type-based selection
(clad.context:select-edges :type :line)
(clad.context:select-edges :type :circle)

;; Parallel to axis
(clad.context:select-edges :parallel :x)
(clad.context:select-edges :parallel :y)
(clad.context:select-edges :parallel :z)

;; Combined selectors (AND logic)
(clad.context:select-edges :type :line :parallel :x)
```

## Testing Examples

All examples can be tested by loading them:

```lisp
;; Run a specific example
(load "examples/02-patterns.lisp")

;; The demo functions are also available
(demo-circular-pattern)

;; Visualize the results
(clad:view (mounting-plate-circular) :name "my-plate")
```

## Example Output

Each example file prints informative output when loaded:

```
================================================================================
                      CLAD Pattern Examples
================================================================================

Example 1: Linear Pattern
=========================

Flange: 200x60x10mm with 5 bolt holes
Holes are evenly spaced along the length
Pattern parameters:
  count: 5 holes
  spacing: calculated for even distribution
  direction: along X-axis

...

================================================================================
All pattern examples completed!

To visualize:
  (clad:view (flange-linear) :name "flange")
  (clad:view (mounting-plate-circular) :name "circular-plate")
================================================================================
```

## Next Steps

After working through these examples:

1. **Modify parameters** - Try changing dimensions and counts
2. **Combine features** - Mix patterns with fillets/chamfers
3. **Create assemblies** - Build multi-part systems
4. **Export models** - Use `clad.export:export-step` and `clad.export:export-stl` to save designs
5. **Build your own** - Apply these patterns to your designs

## Advanced Features

Beyond these examples, CLAD includes powerful production-ready features:

### Thread Modeling
Create production-ready threaded features with accurate ISO 68-1 geometry:

```lisp
;; Using DSL integration (recommended)
(defpart threaded-shaft ()
  (cylinder :radius 3.0 :height 50.0)
  (thread :m6 :length 30.0 :type :external :position '(0 0 10.0)))

;; Low-level API for advanced control
(clad.features.helical-sweep:make-external-thread :m8 30.0)
(clad.features.helical-sweep:make-internal-thread :m6 20.0)

;; Thread utilities
(clad.features:tap-drill-size :m6)            ; => 5.0 mm
(clad.features:thread-designation-string :m8) ; => "M8.0 x 1.25"
(clad.features:list-threads-by-standard "UNC") ; => All UNC threads
```

**Comprehensive thread database (119 specifications):**
- ISO Metric Coarse: M1.6 to M64 (30 sizes)
- ISO Metric Fine: M3×0.35 to M30×2.0 (17 sizes)
- UNC: #0-80 to 2-4.5 (26 sizes)
- UNF: #0-80 to 1-1/2-12 (23 sizes)

**See also:** `07-threads.lisp` for complete tutorial

### Tolerancing & GD&T
Full geometric dimensioning and tolerancing per ASME Y14.5:
```lisp
;; Establish datum reference frame
(:datum "A" :on-face :direction :-z :extreme :min)

;; Form tolerances (flatness, straightness, circularity, cylindricity)
(:flatness :on-face :direction :-z :extreme :min :tolerance 0.05)

;; Orientation tolerances (perpendicularity, parallelism, angularity)
(:perpendicularity :on-face :direction :+z :extreme :max
                   :tolerance 0.1 :datum-ref "A")

;; Location tolerances (position, concentricity, symmetry)
(:position :on-face :type :cylindrical
           :tolerance 0.2 :datum-refs ("A" "B" "C") :mmc t)
```

Exports to STEP AP242 with full PMI (Product Manufacturing Information) for CNC machining.

See **USER_GUIDE.md** Section "Tolerancing and GD&T" for comprehensive documentation.

### STL Export for 3D Printing
Export parts with configurable mesh quality:
```lisp
;; Standard export (binary, medium resolution)
(clad.export:export-stl part "bracket.stl")

;; High detail for precise parts
(clad.export:export-stl gear "gear.stl" :resolution :high)

;; ASCII format for debugging
(clad.export:export-stl test "test.stl" :ascii t)

;; Ultra high resolution for tiny features
(clad.export:export-stl mini "mini.stl" :resolution :ultra)
```

**Resolution levels**: `:low` (fast), `:medium` (recommended), `:high` (detailed), `:ultra` (maximum)

**Format options**: Binary STL (default, smaller files) or ASCII STL (human-readable)

See **USER_GUIDE.md** Section "STL Export (3D Printing)" for comprehensive documentation.

## Additional Resources

- **Main Documentation**: See `/home/jack/projects/clad/README.md`
- **Test Files**: Check `/home/jack/projects/clad/tests/` for more examples
- **API Reference**: Browse source files in `/home/jack/projects/clad/src/`

## Tips

1. **Start simple** - Begin with 01-basic-dsl.lisp
2. **Experiment** - Change parameters and see results
3. **View in browser** - Use `clad:view` to see 3D models
4. **Read the code** - Examples are heavily commented
5. **Build incrementally** - Add features one at a time

## Troubleshooting

**Example won't load:**
```lisp
;; Make sure CLAD is loaded first
(asdf:load-system :clad)
```

**Can't visualize:**
```lisp
;; Start the viewer first
(clad:start-viewer)

;; Then view your part
(clad:view (my-part) :name "test")

;; Open browser to http://localhost:8080
```

**Parameter errors:**
```lisp
;; Check parameter count
(my-part)              ; Use defaults
(my-part 200)          ; Override first parameter
(my-part 200 100 50)   ; Override all three
```

## Contributing

Found a bug in an example? Have a suggestion for a new example?
Please file an issue or submit a pull request!

## Recent Updates

All example files have been tested and verified to work correctly:

- **02-patterns.lisp**: Fixed nested pattern usage (use separate patterns instead)
- **03-fillets-chamfers.lisp**: Removed geometry-problematic size-based example
- **04-advanced-features.lisp**: Fixed wire construction API (use `make-wire` with line segments)
- **05-assemblies.lisp**: Updated to use correct assembly API (metadata for descriptions)
- **06-advanced-selectors.lisp**: Comprehensive selector system showcase
- **06-advanced-selectors-showcase.lisp**: NEW - Boolean combinators, position selectors, and face-plane operations

---

**Generated:** November 2025
**CLAD Version:** 1.0.0
**Total Examples:** 40+ demonstrations across 7 files
