# CLAD Selector Syntax Reference

## Quick Reference

### Direction Selectors

Selects faces based on their orientation and position.

**Syntax:**
```lisp
:direction <AXIS> :extreme <EXTREME>
```

**Parameters:**
- `<AXIS>` - Which direction to look: `:+x`, `:-x`, `:+y`, `:-y`, `:+z`, `:-z`
- `<EXTREME>` - Which extreme face: `:max` (furthest/highest) or `:min` (nearest/lowest)

**Examples:**

```lisp
;; Top face (highest Z)
(:on-face :direction :+z :extreme :max
  (:cut (make-cylinder 5 10)))

;; Bottom face (lowest Z)
(:on-face :direction :-z :extreme :min
  (:add (make-box 10 10 2)))

;; Right side (maximum X)
(:on-face :direction :+x :extreme :max
  (:cut (make-hole 6)))

;; Left side (minimum X)
(:on-face :direction :-x :extreme :min
  (:add (make-tab 20 5)))

;; Front face (maximum Y)
(:on-face :direction :+y :extreme :max
  (:cut (make-slot 8 20)))

;; Back face (minimum Y)
(:on-face :direction :-y :extreme :min
  (:add (make-bracket 15 10)))
```

---

## Understanding the Syntax

### It's a Key-Value Pair!

Think of selector syntax as a property list (plist):

```lisp
:direction :+z    ; KEY: direction, VALUE: +z
:extreme :max     ; KEY: extreme, VALUE: max
```

**NOT two separate selectors!**

### Common Confusion

❌ **WRONG:** "Should I use `:max` or `:extreme`?"
✅ **RIGHT:** "Use `:extreme :max` together as a key-value pair"

---

## Why Both `:direction` AND `:extreme`?

At first glance, `:direction :+z :extreme :max` might seem redundant - doesn't "pointing up" mean "highest"? **Not always!**

### The Two Parameters Serve Different Purposes:

- **`:direction`** - Filters by **surface normal** (which way face points)
- **`:extreme`** - Filters by **position in space** (coordinate value)

### Example: Simple Box (Seems Redundant)

```
     ┌─────┐  ← Top: points +Z AND at max Z
     │     │
     └─────┘  ← Bottom: points -Z AND at min Z
```

For a simple box, yes, they seem to overlap.

### Example: Stepped Geometry (NOT Redundant!)

```
    ┌─────┐  ← Face A: points +Z, Z=50 (highest)
    │     │
  ┌─┴─────┴─┐  ← Face B: ALSO points +Z, but Z=30!
  │         │
  └─────────┘  ← Bottom: points -Z, Z=0
```

**Now the distinction matters:**

```lisp
;; Select ONLY the highest upward-pointing face (Face A)
(:on-face :direction :+z :extreme :max ...)

;; Select ONLY the lowest upward-pointing face (Face B)
(:on-face :direction :+z :extreme :min ...)

;; Select ALL horizontal faces (both A and B)
(:on-face :parallel :z ...)
```

### Real-World Use Cases:

**1. Mounting Bracket (L-shape)**
```
      │      ← Vertical part top (points +Z, at Z=50)
      │
──────┴──────  ← Horizontal part top (points +Z, at Z=20)
```

Both surfaces point upward (+Z), but `:extreme :max` selects only the higher one.

**2. Pocket in a Plate**
```
┌─────────────┐  ← Top surface (points +Z, at Z=10)
│   ┌─────┐   │
│   │ ┌─┐ │   │  ← Nested pocket bottoms (all point +Z!)
│   │ └─┘ │   │     but at different Z heights
│   └─────┘   │
└─────────────┘  ← Bottom (points -Z, at Z=0)
```

Multiple faces point upward, `:extreme` selects which one by height.

**3. Angled Parts**
```
    ┌────────/  ← Angled face (points +Z AND +X)
    │       /
    └──────/    ← Not at max Z, but still points upward
```

Face might point partially upward but not be at the maximum Z coordinate.

### Summary

The combination provides **precise control**:
- `:direction` alone → "all faces pointing this way"
- `:extreme` alone → "face at this position" (less common)
- **Both together** → "the face pointing this way, at this position"

This is essential for:
- ✅ Stepped geometries
- ✅ L-brackets and complex shapes
- ✅ Nested features (pockets, bosses)
- ✅ Assemblies with overlapping parts
- ✅ Angled or rotated components

---

## All 6 Directions Visualized

```
        +Z (top)
         ↑
         |
         |
-X ←-----+-----→ +X
(left)   |   (right)
         |
         ↓
        -Z (bottom)

      (looking from +Y / front)
```

3D View:
```
         +Z
          ↑
          |
          |
    ------+------
   /      |     /|
  /       |    / |
 /-------------/  |
|         +Y  |   |
|        ↗    |   /
|       /     |  /
|      /      | /
|------------+/
     -X ← → +X
```

---

## Complete Examples by Direction

### Top Face (+Z, max)
```lisp
(:on-face :direction :+z :extreme :max
  (:add (make-cylinder 15 10))           ; Add boss
  (:cut (make-cylinder 3 20))            ; Cut hole
  (:circular-pattern :count 4 :radius 30 ; Pattern holes
    (:cut (make-cylinder 3 15))))
```

### Bottom Face (-Z, min)
```lisp
(:on-face :direction :-z :extreme :min
  (:add (make-box 80 3 5))               ; Support rib
  (:add (make-box 3 80 5)))              ; Cross rib
```

### Right Side (+X, max)
```lisp
(:on-face :direction :+x :extreme :max
  (:cut (rotate (make-cylinder 4 12) :y 90)))  ; Side hole
```

### Left Side (-X, min)
```lisp
(:on-face :direction :-x :extreme :min
  (:add (make-box 20 30 5)))             ; Mounting tab
```

### Front Face (+Y, max)
```lisp
(:on-face :direction :+y :extreme :max
  (:linear-pattern :count 3 :spacing 15
    (:cut (make-box 8 12 3))))           ; Vent slots
```

### Back Face (-Y, min)
```lisp
(:on-face :direction :-y :extreme :min
  (:add (make-box 25 8 4)))              ; Back tab
```

---

## Boolean Selector Combinators

CLAD supports logical combination of selectors using AND, OR, and NOT operators for complex selection queries.

### AND Combinator

Selects entities that match **ALL** criteria:

**Syntax:**
```lisp
:and <selector1> <selector2> ...
```

**Examples:**

```lisp
;; Select planar faces pointing upward
(:on-face :and :type :plane :direction :+z
  (:fillet 2.0))

;; Select large planar faces pointing upward
(:on-face :and :type :plane
               :direction :+z
               :area :> 5000.0
  (:cut (make-cylinder 5 10)))

;; Select vertical straight edges
(:on-edge :and :type :line :parallel :z
  (:chamfer 1.0))

;; Three criteria: planar AND upward AND large
(:on-face :and :type :plane
               :direction :+z :extreme :max
               :area :> 1000.0
  (:pattern ...))
```

**Use Cases:**
- Filtering by multiple geometric properties
- Combining type, direction, and size constraints
- Precise feature selection on complex geometry

### OR Combinator

Selects entities that match **ANY** criteria:

**Syntax:**
```lisp
:or <selector1> <selector2> ...
```

**Examples:**

```lisp
;; Select edges parallel to X OR Z (all horizontal/vertical edges)
(:on-edge :or :parallel :x :parallel :z
  (:fillet 2.0))

;; Select top OR bottom faces
(:on-face :or :direction :+z :extreme :max
              :direction :-z :extreme :min
  (:add (make-pattern ...)))

;; Select cylindrical faces OR faces with large area
(:on-face :or :type :cylinder
              :area :> 5000.0
  (:texture "brushed-metal"))
```

**Use Cases:**
- Applying operations to multiple face/edge types
- Selecting from multiple directions simultaneously
- Batch operations on different geometric features

### NOT Combinator

Selects entities that **DO NOT** match the criteria:

**Syntax:**
```lisp
:not <selector>
```

**Examples:**

```lisp
;; Select all faces EXCEPT cylindrical ones (all planar faces)
(:on-face :not :type :cylinder
  (:fillet 2.0))

;; Select all faces EXCEPT the top face
(:on-face :not :direction :+z :extreme :max
  (:chamfer 1.0))

;; Select all edges EXCEPT vertical ones
(:on-edge :not :parallel :z
  (:round 0.5))
```

**Use Cases:**
- Exclude specific features from operations
- "Everything except..." selections
- Inverse selection patterns

### Nested Combinators

Combinators can be nested for complex logic:

**Examples:**

```lisp
;; Planar faces that are either top OR bottom
(:on-face :and :type :plane
               :or :direction :+z :extreme :max
                   :direction :-z :extreme :min
  (:texture "grip-pattern"))

;; Vertical edges parallel to X OR Y (but NOT Z)
(:on-edge :and :not :parallel :z
               :or :parallel :x :parallel :y
  (:chamfer 1.0))

;; Large faces that are NOT cylindrical
(:on-face :and :not :type :cylinder
               :area :> 1000.0
  (:add (make-boss ...)))
```

### Combinator Tips

**1. Order Doesn't Matter (within same combinator)**
```lisp
;; These are equivalent:
:and :type :plane :direction :+z
:and :direction :+z :type :plane
```

**2. Use Parentheses for Clarity (in nested cases)**
```lisp
;; Helps readability:
(:on-face :and :type :plane
               (:or :direction :+z :direction :-z)
  ...)
```

**3. Test Selectors Individually First**
```lisp
;; Test each part:
(:on-face :type :plane ...)        ; Does this work?
(:on-face :direction :+z ...)      ; Does this work?
;; Then combine:
(:on-face :and :type :plane :direction :+z ...)
```

**4. Use NOT Carefully**
```lisp
;; Be explicit:
:not :type :cylinder  ; Selects everything EXCEPT cylinders

;; Not the same as:
:type :plane          ; Only selects planes (misses other types)
```

---

## Position-Based Selectors

Select shapes based on their position in 3D space.

### At-Position Selectors

Select faces/edges at a specific coordinate:

**Syntax:**
```lisp
:at-x <value> :tolerance <tolerance>
:at-y <value> :tolerance <tolerance>
:at-z <value> :tolerance <tolerance>
```

**Parameters:**
- `<value>` - Coordinate value in mm
- `:tolerance` - Optional tolerance (default: 0.01mm)

**Examples:**

```lisp
;; Select faces at Z = 50mm
(:on-face :at-z 50.0
  (:cut (clad.core:make-cylinder 5 10)))

;; Select faces at Z = 50mm with 0.5mm tolerance
(:on-face :at-z 50.0 :tolerance 0.5
  (:add (clad.core:make-box 20 20 5)))

;; Select edges at X = -25mm
(:on-edge :at-x -25.0
  (:fillet 2.0))
```

**Use Cases:**
- Select features at specific heights in stepped geometry
- Target faces at precise positions
- Work with imported geometry where positions are known

### Range Selectors

Select shapes within a coordinate range:

**Syntax:**
```lisp
:between-x <min> <max>
:between-y <min> <max>
:between-z <min> <max>
```

**Parameters:**
- `<min>` - Minimum coordinate value
- `<max>` - Maximum coordinate value

**Examples:**

```lisp
;; Select all faces between Z=10 and Z=40
(:on-face :between-z 10.0 40.0
  (:chamfer 1.5))

;; Select edges in the middle third of the part (X direction)
(:on-edge :between-x 33.0 66.0
  (:round 0.5))

;; Combined with other selectors
(:on-face :and :type :plane
               :between-z 20.0 80.0
  (:texture "brushed"))
```

**Use Cases:**
- Select features in specific regions
- Apply operations to middle sections
- Batch processing of features in a range

### Bounding Box Selector

Select shapes within a 3D box region:

**Syntax:**
```lisp
:within-box '(min-x min-y min-z) '(max-x max-y max-z)
```

**Parameters:**
- First list: Minimum corner coordinates
- Second list: Maximum corner coordinates

**Examples:**

```lisp
;; Select faces within a centered box
(:on-face :within-box '(-25 -25 0) '(25 25 50)
  (:fillet 3.0))

;; Select edges in upper-right quadrant
(:on-edge :within-box '(0 0 20) '(100 100 100)
  (:chamfer 2.0))

;; Combined with type selector
(:on-face :and :type :plane
               :within-box '(10 10 10) '(90 90 90)
  (:add (clad.core:make-cylinder 5 10)))
```

**Use Cases:**
- Spatial filtering of features
- Region-specific operations
- Working with sub-assemblies or components

### Proximity Selector

Select shapes near a point:

**Syntax:**
```lisp
:near-point '(x y z) :radius <distance>
```

**Parameters:**
- Point: List of coordinates `'(x y z)`
- `:radius` - Search radius (default: 10mm)

**Examples:**

```lisp
;; Select faces near the origin
(:on-face :near-point '(0 0 0) :radius 50.0
  (:add (clad.core:make-sphere 10)))

;; Select edges near a specific point
(:on-edge :near-point '(100 50 25) :radius 10.0
  (:fillet 2.0))

;; Find faces near assembly mating point
(:on-face :and :type :plane
               :near-point '(75 75 20) :radius 15.0
  (:cut-circle 6 :depth 12))
```

**Use Cases:**
- Select features around connection points
- Find geometry near datum points
- Spatial queries for feature placement

### Combining Position Selectors

Position selectors work with Boolean combinators:

**Examples:**

```lisp
;; Planar faces at top OR bottom
(:on-face :and :type :plane
               (:or :at-z 0.0 :tolerance 0.1
                    :at-z 100.0 :tolerance 0.1)
  (:finish "smooth"))

;; Vertical edges NOT at the base
(:on-edge :and :parallel :z
               :not :at-z 0.0 :tolerance 1.0
  (:fillet 5.0))

;; Faces in box AND near point
(:on-face :and :within-box '(0 0 0) '(50 50 50)
               :near-point '(25 25 25) :radius 20.0
  (:add ...))
```

---

## Other Selector Types

### Parallel Selector
Selects faces parallel to an axis:

```lisp
:parallel :z      ; All faces parallel to Z axis
:parallel :x      ; All faces parallel to X axis
:parallel :y      ; All faces parallel to Y axis
```

Example:
```lisp
(:on-face :parallel :z
  (:cut (make-cylinder 2 15)))  ; Cuts on all horizontal faces
```

### Perpendicular Selector
Selects faces perpendicular to an axis:

```lisp
:perpendicular :z  ; All vertical faces
:perpendicular :x  ; All faces perpendicular to X
:perpendicular :y  ; All faces perpendicular to Y
```

---

## Combining Selectors

You can use multiple `:on-face` blocks:

```lisp
(defpart multi-feature-plate ((size 100))
  (:body (make-box size size 10))

  ;; Feature on top
  (:on-face :direction :+z :extreme :max
    (:cut (make-cylinder 5 15)))

  ;; Feature on bottom
  (:on-face :direction :-z :extreme :min
    (:add (make-box 20 20 3)))

  ;; Feature on sides
  (:on-face :direction :+x :extreme :max
    (:cut (make-hole 4))))
```

---

## Edge Selectors

Similar syntax for edges:

```lisp
(:on-edge :direction :+z :extreme :max
  (fillet 2))  ; Fillet top edges

(:on-edge :parallel :z
  (chamfer 1)) ; Chamfer all vertical edges
```

---

## Pattern Integration

Patterns work with any selector:

```lisp
;; Circular pattern on top
(:on-face :direction :+z :extreme :max
  (:circular-pattern :count 6 :radius 30
    (:cut (make-cylinder 3 10))))

;; Linear pattern on front
(:on-face :direction :+y :extreme :max
  (:linear-pattern :count 4 :spacing 20
    (:cut (make-box 5 10 3))))

;; Grid pattern on bottom
(:on-face :direction :-z :extreme :min
  (:grid-pattern :count-x 3 :count-y 3 :spacing-x 25 :spacing-y 25
    (:cut (make-cylinder 2 5))))
```

---

## Common Patterns

### Mounting Holes at Corners
```lisp
(:on-face :direction :+z :extreme :max
  (:circular-pattern :count 4 :radius 40 :angle-start 45 :angle-end 315
    (:cut (make-cylinder 3 15))))
```

### Ventilation Slots
```lisp
(:on-face :direction :+y :extreme :max
  (:linear-pattern :count 5 :spacing 12
    (:cut (make-box 6 15 2))))
```

### Support Ribs
```lisp
(:on-face :direction :-z :extreme :min
  (:add (make-box 80 2 4))   ; Horizontal rib
  (:add (make-box 2 80 4)))  ; Vertical rib
```

---

## Thread Modeling

CLAD includes built-in support for standard threaded features (bolts, holes, nuts) for mechanical assemblies.

### External Threads (Bolts, Studs)

Create external threads using `:add` operation:

```lisp
;; Add M6 external thread on top face
(:on-face :direction :+z :extreme :max
  (:add (clad.features:make-external-thread :m6 :length 30)))

;; Add M8 thread boss
(defpart threaded-boss ((diameter 16) (boss-height 25) (thread-length 20))
  (:body (clad.core:make-box 50 50 10))
  (:on-face :direction :+z :extreme :max
    (:add (clad.core:make-cylinder (/ diameter 2) boss-height))
    (:add (clad.features:make-external-thread :m8 :length thread-length))))
```

### Internal Threads (Threaded Holes)

Create internal threads using `:cut` operation:

```lisp
;; Cut M6 threaded hole on top face
(:on-face :direction :+z :extreme :max
  (:cut (clad.features:make-internal-thread :m6 :depth 25)))

;; Tapped holes with circular pattern
(defpart mounting-plate ((hole-count 4))
  (:body (clad.core:make-box 100 100 10))
  (:on-face :direction :+z :extreme :max
    (:circular-pattern :count hole-count :radius 35
      (:cut (clad.core:make-cylinder 2.5 12))  ; Clearance hole through
      (:cut (clad.features:make-internal-thread :m6 :depth 15)))))  ; Thread
```

### Available Thread Standards

**ISO Metric:**
- `:m3`, `:m6`, `:m8`, `:m10`

**ISO Metric Fine:**
- `:m8x1.0`, `:m10x1.25`

**Unified (UNC/UNF):**
- `:1/4-20`

**Query Available Threads:**
```lisp
(clad.features:list-thread-specs)
;; => (:M3 :M6 :M8 :M10 :M8X1.0 :M10X1.25 :1/4-20)
```

### Thread Calculations

**Tap Drill Sizing:**
```lisp
;; Calculate correct drill size for M6 tap
(clad.features:tap-drill-size :m6)
;; => 5.0 mm

;; Create hole with proper tap drill
(:on-face :direction :+z :extreme :max
  (:cut (clad.core:make-cylinder
          (/ (clad.features:tap-drill-size :m8) 2)
          20)))
```

**Minor Diameter:**
```lisp
;; Get thread minor diameter (root diameter)
(clad.features:thread-minor-diameter :m6)
;; => 4.917 mm
```

### Thread DSL Examples

**Bolt with Head:**
```lisp
(defpart hex-bolt ((thread-spec :m8) (thread-length 30) (head-size 13))
  (:body (clad.core:make-cylinder (/ head-size 2) 5))  ; Head
  (:on-face :direction :-z :extreme :min
    (:add (clad.features:make-external-thread thread-spec :length thread-length))))
```

**Threaded Insert:**
```lisp
(defpart threaded-insert-boss ((outer-dia 12) (thread-spec :m6))
  (:body (clad.core:make-cylinder (/ outer-dia 2) 20))
  (:on-face :direction :+z :extreme :max
    (:cut (clad.features:make-internal-thread thread-spec :depth 18))))
```

**Assembly Mating Holes:**
```lisp
(defpart mating-plate ((spacing 80))
  (:body (clad.core:make-box 100 100 8))
  (:on-face :direction :+z :extreme :max
    ;; Corner clearance holes for bolts
    (:circular-pattern :count 4 :radius (/ spacing 2) :angle-start 45 :angle-end 315
      (:cut (clad.core:make-cylinder 4.5 10)))))  ; M8 clearance
```

### Engineering Notes

**Cosmetic vs. Detailed:**
- Current implementation uses cosmetic threads (cylinders at major diameter)
- Suitable for assemblies where exact thread form isn't critical
- Fast geometry generation and lightweight exports

**Thread Engagement:**
- For blind holes, use depth ≥ 1.5× major diameter for full strength
- Example: M6 thread needs ≥ 9mm depth

**Clearance Holes:**
- Use major diameter + 0.5mm for bolt clearance
- M6 bolt → 6.5mm hole, M8 bolt → 8.5mm hole

---

## Tips & Tricks

### 1. Visualize Coordinates
Always think: "Which face do I want to select?"
- Top? → `:direction :+z :extreme :max`
- Bottom? → `:direction :-z :extreme :min`
- Right? → `:direction :+x :extreme :max`

### 2. Consistent Axis Convention
CLAD uses:
- **+Z** = UP (standard CAD convention)
- **+X** = RIGHT
- **+Y** = FORWARD/FRONT

### 3. Test One Direction First
When learning, add features to just one face first, then expand to others.

### 4. Use Comments
```lisp
;; Top features
(:on-face :direction :+z :extreme :max ...)

;; Bottom features
(:on-face :direction :-z :extreme :min ...)
```

---

## Troubleshooting

### "My selector isn't working!"

**Check:**
1. Is the syntax correct? `:direction :+z :extreme :max` (not `:direction :+z :max`)
2. Does the face exist? (Did you create a box/part first?)
3. Are you using the right axis? (+X vs -X, etc.)

**Debug:**
```lisp
;; Test with a simple cut to verify selector
(:on-face :direction :+z :extreme :max
  (:cut (make-cylinder 5 20)))  ; Should appear on top
```

### "Features appear on wrong face"

Check your coordinate system:
- Rotate the model in the viewer
- Verify which face is actually "+Z" in your part
- Remember: +Z is UP by default

---

## Summary

**Key Concept:** `:extreme :max` is a KEY-VALUE pair, not two separate things!

```lisp
:direction <AXIS> :extreme <MIN/MAX>
   ↑       ↑        ↑       ↑
  KEY    VALUE     KEY    VALUE
```

**All 6 Faces:**
- Top: `:direction :+z :extreme :max`
- Bottom: `:direction :-z :extreme :min`
- Right: `:direction :+x :extreme :max`
- Left: `:direction :-x :extreme :min`
- Front: `:direction :+y :extreme :max`
- Back: `:direction :-y :extreme :min`

---

**See also:**
- `examples/interactive-demo.lisp` - Shows all 6 directions in action
- `examples/mounting-bracket.lisp` - Real-world examples
- Design guide Phase 3 for advanced selectors
