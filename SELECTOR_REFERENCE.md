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
