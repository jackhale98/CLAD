# Using Phase 8 Advanced Features in CLAD DSL

## Summary

Phase 8 advanced features (sweeps, lofts, mirroring) are **fully functional** and can be used in `defpart` definitions today! You have two approaches:

### ✅ **Available Today** (No Changes Needed)

Use the advanced features directly in your `:body` and `:add` forms:

```lisp
(clad.dsl:defpart my-part ()
  "Part using advanced features"
  ;; Use loft, sweep, or pipe in :body
  (:body
    (let* ((base (clad.core:make-circle-wire '(0 0 0) 20))
           (top (clad.core:make-circle-wire '(0 0 30) 10)))
      (clad.core:make-loft (list base top) :solid t)))

  ;; Add swept features on selected faces
  (:on-face :direction :+z :extreme :max
    (:add
      (let ((path (clad.core:make-spline '((15 0 30) (25 0 30)) :closed nil)))
        (clad.core:make-pipe path 3)))))
```

### 🔮 **Future Enhancement** (Optional)

For more declarative syntax, we could add native DSL forms like:

```lisp
;; Proposed future syntax (not yet implemented)
(:loft :sections (section1 section2 section3) :solid t)
(:sweep :profile profile-wire :path path-wire)
(:pipe :path path-wire :radius 3)
(:mirror :plane-origin (0 0 0) :plane-normal (1 0 0))
```

---

## Current Usage Patterns

### 1. Loft in :body

```lisp
(clad.dsl:defpart lofted-vase ((base-radius 20) (top-radius 5) (height 30))
  "Create a vase using loft"
  (:body
    (let* ((base (clad.core:make-circle-wire '(0 0 0) base-radius))
           (mid (clad.core:make-circle-wire `(0 0 ,(/ height 2))
                                             (/ (+ base-radius top-radius) 2)))
           (top (clad.core:make-circle-wire `(0 0 ,height) top-radius)))
      (clad.core:make-loft (list base mid top) :solid t :ruled nil))))
```

### 2. Pipe Sweep in :body

```lisp
(clad.dsl:defpart curved-tube ((radius 3))
  "Create curved tube"
  (:body
    (let ((path (clad.core:make-spline '((0 0 0) (10 10 10) (20 5 15))
                                        :closed nil)))
      (clad.core:make-pipe path radius))))
```

###3. Profile Sweep in :body

```lisp
(clad.dsl:defpart swept-part ()
  "Sweep custom profile along path"
  (:body
    (let* ((profile (clad.core:make-circle-wire '(0 0 0) 5 :axis '(1 0 0)))
           (path (clad.core:make-spline '((0 0 0) (20 5 0) (40 0 5)) :closed nil)))
      (clad.core:make-sweep profile path))))
```

### 4. Mirroring with Helper Function

```lisp
(defun make-symmetric-bracket (width height)
  (let* ((half (clad.core:make-box (/ width 2) height 10))
         (mirrored (clad.core:mirror-shape half '(0 0 0) '(1 0 0))))
    (clad.core:fuse-shapes half mirrored)))

(clad.dsl:defpart bracket ((width 60) (height 40))
  "Symmetric bracket"
  (:body (make-symmetric-bracket width height))

  ;; Add features to the symmetric part
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate (clad.core:make-cylinder 5 15) 0 (/ height 2) 0))))
```

### 5. Combining Advanced Features with Boolean Ops

```lisp
(clad.dsl:defpart composite-part ()
  "Combine loft with traditional DSL operations"
  ;; Start with lofted base
  (:body
    (let ((base (clad.core:make-circle-wire '(0 0 0) 15))
          (top (clad.core:make-circle-wire '(0 0 20) 10)))
      (clad.core:make-loft (list base top) :solid t)))

  ;; Add a swept pipe feature
  (:on-face :direction :+z :extreme :max
    (:add
      (clad.core:make-pipe
        (clad.core:make-spline '((15 0 20) (25 0 20) (25 10 20)) :closed nil)
        3)))

  ;; Cut a hole
  (:on-face :direction :+z :extreme :max
    (:cut (clad.core:translate (clad.core:make-cylinder 4 25) 0 0 -5))))
```

---

## API Reference

### Available Functions

```lisp
;; Loft
(clad.core:make-loft sections &key (solid t) (ruled nil))
  ;; SECTIONS - list of wire shapes
  ;; SOLID - t for solid, nil for shell
  ;; RULED - t for ruled surface, nil for smooth

;; Pipe (circular profile sweep)
(clad.core:make-pipe path radius)
  ;; PATH - wire or edge shape
  ;; RADIUS - radius of circular profile

;; Profile Sweep
(clad.core:make-sweep profile path)
  ;; PROFILE - wire shape to sweep
  ;; PATH - wire or edge to sweep along

;; Mirroring
(clad.core:mirror-shape shape plane-origin plane-normal)
  ;; SHAPE - shape to mirror
  ;; PLANE-ORIGIN - (x y z) point on plane
  ;; PLANE-NORMAL - (nx ny nz) normal vector

;; Helper: Create circular wire
(clad.core:make-circle-wire center radius &key (axis '(0 0 1)))
  ;; CENTER - (x y z) center point
  ;; RADIUS - circle radius
  ;; AXIS - (x y z) normal vector (default Z-axis)
```

---

## Examples

See the complete working examples in:
- `examples/advanced-sweeps-lofts-mirror.lisp` - Core API examples
- `examples/dsl-advanced-features-demo.lisp` - DSL integration examples

To run:
```lisp
(load "examples/dsl-advanced-features-demo.lisp")
```

---

## Benefits of Current Approach

✅ **Works immediately** - No DSL modifications needed
✅ **Full power** - Access to all API parameters
✅ **Flexible** - Combine with any Lisp code
✅ **Type-safe** - Compile-time checking of arguments
✅ **Composable** - Mix with existing DSL features

---

## Future DSL Integration (Optional)

If you want more declarative syntax, we added a `:mirror` top-level form to `defpart` that you can use like:

```lisp
(:mirror :plane-origin (0 0 0) :plane-normal (1 0 0))
```

This will mirror the current shape and add it to the result.

To add similar forms for `:loft`, `:sweep`, and `:pipe`, we would need to extend the `expand-feature-form-at-compile-time` function in `src/dsl/defpart.lisp`. This is straightforward but optional since the current approach works well.

---

## Recommendation

**Use the current approach!** The ability to call advanced features directly in `:body` and `:add` forms gives you maximum flexibility while keeping the DSL clean and simple. The declarative patterns work beautifully with Common Lisp's let-binding and function composition.

Save native DSL forms for features that would benefit from implicit context (like fillet/chamfer which apply to selected edges). Advanced shape creation functions work great as explicit function calls.
