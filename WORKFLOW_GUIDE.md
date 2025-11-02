# Perfect REPL-Driven CAD Workflow with CLAD

## The Problem with `interactive-demo.lisp`

The interactive demo uses **global parameters** with explicit `:args`:

```lisp
(defparameter *boss-height* 15)

(clad.dsl:defpart demo-plate
    ((size 100) (boss-h 15) ...)  ; boss-h defaults to 15
  ...)

(clad:show 'demo-plate
           :args (list *base-size* ... *boss-height*))  ; Uses globals
```

**Problem:** When you change `*boss-height*` and C-c C-c on the defpart:
1. The defpart function signature still has `(boss-h 15)` as default
2. Auto-rebuild uses cached `:args` from the original `show` call
3. Your change to `*boss-height*` is ignored! ❌

## The Solution: `simple-plate.lisp`

Use **embedded parameters** with NO explicit `:args`:

```lisp
(clad.dsl:defpart mounting-plate
    ((boss-height 15))  ; Default value IN the defpart
  ...)

(clad:show 'mounting-plate)  ; No :args - uses defaults!
```

**How it works:**
1. Change `(boss-height 15)` to `(boss-height 25)` in your source
2. Press C-c C-c in Emacs (re-evaluates the defpart)
3. `defpart` macro calls `maybe-rebuild-current-part`
4. Auto-rebuild calls `rebuild` which uses the NEW function definition
5. Browser updates within 2 seconds! ✅

---

## Perfect Workflow: Step by Step

### 1. Initial Setup (Once)

Open Emacs/SLIME/Sly and start your REPL:

```lisp
CLAD> (load "examples/simple-plate.lisp")
;; Loads the file

CLAD> (quick-view)
;; Opens browser to http://localhost:8080/?model=/models/mounting-plate.glb
;; You see your part!
```

### 2. Live Development (Repeat Forever!)

Open `examples/simple-plate.lisp` in Emacs:

```lisp
(clad.dsl:defpart mounting-plate
    ((size 100)
     (thickness 10)
     (hole-diameter 6)
     (hole-count 8)
     (boss-diameter 30)
     (boss-height 15))      ; ← EDIT THIS
  "A simple mounting plate"

  (:body (clad.core:make-box size size thickness))

  (:on-face :direction :+z :extreme :max
    (:add (clad.core:translate
            (clad.core:make-cylinder (/ boss-diameter 2) boss-height)
            (/ size 2) (/ size 2) thickness)))
  ...)
```

**Edit:**
- Change `(boss-height 15)` to `(boss-height 25)`

**Press C-c C-c** (with cursor anywhere in the defpart)

**Watch:**
- REPL shows: `[Rebuilding... done in 0.234s]`
- Browser auto-reloads within 2 seconds!
- You see the taller boss! ✨

**That's it!** No function calls, no manual rebuilds, just code.

---

## Advanced: Multiple Parameters

Change multiple parameters at once:

```lisp
(clad.dsl:defpart mounting-plate
    ((size 120)              ; Changed from 100
     (thickness 15)          ; Changed from 10
     (boss-height 25))       ; Changed from 15
  ...)
```

Press C-c C-c → Everything updates!

---

## Advanced: Add New Features

Add a new feature to the part:

```lisp
(clad.dsl:defpart mounting-plate
    ((size 100) ...)
  "A simple mounting plate"

  (:body (clad.core:make-box size size thickness))

  ;; ... existing features ...

  ;; NEW: Add a slot on the side!
  (:on-face :direction :+x :extreme :max
    (:cut (clad.core:translate
            (clad.core:rotate
              (clad.core:make-box 8 12 3)
              :y 90)
            size (/ size 2) (/ thickness 2)))))
```

Press C-c C-c → New slot appears!

---

## Comparison: Old vs New Workflow

### Old Workflow (Global Parameters)

```lisp
;; 1. Edit global parameter
(defparameter *boss-height* 25)  ; C-c C-c

;; 2. Call a preset function
(preset-large)  ; Must execute

;; 3. Or manually rebuild
(clad:rebuild)  ; Must execute
```

**Issues:**
- ❌ Must call functions after every change
- ❌ Globals and defpart can get out of sync
- ❌ Easy to forget to rebuild
- ❌ Not pure REPL-driven

### New Workflow (Embedded Parameters)

```lisp
;; 1. Edit defpart directly
(clad.dsl:defpart mounting-plate
    ((boss-height 25))  ; Changed here
  ...)

;; 2. Press C-c C-c

;; 3. That's it!
```

**Benefits:**
- ✅ One action: C-c C-c
- ✅ Single source of truth
- ✅ Can't get out of sync
- ✅ Pure REPL-driven development
- ✅ True "immediate feedback" CAD

---

## Why This Works

The magic is in how `defpart` integrates with `show`:

```lisp
;; defpart macro (simplified)
(defmacro defpart (name params &body body)
  `(progn
     ;; Define the function
     (defun ,name ,lambda-list ...)

     ;; Trigger auto-rebuild if this is the current part
     (when (find-package :clad.auto-rebuild)
       (funcall (intern "MAYBE-REBUILD-CURRENT-PART" :clad.auto-rebuild)
               ',name))))
```

When you C-c C-c on a defpart:
1. Function gets redefined with **new parameter defaults**
2. `maybe-rebuild-current-part` checks: "Is this the current part?"
3. If yes: calls `rebuild`
4. `rebuild` calls the function with **NO args** (uses new defaults!)
5. New shape generated
6. Exported to GLB
7. Browser detects change via `Last-Modified` header
8. Auto-reloads within 2 seconds!

---

## Best Practices

### ✅ DO:
- Keep parameters in the defpart signature
- Use `(quick-view)` once to start
- Edit the defpart and C-c C-c to update
- Keep browser window visible while coding

### ❌ DON'T:
- Use global parameters with `:args`
- Call `show` multiple times with different `:args`
- Call `rebuild` manually (it happens automatically!)
- Close the browser and reopen (just keep it open!)

---

## Emacs/SLIME Keybindings

| Key     | Action                          | Effect                        |
|---------|---------------------------------|-------------------------------|
| C-c C-c | Evaluate defpart                | Auto-rebuild triggered!       |
| C-c C-k | Compile/load entire file        | All defparts rebuilt          |
| C-x C-e | Evaluate expression             | Manual control (advanced)     |

---

## Workflow for a New Part

1. **Copy the template:**
   ```bash
   cp examples/simple-plate.lisp examples/my-bracket.lisp
   ```

2. **Edit the part name:**
   ```lisp
   (clad.dsl:defpart my-bracket
       ((width 100) (height 50) ...)
     ...)
   ```

3. **Update quick-view:**
   ```lisp
   (defun quick-view ()
     (clad:show 'my-bracket))  ; Changed from mounting-plate
   ```

4. **Load and view:**
   ```lisp
   (load "examples/my-bracket.lisp")
   (quick-view)
   ```

5. **Start designing:**
   - Edit defpart
   - C-c C-c
   - Watch updates!

---

## Real-World Example Session

```lisp
;; Terminal 1: Start REPL
$ sbcl
CLAD> (load "examples/simple-plate.lisp")
CLAD> (quick-view)
;; Browser opens, shows default part

;; Terminal 2: Edit file in Emacs
;; Change (boss-height 15) to (boss-height 25)
;; Press C-c C-c

;; Back to Terminal 1:
[Rebuilding... done in 0.234s]

;; Browser: Auto-reloads after ~2 seconds
;; You see: Taller boss! ✨

;; Continue editing in Emacs
;; Change (hole-count 8) to (hole-count 12)
;; Press C-c C-c

;; Terminal 1:
[Rebuilding... done in 0.198s]

;; Browser: Auto-reloads
;; You see: More holes! ✨

;; And so on... Pure flow!
```

---

## Debugging: What if it doesn't update?

### Check 1: Is auto-rebuild enabled?
```lisp
CLAD> *auto-rebuild*
T  ; Should be T
```

If NIL:
```lisp
CLAD> (setf *auto-rebuild* t)
```

### Check 2: Did the defpart redefine correctly?
After C-c C-c, you should see in REPL:
```
[Rebuilding... done in 0.XXXs]
```

If you don't see this, the defpart didn't trigger rebuild.

### Check 3: Is the viewer running?
```lisp
CLAD> clad.viewer:*viewer-running-p*
T  ; Should be T
```

### Check 4: Check the file timestamp
```lisp
CLAD> (file-write-date
       (merge-pathnames "viewer/models/mounting-plate.glb"
         (asdf:system-source-directory :clad)))
3971015312  ; Should change after C-c C-c
```

### Check 5: Browser cache
- Open DevTools (F12)
- Go to Network tab
- Filter for "mounting-plate.glb"
- Should see new request with 200 status after each update

---

## Summary

**The perfect CAD workflow:**

1. `(load "examples/simple-plate.lisp")` - Once
2. `(quick-view)` - Once
3. Edit defpart → C-c C-c → See changes → Repeat forever! ✨

**No manual function calls. No rebuilds. No friction. Just code.**

This is REPL-driven CAD development at its finest!
