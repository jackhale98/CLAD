# STEP AP242 PMI Export Implementation Roadmap

## Overview

This document outlines the implementation plan for exporting CLAD parts with Product Manufacturing Information (PMI) to STEP AP242 format. STEP AP242 is the ISO standard for "Managed model-based 3D engineering" and includes full support for GD&T, dimensions, tolerances, and datums.

## Current Status

✅ **Completed:**
- Priority 1-4: GD&T validation, selector validation, error messages, edge cases
- 25+ STEP AP242 PMI export tests created (`tests/step-pmi-tests.lisp`)
- Research into OpenCASCADE XDE capabilities

🔲 **Remaining:**
- C++ wrapper functions for OpenCASCADE XDE
- PMI entity creation module
- STEP AP242 export integration
- Test execution and verification

## Architecture

### 1. OpenCASCADE XDE Components

OpenCASCADE's Extended Data Exchange (XDE) module provides:

- **XCAFDoc_DocumentTool** - Document management
- **XCAFDoc_DimTolTool** - PMI management (dimensions, tolerances, datums)
- **XCAFDoc_Dimension** - Dimensional specifications
- **XCAFDoc_GeomTolerance** - Geometric tolerance constraints
- **XCAFDoc_Datum** - Datum references
- **STEPCAFControl_Writer** - STEP AP242 export with PMI

### 2. Implementation Layers

```
┌──────────────────────────────────────────┐
│  Lisp Layer (src/export/step-ap242.lisp) │
│  - export-step-ap242 function            │
│  - PMI entity mapping                    │
└────────────────┬─────────────────────────┘
                 │
┌────────────────▼─────────────────────────┐
│  Lisp PMI Module (src/export/pmi.lisp)   │
│  - clad→XDE dimension mapping            │
│  - clad→XDE tolerance mapping            │
│  - clad→XDE datum mapping                │
└────────────────┬─────────────────────────┘
                 │
┌────────────────▼─────────────────────────┐
│  FFI Layer (src/ffi/step-pmi.lisp)       │
│  - Lisp wrapper functions                │
└────────────────┬─────────────────────────┘
                 │
┌────────────────▼─────────────────────────┐
│  C++ Wrapper (c-wrapper/step_pmi.cpp)    │
│  - XCAFDoc C++ → C FFI bindings          │
└──────────────────────────────────────────┘
```

## Implementation Steps

### Step 1: C++ Wrapper Functions (c-wrapper/step_pmi.cpp)

Create C wrapper functions for OpenCASCADE XDE classes:

```cpp
// Document and tool management
extern "C" {
    // Create XDE document
    void* occt_create_xcaf_doc();

    // Get DimTol tool
    void* occt_get_dimtol_tool(void* doc);

    // Get Shape tool
    void* occt_get_shape_tool(void* doc);

    // Add shape to document
    void* occt_add_shape_to_doc(void* shape_tool, void* shape);

    // Dimension management
    void* occt_add_dimension(void* dimtol_tool);
    void occt_set_dimension_type(void* dim_label, int dim_type);
    void occt_set_dimension_value(void* dim_label, double value);
    void occt_set_dimension_tolerance(void* dim_label, double upper, double lower);
    void occt_link_dimension_to_shape(void* dimtol_tool, void* dim_label, void* shape_label);

    // Datum management
    void* occt_add_datum(void* dimtol_tool);
    void occt_set_datum_name(void* datum_label, const char* name);
    void occt_link_datum_to_shape(void* dimtol_tool, void* datum_label, void* shape_label);

    // Geometric tolerance management
    void* occt_add_geom_tolerance(void* dimtol_tool);
    void occt_set_geom_tolerance_type(void* tol_label, int tol_type);
    void occt_set_geom_tolerance_value(void* tol_label, double value);
    void occt_add_geom_tolerance_datum_ref(void* tol_label, void* datum_label);
    void occt_link_geom_tolerance_to_shape(void* dimtol_tool, void* tol_label, void* shape_label);

    // STEP AP242 export
    int occt_export_step_ap242(void* doc, const char* filename);
}
```

**Enums:**
```cpp
// Dimension types
enum DimensionType {
    DIM_LINEAR_DISTANCE = 0,
    DIM_ANGULAR = 1,
    DIM_RADIUS = 2,
    DIM_DIAMETER = 3
};

// Geometric tolerance types
enum GeomToleranceType {
    GDT_FLATNESS = 0,
    GDT_STRAIGHTNESS = 1,
    GDT_CIRCULARITY = 2,
    GDT_CYLINDRICITY = 3,
    GDT_PERPENDICULARITY = 4,
    GDT_PARALLELISM = 5,
    GDT_ANGULARITY = 6,
    GDT_POSITION = 7,
    GDT_CONCENTRICITY = 8,
    GDT_SYMMETRY = 9,
    GDT_PROFILE_SURFACE = 10,
    GDT_PROFILE_LINE = 11,
    GDT_CIRCULAR_RUNOUT = 12,
    GDT_TOTAL_RUNOUT = 13
};
```

### Step 2: FFI Bindings (src/ffi/step-pmi.lisp)

```lisp
(in-package :clad.ffi)

;; Document management
(defcfun ("occt_create_xcaf_doc" ffi-create-xcaf-doc) :pointer)
(defcfun ("occt_get_dimtol_tool" ffi-get-dimtol-tool) :pointer (doc :pointer))
(defcfun ("occt_get_shape_tool" ffi-get-shape-tool) :pointer (doc :pointer))
(defcfun ("occt_add_shape_to_doc" ffi-add-shape-to-doc) :pointer
  (shape-tool :pointer) (shape :pointer))

;; Dimension management
(defcfun ("occt_add_dimension" ffi-add-dimension) :pointer (dimtol-tool :pointer))
(defcfun ("occt_set_dimension_type" ffi-set-dimension-type) :void
  (dim-label :pointer) (dim-type :int))
(defcfun ("occt_set_dimension_value" ffi-set-dimension-value) :void
  (dim-label :pointer) (value :double))
(defcfun ("occt_set_dimension_tolerance" ffi-set-dimension-tolerance) :void
  (dim-label :pointer) (upper :double) (lower :double))
(defcfun ("occt_link_dimension_to_shape" ffi-link-dimension-to-shape) :void
  (dimtol-tool :pointer) (dim-label :pointer) (shape-label :pointer))

;; Datum management
(defcfun ("occt_add_datum" ffi-add-datum) :pointer (dimtol-tool :pointer))
(defcfun ("occt_set_datum_name" ffi-set-datum-name) :void
  (datum-label :pointer) (name :string))
(defcfun ("occt_link_datum_to_shape" ffi-link-datum-to-shape) :void
  (dimtol-tool :pointer) (datum-label :pointer) (shape-label :pointer))

;; Geometric tolerance management
(defcfun ("occt_add_geom_tolerance" ffi-add-geom-tolerance) :pointer
  (dimtol-tool :pointer))
(defcfun ("occt_set_geom_tolerance_type" ffi-set-geom-tolerance-type) :void
  (tol-label :pointer) (tol-type :int))
(defcfun ("occt_set_geom_tolerance_value" ffi-set-geom-tolerance-value) :void
  (tol-label :pointer) (value :double))
(defcfun ("occt_add_geom_tolerance_datum_ref" ffi-add-geom-tolerance-datum-ref) :void
  (tol-label :pointer) (datum-label :pointer))
(defcfun ("occt_link_geom_tolerance_to_shape" ffi-link-geom-tolerance-to-shape) :void
  (dimtol-tool :pointer) (tol-label :pointer) (shape-label :pointer))

;; STEP AP242 export
(defcfun ("occt_export_step_ap242" ffi-export-step-ap242) :int
  (doc :pointer) (filename :string))
```

### Step 3: PMI Entity Mapping Module (src/export/pmi.lisp)

```lisp
(in-package :clad.export)

(defun create-dimension-entity (dimtol-tool dimension shape-label)
  "Create XDE dimension entity from CLAD toleranced-dimension"
  (let ((dim-label (clad.ffi:ffi-add-dimension dimtol-tool)))
    ;; Set dimension type
    (clad.ffi:ffi-set-dimension-type dim-label +dim-linear-distance+)

    ;; Set nominal value
    (clad.ffi:ffi-set-dimension-value dim-label
                                      (clad.units:dimension-nominal dimension))

    ;; Set tolerance if present
    (let ((tol (clad.units:dimension-tolerance dimension)))
      (when tol
        (typecase tol
          (clad.units:bilateral-tolerance-spec
           (clad.ffi:ffi-set-dimension-tolerance
            dim-label
            (clad.units:tolerance-upper tol)
            (clad.units:tolerance-lower tol)))
          (clad.units:fit-tolerance-spec
           (clad.ffi:ffi-set-dimension-tolerance
            dim-label
            (clad.units:tolerance-upper tol)
            (clad.units:tolerance-lower tol))))))

    ;; Link to shape
    (clad.ffi:ffi-link-dimension-to-shape dimtol-tool dim-label shape-label)

    dim-label))

(defun create-datum-entity (dimtol-tool datum-label datum-spec shape-label)
  "Create XDE datum entity from CLAD datum specification"
  (let ((datum (clad.ffi:ffi-add-datum dimtol-tool)))
    ;; Set datum name
    (clad.ffi:ffi-set-datum-name datum datum-label)

    ;; Link to shape (using selector to find actual shape)
    (clad.ffi:ffi-link-datum-to-shape dimtol-tool datum shape-label)

    datum))

(defun create-geometric-tolerance-entity (dimtol-tool tolerance shape-label datum-map)
  "Create XDE geometric tolerance from CLAD geometric-tolerance"
  (let ((tol-label (clad.ffi:ffi-add-geom-tolerance dimtol-tool)))
    ;; Map CLAD GDT type to OCCT type
    (let ((occt-type (gdt-type-to-occt-type (clad.gdt:tolerance-gdt-type tolerance))))
      (clad.ffi:ffi-set-geom-tolerance-type tol-label occt-type))

    ;; Set tolerance value
    (clad.ffi:ffi-set-geom-tolerance-value tol-label
                                           (clad.gdt:tolerance-zone-value tolerance))

    ;; Add datum references
    (let ((datum-refs (clad.gdt:tolerance-datum-refs tolerance)))
      (dolist (datum-ref datum-refs)
        (let ((datum-entity (gethash datum-ref datum-map)))
          (when datum-entity
            (clad.ffi:ffi-add-geom-tolerance-datum-ref tol-label datum-entity)))))

    ;; Link to shape
    (clad.ffi:ffi-link-geom-tolerance-to-shape dimtol-tool tol-label shape-label)

    tol-label))
```

### Step 4: STEP AP242 Export Function (src/export/step-ap242.lisp)

```lisp
(in-package :clad.export)

(defun export-step-ap242 (shape filename)
  "Export shape to STEP AP242 file with full PMI (dimensions, datums, GD&T).

  Arguments:
    shape    - clad.core:shape or clad.shapes:cad-shape to export
    filename - Path to output STEP file

  Returns: T on success

  The exported STEP file will include:
    - Geometric model (solid geometry)
    - Dimensional tolerances from (dim ... :tol ...) specifications
    - Datum references from (:datum ...) forms
    - Geometric tolerances from (:flatness ...), (:perpendicularity ...), etc.

  Compatible with:
    - FreeCAD (with PMI support)
    - SolidWorks
    - CAx-IF compliant CAD systems"

  ;; Unwrap shape if needed
  (let ((core-shape (if (typep shape 'clad.shapes:cad-shape)
                        (clad.shapes::core-shape shape)
                        shape)))
    (unless (clad.core:valid-shape-p core-shape)
      (error "Invalid shape: ~S" shape))

    (unless (stringp filename)
      (error "Filename must be a string: ~S" filename))

    ;; Ensure directory exists
    (ensure-directories-exist filename)

    ;; Create XDE document
    (let ((doc (clad.ffi:ffi-create-xcaf-doc)))
      (unwind-protect
          (progn
            ;; Get tools
            (let ((shape-tool (clad.ffi:ffi-get-shape-tool doc))
                  (dimtol-tool (clad.ffi:ffi-get-dimtol-tool doc)))

              ;; Add shape to document
              (let ((shape-label (clad.ffi:ffi-add-shape-to-doc
                                  shape-tool
                                  (clad.core:shape-handle core-shape))))

                ;; Extract and add PMI from metadata
                (let ((metadata (clad.core:shape-metadata core-shape)))
                  (when metadata
                    ;; Add dimensional tolerances
                    (add-dimensional-pmi metadata dimtol-tool shape-label)

                    ;; Add datums
                    (let ((datum-map (add-datum-pmi metadata dimtol-tool shape-label)))

                      ;; Add geometric tolerances
                      (add-geometric-tolerance-pmi metadata dimtol-tool shape-label datum-map))))))

            ;; Export to STEP AP242
            (let ((result (clad.ffi:ffi-export-step-ap242 doc filename)))
              (unless (zerop result)
                (error "STEP AP242 export failed with code ~D" result)))

            (format t "~&Exported STEP AP242 file with PMI: ~A~%" filename)
            t)

        ;; Cleanup document (if needed)
        nil))))
```

## Testing Plan

### Phase 1: Unit Tests
1. Test dimensional tolerance export
2. Test datum export
3. Test geometric tolerance export (all 14 types)
4. Test ISO fit export

### Phase 2: Integration Tests
1. Test complete parts with multiple PMI types
2. Test file format validation
3. Test roundtrip (export → import → verify)

### Phase 3: Compatibility Tests
1. Verify FreeCAD can open files with PMI
2. Verify SolidWorks compatibility
3. Verify CAx-IF compliance

## Success Criteria

- [ ] All 25+ STEP AP242 PMI tests pass
- [ ] Exports include dimensional tolerances
- [ ] Exports include datum references
- [ ] Exports include all 14 GD&T tolerance types
- [ ] Files validate against STEP AP242 schema
- [ ] Files open in FreeCAD with PMI visible
- [ ] Files open in SolidWorks with PMI visible

## Timeline Estimate

- **C++ Wrapper Development:** 1-2 days
- **FFI Bindings:** 0.5 days
- **PMI Entity Mapping:** 1 day
- **STEP AP242 Export:** 1 day
- **Testing & Debugging:** 1-2 days

**Total: 4.5-6.5 days**

## Next Steps

1. **Implement C++ wrapper functions** in `c-wrapper/step_pmi.cpp`
2. **Update CMakeLists.txt** to include XDE libraries
3. **Create FFI bindings** in `src/ffi/step-pmi.lisp`
4. **Implement PMI mapping** in `src/export/pmi.lisp`
5. **Implement export function** in `src/export/step-ap242.lisp`
6. **Run tests** and iterate

## References

- [OpenCASCADE XDE Documentation](https://dev.opencascade.org/doc/overview/html/occt_user_guides__xde.html)
- [STEP AP242 Standard](https://www.iso.org/standard/66654.html)
- [CAx-IF Recommended Practices for PMI](https://www.cax-if.org/joint_testing_info.html#recpracs)
