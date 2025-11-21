# Advanced CAD Development Roadmap
## Making CLAD Production-Ready for Professional Mechanical Design

**Date:** November 15, 2025
**Current Status:** Phase T Complete (GD&T, Tolerancing, STEP AP242 PMI)

---

## Overview

CLAD currently has excellent foundations:
- ✅ Core geometry primitives
- ✅ Boolean operations
- ✅ Transformations, fillets, curves
- ✅ Complete units system
- ✅ Industrial-grade GD&T (ASME Y14.5-2018)
- ✅ Advanced selectors
- ✅ Sketch system with constraints
- ✅ Assembly system
- ✅ STEP export with PMI
- ✅ DSL for parametric parts

This roadmap identifies critical gaps for professional mechanical design workflows.

---

## Tier 1: Essential Features (High Impact, Common Use)

### 1. Threads & Fasteners ⭐⭐⭐

**Why Critical:**
- 90% of mechanical assemblies use threaded fasteners
- Thread modeling is tedious without library support
- Standard compliance (ISO, ANSI, DIN)

**Implementation:**

#### Phase TH1: Thread Modeling (2-3 weeks)
```lisp
;; Basic thread creation
(defun make-external-thread (diameter length pitch)
  "Create external (male) thread - ISO metric or unified"
  ...)

(defun make-internal-thread (diameter depth pitch)
  "Create internal (female) thread - tapped hole"
  ...)

;; High-level API
(defun make-metric-thread (designation length &key (class "6H"))
  "Create ISO metric thread: M6, M8, M10, etc."
  ;; designation: "M6", "M8x1.25", etc.
  ;; class: "6H" (internal), "6g" (external)
  ...)

(defun make-unified-thread (designation length &key (class "2B"))
  "Create unified thread: 1/4-20, #10-32, etc."
  ...)
```

**Thread Database:**
- ISO metric coarse (M1-M100)
- ISO metric fine (M8x1, M10x1.25, etc.)
- Unified coarse (UNC: #0-80 to 4")
- Unified fine (UNF: #0-80 to 1.5")
- Pipe threads (NPT, BSPT)

#### Phase TH2: Standard Fastener Library (2 weeks)
```lisp
;; Parametric fastener generation
(defpart hex-bolt (diameter length)
  "ISO 4014 hex head bolt"
  (:body (make-hex-head diameter))
  (:body (make-external-thread (format nil "M~D" diameter) length)))

(defpart socket-cap-screw (diameter length)
  "ISO 4762 socket head cap screw"
  ...)

(defpart hex-nut (diameter &key (style :standard))
  "ISO 4032 hex nut"
  ...)

;; High-level API
(defun make-fastener (type designation length &rest options)
  "Universal fastener creator
   Examples:
     (make-fastener :hex-bolt \"M6\" 30)
     (make-fastener :socket-cap \"#10-32\" 25)
     (make-fastener :hex-nut \"M8\" :style :lock)"
  ...)
```

**Standard Fastener Library:**
- Hex bolts (ISO 4014, ANSI B18.2.1)
- Socket cap screws (ISO 4762, ANSI B18.3)
- Hex nuts (ISO 4032, ANSI B18.2.2)
- Lock nuts (ISO 7040, 7042)
- Washers (ISO 7089, 7090, 7093)
- Set screws (ISO 4026, 4027, 4028, 4029)
- Machine screws (ISO 1207, 7045, 7046, 7047)

**Benefits:**
- Massive time savings (seconds vs minutes per fastener)
- Standard compliance guaranteed
- BOM integration
- Automatic mass properties

**Effort:** 4-5 weeks
**Impact:** ⭐⭐⭐ Critical for mechanical design

---

### 2. Sheet Metal Operations ⭐⭐⭐

**Why Critical:**
- Major manufacturing method (chassis, enclosures, brackets)
- Specialized workflow (bend/unfold)
- K-factor calculations essential for accuracy

**Implementation:**

#### Phase SM1: Basic Sheet Metal (2-3 weeks)
```lisp
(defun make-sheet-metal-base (thickness)
  "Create sheet metal context with bend parameters"
  (make-instance 'sheet-metal-part
                 :thickness thickness
                 :k-factor 0.45  ; Default for mild steel
                 :bend-radius (* 1.5 thickness)))

(defun add-bend (part angle position &key (radius :auto))
  "Add bend to sheet metal part
   - Calculates bend allowance using K-factor
   - Updates flat pattern automatically"
  ...)

(defun add-flange (part length angle &key (edge-selector :all))
  "Add flange along edge(s)"
  ...)

(defun add-hem (part type &key (edge-selector :auto))
  "Add hem edge treatment
   Types: :open, :closed, :teardrop"
  ...)

(defun add-relief (part type &key (position :auto))
  "Add bend relief
   Types: :rectangular, :round, :tear"
  ...)

(defun unfold (part)
  "Generate flat pattern for manufacturing
   Returns:
     - Flat pattern geometry
     - Bend lines with angles
     - Bend sequence"
  ...)
```

#### Phase SM2: Advanced Features (1-2 weeks)
```lisp
;; Corner treatments
(defun add-miter (part corner-edges &key (type :closed))
  "Miter corner joints"
  ...)

;; Forming operations
(defun add-louver (part position size)
  "Add louver for ventilation"
  ...)

(defun add-lance (part position size)
  "Add lance/tab for assembly"
  ...)

(defun add-emboss (part text position &key (height 0.5))
  "Add embossed text/logo"
  ...)
```

**Material Database:**
- K-factor tables (aluminum, steel, stainless, copper)
- Minimum bend radius by material/thickness
- Bend allowance tables

**Benefits:**
- Complete sheet metal workflow
- Accurate flat patterns
- Integration with GD&T (bend tolerances)
- Manufacturing-ready outputs

**Effort:** 3-5 weeks
**Impact:** ⭐⭐⭐ Essential for enclosures/chassis

---

### 3. STL Export (3D Printing) ⭐⭐⭐

**Why Critical:**
- 3D printing workflow essential
- STL is universal format (slicers, mesh tools)
- Rapid prototyping capability

**Implementation:**

#### Phase STL1: Basic STL Export (1 week)
```lisp
(defun export-stl (shape filename &key (ascii nil) (resolution :medium))
  "Export shape to STL format

   Arguments:
     shape      - CLAD shape to export
     filename   - Output .stl file path
     ascii      - T for ASCII STL, NIL for binary (default)
     resolution - Tessellation quality: :low, :medium, :high, :ultra
                  Controls triangle count vs accuracy tradeoff

   Binary STL format (default):
     - Compact file size
     - Fast loading
     - Industry standard

   ASCII STL format:
     - Human readable
     - Easier debugging
     - Larger files

   Resolution guide:
     :low    - Draft prints, fast slicing (0.5mm tolerance)
     :medium - Standard prints (0.1mm tolerance)
     :high   - Detailed prints (0.05mm tolerance)
     :ultra  - Ultra-detailed (0.01mm tolerance)

   Returns: T on success
   Signals: stl-export-error on failure"
  ...)

;; Using OpenCASCADE's STL writer
(defun ffi-export-stl (shape-handle filename &key (linear-deflection 0.1)
                                                  (angular-deflection 0.5)
                                                  (ascii nil))
  "FFI wrapper for OCCT StlAPI_Writer"
  ...)
```

**OpenCASCADE Integration:**
```cpp
// c-wrapper/export.cpp - ADD
extern "C" {
    int occt_export_stl(void* shape, const char* filename,
                       double linear_deflection,
                       double angular_deflection,
                       int ascii_mode);
}

// Uses StlAPI_Writer with configurable tessellation
```

**Benefits:**
- 3D printing workflow
- Mesh analysis tools integration
- Visualization in mesh viewers
- Sharing with non-CAD users

**Effort:** 1 week
**Impact:** ⭐⭐⭐ Critical for prototyping

---

### 4. Mass Properties Analysis ⭐⭐⭐

**Why Critical:**
- Weight estimates for design validation
- Center of mass for assembly balancing
- Moments of inertia for dynamics analysis
- BOM weight calculations

**Implementation:**

#### Phase MP1: Basic Mass Properties (1 week)
```lisp
(defclass mass-properties ()
  ((volume :accessor volume)
   (surface-area :accessor surface-area)
   (centroid :accessor centroid)  ; Center of volume
   (mass :accessor mass)
   (center-of-mass :accessor center-of-mass)  ; = centroid for uniform density
   (moments-of-inertia :accessor moments-of-inertia)  ; Ixx, Iyy, Izz, Ixy, Ixz, Iyz
   (principal-moments :accessor principal-moments)    ; I1, I2, I3
   (principal-axes :accessor principal-axes)         ; Eigenvectors
   (bounding-box :accessor bounding-box)))

(defun compute-mass-properties (shape &key (density 1.0) (units :mm))
  "Compute comprehensive mass properties

   Arguments:
     shape   - CLAD shape or assembly
     density - Material density (g/cm³ or kg/m³)
     units   - :mm, :cm, :m, :inch

   Returns: mass-properties object

   Examples:
     ;; Aluminum part (2.7 g/cm³)
     (compute-mass-properties my-part :density 2.7)

     ;; Steel assembly (7.85 g/cm³)
     (compute-mass-properties my-assembly :density 7.85)"
  ...)

;; Convenience functions
(defun get-volume (shape &key (units :mm))
  "Get volume in specified units"
  ...)

(defun get-mass (shape density &key (units :mm))
  "Get mass with material density"
  ...)

(defun get-center-of-mass (shape &optional density)
  "Get center of mass (centroid if density uniform)"
  ...)

(defun get-inertia-tensor (shape &optional density)
  "Get 3x3 inertia tensor"
  ...)
```

**OpenCASCADE Integration:**
```cpp
// c-wrapper/analysis.cpp - NEW FILE
extern "C" {
    // Uses GProp_GProps for volume properties
    void* occt_compute_mass_props(void* shape);
    double occt_get_volume(void* props);
    double occt_get_surface_area(void* props);
    void occt_get_centroid(void* props, double* x, double* y, double* z);
    void occt_get_inertia_matrix(void* props, double* matrix_9);
}
```

#### Phase MP2: Material Database (1 week)
```lisp
;; Material database
(defparameter *materials*
  '(;; Metals
    (:aluminum-6061   :density 2.70  :units :g/cm3)
    (:steel-1018      :density 7.87  :units :g/cm3)
    (:stainless-304   :density 8.00  :units :g/cm3)
    (:titanium-6al4v  :density 4.43  :units :g/cm3)
    (:brass           :density 8.50  :units :g/cm3)
    (:copper          :density 8.96  :units :g/cm3)
    ;; Plastics
    (:abs             :density 1.05  :units :g/cm3)
    (:pla             :density 1.24  :units :g/cm3)
    (:nylon           :density 1.15  :units :g/cm3)
    (:polycarbonate   :density 1.20  :units :g/cm3)
    ;; Others
    (:wood-oak        :density 0.75  :units :g/cm3)
    (:glass           :density 2.50  :units :g/cm3)))

(defun set-material (shape material-keyword)
  "Assign material to shape
   Stores in metadata for mass calculations"
  (setf (shape-metadata shape)
        (append (shape-metadata shape)
                (list :material material-keyword
                      :density (getf (getf *materials* material-keyword) :density)))))

(defun compute-assembly-mass-properties (assembly)
  "Compute mass props respecting per-part materials"
  ...)
```

**Benefits:**
- Design validation (weight requirements)
- Center of mass analysis
- Dynamics/FEA prep
- Cost estimation (material volume)
- BOM integration

**Effort:** 2 weeks
**Impact:** ⭐⭐⭐ Essential for engineering validation

---

### 5. Standard Part Libraries ⭐⭐

**Why Important:**
- Massive productivity boost (drag & drop vs model from scratch)
- Ensure standard compliance
- Supplier compatibility

**Implementation:**

#### Phase LIB1: Core Standard Parts (2-3 weeks)
```lisp
;; Bearings
(defpart ball-bearing (bore-diameter outer-diameter width &key (type :deep-groove))
  "Standard ball bearing (ISO 15, ABEC ratings)"
  ...)

;; O-rings and seals
(defpart o-ring (as568-size)
  "AS568 standard O-ring sizes"
  ;; AS568-010 through AS568-475
  ...)

(defpart shaft-seal (shaft-diameter &key (type :radial))
  "Standard shaft seals"
  ...)

;; Pins and keys
(defpart dowel-pin (diameter length &key (tolerance :m6))
  "ISO 2338 dowel pins"
  ...)

(defpart key (shaft-diameter length &key (type :parallel))
  "ISO 773 parallel keys, ISO 3912 taper keys"
  ...)

;; Springs
(defpart compression-spring (&key wire-diameter outer-diameter
                                  free-length coils)
  "Compression spring generator"
  ...)

;; Structural shapes
(defpart i-beam (designation length)
  "AISC W-shapes, S-shapes"
  ;; W8x31, W12x26, etc.
  ...)

(defpart angle (designation length)
  "AISC L-shapes"
  ;; L3x3x1/4, L4x3x3/8, etc.
  ...)

(defpart channel (designation length)
  "AISC C-shapes"
  ...)

(defpart tube (outer-diameter wall-thickness length &key (type :round))
  "Structural tubing - round, square, rectangular"
  ...)
```

**Database Coverage:**
- 1000+ fastener sizes (bolts, screws, nuts, washers)
- 200+ bearing sizes (ball, roller, thrust)
- 100+ O-ring sizes
- 50+ structural shapes
- Standard keys, pins, springs

**Benefits:**
- 10-100x faster than modeling from scratch
- Perfect dimensional accuracy
- Automatic BOM integration
- Supplier part numbers

**Effort:** 2-3 weeks (initial), ongoing expansion
**Impact:** ⭐⭐ High productivity gain

---

## Tier 2: Important Professional Features

### 6. STEP Import (Roundtrip Capability) ⭐⭐

**Why Important:**
- Interoperability with other CAD systems
- Import vendor models (STEP is universal exchange)
- Modification workflows
- Assembly integration

**Implementation:**

#### Phase IMPORT1: Basic STEP Import (1-2 weeks)
```lisp
(defun import-step (filename &key (merge-compounds t))
  "Import STEP file as CLAD shape

   Arguments:
     filename        - Path to .step/.stp file
     merge-compounds - Merge multi-body parts into single shape

   Returns: shape or list of shapes

   Supported:
     - AP203 (Configuration Controlled Design)
     - AP214 (Automotive Design)
     - AP242 (Managed Model-based 3D Engineering)

   PMI Import (AP242):
     - Datums → shape metadata
     - Geometric tolerances → shape metadata
     - Dimensions → shape metadata
     - (Requires XDE integration)"
  ...)

;; OpenCASCADE wrapper
(defcfun ("occt_import_step" ffi-import-step) :pointer
  (filename :string))
```

**Roundtrip Validation:**
- Export STEP → Import → Compare geometry
- Preserve PMI through roundtrip
- Tolerance preservation

**Benefits:**
- Import supplier models
- Multi-CAD workflows
- Model reuse
- Collaboration

**Effort:** 1-2 weeks (basic), 1 week (PMI)
**Impact:** ⭐⭐ Important for collaboration

---

### 7. 2D Drawing Generation ⭐⭐

**Why Important:**
- Manufacturing documentation
- ISO/ASME drawing standards
- Shop floor communication
- Approval workflows

**Implementation:**

#### Phase DWG1: Basic Projections (2 weeks)
```lisp
(defun create-drawing (part &key (sheet-size :a3) (scale 1.0))
  "Create 2D technical drawing from 3D model"
  ...)

(defun add-orthographic-view (drawing part view-type
                              &key (position '(0 0)) (scale :auto))
  "Add orthographic projection
   view-type: :front, :top, :right, :left, :bottom, :rear, :isometric"
  ...)

(defun add-section-view (drawing part cutting-plane
                         &key (position '(0 0)) (label "A-A"))
  "Add section view with cutting plane"
  ...)

(defun add-detail-view (drawing detail-region
                        &key (scale 2.0) (label "A"))
  "Add detail view (magnified region)"
  ...)

(defun add-dimensions (drawing &key (auto-dimension t))
  "Add dimension annotations
   Reads from GD&T metadata"
  ...)

(defun export-drawing (drawing filename &key (format :pdf))
  "Export drawing to PDF, DXF, or SVG"
  ...)
```

**Standard Compliance:**
- ISO 128 (Technical drawings)
- ASME Y14.5 (Dimensioning)
- ISO 5456 (Projection methods)

**Benefits:**
- Complete documentation workflow
- Manufacturing communication
- GD&T visualization
- Approval packages

**Effort:** 2-3 weeks
**Impact:** ⭐⭐ Critical for manufacturing

---

### 8. Additional Export Formats ⭐

**Quick Wins:**

```lisp
;; IGES export (1-2 days)
(defun export-iges (shape filename)
  "Export to IGES format (older CAD interchange)"
  ;; OCCT IGESControl_Writer
  ...)

;; OBJ export (1 day)
(defun export-obj (shape filename &key (include-normals t))
  "Export to Wavefront OBJ (visualization)"
  ...)

;; glTF export (2-3 days)
(defun export-gltf (shape filename &key (binary t) (embed-textures t))
  "Export to glTF 2.0 (web visualization, AR/VR)"
  ...)

;; DXF 2D export (3-4 days - integrate with drawing generation)
(defun export-dxf (drawing filename)
  "Export 2D drawing to DXF (AutoCAD interchange)"
  ...)
```

**Effort:** 1-2 weeks total
**Impact:** ⭐ Nice to have

---

## Tier 3: Advanced/Future Features

### 9. Surfacing Tools ⭐

**For organic/complex shapes:**
- Loft operations
- Sweep operations
- Boundary surfaces
- Fillet surfaces
- NURBS surface creation

**Effort:** 3-4 weeks
**Impact:** ⭐ Specialized use cases

---

### 10. Direct Editing ⭐

**Modify imported geometry:**
- Move face
- Delete face
- Offset face
- Replace face
- Blend edges

**Effort:** 2-3 weeks
**Impact:** ⭐ Advanced workflows

---

### 11. Simulation Integration ⭐

**FEA/CFD preparation:**
- Mesh generation
- Boundary condition application
- Material assignment
- Analysis result visualization

**Effort:** 4-6 weeks (complex)
**Impact:** ⭐ Specialized domain

---

## Recommended Implementation Priority

### Phase R1: Critical Productivity (8-10 weeks)
1. **Threads & Fasteners** (4-5 weeks) - Tier 1 ⭐⭐⭐
2. **STL Export** (1 week) - Tier 1 ⭐⭐⭐
3. **Mass Properties** (2 weeks) - Tier 1 ⭐⭐⭐
4. **Standard Part Libraries** (2-3 weeks) - Tier 1 ⭐⭐

**Deliverable:** CLAD can handle 80% of mechanical design workflows

### Phase R2: Manufacturing (5-7 weeks)
1. **Sheet Metal** (3-5 weeks) - Tier 1 ⭐⭐⭐
2. **2D Drawing Generation** (2-3 weeks) - Tier 2 ⭐⭐

**Deliverable:** Complete design-to-manufacturing workflow

### Phase R3: Interoperability (2-4 weeks)
1. **STEP Import** (1-2 weeks) - Tier 2 ⭐⭐
2. **Additional Exports** (1-2 weeks) - Tier 2 ⭐

**Deliverable:** Full CAD ecosystem integration

### Phase R4: Advanced (8-12 weeks) - Optional
1. **Surfacing Tools** (3-4 weeks)
2. **Direct Editing** (2-3 weeks)
3. **Simulation Prep** (4-6 weeks)

**Deliverable:** Professional CAD system feature parity

---

## Quick Win Priorities (Next 2 Weeks)

If you want immediate high-impact additions:

**Week 1:**
- STL Export (2-3 days) ⭐⭐⭐
- Mass Properties (3-4 days) ⭐⭐⭐

**Week 2:**
- Basic thread modeling (5 days) ⭐⭐⭐

**Result:** Covers 3D printing, weight analysis, and threaded assemblies - three of the most common needs.

---

## Comparison: Current vs Complete System

| Feature | Current | After Tier 1 | After Tier 2 |
|---------|---------|--------------|--------------|
| Geometric modeling | ✅ Excellent | ✅ | ✅ |
| GD&T/Tolerancing | ✅ Excellent | ✅ | ✅ |
| Assembly | ✅ Good | ✅ | ✅ |
| Threads/Fasteners | ❌ | ✅ Complete | ✅ |
| Sheet metal | ❌ | ✅ Complete | ✅ |
| 3D printing (STL) | ❌ | ✅ | ✅ |
| Mass properties | ❌ | ✅ | ✅ |
| Standard parts | ❌ | ✅ 1000+ parts | ✅ |
| STEP import | ❌ | ❌ | ✅ |
| 2D drawings | ❌ | ❌ | ✅ |
| **Workflow coverage** | **60%** | **85%** | **95%** |

---

## Conclusion

**Current CLAD Status:** Excellent foundation with advanced GD&T

**Critical Gaps for Production:**
1. Threads/Fasteners (affects 90% of designs)
2. Sheet metal (major workflow)
3. STL export (3D printing essential)
4. Mass properties (weight analysis)
5. Standard part libraries (productivity)

**Recommended Path:**
- **Now:** Implement Tier 1 features (8-10 weeks)
- **Next:** Add manufacturing features (5-7 weeks)
- **Future:** Interoperability and advanced features (as needed)

**After Tier 1:** CLAD will be production-ready for most mechanical design workflows, competitive with commercial CAD systems for programmatic design.

**Your decision:** Which tier should we tackle first? I recommend starting with the "Quick Win Priorities" (STL export + Mass properties) for immediate impact.
