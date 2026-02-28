;;;; clad.asd --- ASDF system definition for CLAD

(defsystem "clad"
  :description "A source-code based CAD design system in Common Lisp"
  :version "0.1.0"
  :author "CLAD Contributors"
  :license "MIT"
  :depends-on (#:cffi
               #:trivial-garbage    ; For finalization
               #:alexandria         ; Utilities
               #:hunchentoot        ; Web server
               #:cl-fad             ; File utilities
               #:bordeaux-threads   ; For file watching (Phase 6)
               #:fiveam)            ; Testing framework
  :serial t
  :components ((:module "src"
                :serial t
                :components
                (;; Layer 0: Package definitions
                 (:file "packages")

                 ;; Layer 1: FFI bindings
                 (:module "ffi"
                  :serial t
                  :components
                  ((:file "types")
                   (:file "exception-handling")
                   (:file "memory-management")
                   (:file "primitives")
                   (:file "booleans")
                   (:file "transformations")
                   (:file "queries")
                   (:file "export")
                   (:file "fillets")         ; Phase 8
                   (:file "curves")          ; Phase 8
                   (:file "advanced-ops")))  ; Phase 8

                 ;; Layer 2: Functional core
                 (:module "core"
                  :serial t
                  :components
                  ((:file "primitives")
                   (:file "booleans")
                   (:file "transformations")
                   (:file "fillets")          ; Phase 8
                   (:file "curves")           ; Phase 8
                   (:file "advanced-ops")))   ; Phase 8

                 ;; Layer 3: Units system
                 (:module "units"
                  :serial t
                  :components
                  ((:file "units")
                   (:file "conversions")
                   (:file "dimension")))

                 ;; Layer 3.5: GD&T System (Phase T2, T3)
                 (:module "gdt"
                  :serial t
                  :components
                  ((:file "datums")
                   (:file "geometric-tolerances")
                   (:file "validation")))

                 ;; Layer 4: CLOS shapes
                 (:module "shapes"
                  :serial t
                  :components
                  ((:file "classes")
                   (:file "methods")))

                 ;; Layer 5: Selectors (Phase 3, Phase 8)
                 (:module "selectors"
                  :serial t
                  :components
                  ((:file "base")
                   (:file "utilities")
                   (:file "direction")
                   (:file "geometric")
                   (:file "type")         ; Phase 8
                   (:file "size")         ; Phase 8
                   (:file "combinators")
                   (:file "position")     ; Phase 2 - Position-based selectors
                   (:file "custom")
                   (:file "inspection")   ; Phase 3 - Debugging/inspection tools
                   (:file "api")))

                 ;; Layer 5.5: Workplanes (Phase 4)
                 (:module "workplane"
                  :serial t
                  :components
                  ((:file "workplane")))

                 ;; Layer 5.75: Context API (Phase 4)
                 (:module "context"
                  :serial t
                  :components
                  ((:file "context")))

                 ;; Layer 5.9: DSL (Phase 5)
                 (:module "dsl"
                  :serial t
                  :components
                  ((:file "defpart")
                   (:file "patterns")
                   (:file "deffeature")))

                 ;; Layer 5.95: Analysis (Mass Properties)
                 (:module "analysis"
                  :serial t
                  :components
                  ((:file "mass-properties")))  ; Option 1: Mass properties analysis

                 ;; Layer 5.96: Features (Threads, Fasteners, etc.)
                 (:module "features"
                  :serial t
                  :components
                  ((:file "threads")           ; Thread modeling (simplified)
                   (:file "thread-profile")    ; Phase 1: Thread profile geometry
                   (:file "helical-path")      ; Phase 2: Helical path generation
                   (:file "helical-sweep")     ; Phase 3: Helical sweep operation
                   (:file "thread-boolean")))  ; Phase 4: Thread boolean operations & DSL

                 ;; Layer 6: Export functionality
                 (:module "export"
                  :serial t
                  :components
                  ((:file "step")
                   (:file "step-ap242")  ; Phase T4: STEP AP242 PMI export
                   (:file "stl")))       ; Option 1: STL export for 3D printing

                 ;; Layer 7: Viewer
                 (:module "viewer"
                  :serial t
                  :components
                  ((:file "server")))

                 ;; Layer 8: Auto-Rebuild (Phase 6)
                 (:module "auto-rebuild"
                  :serial t
                  :components
                  ((:file "auto-rebuild")))

                 ;; Layer 9: Sketch System (Phase 9)
                 (:module "sketch"
                  :serial t
                  :components
                  ((:file "entities")
                   (:file "constraints")
                   (:file "solver")
                   (:file "conversion")   ; Week 7-8: Sketch-to-3D
                   (:file "validation")   ; Week 9-10: Validation
                   (:file "dsl")))

                 ;; Layer 10: Assembly System (Phase 10)
                 (:module "assembly"
                  :serial t
                  :components
                  ((:file "assembly")
                   (:file "constraints")
                   (:file "solver")
                   (:file "bom")
                   (:file "dsl")
                   (:file "view")))))))

(defsystem "clad/cli"
  :description "CLI interface for CLAD"
  :version "0.1.0"
  :depends-on (#:clad)
  :serial t
  :components ((:module "src"
                :components
                ((:module "cli"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "args")
                   (:file "output")
                   (:file "commands")
                   (:file "main")))))))

(defsystem "clad/tests"
  :description "Test suite for CLAD"
  :depends-on (#:clad #:clad/cli #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "test-suite")
                 (:file "ffi-tests")
                 (:file "core-tests")
                 (:file "units-tests")
                 (:file "tolerance-tests")            ; Phase T1: Dimensional tolerancing
                 (:file "datum-tests")                ; Phase T2: Datum system
                 (:file "gdt-tests")                  ; Phase T3: Geometric tolerancing
                 (:file "gdt-validation-tests")       ; Priority 1: GD&T validation
                 (:file "selector-validation-tests")  ; Priority 2: Selector validation
                 (:file "gdt-edge-cases-tests")       ; Priority 4: Edge case test suite
                 (:file "step-pmi-tests")             ; Phase T4: STEP AP242 PMI export
                 (:file "stl-export-tests")           ; Option 1: STL export tests (TDD)
                 (:file "mass-properties-tests")      ; Option 1: Mass properties tests (TDD)
                 (:file "thread-tests")               ; Option 1: Thread modeling tests (TDD)
                 (:file "thread-profile-tests")       ; Phase 1: Thread profile geometry tests (TDD)
                 (:file "helical-path-tests")         ; Phase 2: Helical path generation tests (TDD)
                 (:file "helical-sweep-tests")        ; Phase 3: Helical sweep operation tests (TDD)
                 (:file "thread-boolean-tests")       ; Phase 4: Thread boolean operations tests (TDD)
                 (:file "thread-dsl-tests")           ; Phase 4: Thread DSL integration tests (TDD)
                 (:file "shapes-tests")
                 (:file "selector-tests")
                 (:file "selector-combinator-tests")  ; Phase 1 TDD - AND/OR/NOT combinators
                 (:file "position-selector-tests")    ; Phase 2 TDD - Position-based selectors
                 (:file "selector-inspection-tests")  ; Phase 3 TDD - Inspection/debugging
                 (:file "workplane-tests")
                 (:file "face-plane-tests")           ; Phase 4 TDD - Face-plane operations
                 (:file "context-tests")
                 (:file "dsl-tests")
                 (:file "advanced-features-tests")  ; Phase 8: selectors + fillets
                 (:file "sketch-tests")             ; Phase 9: sketch system
                 (:file "assembly-tests")           ; Phase 10: assembly system
                 (:file "cli-tests"))))             ; CLI tests
  :perform (test-op (op c)
                    (declare (ignorable op c))
                    (symbol-call :fiveam '#:run! :clad-tests)))
