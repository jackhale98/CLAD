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
                   (:file "custom")
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

                 ;; Layer 6: Export functionality
                 (:module "export"
                  :serial t
                  :components
                  ((:file "step")))

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
                   (:file "dsl")))))))


(defsystem "clad/tests"
  :description "Test suite for CLAD"
  :depends-on (#:clad #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "test-suite")
                 (:file "ffi-tests")
                 (:file "core-tests")
                 (:file "units-tests")
                 (:file "shapes-tests")
                 (:file "selector-tests")
                 (:file "workplane-tests")
                 (:file "context-tests")
                 (:file "dsl-tests")
                 (:file "advanced-features-tests")  ; Phase 8: selectors + fillets
                 (:file "sketch-tests"))))          ; Phase 9: sketch system
  :perform (test-op (op c)
                    (declare (ignorable op c))
                    (symbol-call :fiveam '#:run! :clad-tests)))
