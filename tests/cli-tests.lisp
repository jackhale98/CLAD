;;;; tests/cli-tests.lisp --- Tests for CLAD CLI

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite cli-tests
  :description "Tests for the CLAD CLI"
  :in clad-tests)

(in-suite cli-tests)

;;; ============================================================================
;;; Argument Parsing: Basic Tests
;;; ============================================================================

(test cli-parse-help-flag
  "Parsing --help flag"
  (let ((opts (clad/cli:parse-arguments '("--help"))))
    (is-true (clad/cli:option-help opts)))
  (let ((opts (clad/cli:parse-arguments '("-h"))))
    (is-true (clad/cli:option-help opts))))

(test cli-parse-version-flag
  "Parsing --version flag"
  (let ((opts (clad/cli:parse-arguments '("--version"))))
    (is-true (clad/cli:option-version opts)))
  (let ((opts (clad/cli:parse-arguments '("-V"))))
    (is-true (clad/cli:option-version opts))))

(test cli-parse-command-and-file
  "Parsing command and file positional arguments"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp"))))
    (is (equal "build" (clad/cli:option-command opts)))
    (is (equal "design.lisp" (clad/cli:option-file opts)))))

(test cli-parse-command-only
  "Parsing command without file"
  (let ((opts (clad/cli:parse-arguments '("repl"))))
    (is (equal "repl" (clad/cli:option-command opts)))
    (is (null (clad/cli:option-file opts)))))

(test cli-parse-empty-args
  "Parsing empty arguments"
  (let ((opts (clad/cli:parse-arguments '())))
    (is (null (clad/cli:option-command opts)))
    (is (null (clad/cli:option-file opts)))
    (is (null (clad/cli:option-help opts)))))

;;; ============================================================================
;;; Argument Parsing: Build Format Flags
;;; ============================================================================

(test cli-parse-step-flag
  "Parsing --step format flag"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--step"))))
    (is-true (clad/cli:option-step opts))))

(test cli-parse-stl-flag
  "Parsing --stl format flag"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--stl"))))
    (is-true (clad/cli:option-stl opts))))

(test cli-parse-gltf-flag
  "Parsing --gltf format flag"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--gltf"))))
    (is-true (clad/cli:option-gltf opts))))

(test cli-parse-multiple-formats
  "Parsing multiple format flags together"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--step" "--stl" "--gltf"))))
    (is-true (clad/cli:option-step opts))
    (is-true (clad/cli:option-stl opts))
    (is-true (clad/cli:option-gltf opts))))

(test cli-parse-ascii-flag
  "Parsing --ascii flag for STL"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--stl" "--ascii"))))
    (is-true (clad/cli:option-stl opts))
    (is-true (clad/cli:option-ascii opts))))

;;; ============================================================================
;;; Argument Parsing: Options with Values
;;; ============================================================================

(test cli-parse-output-dir
  "Parsing --output-dir with value"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--output-dir" "./out"))))
    (is (equal "./out" (clad/cli:option-output-dir opts)))))

(test cli-parse-part-name
  "Parsing --part with value"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--part" "bracket"))))
    (is (equal "bracket" (clad/cli:option-part opts)))))

(test cli-parse-resolution
  "Parsing --resolution with various values"
  (dolist (res '("low" "medium" "high" "ultra"))
    (let ((opts (clad/cli:parse-arguments
                 (list "build" "design.lisp" "--stl" "--resolution" res))))
      (is (eq (intern (string-upcase res) :keyword)
              (clad/cli:option-resolution opts))))))

(test cli-parse-port
  "Parsing --port with value"
  (let ((opts (clad/cli:parse-arguments '("view" "design.lisp" "--port" "3000"))))
    (is (= 3000 (clad/cli:option-port opts)))))

(test cli-parse-interval
  "Parsing --interval with value"
  (let ((opts (clad/cli:parse-arguments '("watch" "design.lisp" "--interval" "2.0"))))
    (is (= 2.0 (clad/cli:option-interval opts)))))

(test cli-parse-material
  "Parsing --material with value"
  (let ((opts (clad/cli:parse-arguments '("info" "design.lisp" "--material" "aluminum"))))
    (is (eq :aluminum (clad/cli:option-material opts)))))

;;; ============================================================================
;;; Argument Parsing: --param Accumulation
;;; ============================================================================

(test cli-parse-single-param
  "Parsing single --param key=value"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--param" "width=150"))))
    (is (equal '(:width 150) (clad/cli:option-params opts)))))

(test cli-parse-multiple-params
  "Parsing multiple --param flags accumulate"
  (let ((opts (clad/cli:parse-arguments
               '("build" "design.lisp" "--param" "width=150" "--param" "thickness=8"))))
    (is (equal '(:width 150 :thickness 8) (clad/cli:option-params opts)))))

(test cli-parse-param-float-value
  "Parsing --param with float value"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--param" "radius=25.5"))))
    (is (eq :radius (first (clad/cli:option-params opts))))
    (is (= 25.5 (second (clad/cli:option-params opts))))))

;;; ============================================================================
;;; Argument Parsing: Boolean Flags
;;; ============================================================================

(test cli-parse-no-browser
  "Parsing --no-browser flag"
  (let ((opts (clad/cli:parse-arguments '("view" "design.lisp" "--no-browser"))))
    (is-true (clad/cli:option-no-browser opts))))

(test cli-parse-mass-properties
  "Parsing --mass-properties flag"
  (let ((opts (clad/cli:parse-arguments '("info" "design.lisp" "--mass-properties"))))
    (is-true (clad/cli:option-mass-properties opts))))

(test cli-parse-json-flag
  "Parsing --json flag"
  (let ((opts (clad/cli:parse-arguments '("info" "design.lisp" "--json"))))
    (is-true (clad/cli:option-json opts))))

(test cli-parse-quiet-flag
  "Parsing --quiet flag"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--quiet"))))
    (is-true (clad/cli:option-quiet opts))))

;;; ============================================================================
;;; Argument Parsing: Default Values
;;; ============================================================================

(test cli-default-resolution
  "Default resolution is :medium"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp"))))
    (is (eq :medium (clad/cli:option-resolution opts)))))

(test cli-default-port
  "Default port is 8080"
  (let ((opts (clad/cli:parse-arguments '("view" "design.lisp"))))
    (is (= 8080 (clad/cli:option-port opts)))))

(test cli-default-interval
  "Default interval is 0.5"
  (let ((opts (clad/cli:parse-arguments '("watch" "design.lisp"))))
    (is (= 0.5 (clad/cli:option-interval opts)))))

;;; ============================================================================
;;; Argument Parsing: Error Cases
;;; ============================================================================

(test cli-parse-unknown-option-signals-error
  "Unknown option signals cli-argument-error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("--unknown-flag"))))

(test cli-parse-missing-output-dir-value
  "Missing value for --output-dir signals error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("build" "design.lisp" "--output-dir"))))

(test cli-parse-missing-part-value
  "Missing value for --part signals error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("build" "design.lisp" "--part"))))

(test cli-parse-missing-resolution-value
  "Missing value for --resolution signals error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("build" "design.lisp" "--resolution"))))

(test cli-parse-invalid-resolution-value
  "Invalid resolution value signals error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("build" "design.lisp" "--resolution" "extreme"))))

(test cli-parse-missing-port-value
  "Missing value for --port signals error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("view" "design.lisp" "--port"))))

(test cli-parse-invalid-port-value
  "Invalid port value signals error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("view" "design.lisp" "--port" "abc"))))

(test cli-parse-missing-param-value
  "Missing value for --param signals error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("build" "design.lisp" "--param"))))

(test cli-parse-invalid-param-format
  "Invalid --param format (no =) signals error"
  (signals clad/cli:cli-argument-error
    (clad/cli:parse-arguments '("build" "design.lisp" "--param" "width"))))

;;; ============================================================================
;;; Argument Parsing: Combined Options
;;; ============================================================================

(test cli-parse-full-build-command
  "Parsing a realistic full build command"
  (let ((opts (clad/cli:parse-arguments
               '("build" "design.lisp"
                 "--part" "bracket"
                 "--step" "--stl"
                 "--resolution" "high"
                 "--output-dir" "./output"
                 "--param" "width=150"
                 "--param" "thickness=8"
                 "--quiet"))))
    (is (equal "build" (clad/cli:option-command opts)))
    (is (equal "design.lisp" (clad/cli:option-file opts)))
    (is (equal "bracket" (clad/cli:option-part opts)))
    (is-true (clad/cli:option-step opts))
    (is-true (clad/cli:option-stl opts))
    (is (eq :high (clad/cli:option-resolution opts)))
    (is (equal "./output" (clad/cli:option-output-dir opts)))
    (is (equal '(:width 150 :thickness 8) (clad/cli:option-params opts)))
    (is-true (clad/cli:option-quiet opts))))

(test cli-parse-full-info-command
  "Parsing a realistic info command"
  (let ((opts (clad/cli:parse-arguments
               '("info" "design.lisp"
                 "--part" "bracket"
                 "--mass-properties"
                 "--material" "steel"
                 "--json"))))
    (is (equal "info" (clad/cli:option-command opts)))
    (is (equal "design.lisp" (clad/cli:option-file opts)))
    (is (equal "bracket" (clad/cli:option-part opts)))
    (is-true (clad/cli:option-mass-properties opts))
    (is (eq :steel (clad/cli:option-material opts)))
    (is-true (clad/cli:option-json opts))))

;;; ============================================================================
;;; Output Formatting: JSON Serialization
;;; ============================================================================

(test cli-json-null
  "JSON null serialization"
  (is (equal "null" (clad/cli:to-json-string nil))))

(test cli-json-true
  "JSON true serialization"
  (is (equal "true" (clad/cli:to-json-string t))))

(test cli-json-integer
  "JSON integer serialization"
  (is (equal "42" (clad/cli:to-json-string 42))))

(test cli-json-float
  "JSON float serialization"
  (let ((result (clad/cli:to-json-string 3.14)))
    (is (search "3.14" result))))

(test cli-json-string
  "JSON string serialization"
  (is (equal "\"hello\"" (clad/cli:to-json-string "hello"))))

(test cli-json-string-escaping
  "JSON string escaping"
  (let ((result (clad/cli:to-json-string "hello \"world\"")))
    (is (search "\\\"" result))))

(test cli-json-keyword
  "JSON keyword serialization"
  (is (equal "\"aluminum\"" (clad/cli:to-json-string :aluminum))))

(test cli-json-list-as-array
  "JSON list serialization as array"
  (is (equal "[1,2,3]" (clad/cli:to-json-string '(1 2 3)))))

(test cli-json-plist-as-object
  "JSON plist serialization as object"
  (let ((result (clad/cli:to-json-string '(:name "test" :value 42))))
    (is (search "\"name\":\"test\"" result))
    (is (search "\"value\":42" result))))

;;; ============================================================================
;;; Output Formatting: Text Output
;;; ============================================================================

(test cli-print-info-respects-quiet
  "print-info respects *quiet* binding"
  (let ((output (with-output-to-string (*standard-output*)
                  (let ((clad/cli::*quiet* t))
                    (clad/cli:print-info "should not appear")))))
    (is (equal "" output))))

(test cli-print-info-normal
  "print-info outputs when not quiet"
  (let ((output (with-output-to-string (*standard-output*)
                  (let ((clad/cli::*quiet* nil))
                    (clad/cli:print-info "hello ~A" "world")))))
    (is (search "hello world" output))))

(test cli-print-error-to-stderr
  "print-error writes to *error-output*"
  (let ((output (with-output-to-string (*error-output*)
                  (clad/cli:print-error "test error ~A" "message"))))
    (is (search "Error: test error message" output))))

(test cli-print-warning-to-stderr
  "print-warning writes to *error-output*"
  (let ((output (with-output-to-string (*error-output*)
                  (clad/cli:print-warning "test warning"))))
    (is (search "Warning: test warning" output))))

;;; ============================================================================
;;; Output Formatting: Part Info
;;; ============================================================================

(test cli-format-part-info-text
  "format-part-info in text mode"
  (let ((output (with-output-to-string (*standard-output*)
                  (clad/cli:format-part-info
                   '((test-part . ((width 100) (height 50))))))))
    (is (search "Parts found: 1" output))
    (is (search "test-part" output))))

(test cli-format-part-info-json
  "format-part-info in JSON mode"
  (let ((output (with-output-to-string (*standard-output*)
                  (clad/cli:format-part-info
                   '((test-part . ((width 100) (height 50))))
                   :json t))))
    (is (search "\"name\":\"test-part\"" output))
    (is (search "\"parameters\"" output))))

;;; ============================================================================
;;; Command Dispatch: Help and Version
;;; ============================================================================

(test cli-dispatch-help
  "Dispatch --help returns 0 and prints usage"
  (let ((output (with-output-to-string (*standard-output*)
                  (let ((result (clad/cli:main '("--help"))))
                    (is (= 0 result))))))
    (is (search "Usage:" output))))

(test cli-dispatch-version
  "Dispatch --version returns 0 and prints version"
  (let ((output (with-output-to-string (*standard-output*)
                  (let ((result (clad/cli:main '("--version"))))
                    (is (= 0 result))))))
    (is (search "clad" output))))

(test cli-dispatch-no-command
  "Dispatch with no command returns 1"
  (let* ((err-output (with-output-to-string (*error-output*)
                       (let ((result (clad/cli:main '())))
                         (is (= 1 result))))))
    (is (search "Error:" err-output))))

(test cli-dispatch-unknown-command
  "Dispatch with unknown command returns 1"
  (let* ((err-output (with-output-to-string (*error-output*)
                       (let ((result (clad/cli:main '("foo"))))
                         (is (= 1 result))))))
    (is (search "Error:" err-output))))

;;; ============================================================================
;;; Command Dispatch: Build Without File
;;; ============================================================================

(test cli-build-no-file-returns-error
  "Build command without file returns error"
  (let ((err-output (with-output-to-string (*error-output*)
                      (let ((result (clad/cli:main '("build"))))
                        (is (= 1 result))))))
    (is (search "Error:" err-output))))

(test cli-info-no-file-returns-error
  "Info command without file returns error"
  (let ((err-output (with-output-to-string (*error-output*)
                      (let ((result (clad/cli:main '("info"))))
                        (is (= 1 result))))))
    (is (search "Error:" err-output))))

(test cli-check-no-file-returns-error
  "Check command without file returns error"
  (let ((err-output (with-output-to-string (*error-output*)
                      (let ((result (clad/cli:main '("check"))))
                        (is (= 1 result))))))
    (is (search "Error:" err-output))))

;;; ============================================================================
;;; Command Dispatch: Nonexistent File
;;; ============================================================================

(test cli-build-nonexistent-file
  "Build command with nonexistent file returns error"
  (let ((err-output (with-output-to-string (*error-output*)
                      (with-output-to-string (*standard-output*)
                        (let ((result (clad/cli:main '("build" "/nonexistent/file.lisp"))))
                          (is (= 1 result)))))))
    (is (search "File not found" err-output))))

;;; ============================================================================
;;; Part Discovery
;;; ============================================================================

(test cli-discover-parts-finds-defpart
  "discover-parts finds parts defined with defpart"
  ;; Define a test part
  (clad.dsl:defpart cli-test-part-alpha ((width 100) (height 50))
    "Test part for CLI discovery"
    (:body (clad.core:make-box width height 10)))

  ;; Discover parts - should include our test part
  (let ((parts (clad/cli:discover-parts)))
    (is (find 'cli-test-part-alpha parts :key #'car))))

(test cli-discover-parts-extracts-params
  "discover-parts extracts parameter names and defaults"
  ;; Define a test part with known params
  (clad.dsl:defpart cli-test-part-beta ((radius 25) (length 100))
    "Test part for param extraction"
    (:body (clad.core:make-cylinder radius length)))

  (let* ((parts (clad/cli:discover-parts))
         (entry (find 'cli-test-part-beta parts :key #'car)))
    (is (not (null entry)))
    (let ((params (cdr entry)))
      ;; Should have 2 parameters
      (is (= 2 (length params)))
      ;; Check param names
      (is (find 'radius params :key #'first))
      (is (find 'length params :key #'first)))))

;;; ============================================================================
;;; Part Resolution
;;; ============================================================================

(test cli-resolve-part-single
  "resolve-part auto-selects when only one part"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp")))
        (parts '((my-single-part . ((width 100))))))
    (multiple-value-bind (sym kwargs) (clad/cli:resolve-part opts parts)
      (is (eq 'my-single-part sym))
      (is (null kwargs)))))

(test cli-resolve-part-by-name
  "resolve-part finds part by --part name"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--part" "widget")))
        (parts '((bracket . ((width 100)))
                 (widget . ((size 50))))))
    (multiple-value-bind (sym kwargs) (clad/cli:resolve-part opts parts)
      (is (eq 'widget sym)))))

(test cli-resolve-part-with-params
  "resolve-part passes --param overrides as kwargs"
  (let ((opts (clad/cli:parse-arguments
               '("build" "design.lisp" "--param" "width=200")))
        (parts '((single-part . ((width 100))))))
    (multiple-value-bind (sym kwargs) (clad/cli:resolve-part opts parts)
      (is (eq 'single-part sym))
      (is (equal '(:width 200) kwargs)))))

(test cli-resolve-part-multiple-no-selection-signals-error
  "resolve-part errors when multiple parts and no --part"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp")))
        (parts '((part-a . nil) (part-b . nil))))
    (signals clad/cli:cli-argument-error
      (clad/cli:resolve-part opts parts))))

(test cli-resolve-part-not-found-signals-error
  "resolve-part errors when --part name doesn't match"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp" "--part" "nonexistent")))
        (parts '((bracket . nil) (widget . nil))))
    (signals clad/cli:cli-argument-error
      (clad/cli:resolve-part opts parts))))

(test cli-resolve-part-no-parts-signals-error
  "resolve-part errors when no parts found"
  (let ((opts (clad/cli:parse-arguments '("build" "design.lisp")))
        (parts '()))
    (signals clad/cli:cli-argument-error
      (clad/cli:resolve-part opts parts))))

;;; ============================================================================
;;; Main Error Handling
;;; ============================================================================

(test cli-main-catches-cli-argument-error
  "main catches cli-argument-error and returns 1"
  (let ((*error-output* (make-string-output-stream)))
    (is (= 1 (clad/cli:main '("--unknown"))))))

(test cli-main-catches-file-error
  "main catches file-error and returns 1"
  (let ((*error-output* (make-string-output-stream))
        (*standard-output* (make-string-output-stream)))
    (is (= 1 (clad/cli:main '("build" "/no/such/file.lisp"))))))

;;; ============================================================================
;;; Print Usage/Version Output
;;; ============================================================================

(test cli-print-usage-contains-commands
  "print-usage lists all commands"
  (let ((output (with-output-to-string (*standard-output*)
                  (clad/cli::print-usage))))
    (is (search "build" output))
    (is (search "view" output))
    (is (search "watch" output))
    (is (search "info" output))
    (is (search "check" output))
    (is (search "repl" output))))

(test cli-print-version-contains-version
  "print-version contains version string"
  (let ((output (with-output-to-string (*standard-output*)
                  (clad/cli::print-version))))
    (is (search clad/cli:*clad-cli-version* output))))

;;; ============================================================================
;;; End of CLI Tests
;;; ============================================================================
