;;;; src/cli/package.lisp
;;;; Package definition for CLAD CLI

(defpackage #:clad/cli
  (:use #:cl)
  (:export
   ;; Main entry points
   #:main
   #:run-cli

   ;; CLI options
   #:cli-options
   #:option-command
   #:option-file
   #:option-output-dir
   #:option-part
   #:option-params
   #:option-step
   #:option-stl
   #:option-gltf
   #:option-resolution
   #:option-ascii
   #:option-port
   #:option-no-browser
   #:option-interval
   #:option-mass-properties
   #:option-material
   #:option-json
   #:option-quiet
   #:option-help
   #:option-version

   ;; Argument parsing
   #:parse-arguments
   #:cli-argument-error

   ;; Output formatting
   #:print-error
   #:print-info
   #:print-warning
   #:format-lisp-error
   #:format-part-info
   #:format-mass-properties
   #:to-json-string

   ;; Commands
   #:dispatch-command
   #:load-design-file
   #:discover-parts
   #:resolve-part
   #:execute-build-command
   #:execute-view-command
   #:execute-watch-command
   #:execute-info-command
   #:execute-check-command
   #:execute-repl-command

   ;; Build
   #:build-executable

   ;; Version
   #:*clad-cli-version*))
