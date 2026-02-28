;;;; src/cli/main.lisp
;;;; Main entry point for CLAD CLI

(in-package #:clad/cli)

(defun main (&optional (args (uiop:command-line-arguments)))
  "Main entry point for the CLI.
   ARGS should be a list of command-line argument strings.
   Returns an exit code (0 for success, non-zero for error)."
  (handler-case
      (let ((opts (parse-arguments args)))
        (dispatch-command opts))
    ;; CLI argument errors
    (cli-argument-error (e)
      (print-error "~A" (cli-argument-error-message e))
      (format *error-output* "Run 'clad --help' for usage.~%")
      1)
    ;; OCCT construction errors
    (clad.ffi:occt-construction-error (e)
      (print-error "~A" (format-lisp-error e))
      1)
    ;; OCCT errors
    (clad.ffi:occt-error (e)
      (print-error "~A" (format-lisp-error e))
      1)
    ;; File errors
    (file-error (e)
      (print-error "File not found: ~A" (file-error-pathname e))
      1)
    ;; Reader errors (syntax errors in design files)
    (reader-error (e)
      (print-error "Syntax error in design file: ~A" e)
      1)
    ;; Catch-all for unexpected errors
    (error (e)
      (print-error "Unexpected error: ~A" e)
      2)))

(defun run-cli ()
  "Run the CLI and exit with appropriate status code"
  (let ((exit-code (main)))
    (uiop:quit exit-code)))

;;; ============================================================================
;;; Build Support
;;; ============================================================================

(defun build-executable (&optional (output-path "clad"))
  "Build a standalone executable"
  #+sbcl
  (sb-ext:save-lisp-and-die output-path
                            :toplevel #'run-cli
                            :executable t
                            :compression t
                            :save-runtime-options t)
  #-sbcl
  (error "Building executables is only supported on SBCL"))
