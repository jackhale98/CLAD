;;;; src/cli/commands.lisp
;;;; Command implementations for CLAD CLI

(in-package #:clad/cli)

;;; ============================================================================
;;; Design File Loading
;;; ============================================================================

(defun load-design-file (filepath)
  "Load a design file safely. Returns T on success, signals error on failure."
  (let ((path (probe-file filepath)))
    (unless path
      (error 'file-error :pathname filepath))
    (load path :verbose nil :print nil)
    t))

;;; ============================================================================
;;; Part Discovery
;;; ============================================================================

(defun discover-parts ()
  "Find all parts defined with defpart across all packages.
   Returns a sorted list of (symbol . param-info) where param-info
   is a list of (name default) pairs extracted from the function's lambda list."
  (let ((parts nil))
    (dolist (pkg (list-all-packages))
      (do-symbols (sym pkg)
        (when (and (get sym 'clad.dsl::clad-part-function)
                   (fboundp sym))
          (push (cons sym (extract-part-params sym)) parts))))
    ;; Sort by symbol name
    (sort parts #'string< :key (lambda (entry) (symbol-name (car entry))))))

(defun extract-part-params (part-symbol)
  "Extract parameter names and defaults from a part function's lambda list.
   Returns a list of (name default) pairs."
  (handler-case
      (let ((lambda-list (sb-introspect:function-lambda-list
                          (symbol-function part-symbol))))
        ;; Skip &optional marker, collect (name default) pairs
        (let ((params nil)
              (in-optional nil))
          (dolist (item lambda-list)
            (cond
              ((eq item '&optional) (setf in-optional t))
              ((eq item '&rest) (return))
              ((eq item '&key) (return))
              (in-optional
               (if (listp item)
                   (push (list (first item) (second item)) params)
                   (push (list item nil) params)))))
          (nreverse params)))
    (error ()
      ;; If introspection fails, return empty list
      nil)))

;;; ============================================================================
;;; Part Resolution
;;; ============================================================================

(defun resolve-part (opts parts)
  "Resolve which part to use based on --part option and discovered parts.
   Returns (values part-symbol kwargs) where kwargs is a keyword plist."
  (let ((part-name (option-part opts))
        (part-sym nil))
    (cond
      ;; Explicit --part given
      (part-name
       (setf part-sym
             (find part-name parts
                   :test (lambda (name entry)
                           (string-equal name (symbol-name (car entry))))
                   :key #'identity))
       (unless part-sym
         (error 'cli-argument-error
                :message (format nil "Part '~A' not found. Available parts: ~{~A~^, ~}"
                                 part-name
                                 (mapcar (lambda (e) (string-downcase (symbol-name (car e))))
                                         parts))))
       (setf part-sym (car part-sym)))

      ;; No --part, exactly one part found
      ((= (length parts) 1)
       (setf part-sym (car (first parts))))

      ;; No --part, multiple parts
      ((> (length parts) 1)
       (error 'cli-argument-error
              :message (format nil "Multiple parts found. Use --part to specify one: ~{~A~^, ~}"
                               (mapcar (lambda (e) (string-downcase (symbol-name (car e))))
                                       parts))))

      ;; No parts at all
      (t
       (error 'cli-argument-error
              :message "No parts found in design file. Define parts with (defpart ...)")))

    ;; Build kwargs from --param overrides
    (values part-sym (option-params opts))))

;;; ============================================================================
;;; Build Command
;;; ============================================================================

(defun execute-build-command (opts)
  "Export CAD files from a design file."
  (let ((file (option-file opts)))
    (unless file
      (error 'cli-argument-error :message "build command requires a design file"))

    ;; Load the design
    (print-info "Loading design: ~A" file)
    (load-design-file file)

    ;; Discover and resolve part
    (let ((parts (discover-parts)))
      (multiple-value-bind (part-sym kwargs) (resolve-part opts parts)
        (print-info "Building part: ~A" (string-downcase (symbol-name part-sym)))

        ;; Generate the shape
        (let ((shape (if kwargs
                         (apply (symbol-function part-sym) kwargs)
                         (funcall (symbol-function part-sym)))))

          ;; Determine output directory
          (let ((output-dir (or (option-output-dir opts) ".")))
            (ensure-directories-exist (format nil "~A/" output-dir))

            ;; Default to STEP if no format specified
            (let ((any-format (or (option-step opts)
                                  (option-stl opts)
                                  (option-gltf opts))))
              (unless any-format
                (setf (option-step opts) t))

              ;; Export STEP
              (when (option-step opts)
                (let ((filename (format nil "~A/~A.step"
                                        output-dir
                                        (string-downcase (symbol-name part-sym)))))
                  (print-info "Exporting STEP: ~A" filename)
                  (clad.export:export-step shape filename)))

              ;; Export STL
              (when (option-stl opts)
                (let ((filename (format nil "~A/~A.stl"
                                        output-dir
                                        (string-downcase (symbol-name part-sym)))))
                  (print-info "Exporting STL: ~A (~A)" filename (option-resolution opts))
                  (clad.export:export-stl shape filename
                                         :ascii (option-ascii opts)
                                         :resolution (option-resolution opts))))

              ;; Export glTF
              (when (option-gltf opts)
                (let ((filename (format nil "~A/~A.glb"
                                        output-dir
                                        (string-downcase (symbol-name part-sym)))))
                  (print-info "Exporting glTF: ~A" filename)
                  (clad.export:export-gltf shape filename))))))

        (print-info "Build complete.")
        0))))

;;; ============================================================================
;;; View Command
;;; ============================================================================

(defun execute-view-command (opts)
  "Open a part in the 3D viewer."
  (let ((file (option-file opts)))
    (unless file
      (error 'cli-argument-error :message "view command requires a design file"))

    (print-info "Loading design: ~A" file)
    (load-design-file file)

    (let ((parts (discover-parts)))
      (multiple-value-bind (part-sym kwargs) (resolve-part opts parts)
        (print-info "Viewing part: ~A" (string-downcase (symbol-name part-sym)))

        (let ((shape (if kwargs
                         (apply (symbol-function part-sym) kwargs)
                         (funcall (symbol-function part-sym)))))

          ;; Start viewer
          (clad.viewer:start-viewer
           :port (option-port opts)
           :open-browser (not (option-no-browser opts)))

          ;; Display the shape
          (clad.viewer:view shape
                           :name (string-downcase (symbol-name part-sym))
                           :auto-start nil)

          (print-info "Viewer running on port ~A. Press Ctrl+C to stop." (option-port opts))

          ;; Block to keep process alive
          (handler-case
              (loop (sleep 1))
            (#+sbcl sb-sys:interactive-interrupt
             #-sbcl condition ()
              (format t "~&Shutting down viewer...~%")
              (clad.viewer:stop-viewer))))))
    0))

;;; ============================================================================
;;; Watch Command
;;; ============================================================================

(defun execute-watch-command (opts)
  "Watch a file for changes and auto-rebuild."
  (let ((file (option-file opts)))
    (unless file
      (error 'cli-argument-error :message "watch command requires a design file"))

    (print-info "Loading design: ~A" file)
    (load-design-file file)

    (let ((parts (discover-parts)))
      (multiple-value-bind (part-sym kwargs) (resolve-part opts parts)
        (declare (ignore kwargs))

        ;; Show the part first
        (clad.auto-rebuild:show part-sym)

        ;; Watch the file
        (clad.auto-rebuild:watch file part-sym :interval (option-interval opts))

        (print-info "Watching ~A for changes (interval: ~As). Press Ctrl+C to stop."
                    file (option-interval opts))

        ;; Block to keep process alive
        (handler-case
            (loop (sleep 1))
          (#+sbcl sb-sys:interactive-interrupt
           #-sbcl condition ()
            (format t "~&Stopping watchers...~%")
            (clad.auto-rebuild:stop-all-watchers)
            (clad.viewer:stop-viewer))))))
  0)

;;; ============================================================================
;;; Info Command
;;; ============================================================================

(defun execute-info-command (opts)
  "Show information about parts in a design file."
  (let ((file (option-file opts)))
    (unless file
      (error 'cli-argument-error :message "info command requires a design file"))

    (print-info "Loading design: ~A" file)
    (load-design-file file)

    (let ((parts (discover-parts)))
      (if (option-part opts)
          ;; Show info for specific part
          (multiple-value-bind (part-sym kwargs) (resolve-part opts parts)
            (print-info "Part: ~A" (string-downcase (symbol-name part-sym)))

            (when (or (option-mass-properties opts) (option-material opts))
              ;; Build shape and show mass properties
              (let* ((shape (if kwargs
                                (apply (symbol-function part-sym) kwargs)
                                (funcall (symbol-function part-sym))))
                     (props (if (option-material opts)
                                (clad.analysis:mass-properties shape
                                                              :material (option-material opts))
                                (clad.analysis:mass-properties shape))))
                (format-mass-properties props :json (option-json opts))))

            ;; Show parameters
            (unless (option-mass-properties opts)
              (let ((entry (find part-sym parts :key #'car)))
                (format-part-info (list entry) :json (option-json opts)))))

          ;; List all parts
          (format-part-info parts :json (option-json opts)))))
  0)

;;; ============================================================================
;;; Check Command
;;; ============================================================================

(defun execute-check-command (opts)
  "Validate all parts in a design file."
  (let ((file (option-file opts)))
    (unless file
      (error 'cli-argument-error :message "check command requires a design file"))

    (print-info "Loading design: ~A" file)
    (load-design-file file)

    (let ((parts (discover-parts))
          (passed 0)
          (failed 0)
          (failures nil))

      (dolist (entry parts)
        (let ((sym (car entry)))
          (format t "  Checking ~A... " (string-downcase (symbol-name sym)))
          (finish-output)
          (handler-case
              (let ((shape (funcall (symbol-function sym))))
                (if (and shape (clad.core:valid-shape-p shape))
                    (progn
                      (format t "OK~%")
                      (incf passed))
                    (progn
                      (format t "FAIL (invalid shape)~%")
                      (incf failed)
                      (push (cons sym "produced invalid shape") failures))))
            (error (e)
              (format t "FAIL~%")
              (incf failed)
              (push (cons sym (format nil "~A" e)) failures)))))

      (format t "~%Results: ~D passed, ~D failed out of ~D parts~%"
              passed failed (+ passed failed))

      (when failures
        (format t "~%Failures:~%")
        (dolist (f (nreverse failures))
          (format t "  ~A: ~A~%"
                  (string-downcase (symbol-name (car f)))
                  (cdr f))))

      (if (zerop failed) 0 1))))

;;; ============================================================================
;;; REPL Command
;;; ============================================================================

(defun execute-repl-command (opts)
  "Start an interactive REPL, optionally loading a design file."
  (let ((file (option-file opts)))
    ;; Optionally load file
    (when file
      (print-info "Loading design: ~A" file)
      (load-design-file file))

    ;; Print banner
    (format t "~%")
    (format t "================================================================================~%")
    (format t "CLAD Interactive REPL~%")
    (format t "================================================================================~%")
    (format t "~%")
    (when file
      (let ((parts (discover-parts)))
        (format t "Design loaded: ~A~%" file)
        (format t "Parts found: ~D~%" (length parts))
        (dolist (entry parts)
          (format t "  - ~A~%" (string-downcase (symbol-name (car entry)))))
        (format t "~%")))
    (format t "Quick start:~%")
    (format t "  (clad:view (my-part))                    ; View a part~%")
    (format t "  (clad:export-step (my-part) \"out.step\")  ; Export to STEP~%")
    (format t "  (clad:show 'my-part)                     ; Show with auto-rebuild~%")
    (format t "~%")
    (format t "Type (quit) or Ctrl-D to exit.~%")
    (format t "================================================================================~%")
    (format t "~%")

    ;; Enter REPL
    #+sbcl
    (sb-impl::toplevel-repl nil)
    #-sbcl
    (progn
      (format t "Interactive REPL is only supported on SBCL.~%")
      (format t "Please use your Lisp implementation's REPL directly.~%")))
  0)

;;; ============================================================================
;;; Command Dispatch
;;; ============================================================================

(defun dispatch-command (opts)
  "Dispatch to the appropriate command based on options.
   Returns an exit code (0 for success, non-zero for error)."
  (let ((*quiet* (option-quiet opts)))
    (cond
      ;; Help and version (highest priority)
      ((option-help opts)
       (print-usage)
       0)
      ((option-version opts)
       (print-version)
       0)

      ;; Command dispatch
      ((null (option-command opts))
       (print-error "No command specified. Run 'clad --help' for usage.")
       1)

      ((string-equal (option-command opts) "build")
       (execute-build-command opts))

      ((string-equal (option-command opts) "view")
       (execute-view-command opts))

      ((string-equal (option-command opts) "watch")
       (execute-watch-command opts))

      ((string-equal (option-command opts) "info")
       (execute-info-command opts))

      ((string-equal (option-command opts) "check")
       (execute-check-command opts))

      ((string-equal (option-command opts) "repl")
       (execute-repl-command opts))

      (t
       (print-error "Unknown command: ~A. Run 'clad --help' for usage."
                    (option-command opts))
       1))))
