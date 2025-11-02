;;;; src/viewer/server.lisp --- Web-based 3D viewer using three.js

(in-package :clad.viewer)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defvar *viewer-port* 8080
  "Port for the web viewer server")

(defvar *viewer-server* nil
  "Hunchentoot acceptor instance")

(defvar *viewer-running-p* nil
  "T if viewer server is running")

(defvar *current-model-path* nil
  "Path to the currently displayed model")

(defvar *viewer-base-dir*
  (merge-pathnames "viewer/"
                   (asdf:system-source-directory :clad))
  "Base directory for viewer files")

(defvar *models-dir*
  (ensure-directories-exist
   (merge-pathnames "viewer/models/"
                    (asdf:system-source-directory :clad)))
  "Directory for exported models")

;;; ============================================================================
;;; Web Server
;;; ============================================================================

(defun start-viewer (&key (port *viewer-port*) (open-browser t))
  "Start the web viewer server.

  Arguments:
    port         - Port to run the server on (default: 8080)
    open-browser - Whether to open browser automatically (default: T)

  Returns: T on success

  Example:
    (start-viewer)
    (start-viewer :port 3000)
    (start-viewer :open-browser nil)"

  (when *viewer-running-p*
    (format t "~&Viewer already running on port ~A~%" *viewer-port*)
    (return-from start-viewer t))

  (setf *viewer-port* port)

  ;; Create Hunchentoot acceptor
  (setf *viewer-server*
        (make-instance 'hunchentoot:easy-acceptor
                       :port port
                       :document-root *viewer-base-dir*
                       :access-log-destination nil
                       :message-log-destination nil))

  ;; Set up custom dispatcher for models directory with proper cache headers
  (push (hunchentoot:create-prefix-dispatcher
         "/models/"
         (lambda ()
           ;; Set cache headers that allow Last-Modified to work
           ;; no-cache = must revalidate, but can cache (uses Last-Modified!)
           ;; no-store would prevent caching entirely and break auto-reload
           (setf (hunchentoot:header-out "Cache-Control") "no-cache, must-revalidate"
                 (hunchentoot:header-out "Pragma") "no-cache")
           ;; Serve the file
           (let* ((script-name (hunchentoot:script-name*))
                  (file-name (subseq script-name (length "/models/")))
                  (file-path (merge-pathnames file-name *models-dir*)))
             (if (probe-file file-path)
                 (hunchentoot:handle-static-file file-path)
                 (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)))))
        hunchentoot:*dispatch-table*)

  ;; Start the server
  (handler-case
      (progn
        (hunchentoot:start *viewer-server*)
        (setf *viewer-running-p* t)
        (format t "~&~%")
        (format t "╔════════════════════════════════════════════════════════╗~%")
        (format t "║  CLAD Viewer Started                                   ║~%")
        (format t "╠════════════════════════════════════════════════════════╣~%")
        (format t "║  URL: http://localhost:~A/~26A║~%" port "")
        (format t "║                                                        ║~%")
        (format t "║  Open this URL in your web browser to view 3D models  ║~%")
        (format t "║  Use (view <shape>) to display shapes                 ║~%")
        (format t "╚════════════════════════════════════════════════════════╝~%")
        (format t "~%")

        ;; Only open browser if requested (viewer page, not specific model)
        (when open-browser
          #+(or darwin macos)
          (uiop:run-program (list "open" (format nil "http://localhost:~A/" port))
                            :ignore-error-status t)
          #+linux
          (uiop:run-program (list "xdg-open" (format nil "http://localhost:~A/" port))
                            :ignore-error-status t)
          #+windows
          (uiop:run-program (list "cmd" "/c" "start" (format nil "http://localhost:~A/" port))
                            :ignore-error-status t))

        t)
    (error (e)
      (format t "~&Error starting viewer: ~A~%" e)
      (setf *viewer-running-p* nil)
      nil)))

(defun stop-viewer ()
  "Stop the web viewer server.

  Returns: T on success

  Example:
    (stop-viewer)"

  (unless *viewer-running-p*
    (format t "~&Viewer is not running~%")
    (return-from stop-viewer nil))

  (when *viewer-server*
    (hunchentoot:stop *viewer-server*)
    (setf *viewer-server* nil)
    (setf *viewer-running-p* nil)
    (format t "~&Viewer stopped~%")
    t))

;;; ============================================================================
;;; Shape Viewing
;;; ============================================================================

(defun view (shape &key (name "current") (auto-start t))
  "View a shape in the web browser using three.js.

  This is the main entry point for visualizing shapes. It will:
  1. Export the shape to glTF format
  2. Start the web server (if not already running)
  3. Open the viewer in your default browser (first time only)

  The viewer automatically reloads when you call view() again with
  updated shapes.

  Arguments:
    shape      - clad.core:shape or clad.shapes:cad-shape to view
    name       - Name for the model file (default: \"current\")
    auto-start - Automatically start viewer if not running (default: T)

  Returns: T on success

  Examples:
    ;; View a simple box
    (view (make-box 100 50 30))

    ;; View a complex shape
    (view (cut-shapes
            (make-cylinder 50 100)
            (make-box 60 60 120)))

    ;; View with a custom name
    (view my-shape :name \"my-design\")

  Tips:
    - The viewer updates automatically when you call view() again
    - Use mouse to rotate, right-click to pan, scroll to zoom
    - Click 'Fit to View' button to reset the camera"

  ;; Handle both core shapes and CLOS shapes
  (let ((core-shape (etypecase shape
                      (clad.core:shape shape)
                      (clad.shapes:cad-shape (clad.shapes:unwrap-shape shape)))))

    (unless (clad.core:valid-shape-p core-shape)
      (error "Invalid shape provided to viewer"))

    ;; Ensure viewer is running (don't open browser yet)
    (when (and auto-start (not *viewer-running-p*))
      (start-viewer :open-browser nil))

    (unless *viewer-running-p*
      (error "Viewer is not running. Call (start-viewer) first or use :auto-start t"))

    ;; Export shape to glTF
    (let* ((model-filename (format nil "~A.glb" name))
           (model-path (merge-pathnames model-filename *models-dir*))
           (model-url (format nil "http://localhost:~A/?model=/models/~A"
                             *viewer-port* model-filename)))

      (format t "~&Exporting shape to glTF...~%")
      (clad.export:export-gltf core-shape (namestring model-path)
                               :linear-deflection 0.1
                               :angular-deflection 0.5)

      (setf *current-model-path* model-path)

      (format t "~&Model exported: ~A~%" model-filename)
      (format t "~&View at: ~A~%" model-url)

      ;; Open browser to the specific model URL (first time only)
      (when auto-start
        (format t "~&Opening browser to model...~%")
        #+(or darwin macos)
        (uiop:run-program (list "open" model-url) :ignore-error-status t)
        #+linux
        (uiop:run-program (list "xdg-open" model-url) :ignore-error-status t)
        #+windows
        (uiop:run-program (list "cmd" "/c" "start" model-url) :ignore-error-status t))

      (format t "~&~%The viewer will auto-reload within 2 seconds.~%")

      t)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun view-file (filename)
  "View a glTF/GLB file that already exists.

  Arguments:
    filename - Path to .glb or .gltf file

  Example:
    (view-file \"my-model.glb\")"

  (unless *viewer-running-p*
    (start-viewer))

  (let* ((source-path (probe-file filename))
         (target-name (file-namestring filename))
         (target-path (merge-pathnames target-name *models-dir*)))

    (unless source-path
      (error "File not found: ~A" filename))

    ;; Copy file to models directory
    (uiop:copy-file source-path target-path)

    (format t "~&View at: http://localhost:~A/?model=/models/~A~%"
            *viewer-port* target-name)
    t))

(defun clear-models ()
  "Clear all model files from the viewer models directory.

  Returns: Number of files deleted"

  (let ((count 0))
    (dolist (file (uiop:directory-files *models-dir*))
      (when (member (pathname-type file) '("glb" "gltf") :test #'string-equal)
        (delete-file file)
        (incf count)))

    (format t "~&Deleted ~A model file~:P~%" count)
    count))
