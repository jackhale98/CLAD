;;;; src/auto-rebuild/auto-rebuild.lisp --- Auto-rebuild system for REPL-driven CAD

(in-package :clad.auto-rebuild)

;;; ============================================================================
;;; State Management
;;; ============================================================================

(defvar *auto-rebuild* t
  "Whether to automatically rebuild on part redefinition.
  When T, redefining a defpart that is the current part will trigger rebuild.")

(defvar *current-part* nil
  "Currently displayed part. Can be:
   - NIL (no part displayed)
   - SYMBOL (part function name)
   - FUNCTION (part function)
   - CONS of (SYMBOL . ARGS) (part with specific args)")

(defvar *current-part-args* nil
  "Arguments to use when rebuilding the current part.
  NIL means use default arguments from the part definition.")

(defvar *file-watchers* (make-hash-table :test 'equal)
  "Hash table mapping file paths to watcher thread objects.")

;;; ============================================================================
;;; Conditions
;;; ============================================================================

(define-condition no-current-part-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No current part set. Use (show 'part-name) first."))))

;;; ============================================================================
;;; Core API: SHOW
;;; ============================================================================

(defun show (part-or-symbol &key (args nil args-provided-p) (make-current t) (name nil) (open-browser :auto))
  "Display a part and optionally set it as the current part for auto-rebuild.

  Arguments:
    part-or-symbol - Can be:
                     - SYMBOL: name of a part function (e.g., 'my-bracket)
                     - FUNCTION: the part function itself
                     - SHAPE: a shape instance to display
    args           - Optional list of arguments to pass to the part function
    make-current   - If T (default), set as current part for auto-rebuild
    name           - Optional name for the model file (default: derived from symbol)
    open-browser   - :auto (default) = open only if not already shown
                     T = always open browser
                     NIL = never open browser

  Returns: The generated shape

  Examples:
    (show 'bracket)                    ; Show bracket, open browser if first time
    (show 'bracket :args '(100 50))    ; Show with args, open browser if first time
    (show my-shape :make-current nil)  ; Just display, don't track
    (show 'bracket :open-browser nil)  ; Update without opening browser

  This function is the main entry point for REPL-driven development.
  After calling show, redefining the part will automatically trigger rebuild."

  (let* ((part-symbol (cond
                        ((symbolp part-or-symbol) part-or-symbol)
                        ((functionp part-or-symbol) nil)
                        (t nil)))
         (model-name (or name
                        (when part-symbol (string-downcase (symbol-name part-symbol)))
                        "current"))
         (shape nil))

    ;; Generate or extract the shape
    (setf shape
          (cond
            ;; Case 1: Symbol - call the function
            ((symbolp part-or-symbol)
             (let ((fn (symbol-function part-or-symbol)))
               (if args-provided-p
                   (apply fn args)
                   (funcall fn))))

            ;; Case 2: Function - call it
            ((functionp part-or-symbol)
             (if args-provided-p
                 (apply part-or-symbol args)
                 (funcall part-or-symbol)))

            ;; Case 3: Already a shape - use it directly
            ((or (typep part-or-symbol 'clad.core:shape)
                 (typep part-or-symbol 'clad.shapes:cad-shape))
             part-or-symbol)

            (t
             (error "Invalid argument to SHOW: ~A. Expected symbol, function, or shape."
                    part-or-symbol))))

    ;; Determine if we should open the browser
    (let ((should-open-browser
           (cond
             ((eq open-browser t) t)              ; Explicit T = always open
             ((eq open-browser nil) nil)          ; Explicit NIL = never open
             ((eq open-browser :auto)             ; :auto = open if first time
              (and make-current
                   (not (equal *current-part* part-or-symbol))))
             (t nil))))

      ;; Update current part tracking
      (when make-current
        (setf *current-part* part-or-symbol)
        (setf *current-part-args* (when args-provided-p args)))

      ;; Display the shape using the existing viewer
      (clad.viewer:view shape :name model-name :auto-start should-open-browser)

      ;; Print status
      (format t "~&~%")
      (format t "╔════════════════════════════════════════════════════════╗~%")
      (format t "║  Part displayed successfully                           ║~%")
      (format t "╠════════════════════════════════════════════════════════╣~%")
      (when make-current
        (format t "║  Auto-rebuild: ~:[DISABLED~;ENABLED~]~28@A║~%" *auto-rebuild* "")
        (format t "║  Current part: ~A~30@A║~%"
                (if part-symbol part-symbol "<function>")
                ""))
      (format t "║                                                        ║~%")
      (format t "║  Modify the part definition and it will auto-update!  ║~%")
      (format t "╚════════════════════════════════════════════════════════╝~%")
      (format t "~%")

      ;; Return the shape
      shape)))

;;; ============================================================================
;;; Core API: REBUILD
;;; ============================================================================

(defun rebuild ()
  "Rebuild and display the current part.

  This function regenerates the current part from its definition and
  updates the viewer. The current part must be set via SHOW first.

  Returns: The newly generated shape, or NIL on error

  Examples:
    (show 'bracket)
    ;; ... modify bracket definition ...
    (rebuild)  ; Manually trigger rebuild

  Errors:
    Signals NO-CURRENT-PART-ERROR if no current part is set."

  (unless *current-part*
    (error 'no-current-part-error))

  (format t "~&[Rebuilding...")
  (finish-output)

  (let* ((start-time (get-internal-real-time))
         (shape nil)
         (success nil))

    ;; Try to rebuild
    (handler-case
        (progn
          (setf shape
                (cond
                  ;; Symbol - call the function
                  ((symbolp *current-part*)
                   (let ((fn (symbol-function *current-part*)))
                     (if *current-part-args*
                         (apply fn *current-part-args*)
                         (funcall fn))))

                  ;; Function - call it
                  ((functionp *current-part*)
                   (if *current-part-args*
                       (apply *current-part* *current-part-args*)
                       (funcall *current-part*)))

                  ;; Cons (symbol . args) - for future compatibility
                  ((consp *current-part*)
                   (apply (symbol-function (car *current-part*))
                          (cdr *current-part*)))

                  (t
                   (error "Invalid current part: ~A" *current-part*))))

          (setf success t))

      (error (e)
        (format t " FAILED]~%")
        (format t "~&╔════════════════════════════════════════════════════════╗~%")
        (format t "║  Rebuild Error                                         ║~%")
        (format t "╚════════════════════════════════════════════════════════╝~%")
        (format t "~&Error: ~A~%" e)
        (format t "~%The previous model is still displayed.~%")
        (return-from rebuild nil)))

    ;; If successful, update the viewer
    (when success
      (let* ((elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second))
             (model-name (if (symbolp *current-part*)
                            (string-downcase (symbol-name *current-part*))
                            "current")))

        ;; Update viewer without opening browser (it's already open)
        (clad.viewer:view shape :name model-name :auto-start nil)

        (format t " done in ~,3Fs]~%" elapsed)
        shape))))

;;; ============================================================================
;;; Core API: TOGGLE
;;; ============================================================================

(defun toggle-auto-rebuild ()
  "Toggle automatic rebuilding on/off.

  When auto-rebuild is enabled, redefining a defpart that is the current
  part will automatically trigger a rebuild.

  Returns: New value of *auto-rebuild*

  Example:
    (toggle-auto-rebuild)  ; Disable auto-rebuild
    (toggle-auto-rebuild)  ; Re-enable auto-rebuild"

  (setf *auto-rebuild* (not *auto-rebuild*))
  (format t "~&Auto-rebuild: ~:[DISABLED~;ENABLED~]~%" *auto-rebuild*)
  *auto-rebuild*)

;;; ============================================================================
;;; Internal: Trigger Rebuild (called from defpart)
;;; ============================================================================

(defun maybe-rebuild-current-part (part-symbol)
  "Internal function called by defpart to potentially trigger auto-rebuild.

  Arguments:
    part-symbol - The symbol of the part that was just redefined

  If auto-rebuild is enabled and the redefined part is the current part,
  this triggers a rebuild automatically."

  (when (and *auto-rebuild*
             *current-part*
             (or (eq *current-part* part-symbol)
                 (and (consp *current-part*)
                      (eq (car *current-part*) part-symbol))))
    (rebuild)))

;;; ============================================================================
;;; File Watching
;;; ============================================================================

#+bordeaux-threads
(defun watch (filepath part-symbol &key (interval 0.5))
  "Watch a file and auto-rebuild when it changes.

  Arguments:
    filepath    - Path to the file to watch (string or pathname)
    part-symbol - Symbol of the part function defined in the file
    interval    - Polling interval in seconds (default: 0.5)

  Returns: The watcher thread

  The watcher will:
  1. Monitor the file for changes (using modification time)
  2. When changed, reload the file
  3. If the part is the current part, trigger rebuild

  Example:
    (show 'bracket)
    (watch \"~/designs/bracket.lisp\" 'bracket)
    ;; Now edit the file in your editor - it will auto-reload!

  To stop watching:
    (stop-watching \"~/designs/bracket.lisp\")

  Note: Requires bordeaux-threads. File watching uses polling."

  (let* ((filepath-str (namestring (truename filepath)))
         (last-modified (file-write-date filepath-str)))

    ;; Stop any existing watcher for this file
    (stop-watching filepath-str)

    ;; Create new watcher thread
    (let ((watcher
           (bt:make-thread
            (lambda ()
              (loop
                (sleep interval)
                (handler-case
                    (let ((current-time (file-write-date filepath-str)))
                      (when (and current-time (> current-time last-modified))
                        (setf last-modified current-time)
                        (format t "~&╔════════════════════════════════════════════════════════╗~%")
                        (format t "║  File changed: ~A~%║~%" (file-namestring filepath-str))
                        (format t "╚════════════════════════════════════════════════════════╝~%")

                        ;; Try to reload the file
                        (handler-case
                            (progn
                              (load filepath-str)
                              (format t "~&✓ File reloaded~%")

                              ;; If this part is current, rebuild it
                              (when (eq *current-part* part-symbol)
                                (rebuild)))

                          (error (e)
                            (format t "~&✗ Error loading file: ~A~%" e)))))

                  ;; Catch thread termination
                  (sb-thread:thread-error ()
                    (return-from nil))

                  ;; Catch general errors
                  (error (e)
                    (format t "~&Watch error: ~A~%" e)))))
            :name (format nil "clad-watcher:~A" (file-namestring filepath-str)))))

      ;; Store the watcher
      (setf (gethash filepath-str *file-watchers*) watcher)

      (format t "~&Watching ~A for changes (polling every ~As)~%"
              filepath-str interval)

      watcher)))

#-bordeaux-threads
(defun watch (filepath part-symbol &key (interval 0.5))
  "File watching not available - bordeaux-threads not loaded."
  (declare (ignore filepath part-symbol interval))
  (warn "File watching requires bordeaux-threads, which is not available.")
  nil)

(defun stop-watching (filepath)
  "Stop watching a file.

  Arguments:
    filepath - Path to the file (string or pathname)

  Returns: T if a watcher was stopped, NIL if no watcher existed

  Example:
    (stop-watching \"~/designs/bracket.lisp\")"

  (let* ((filepath-str (namestring (probe-file filepath)))
         (watcher (gethash filepath-str *file-watchers*)))

    (when watcher
      #+bordeaux-threads
      (bt:destroy-thread watcher)
      (remhash filepath-str *file-watchers*)
      (format t "~&Stopped watching ~A~%" filepath-str)
      t)))

(defun stop-all-watchers ()
  "Stop all active file watchers.

  Returns: Number of watchers stopped

  Example:
    (stop-all-watchers)"

  (let ((count 0))
    (maphash (lambda (filepath watcher)
               (declare (ignore filepath))
               #+bordeaux-threads
               (bt:destroy-thread watcher)
               (incf count))
             *file-watchers*)

    (clrhash *file-watchers*)

    (format t "~&Stopped ~A watcher~:P~%" count)
    count))
