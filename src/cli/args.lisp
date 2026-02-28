;;;; src/cli/args.lisp
;;;; Command-line argument parsing for CLAD CLI

(in-package #:clad/cli)

(defvar *clad-cli-version* "0.1.0"
  "CLAD CLI version string")

(define-condition cli-argument-error (error)
  ((message :initarg :message :reader cli-argument-error-message))
  (:documentation "Error during argument parsing")
  (:report (lambda (condition stream)
             (format stream "~A" (cli-argument-error-message condition)))))

(defclass cli-options ()
  ((command :initarg :command :initform nil :accessor option-command
            :documentation "Command name (build, view, watch, info, check, repl)")
   (file :initarg :file :initform nil :accessor option-file
         :documentation "Design file path")
   (output-dir :initarg :output-dir :initform nil :accessor option-output-dir
               :documentation "Output directory for exported files")
   (part :initarg :part :initform nil :accessor option-part
         :documentation "Part name to operate on")
   (params :initarg :params :initform nil :accessor option-params
           :documentation "Parameter overrides as plist (:key value ...)")
   (step :initarg :step :initform nil :accessor option-step
         :documentation "Export STEP format")
   (stl :initarg :stl :initform nil :accessor option-stl
        :documentation "Export STL format")
   (gltf :initarg :gltf :initform nil :accessor option-gltf
          :documentation "Export glTF format")
   (resolution :initarg :resolution :initform :medium :accessor option-resolution
               :documentation "Mesh resolution (low, medium, high, ultra)")
   (ascii :initarg :ascii :initform nil :accessor option-ascii
          :documentation "Use ASCII format for STL")
   (port :initarg :port :initform 8080 :accessor option-port
         :documentation "Port for viewer server")
   (no-browser :initarg :no-browser :initform nil :accessor option-no-browser
               :documentation "Don't open browser automatically")
   (interval :initarg :interval :initform 0.5 :accessor option-interval
             :documentation "Watch polling interval in seconds")
   (mass-properties :initarg :mass-properties :initform nil :accessor option-mass-properties
                    :documentation "Show mass properties in info command")
   (material :initarg :material :initform nil :accessor option-material
             :documentation "Material keyword for mass calculations")
   (json :initarg :json :initform nil :accessor option-json
         :documentation "Output in JSON format")
   (quiet :initarg :quiet :initform nil :accessor option-quiet
          :documentation "Suppress informational output")
   (help :initarg :help :initform nil :accessor option-help
         :documentation "Show help message")
   (version :initarg :version :initform nil :accessor option-version
            :documentation "Show version"))
  (:documentation "Parsed command-line options"))

(defun parse-param-value (value-string)
  "Parse a parameter value string, safely handling numbers and strings.
   Binds *read-eval* to NIL for safety."
  (let ((*read-eval* nil))
    (handler-case
        (let ((val (read-from-string value-string)))
          val)
      (error ()
        ;; If read fails, return as string
        value-string))))

(defun parse-arguments (args)
  "Parse command-line arguments into a cli-options object.
   ARGS should be a list of strings."
  (let ((opts (make-instance 'cli-options))
        (positional nil)
        (i 0))
    (loop while (< i (length args))
          for arg = (nth i args)
          do (cond
               ;; Help and version (can appear anywhere)
               ((or (string= arg "--help") (string= arg "-h"))
                (setf (option-help opts) t))
               ((or (string= arg "--version") (string= arg "-V"))
                (setf (option-version opts) t))

               ;; Format flags
               ((string= arg "--step")
                (setf (option-step opts) t))
               ((string= arg "--stl")
                (setf (option-stl opts) t))
               ((string= arg "--gltf")
                (setf (option-gltf opts) t))
               ((string= arg "--ascii")
                (setf (option-ascii opts) t))

               ;; Boolean flags
               ((string= arg "--no-browser")
                (setf (option-no-browser opts) t))
               ((string= arg "--mass-properties")
                (setf (option-mass-properties opts) t))
               ((string= arg "--json")
                (setf (option-json opts) t))
               ((string= arg "--quiet")
                (setf (option-quiet opts) t))

               ;; Options with values
               ((string= arg "--output-dir")
                (incf i)
                (if (< i (length args))
                    (setf (option-output-dir opts) (nth i args))
                    (error 'cli-argument-error
                           :message "--output-dir requires a directory path")))

               ((string= arg "--part")
                (incf i)
                (if (< i (length args))
                    (setf (option-part opts) (nth i args))
                    (error 'cli-argument-error
                           :message "--part requires a part name")))

               ((string= arg "--resolution")
                (incf i)
                (if (< i (length args))
                    (let ((res (intern (string-upcase (nth i args)) :keyword)))
                      (if (member res '(:low :medium :high :ultra))
                          (setf (option-resolution opts) res)
                          (error 'cli-argument-error
                                 :message (format nil "Invalid resolution: ~A. Must be low, medium, high, or ultra"
                                                  (nth i args)))))
                    (error 'cli-argument-error
                           :message "--resolution requires a value (low, medium, high, ultra)")))

               ((string= arg "--port")
                (incf i)
                (if (< i (length args))
                    (let ((n (parse-integer (nth i args) :junk-allowed t)))
                      (if (and n (plusp n) (<= n 65535))
                          (setf (option-port opts) n)
                          (error 'cli-argument-error
                                 :message "--port requires a valid port number (1-65535)")))
                    (error 'cli-argument-error
                           :message "--port requires a number")))

               ((string= arg "--interval")
                (incf i)
                (if (< i (length args))
                    (let ((n (read-from-string (nth i args))))
                      (if (and (numberp n) (plusp n))
                          (setf (option-interval opts) (float n))
                          (error 'cli-argument-error
                                 :message "--interval requires a positive number")))
                    (error 'cli-argument-error
                           :message "--interval requires a number (seconds)")))

               ((string= arg "--material")
                (incf i)
                (if (< i (length args))
                    (setf (option-material opts)
                          (intern (string-upcase (nth i args)) :keyword))
                    (error 'cli-argument-error
                           :message "--material requires a material name")))

               ((string= arg "--param")
                (incf i)
                (if (< i (length args))
                    (let* ((param-str (nth i args))
                           (eq-pos (position #\= param-str)))
                      (unless eq-pos
                        (error 'cli-argument-error
                               :message (format nil "Invalid --param format: ~A (expected key=value)"
                                                param-str)))
                      (let ((key (intern (string-upcase (subseq param-str 0 eq-pos)) :keyword))
                            (val (parse-param-value (subseq param-str (1+ eq-pos)))))
                        (setf (option-params opts)
                              (append (option-params opts) (list key val)))))
                    (error 'cli-argument-error
                           :message "--param requires a key=value pair")))

               ;; Unknown option
               ((and (> (length arg) 0) (char= (char arg 0) #\-))
                (error 'cli-argument-error
                       :message (format nil "Unknown option: ~A" arg)))

               ;; Positional arguments
               (t
                (push arg positional)))
          (incf i))

    ;; Assign positional arguments: first = command, second = file
    (let ((positionals (nreverse positional)))
      (when (first positionals)
        (setf (option-command opts) (first positionals)))
      (when (second positionals)
        (setf (option-file opts) (second positionals))))

    opts))

(defun print-usage ()
  "Print usage information"
  (format t "CLAD - Common Lisp CAD~%")
  (format t "Version ~A~%~%" *clad-cli-version*)
  (format t "Usage: clad <command> [file] [options]~%~%")
  (format t "Commands:~%")
  (format t "  build  <file>    Export CAD files (STEP, STL, glTF)~%")
  (format t "  view   <file>    Open part in 3D viewer~%")
  (format t "  watch  <file>    Watch file and auto-rebuild on changes~%")
  (format t "  info   <file>    Show part information and properties~%")
  (format t "  check  <file>    Validate all parts in design file~%")
  (format t "  repl   [file]    Start interactive REPL~%~%")
  (format t "Build Options:~%")
  (format t "  --step                 Export STEP format (default if no format given)~%")
  (format t "  --stl                  Export STL format~%")
  (format t "  --gltf                 Export glTF format~%")
  (format t "  --ascii                Use ASCII format for STL~%")
  (format t "  --resolution RES       Mesh resolution: low, medium, high, ultra (default: medium)~%")
  (format t "  --output-dir DIR       Output directory (default: current directory)~%")
  (format t "  --part NAME            Specific part to export~%")
  (format t "  --param KEY=VALUE      Override part parameter (repeatable)~%~%")
  (format t "View Options:~%")
  (format t "  --port PORT            Viewer server port (default: 8080)~%")
  (format t "  --no-browser           Don't open browser automatically~%~%")
  (format t "Watch Options:~%")
  (format t "  --part NAME            Part to watch (required if multiple parts)~%")
  (format t "  --interval SEC         Polling interval in seconds (default: 0.5)~%~%")
  (format t "Info Options:~%")
  (format t "  --mass-properties      Show mass properties (volume, mass, etc.)~%")
  (format t "  --material MAT         Material for mass calculation~%~%")
  (format t "Output Options:~%")
  (format t "  --json                 Output in JSON format~%")
  (format t "  --quiet                Suppress informational messages~%~%")
  (format t "General Options:~%")
  (format t "  -h, --help             Show this help message~%")
  (format t "  -V, --version          Show version~%~%")
  (format t "Examples:~%")
  (format t "  clad build design.lisp --step --stl~%")
  (format t "  clad build design.lisp --part bracket --param width=150~%")
  (format t "  clad view design.lisp --part bracket~%")
  (format t "  clad info design.lisp --mass-properties --material aluminum~%")
  (format t "  clad check design.lisp~%")
  (format t "  clad repl design.lisp~%"))

(defun print-version ()
  "Print version information"
  (format t "clad ~A~%" *clad-cli-version*))
