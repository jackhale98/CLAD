;;;; src/cli/output.lisp
;;;; Output formatting for CLAD CLI

(in-package #:clad/cli)

;;; ============================================================================
;;; Basic Output Functions
;;; ============================================================================

(defvar *quiet* nil
  "When true, suppress informational output")

(defun print-error (format-string &rest args)
  "Print an error message to stderr"
  (format *error-output* "Error: ~?~%" format-string args))

(defun print-warning (format-string &rest args)
  "Print a warning message to stderr"
  (format *error-output* "Warning: ~?~%" format-string args))

(defun print-info (format-string &rest args)
  "Print an informational message (respects --quiet)"
  (unless *quiet*
    (format t "~?~%" format-string args)))

;;; ============================================================================
;;; Error Formatting
;;; ============================================================================

(defun format-lisp-error (condition)
  "Convert a Lisp condition to a user-friendly error message"
  (typecase condition
    (clad.ffi:occt-construction-error
     (format nil "Construction error: ~A" (clad.ffi:occt-error-message condition)))
    (clad.ffi:occt-error
     (format nil "CAD kernel error: ~A" (clad.ffi:occt-error-message condition)))
    (file-error
     (format nil "File not found: ~A" (file-error-pathname condition)))
    (reader-error
     "Syntax error in design file")
    (t
     (format nil "~A" condition))))

;;; ============================================================================
;;; JSON Output
;;; ============================================================================

(defun escape-json-string (str)
  "Escape a string for JSON output"
  (with-output-to-string (out)
    (loop for char across str
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char char out))))))

(defun to-json-string (obj)
  "Convert an object to a JSON string."
  (typecase obj
    (null "null")
    ((eql t) "true")
    (string (format nil "\"~A\"" (escape-json-string obj)))
    (keyword (format nil "\"~A\"" (escape-json-string (string-downcase (symbol-name obj)))))
    (symbol (format nil "\"~A\"" (escape-json-string (symbol-name obj))))
    (integer (format nil "~D" obj))
    (float (format nil "~F" obj))
    (ratio (format nil "~F" (float obj)))
    (list
     (if (keywordp (first obj))
         ;; Plist - convert to object
         (with-output-to-string (out)
           (write-char #\{ out)
           (loop for (key val) on obj by #'cddr
                 for first = t then nil
                 do (unless first (write-string "," out))
                    (format out "\"~A\":~A"
                            (string-downcase (symbol-name key))
                            (to-json-string val)))
           (write-char #\} out))
         ;; Regular list - convert to array
         (with-output-to-string (out)
           (write-char #\[ out)
           (loop for item in obj
                 for first = t then nil
                 do (unless first (write-string "," out))
                    (write-string (to-json-string item) out))
           (write-char #\] out))))
    (t
     (format nil "\"~A\"" (escape-json-string (format nil "~A" obj))))))

;;; ============================================================================
;;; Part Info Formatting
;;; ============================================================================

(defun format-part-info (parts &key json)
  "Format discovered parts information.
   PARTS is a list of (symbol . param-info) pairs."
  (if json
      (let ((data (mapcar (lambda (entry)
                            (let ((sym (car entry))
                                  (params (cdr entry)))
                              (list :name (string-downcase (symbol-name sym))
                                    :package (package-name (symbol-package sym))
                                    :parameters
                                    (mapcar (lambda (p)
                                              (list :name (string-downcase (symbol-name (first p)))
                                                    :default (to-json-string (second p))))
                                            params))))
                          parts)))
        (format t "~A~%" (to-json-string data)))
      ;; Text output
      (progn
        (format t "Parts found: ~D~%~%" (length parts))
        (format t "~30A ~A~%" "Name" "Parameters")
        (format t "~30A ~A~%" "----" "----------")
        (dolist (entry parts)
          (let ((sym (car entry))
                (params (cdr entry)))
            (format t "~30A ~{~A~^, ~}~%"
                    (string-downcase (symbol-name sym))
                    (mapcar (lambda (p)
                              (format nil "~A=~A"
                                      (string-downcase (symbol-name (first p)))
                                      (second p)))
                            params)))))))

(defun format-mass-properties (props &key json)
  "Format mass properties output.
   PROPS is a plist from clad.analysis:mass-properties."
  (if json
      (format t "~A~%" (to-json-string props))
      ;; Text output
      (progn
        (format t "Mass Properties~%")
        (format t "===============~%")
        (format t "Material:         ~A~%" (getf props :material-name))
        (format t "Density:          ~,3F g/cm3~%" (getf props :density))
        (format t "~%")
        (format t "Volume:           ~,3F mm3~%" (getf props :volume))
        (format t "Surface Area:     ~,3F mm2~%" (getf props :surface-area))
        (format t "Mass:             ~,3F g~%" (getf props :mass))
        (format t "~%")
        (let ((com (getf props :center-of-mass)))
          (format t "Center of Mass:   (~,3F, ~,3F, ~,3F) mm~%"
                  (first com) (second com) (third com)))
        (let ((inertia (getf props :inertia)))
          (when inertia
            (format t "~%")
            (format t "Inertia Tensor (g*mm2):~%")
            (format t "  Ixx: ~,3F~%" (getf inertia :ixx))
            (format t "  Iyy: ~,3F~%" (getf inertia :iyy))
            (format t "  Izz: ~,3F~%" (getf inertia :izz)))))))
