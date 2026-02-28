;;;; src/cli/build.lisp
;;;; Build script for creating CLAD CLI executable
;;;; This file is loaded directly by SBCL (NOT part of the ASDF system)

(require :asdf)

(in-package #:cl-user)

;; Add current directory to ASDF registry
(push (truename ".") asdf:*central-registry*)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load CLAD and CLI
(format t "Loading CLAD...~%")
(asdf:load-system :clad :verbose nil)
(format t "Loading CLAD CLI...~%")
(asdf:load-system :clad/cli :verbose nil)

;; Build the executable
(format t "Building executable...~%")

(let ((output-path (or (second sb-ext:*posix-argv*) "clad")))
  (format t "Output: ~A~%" output-path)
  (sb-ext:save-lisp-and-die output-path
                            :toplevel #'clad/cli:run-cli
                            :executable t
                            :compression t
                            :save-runtime-options t))
