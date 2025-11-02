;;;; test-step-export.lisp - Direct test of STEP export

(load "~/quicklisp/setup.lisp")

(require :asdf)
(pushnew (truename ".") asdf:*central-registry* :test #'equal)

(format t "~&Loading CLAD...~%")
(asdf:load-system :clad)

(in-package :clad.ffi)

(format t "~&OCCT Available: ~A~%~%" *occt-available-p*)

(when *occt-available-p*
  (format t "Creating a box...~%")
  (let ((box-handle (ffi-make-box 100 50 30)))
    (format t "Box handle: ~A~%" box-handle)
    (format t "Valid: ~A~%~%" (handle-valid-p box-handle))

    (format t "Attempting STEP export...~%")
    (handler-case
        (progn
          (ffi-export-step box-handle "/tmp/test-direct.step")
          (format t "Export succeeded!~%")
          (format t "File size: ~A bytes~%"
                  (with-open-file (f "/tmp/test-direct.step")
                    (file-length f))))
      (error (e)
        (format t "Export failed: ~A~%" e)
        (format t "Error type: ~A~%" (type-of e))))))

(sb-ext:exit :code 0)
