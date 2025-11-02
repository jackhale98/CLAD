;;;; test-occt-integration.lisp - Test real OCCT integration

(load "~/quicklisp/setup.lisp")

;; Add current directory to ASDF search path
(require :asdf)
(pushnew (truename ".") asdf:*central-registry* :test #'equal)

;; Load the system
(format t "~&Loading CLAD system...~%")
(asdf:load-system :clad)

(in-package :clad)

(format t "~&~%============================================~%")
(format t "OCCT Integration Test~%")
(format t "============================================~%~%")

;; Check if OCCT is available
(format t "OCCT Available: ~A~%~%" clad.ffi:*occt-available-p*)

(if clad.ffi:*occt-available-p*
    (progn
      (format t "~&Testing real OCCT operations...~%~%")

      ;; Test 1: Create a simple box
      (format t "Test 1: Creating a 100x50x30 mm box...~%")
      (let ((box (make-box 100 50 30)))
        (format t "  Result: ~A~%" box)
        (format t "  Valid: ~A~%~%" (clad.core:valid-shape-p box)))

      ;; Test 2: Create a cylinder
      (format t "Test 2: Creating a cylinder (r=25mm, h=100mm)...~%")
      (let ((cyl (make-cylinder 25 100)))
        (format t "  Result: ~A~%" cyl)
        (format t "  Valid: ~A~%~%" (clad.core:valid-shape-p cyl)))

      ;; Test 3: Create a sphere
      (format t "Test 3: Creating a sphere (r=30mm)...~%")
      (let ((sphere (make-sphere 30)))
        (format t "  Result: ~A~%" sphere)
        (format t "  Valid: ~A~%~%" (clad.core:valid-shape-p sphere)))

      ;; Test 4: Boolean operations
      (format t "Test 4: Boolean union of two boxes...~%")
      (let* ((box1 (make-box 50 50 50))
             (box2 (clad.core:translate (make-box 50 50 50) 25 25 25))
             (result (union-shapes box1 box2)))
        (format t "  Result: ~A~%" result)
        (format t "  Valid: ~A~%~%" (clad.core:valid-shape-p result)))

      ;; Test 5: Export to STEP
      (format t "Test 5: Exporting box to STEP file...~%")
      (let ((box (make-box 100 50 30)))
        (handler-case
            (progn
              (export-step box "/tmp/test-box.step")
              (format t "  Successfully exported to /tmp/test-box.step~%")
              (let ((file-exists (probe-file "/tmp/test-box.step")))
                (format t "  File exists: ~A~%~%" file-exists)))
          (error (e)
            (format t "  Error exporting: ~A~%~%" e))))

      (format t "~&~%============================================~%")
      (format t "✓ All OCCT integration tests completed!~%")
      (format t "============================================~%~%"))

    (progn
      (format t "~&OCCT not available - running in stub mode.~%")
      (format t "To test with real OCCT, ensure the C wrapper library is built and accessible.~%~%")))

;; Exit cleanly
(sb-ext:exit :code 0)
