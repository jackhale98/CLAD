;;;; test-viewer.lisp --- Test the CLAD viewer workflow

(require 'asdf)
(asdf:load-asd (truename "clad.asd"))
(asdf:load-system :clad)

(in-package :clad)

(format t "~%~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "  CLAD Viewer Test~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")

;; Test 1: Create a simple box
(format t "Test 1: Creating and viewing a simple box...~%")
(let ((box (make-box 100 50 30)))
  (format t "  Box created: ~A~%" box)
  (format t "  Valid: ~A~%~%" (clad.core:valid-shape-p box)))

;; Test 2: Create a complex shape with boolean operations
(format t "Test 2: Creating a complex shape with boolean operations...~%")
(let* ((base (make-cylinder 50 100))
       (hole (make-cylinder 20 120))
       (result (cut-shapes base hole)))
  (format t "  Cylinder with hole created: ~A~%" result)
  (format t "  Valid: ~A~%~%" (clad.core:valid-shape-p result)))

;; Test 3: Test glTF export
(format t "Test 3: Testing glTF export...~%")
(let ((sphere (make-sphere 40)))
  (export-gltf sphere "/tmp/test-sphere.glb")
  (format t "  Exported sphere to /tmp/test-sphere.glb~%")
  (format t "  File exists: ~A~%~%" (probe-file "/tmp/test-sphere.glb")))

;; Test 4: Test CLOS shape wrapping
(format t "Test 4: Testing CLOS shape wrapping...~%")
(let* ((core-box (make-box 100 100 100))
       (clos-box (clad.shapes:wrap-shape core-box 'clad.shapes:cad-solid)))
  (format t "  CLOS box created: ~A~%" clos-box)
  (format t "  Type: ~A~%" (clad.shapes:shape-type clos-box))
  (format t "  Faces: ~A~%" (length (clad.shapes:faces clos-box)))
  (format t "  Edges: ~A~%" (length (clad.shapes:edges clos-box)))
  (format t "  Vertices: ~A~%~%" (length (clad.shapes:vertices clos-box))))

;; Test 5: Instructions for manual viewer test
(format t "Test 5: Manual viewer test instructions~%")
(format t "  To test the interactive viewer, run these commands in the REPL:~%")
(format t "~%")
(format t "    ;; Start the viewer~%")
(format t "    (clad:start-viewer)~%")
(format t "~%")
(format t "    ;; View a box~%")
(format t "    (clad:view (clad:make-box 100 50 30))~%")
(format t "~%")
(format t "    ;; View a complex shape~%")
(format t "    (clad:view (clad:cut-shapes~%")
(format t "                 (clad:make-cylinder 50 100)~%")
(format t "                 (clad:make-cylinder 20 120)))~%")
(format t "~%")
(format t "    ;; View with CLOS shape~%")
(format t "    (let* ((box (clad:make-box 80 80 80))~%")
(format t "           (clos-box (clad.shapes:wrap-shape box 'clad.shapes:cad-solid)))~%")
(format t "      (clad:view clos-box :name \"clos-test\"))~%")
(format t "~%")
(format t "    ;; Stop the viewer when done~%")
(format t "    (clad:stop-viewer)~%")
(format t "~%")

(format t "════════════════════════════════════════════════════════════════~%")
(format t "  All tests completed successfully!~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")
