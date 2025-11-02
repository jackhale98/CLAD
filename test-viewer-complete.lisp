;;;; test-viewer-complete.lisp --- Complete viewer test with real OCCT

(ql:quickload :clad :silent t)

(format t "~%~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "  CLAD Viewer - Complete Test with Real OCCT Geometry~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")
(format t "OCCT Status: ~A~%~%"
        (if clad.ffi:*occt-available-p*
            "✓ Real OCCT geometry"
            "✗ Stub mode"))

;; Create test shapes
(format t "Creating test shapes...~%")
(defparameter *box* (clad:make-box 100 50 30))
(defparameter *cylinder* (clad:make-cylinder 40 80))
(defparameter *sphere* (clad:make-sphere 35))
(defparameter *complex-shape*
  (clad:cut-shapes
   (clad:make-cylinder 50 100)
   (clad:make-cylinder 20 120)))

(format t "  ✓ Box: 100×50×30~%")
(format t "  ✓ Cylinder: r=40, h=80~%")
(format t "  ✓ Sphere: r=35~%")
(format t "  ✓ Complex shape: Cylinder with hole~%")
(format t "~%")

;; Start viewer and display first shape
;; Use name "current" to match the default in index.html
(format t "Starting viewer and displaying box...~%")
(clad:view *box* :name "current")

(format t "~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "  Viewer is now running!~%")
(format t "  ~%")
(format t "  Open: http://localhost:8080/?model=/models/box.glb~%")
(format t "  ~%")
(format t "  Try viewing other shapes:~%")
(format t "  ~%")
(format t "    (clad:view *cylinder*)~%")
(format t "    (clad:view *sphere*)~%")
(format t "    (clad:view *complex-shape*)~%")
(format t "  ~%")
(format t "  Create and view custom shapes:~%")
(format t "  ~%")
(format t "    (clad:view (clad:union-shapes~%")
(format t "                 (clad:make-box 80 80 80)~%")
(format t "                 (clad:make-sphere 60)))~%")
(format t "  ~%")
(format t "  The viewer auto-reloads every 2 seconds.~%")
(format t "  ~%")
(format t "  To stop the viewer: (clad:stop-viewer)~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")

;; Give back the REPL - don't block!
;; The viewer runs in a background thread
;; You can now interact with the REPL
