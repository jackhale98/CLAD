;;;; start-viewer.lisp --- Start CLAD viewer with REPL access

(ql:quickload :clad :silent t)

(format t "~%~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "  CLAD Viewer with Real OCCT Geometry~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")
(format t "OCCT Status: ~A~%~%"
        (if clad.ffi:*occt-available-p*
            "✓ Real OCCT geometry enabled"
            "✗ WARNING: Using stub mode"))

;; Create some example shapes
(defparameter *box* (clad:make-box 100 50 30))
(defparameter *cylinder* (clad:make-cylinder 40 80))
(defparameter *sphere* (clad:make-sphere 35))
(defparameter *complex-shape*
  (clad:cut-shapes
   (clad:make-cylinder 50 100)
   (clad:make-cylinder 20 120)))

(format t "Example shapes defined:~%")
(format t "  *box*           - 100×50×30 box~%")
(format t "  *cylinder*      - r=40, h=80~%")
(format t "  *sphere*        - r=35~%")
(format t "  *complex-shape* - Cylinder with hole~%")
(format t "~%")

;; Start the viewer and show the box
(format t "Starting viewer...~%")
(clad:start-viewer)
(sleep 0.5) ; Give server a moment to start
(clad:view *box*)

(format t "~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "  Viewer Commands:~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")
(format t "  ;; View predefined shapes:~%")
(format t "  (clad:view *cylinder*)~%")
(format t "  (clad:view *sphere*)~%")
(format t "  (clad:view *complex-shape*)~%")
(format t "~%")
(format t "  ;; Create and view new shapes:~%")
(format t "  (clad:view (clad:union-shapes~%")
(format t "               (clad:make-box 80 80 80)~%")
(format t "               (clad:make-sphere 60)))~%")
(format t "~%")
(format t "  ;; Stop the viewer:~%")
(format t "  (clad:stop-viewer)~%")
(format t "~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "  Viewer auto-reloads every 2 seconds~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")
