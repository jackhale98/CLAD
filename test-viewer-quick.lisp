;;;; test-viewer-quick.lisp --- Quick test of viewer functionality

(ql:quickload :clad :silent t)

(format t "~%~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "  CLAD Viewer Quick Test~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")

;; Create a simple test shape
(format t "Creating test box...~%")
(defparameter *test-box* (clad:make-box 100 50 30))

;; Start viewer (will open browser automatically)
(format t "Starting viewer...~%")
(clad:start-viewer)

;; View the shape
(format t "~%Displaying box in viewer...~%")
(clad:view *test-box* :name "test-box")

(format t "~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "  Viewer should now be open in your browser!~%")
(format t "  ~%")
(format t "  URL: http://localhost:8080/?model=/models/test-box.glb~%")
(format t "  ~%")
(format t "  You should see a 100x50x30 box.~%")
(format t "  ~%")
(format t "  Try:~%")
(format t "  - Drag to rotate~%")
(format t "  - Right-click drag to pan~%")
(format t "  - Scroll to zoom~%")
(format t "  - Click 'Fit to View' button~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%")
(format t "Press Ctrl+C to exit~%")

;; Keep the REPL alive
(loop (sleep 1))
