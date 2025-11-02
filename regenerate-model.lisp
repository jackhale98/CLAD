;;;; regenerate-model.lisp --- Regenerate the current model with real OCCT

(ql:quickload :clad :silent t)

(format t "~%OCCT Available: ~A~%~%" clad.ffi:*occt-available-p*)

(if clad.ffi:*occt-available-p*
    (progn
      (format t "Exporting box with REAL OCCT geometry...~%")
      (let ((box (clad:make-box 100 50 30))
            (output-path "/home/jack/projects/clad/viewer/models/current.glb"))
        (clad:export-gltf box output-path)
        (format t "~%File size: ~A bytes~%"
                (with-open-file (s output-path) (file-length s)))
        (format t "File type: ~%")
        (uiop:run-program (list "file" output-path) :output *standard-output*)))
    (format t "ERROR: OCCT not available! Using stubs.~%"))
