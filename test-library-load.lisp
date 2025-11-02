;;;; test-library-load.lisp --- Test loading OCCT wrapper library

(ql:quickload :cffi :silent t)

(format t "~%Testing library loading...~%")
(format t "Library path: /home/jack/projects/clad/c-wrapper/build/libocct-wrapper.so~%")
(format t "File exists: ~A~%~%" (probe-file "/home/jack/projects/clad/c-wrapper/build/libocct-wrapper.so"))

(handler-case
    (progn
      (cffi:load-foreign-library "/home/jack/projects/clad/c-wrapper/build/libocct-wrapper.so")
      (format t "~%✓ Library loaded successfully!~%"))
  (error (e)
    (format t "~%✗ Error loading library:~%~A~%~%" e)
    (format t "Trying to get more details...~%")
    (format t "Library dependencies:~%")
    (uiop:run-program "ldd /home/jack/projects/clad/c-wrapper/build/libocct-wrapper.so"
                      :output *standard-output*)))

(format t "~%Now trying to load CLAD system...~%")
(ql:quickload :clad)
(format t "~%OCCT Available in CLAD: ~A~%~%" clad.ffi:*occt-available-p*)
