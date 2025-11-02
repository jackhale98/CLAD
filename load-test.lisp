;;;; load-test.lisp - Test script to load the CLAD system

(format t "~&Loading Quicklisp...~%")
(load "~/quicklisp/setup.lisp")

(format t "~&Installing dependencies if needed...~%")
;; Install dependencies
(ql:quickload :cffi :silent t)
(ql:quickload :trivial-garbage :silent t)
(ql:quickload :alexandria :silent t)
(ql:quickload :fiveam :silent t)

(format t "~&Dependencies loaded~%")

;; Add current directory to ASDF search path
(pushnew (truename ".") asdf:*central-registry* :test #'equal)

(format t "~&Loading CLAD system...~%")

;; Try to load the system
(handler-case
    (progn
      (asdf:load-system :clad)
      (format t "~&✓ System loaded successfully!~%"))
  (error (e)
    (format t "~&✗ Error loading system: ~A~%" e)
    (format t "~&Stack trace:~%")
    (format t "~A~%" (with-output-to-string (s)
                       (sb-debug:print-backtrace :stream s :count 20)))
    (sb-ext:exit :code 1)))

;; Try to run tests
(format t "~&~%Running tests...~%")

(handler-case
    (progn
      (asdf:test-system :clad)
      (format t "~&✓ Tests completed!~%"))
  (error (e)
    (format t "~&✗ Error running tests: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "~&~%All checks passed!~%")
(sb-ext:exit :code 0)
