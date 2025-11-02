;;;; tests/test-suite.lisp --- Main test suite definition

(in-package :clad.tests)

(def-suite clad-tests
  :description "Master test suite for CLAD")

(defun run-tests ()
  "Run all CLAD tests"
  (run! 'clad-tests))

(in-suite clad-tests)
