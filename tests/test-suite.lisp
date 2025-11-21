;;;; tests/test-suite.lisp --- Main test suite definition

(in-package :clad.tests)

(def-suite clad-tests
  :description "Master test suite for CLAD")

(defun run-tests ()
  "Run all CLAD tests"
  (run! 'clad-tests))

(defun approximately= (a b tolerance)
  "Check if two numbers are approximately equal within tolerance."
  (<= (abs (- a b)) tolerance))

(in-suite clad-tests)
