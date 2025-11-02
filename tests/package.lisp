;;;; tests/package.lisp --- Test package definition

(in-package :cl-user)

(defpackage #:clad.tests
  (:use #:cl #:fiveam #:clad)
  (:import-from #:clad.ffi
                #:with-occt-error-handling
                #:occt-error
                #:occt-error-message
                #:occt-domain-error
                #:occt-construction-error
                #:occt-null-object-error
                #:occt-handle
                #:make-occt-handle
                #:handle-ptr
                #:handle-type
                #:handle-valid-p
                #:handle-null-p
                #:copy-occt-handle
                #:ensure-valid-handle
                #:occt-handle-ref-count
                #:clear-handle-cache
                #:error-code-to-condition
                #:+occt-error-domain+
                #:+occt-error-construction+
                #:+occt-error-null-object+
                #:ffi-make-box
                #:ffi-make-cylinder
                #:ffi-make-sphere
                #:ffi-union
                #:ffi-cut
                #:ffi-intersect
                #:ffi-translate
                #:ffi-rotate
                #:ffi-mirror
                #:ffi-scale)
  (:import-from #:clad.core
                #:valid-shape-p
                #:shape-handle
                #:shape-metadata)
  (:import-from #:clad.units
                #:convert-units
                #:toleranced-dimension
                #:dimension-nominal
                #:mm->in #:in->mm
                #:mm->cm #:cm->mm
                #:mm->m #:m->mm
                #:unit-p
                #:effective-units
                #:define-unit-conversion)
  (:export
   #:clad-tests
   #:run-tests
   #:advanced-features-tests  ; Phase 8 test suite
   #:sketch-tests))           ; Phase 9 test suite
