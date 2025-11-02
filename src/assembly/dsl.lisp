;;;; dsl.lisp --- Declarative DSL for assemblies

(in-package #:clad.assembly.dsl)

;;; Placeholder implementations for Phase 10

(defmacro defassembly (name parameters &body body)
  "Placeholder for defassembly macro"
  (declare (ignore parameters body))
  `(defun ,name ()
     (make-assembly :name ',name)))

(defmacro @ref (component &rest selector-spec)
  "Placeholder for @ref macro"
  (declare (ignore component selector-spec))
  `(list :ref))
