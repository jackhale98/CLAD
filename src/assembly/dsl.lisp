;;;; dsl.lisp --- Declarative DSL for assemblies

(in-package #:clad.assembly.dsl)

;;; ==============================================================================
;;; Reference Macro (Week 15-16)
;;; ==============================================================================

(defmacro @ref (component &rest selector-spec)
  "Create a reference to a component feature for mate constraints.

  Args:
    component - Component name (symbol)
    selector-spec - Face/feature selector specification

  Returns: Reference suitable for add-mate

  Example:
    (@ref :base :face-top)
    (@ref :housing '(:face :direction :+z))"
  (if selector-spec
      `(quote (,component ,@selector-spec))
      `',component))

;;; ==============================================================================
;;; Assembly DSL Macro (Week 15-16)
;;; ==============================================================================

(defmacro defassembly (name parameters description &body body)
  "Define a parametric assembly.

  Args:
    name - Assembly function name (symbol)
    parameters - Parameter list ((param default) ...)
    description - Assembly description string
    body - Assembly definition forms

  Body Forms:
    (:component name part &key quantity fixed metadata)
    (:mate type comp1 ref1 comp2 ref2 &rest args)
    (:parameter name value)

  Returns: Defined assembly function

  Example:
    (defassembly motor-assembly
        ((bolt-count 4)
         (shaft-length 100))
      \"DC motor assembly with mounting bolts\"
      (:component :housing (make-box 80 80 60)
                  :fixed t
                  :metadata '(:part-number \"HSG-001\"
                             :material \"Aluminum\"))
      (:component :shaft (make-cylinder 10 shaft-length)
                  :metadata '(:part-number \"SHAFT-001\"
                             :material \"Steel\"))
      (:component :bolt (make-cylinder 3 12)
                  :quantity bolt-count
                  :metadata '(:part-number \"M3-12-SS\"
                             :material \"Stainless Steel\"))
      (:mate :concentric
             :housing :center-hole
             :shaft :axis)
      (:mate :distance
             :housing :face-top
             :shaft :face-bottom
             :offset 5.0))"
  (let ((param-names (mapcar #'first parameters))
        (param-defaults (mapcar #'second parameters)))
    `(defun ,name (&key ,@(mapcar (lambda (p d) `(,p ,d)) param-names param-defaults))
       ,description
       (let ((asm (make-assembly :name ',name :description ,description)))
         ;; Process body forms
         ,@(mapcar (lambda (form)
                     (case (first form)
                       (:component
                        (destructuring-bind (kw comp-name part &rest options) form
                          (declare (ignore kw))
                          `(add-component asm ',comp-name ,part ,@options)))

                       (:mate
                        (destructuring-bind (kw type comp1 ref1 comp2 ref2 &rest options) form
                          (declare (ignore kw))
                          `(add-mate asm ,type ',comp1 ',ref1 ',comp2 ',ref2 ,@options)))

                       (:parameter
                        (destructuring-bind (kw param-name value) form
                          (declare (ignore kw))
                          `(set-parameter asm ',param-name ,value)))

                       (t
                        form)))
                   body)
         ;; Return assembly
         asm))))

;;; ==============================================================================
;;; Assembly DSL Utilities
;;; ==============================================================================

(defmacro with-assembly (assembly &body body)
  "Execute body forms with an assembly context.

  Args:
    assembly - Assembly instance or name
    body - Forms to execute

  Example:
    (with-assembly my-asm
      (add-component asm :part1 (make-box 10 10 10))
      (add-component asm :part2 (make-cylinder 5 20)))"
  `(let ((asm ,assembly))
     ,@body
     asm))
