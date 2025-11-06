;;;; view.lisp --- Assembly visualization helpers

(in-package #:clad.assembly)

;;; ==============================================================================
;;; Assembly to Shape Conversion
;;; ==============================================================================

(defun assembly-to-shape (assembly)
  "Convert an assembly to a combined shape for viewing.

  This function:
  1. Extracts all components from the assembly
  2. Applies their position transforms
  3. Combines them using boolean union into a single shape

  Args:
    assembly - Assembly to convert

  Returns: Combined shape containing all components

  Note: This is a simplified visualization - constraints are not applied,
  only explicit component positions are used.

  Example:
    (let ((assy (make-assembly :name :test)))
      (add-component assy :box1 (clad.core:make-box 50 50 10))
      (add-component assy :box2 (clad.core:make-box 50 50 10))
      (set-component-position (get-component assy :box2) 0 0 15)
      (assembly-to-shape assy))"
  (let ((components (list-components assembly))
        (result-shape nil))

    (when (null components)
      (error "Assembly ~A has no components" (assembly-name assembly)))

    ;; Process each component
    (dolist (comp components)
      (let* ((part (component-part comp))
             (pos (component-position comp))
             ;; Get the shape (handle nested assemblies recursively)
             (shape (if (assembly-p part)
                       (assembly-to-shape part)
                       part))
             ;; Apply position transform
             (transformed-shape
               (if (equal pos '(0 0 0))
                   shape
                   (clad.core:translate shape
                                       (first pos)
                                       (second pos)
                                       (third pos)))))

        ;; Combine with result
        (setf result-shape
              (if result-shape
                  (clad.core:union-shapes result-shape transformed-shape)
                  transformed-shape))))

    result-shape))

;;; ==============================================================================
;;; View Function Override
;;; ==============================================================================

(defun view-assembly (assembly &key (name "assembly") (auto-start t))
  "View an assembly in the 3D viewer.

  Args:
    assembly - Assembly to view
    :name - Name for the viewer (default \"assembly\")
    :auto-start - Start viewer automatically (default t)

  Returns: The viewer result

  This function converts the assembly to a combined shape and
  displays it in the web viewer.

  Example:
    (view-assembly my-assy :name \"motor-assembly\")"
  (let ((combined-shape (assembly-to-shape assembly)))
    (clad.viewer:view combined-shape :name name :auto-start auto-start)))
