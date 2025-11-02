;;;; assembly.lisp --- Assembly modeling with mates and constraints

(in-package #:clad.assembly)

;;; Placeholder implementations for Phase 10

(defclass assembly ()
  ()
  (:documentation "Placeholder for assembly"))

(defclass component ()
  ()
  (:documentation "Placeholder for component"))

;; Stub functions
(defun make-assembly (&key name)
  (declare (ignore name))
  (make-instance 'assembly))

(defun add-component (assembly part &rest args)
  (declare (ignore assembly part args))
  (make-instance 'component))

(defun assembly-name (assembly)
  (declare (ignore assembly))
  nil)

(defun assembly-components (assembly)
  (declare (ignore assembly))
  nil)

(defun assembly-constraints (assembly)
  (declare (ignore assembly))
  nil)

(defun component-name (component)
  (declare (ignore component))
  nil)

(defun component-part (component)
  (declare (ignore component))
  nil)

(defun component-position (component)
  (declare (ignore component))
  '(0 0 0))

(defun component-rotation (component)
  (declare (ignore component))
  '(:axis (0 0 1) :angle 0))

(defun component-transform (component)
  (declare (ignore component))
  nil)

(defun component-fixed-p (component)
  (declare (ignore component))
  nil)

(defun component-quantity (component)
  (declare (ignore component))
  1)

(defun component-metadata (component)
  (declare (ignore component))
  nil)
