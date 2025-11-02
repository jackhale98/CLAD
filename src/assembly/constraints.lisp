;;;; constraints.lisp --- Mate constraints for assemblies

(in-package #:clad.assembly.constraints)

;;; Placeholder implementations for Phase 10

(defclass mate-constraint ()
  ()
  (:documentation "Placeholder for mate constraint"))

;; Stub functions
(defun add-mate (assembly type component1 ref1 component2 ref2 &rest args)
  (declare (ignore assembly type component1 ref1 component2 ref2 args))
  (make-instance 'mate-constraint))

(defun mate-type (mate)
  (declare (ignore mate))
  nil)

(defun mate-component1 (mate)
  (declare (ignore mate))
  nil)

(defun mate-component2 (mate)
  (declare (ignore mate))
  nil)

(defun mate-reference1 (mate)
  (declare (ignore mate))
  nil)

(defun mate-reference2 (mate)
  (declare (ignore mate))
  nil)

(defun mate-offset (mate)
  (declare (ignore mate))
  0.0d0)

(defun mate-error (mate)
  (declare (ignore mate))
  0.0d0)

(defun apply-mate (mate)
  (declare (ignore mate))
  nil)
