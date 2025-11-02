;;;; constraints.lisp --- Constraint types for 2D sketches

(in-package #:clad.sketch.constraints)

;;; Placeholder implementations for Phase 9

(defclass constraint ()
  ()
  (:documentation "Placeholder for constraint"))

(defclass fixed-constraint (constraint)
  ()
  (:documentation "Placeholder for fixed constraint"))

(defclass coincident-constraint (constraint)
  ()
  (:documentation "Placeholder for coincident constraint"))

(defclass distance-constraint (constraint)
  ()
  (:documentation "Placeholder for distance constraint"))

(defclass horizontal-constraint (constraint)
  ()
  (:documentation "Placeholder for horizontal constraint"))

(defclass vertical-constraint (constraint)
  ()
  (:documentation "Placeholder for vertical constraint"))

(defclass parallel-constraint (constraint)
  ()
  (:documentation "Placeholder for parallel constraint"))

(defclass perpendicular-constraint (constraint)
  ()
  (:documentation "Placeholder for perpendicular constraint"))

;; Stub functions
(defun make-constraint (type &rest args)
  (declare (ignore type args))
  (make-instance 'constraint))

(defun constraint-entities (constraint)
  (declare (ignore constraint))
  nil)

(defun constraint-type (constraint)
  (declare (ignore constraint))
  nil)

(defun constraint-parameters (constraint)
  (declare (ignore constraint))
  nil)

(defun constraint-weight (constraint)
  (declare (ignore constraint))
  1.0d0)

(defun constraint-error (constraint)
  (declare (ignore constraint))
  0.0d0)

(defun constraint-jacobian (constraint)
  (declare (ignore constraint))
  nil)

(defun apply-constraint (constraint)
  (declare (ignore constraint))
  nil)
