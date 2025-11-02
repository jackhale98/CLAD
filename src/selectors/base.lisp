;;;; src/selectors/base.lisp --- Base selector protocol

(in-package :clad.selectors)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defvar *default-tolerance* 1e-6
  "Default tolerance for geometric comparisons in selectors")

;;; ============================================================================
;;; Base Selector Protocol
;;; ============================================================================

(defclass base-selector ()
  ((tolerance :initarg :tolerance
              :accessor selector-tolerance
              :initform *default-tolerance*
              :type number
              :documentation "Tolerance for geometric comparisons"))
  (:documentation "Base class for all selectors"))

(defgeneric apply-selector (selector shape-list)
  (:documentation "Apply selector to list of shapes, return filtered list.

  Arguments:
    selector   - A selector instance
    shape-list - List of shapes to filter

  Returns: Filtered list of shapes that match the selector criteria"))

;;; ============================================================================
;;; Default Implementation
;;; ============================================================================

(defmethod apply-selector ((selector base-selector) shape-list)
  "Default implementation returns empty list (override in subclasses)"
  (declare (ignore shape-list))
  nil)
