;;;; src/selectors/custom.lisp --- Custom predicate selectors

(in-package :clad.selectors)

;;; ============================================================================
;;; Custom Predicate Selector
;;; ============================================================================

(defclass custom-selector (base-selector)
  ((predicate :initarg :predicate
              :accessor selector-predicate
              :type function
              :documentation "User-defined predicate function"))
  (:documentation "Selector that filters shapes using a custom predicate function.

  The predicate function receives a shape and returns T if it should be selected.

  Example:
    (custom-selector :predicate (lambda (face) (> (area face) 1000)))
    => Selects faces with area > 1000"))

(defmethod apply-selector ((selector custom-selector) shape-list)
  "Apply custom predicate to filter shapes.

  Algorithm:
    1. For each shape in shape-list
    2. Apply predicate function
    3. Keep shapes where predicate returns true"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let ((predicate (selector-predicate selector)))
    ;; Filter shapes using the custom predicate
    (remove-if-not
     (lambda (shape)
       (handler-case
           (funcall predicate shape)
         ;; If predicate errors, exclude this shape
         (error () nil)))
     shape-list)))
