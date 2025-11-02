;;;; src/selectors/combinators.lisp --- Combinator selectors (AND, OR, NOT)

(in-package :clad.selectors)

;;; ============================================================================
;;; AND Combinator
;;; ============================================================================

(defclass and-selector (base-selector)
  ((selectors :initarg :selectors
              :accessor selector-selectors
              :type list
              :documentation "List of selectors to combine with AND"))
  (:documentation "Combinator that returns intersection of multiple selectors.

  Only shapes that match ALL child selectors are returned.

  Example:
    (and-selector :selectors (list parallel-z min-z))
    => Selects shapes parallel to Z AND at minimum Z"))

(defmethod apply-selector ((selector and-selector) shape-list)
  "Apply AND logic: return shapes that match ALL child selectors.

  Algorithm:
    1. Apply first selector to get initial set
    2. For each remaining selector, apply it and intersect with result
    3. Return final intersection"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let ((child-selectors (selector-selectors selector)))
    (when (null child-selectors)
      (return-from apply-selector shape-list))

    ;; Start with results from first selector
    (let ((result (apply-selector (first child-selectors) shape-list)))
      ;; Intersect with results from remaining selectors
      (dolist (child (rest child-selectors))
        (let ((child-result (apply-selector child shape-list)))
          ;; Intersection: keep only shapes in both result and child-result
          (setf result (intersection result child-result))))
      result)))

;;; ============================================================================
;;; OR Combinator
;;; ============================================================================

(defclass or-selector (base-selector)
  ((selectors :initarg :selectors
              :accessor selector-selectors
              :type list
              :documentation "List of selectors to combine with OR"))
  (:documentation "Combinator that returns union of multiple selectors.

  Shapes that match ANY child selector are returned.

  Example:
    (or-selector :selectors (list top-sel bottom-sel))
    => Selects shapes that are either top OR bottom"))

(defmethod apply-selector ((selector or-selector) shape-list)
  "Apply OR logic: return shapes that match ANY child selector.

  Algorithm:
    1. Apply each selector to shape list
    2. Collect all results
    3. Return union (remove duplicates)"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let ((child-selectors (selector-selectors selector)))
    (when (null child-selectors)
      (return-from apply-selector nil))

    ;; Collect results from all selectors and take union
    (let ((result nil))
      (dolist (child child-selectors)
        (let ((child-result (apply-selector child shape-list)))
          ;; Union: add all shapes from child-result that aren't already in result
          (setf result (union result child-result))))
      result)))

;;; ============================================================================
;;; NOT Combinator
;;; ============================================================================

(defclass not-selector (base-selector)
  ((selector :initarg :selector
             :accessor selector-selector
             :documentation "Selector to negate")
   (universe :initarg :universe
             :accessor selector-universe
             :type list
             :documentation "Universe of all possible shapes for complement"))
  (:documentation "Combinator that returns complement of a selector.

  Returns all shapes from universe that do NOT match the child selector.

  Example:
    (not-selector :selector top-sel :universe all-faces)
    => Selects all faces except the top"))

(defmethod apply-selector ((selector not-selector) shape-list)
  "Apply NOT logic: return shapes that do NOT match child selector.

  Algorithm:
    1. Apply child selector to get matching shapes
    2. Return set difference: shape-list - matching shapes"

  (when (null shape-list)
    (return-from apply-selector nil))

  (let ((child-selector (selector-selector selector))
        (child-result (apply-selector (selector-selector selector) shape-list)))
    ;; Set difference: shapes in shape-list but not in child-result
    (set-difference shape-list child-result)))
