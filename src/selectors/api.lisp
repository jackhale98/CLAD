;;;; src/selectors/api.lisp --- High-level selector API

(in-package :clad.selectors)

;;; ============================================================================
;;; High-Level Select Function
;;; ============================================================================

(defun select (shape-list selector-spec &rest args)
  "High-level convenience function for selecting shapes.

  Usage patterns:
    1. Lambda function:
       (select faces (lambda (f) (> (area f) 1000)))

    2. Direction keyword:
       (select faces :direction :+z :extreme :max)

    3. Parallel keyword:
       (select faces :parallel :z)

    4. Perpendicular keyword:
       (select faces :perpendicular :z)

    5. Type keyword (Phase 8):
       (select edges :type :line)
       (select faces :type :plane)

    6. Size keywords (Phase 8):
       (select faces :area :> 5000.0)
       (select edges :length :between 50.0 100.0)
       (select solids :volume :< 10000.0)
       (select edges :radius := 10.0)

  Arguments:
    shape-list - List of shapes to filter
    selector-spec - Either a function or a keyword selector type
    args - Additional arguments depending on selector type

  Returns: Filtered list of shapes"

  (cond
    ;; Case 1: Lambda function - custom predicate
    ((functionp selector-spec)
     (let ((selector (make-instance 'custom-selector :predicate selector-spec)))
       (apply-selector selector shape-list)))

    ;; Case 2: :direction keyword
    ((eq selector-spec :direction)
     (destructuring-bind (axis &key extreme) args
       (let ((selector (make-instance 'direction-selector
                                      :axis axis
                                      :extreme extreme)))
         (apply-selector selector shape-list))))

    ;; Case 3: :parallel keyword
    ((eq selector-spec :parallel)
     (let ((axis (first args)))
       (let ((selector (make-instance 'parallel-selector :axis axis)))
         (apply-selector selector shape-list))))

    ;; Case 4: :perpendicular keyword
    ((eq selector-spec :perpendicular)
     (let ((axis (first args)))
       (let ((selector (make-instance 'perpendicular-selector :axis axis)))
         (apply-selector selector shape-list))))

    ;; Case 5: :type keyword (Phase 8)
    ((eq selector-spec :type)
     (let ((shape-type (first args)))
       (let ((selector (make-instance 'type-selector :shape-type shape-type)))
         (apply-selector selector shape-list))))

    ;; Case 6: :area, :length, :volume, :radius keywords (Phase 8)
    ((member selector-spec '(:area :length :volume :radius))
     (destructuring-bind (comparator value1 &optional value2) args
       (let ((selector (make-instance 'size-selector
                                      :property selector-spec
                                      :comparator comparator
                                      :value1 value1
                                      :value2 value2)))
         (apply-selector selector shape-list))))

    ;; Unknown selector type
    (t
     (error "Unknown selector type: ~A" selector-spec))))
