;;;; src/selectors/api.lisp --- High-level selector API

(in-package :clad.selectors)

;;; ============================================================================
;;; Selector Parsing Helpers
;;; ============================================================================

(defun parse-selector-from-spec (selector-keyword args)
  "Parse a single selector specification and return (values selector remaining-args).

  Arguments:
    selector-keyword - Keyword identifying selector type (:direction, :type, etc.)
    args - List of remaining arguments

  Returns:
    selector - The created selector object
    remaining-args - Arguments not consumed by this selector"

  (cond
    ;; :direction - consumes 1-2 args (axis, optional :extreme keyword)
    ((eq selector-keyword :direction)
     (let ((axis (first args))
           (remaining (rest args)))
       (if (and remaining (eq (first remaining) :extreme))
           (values (make-instance 'direction-selector
                                  :axis axis
                                  :extreme (second remaining))
                   (rest (rest remaining)))
           (values (make-instance 'direction-selector :axis axis)
                   remaining))))

    ;; :parallel - consumes 1 arg (axis)
    ((eq selector-keyword :parallel)
     (values (make-instance 'parallel-selector :axis (first args))
             (rest args)))

    ;; :perpendicular - consumes 1 arg (axis)
    ((eq selector-keyword :perpendicular)
     (values (make-instance 'perpendicular-selector :axis (first args))
             (rest args)))

    ;; :type - consumes 1 arg (shape-type)
    ((eq selector-keyword :type)
     (values (make-instance 'type-selector :shape-type (first args))
             (rest args)))

    ;; :area, :length, :volume, :radius - consume 2-3 args
    ((member selector-keyword '(:area :length :volume :radius))
     (let ((comparator (first args))
           (value1 (second args))
           (remaining (rest (rest args))))
       (if (and remaining (numberp (first remaining)))
           ;; Three args: comparator value1 value2 (for :between)
           (values (make-instance 'size-selector
                                  :property selector-keyword
                                  :comparator comparator
                                  :value1 value1
                                  :value2 (first remaining))
                   (rest remaining))
           ;; Two args: comparator value1
           (values (make-instance 'size-selector
                                  :property selector-keyword
                                  :comparator comparator
                                  :value1 value1
                                  :value2 nil)
                   remaining))))

    ;; :at-x, :at-y, :at-z - consume 1-2 args (value, optional :tolerance)
    ((member selector-keyword '(:at-x :at-y :at-z))
     (let* ((value (first args))
            (remaining (rest args))
            (tolerance (if (and remaining (eq (first remaining) :tolerance))
                          (progn
                            (setf remaining (rest (rest remaining)))
                            (second (rest args)))
                          0.01d0))
            (axis (case selector-keyword
                    (:at-x :x)
                    (:at-y :y)
                    (:at-z :z))))
       (values (make-instance 'position-selector
                              :axis axis
                              :value (coerce value 'double-float)
                              :tolerance (coerce tolerance 'double-float))
               remaining)))

    ;; :between-x, :between-y, :between-z - consume 2 args (min-value max-value)
    ((member selector-keyword '(:between-x :between-y :between-z))
     (let* ((min-value (first args))
            (max-value (second args))
            (remaining (rest (rest args)))
            (axis (case selector-keyword
                    (:between-x :x)
                    (:between-y :y)
                    (:between-z :z))))
       (values (make-instance 'range-selector
                              :axis axis
                              :min (coerce min-value 'double-float)
                              :max (coerce max-value 'double-float))
               remaining)))

    ;; :within-box - consume 2 args (min-corner max-corner)
    ((eq selector-keyword :within-box)
     (let* ((min-corner (first args))
            (max-corner (second args))
            (remaining (rest (rest args))))
       (values (make-instance 'bbox-selector
                              :min min-corner
                              :max max-corner)
               remaining)))

    ;; :near-point - consume 1 arg (point) + :radius keyword
    ((eq selector-keyword :near-point)
     (let* ((point (first args))
            (remaining (rest args))
            (radius (if (and remaining (eq (first remaining) :radius))
                       (progn
                         (setf remaining (rest (rest remaining)))
                         (second (rest args)))
                       10.0d0)))  ; Default radius 10mm
       (values (make-instance 'proximity-selector
                              :point point
                              :radius (coerce radius 'double-float))
               remaining)))

    (t
     (error "Unknown selector keyword: ~A" selector-keyword))))

(defun parse-child-selectors (args)
  "Parse a list of selector specifications into a list of selector objects.

  Arguments:
    args - List of alternating keywords and their arguments, OR nested combinator lists
           e.g., (:type :planar :direction :+z :area :> 5000.0)
           e.g., (:type :line (:or :parallel :x :parallel :y))

  Returns: List of selector objects"

  (loop while args
        for item = (pop args)
        collect (cond
                  ;; If item is a list, it's a nested combinator expression
                  ((listp item)
                   (let ((combinator (first item))
                         (combinator-args (rest item)))
                     (case combinator
                       (:and
                        (let ((child-selectors (parse-child-selectors combinator-args)))
                          (make-instance 'and-selector :selectors child-selectors)))
                       (:or
                        (let ((child-selectors (parse-child-selectors combinator-args)))
                          (make-instance 'or-selector :selectors child-selectors)))
                       (:not
                        (let ((child-selectors (parse-child-selectors combinator-args)))
                          (when (not (= 1 (length child-selectors)))
                            (error "NOT combinator requires exactly one selector, got ~A" (length child-selectors)))
                          (make-instance 'not-selector :selector (first child-selectors))))
                       (t
                        (error "Unknown combinator in nested expression: ~A" combinator)))))
                  ;; Otherwise it's a keyword selector
                  ((keywordp item)
                   (multiple-value-bind (selector remaining)
                       (parse-selector-from-spec item args)
                     (setf args remaining)
                     selector))
                  (t
                   (error "Expected selector keyword or combinator list, got: ~A" item)))))

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

    7. Boolean combinators (Phase 1):
       (select faces :and :type :planar :direction :+z)
       (select edges :or :parallel :x :parallel :z)
       (select faces :not :type :cylindrical)

    8. Position selectors (Phase 2):
       (select faces :at-z 50.0)
       (select faces :at-x -25.0 :tolerance 0.1)
       (select edges :at-y 0.0 :tolerance 1.0)

    9. Range selectors (Phase 2.2):
       (select faces :between-z -10.0 10.0)
       (select edges :between-x 0.0 50.0)
       (select faces :between-y -25.0 25.0)

   10. Bounding box selector (Phase 2.3):
       (select faces :within-box '(0 0 0) '(50 50 50))
       (select edges :within-box '(-10 -10 -10) '(10 10 10))

   11. Proximity selector (Phase 2.4):
       (select faces :near-point '(0 0 0) :radius 50.0)
       (select edges :near-point '(100 50 25) :radius 10.0)

  Arguments:
    shape-list - List of shapes to filter
    selector-spec - Either a function or a keyword selector type
    args - Additional arguments depending on selector type

  Returns: Filtered list of shapes"

  (cond
    ;; Case 0: Combinator expression as a list - recursively call select with extracted parts
    ((listp selector-spec)
     (let ((combinator (first selector-spec))
           (combinator-args (rest selector-spec)))
       (apply #'select shape-list combinator combinator-args)))

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

    ;; Case 7: :and combinator (Phase 1.1)
    ((eq selector-spec :and)
     (let ((child-selectors (parse-child-selectors args)))
       (let ((selector (make-instance 'and-selector :selectors child-selectors)))
         (apply-selector selector shape-list))))

    ;; Case 8: :or combinator (Phase 1.2)
    ((eq selector-spec :or)
     (let ((child-selectors (parse-child-selectors args)))
       (let ((selector (make-instance 'or-selector :selectors child-selectors)))
         (apply-selector selector shape-list))))

    ;; Case 9: :not combinator (Phase 1.3)
    ((eq selector-spec :not)
     (let ((child-selectors (parse-child-selectors args)))
       (when (not (= 1 (length child-selectors)))
         (error "NOT combinator requires exactly one selector, got ~A" (length child-selectors)))
       (let ((selector (make-instance 'not-selector :selector (first child-selectors))))
         (apply-selector selector shape-list))))

    ;; Case 10: Position selectors (Phase 2.1)
    ((member selector-spec '(:at-x :at-y :at-z))
     (let* ((value (first args))
            (remaining (rest args))
            (tolerance (getf remaining :tolerance 0.01d0))
            (axis (case selector-spec
                    (:at-x :x)
                    (:at-y :y)
                    (:at-z :z))))
       (let ((selector (make-instance 'position-selector
                                      :axis axis
                                      :value (coerce value 'double-float)
                                      :tolerance (coerce tolerance 'double-float))))
         (apply-selector selector shape-list))))

    ;; Case 11: Range selectors (Phase 2.2)
    ((member selector-spec '(:between-x :between-y :between-z))
     (let* ((min-value (first args))
            (max-value (second args))
            (axis (case selector-spec
                    (:between-x :x)
                    (:between-y :y)
                    (:between-z :z))))
       (let ((selector (make-instance 'range-selector
                                      :axis axis
                                      :min (coerce min-value 'double-float)
                                      :max (coerce max-value 'double-float))))
         (apply-selector selector shape-list))))

    ;; Case 12: Bounding box selector (Phase 2.3)
    ((eq selector-spec :within-box)
     (let* ((min-corner (first args))
            (max-corner (second args)))
       (let ((selector (make-instance 'bbox-selector
                                      :min min-corner
                                      :max max-corner)))
         (apply-selector selector shape-list))))

    ;; Case 13: Proximity selector (Phase 2.4)
    ((eq selector-spec :near-point)
     (let* ((point (first args))
            (remaining (rest args))
            (radius (getf remaining :radius 10.0d0)))
       (let ((selector (make-instance 'proximity-selector
                                      :point point
                                      :radius (coerce radius 'double-float))))
         (apply-selector selector shape-list))))

    ;; Unknown selector type
    (t
     (error "Unknown selector type: ~A" selector-spec))))
