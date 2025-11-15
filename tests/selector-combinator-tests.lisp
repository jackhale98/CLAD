;;;; tests/selector-combinator-tests.lisp --- Tests for boolean selector combinators

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite selector-combinator-tests
    :description "Tests for AND/OR/NOT selector combinators"
    :in clad-tests)

(in-suite selector-combinator-tests)

;;; ============================================================================
;;; AND Combinator Tests (TDD Cycle 1.1)
;;; ============================================================================

(test and-combinator-basic
  "AND combinator selects entities matching ALL criteria"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Select planar faces pointing up
         (selected (clad.selectors:select faces
                                          :and
                                          :type :plane
                                          :direction :+z)))
    ;; Should match at least the top face
    (is (>= (length selected) 1))
    ;; All selected faces should be planar
    (is (every (lambda (f)
                 (eq :plane (clad.shapes:geom-type f)))
               selected))))

(test and-combinator-empty
  "AND combinator returns empty when no matches"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Impossible: planar AND cylindrical
         (selected (clad.selectors:select faces
                                          :and
                                          :type :plane
                                          :type :cylinder)))
    (is (null selected))))

(test and-combinator-three-criteria
  "AND combinator works with three criteria"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Planar AND upward-facing AND large area
         (selected (clad.selectors:select faces
                                          :and
                                          :type :plane
                                          :direction :+z
                                          :area :> 5000.0)))
    ;; Should match top face (100*100 = 10000)
    (is (>= (length selected) 1))))

(test and-combinator-preserves-order
  "AND combinator preserves input list order"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (planar-faces (clad.selectors:select faces :type :plane))
         ;; Apply AND with always-true condition
         (selected (clad.selectors:select planar-faces
                                          :and
                                          :type :plane)))
    ;; Should preserve order
    (is (equal planar-faces selected))))

(test and-combinator-with-edges
  "AND combinator works with edges too"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (edges (clad.shapes:edges wrapped))
         ;; Straight edges that are parallel to Z
         (selected (clad.selectors:select edges
                                          :and
                                          :type :line
                                          :parallel :z)))
    ;; Box should have vertical edges parallel to Z
    ;; (accepting >= 4 to account for OpenCASCADE geometry representation)
    (is (>= (length selected) 4))
    ;; All selected edges should be lines
    (is (every (lambda (e)
                 (eq :line (clad.shapes:geom-type e)))
               selected))))

;;; ============================================================================
;;; OR Combinator Tests (TDD Cycle 1.2)
;;; ============================================================================

(test or-combinator-basic
  "OR combinator selects entities matching ANY criteria"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (edges (clad.shapes:edges wrapped))
         ;; Select each criterion separately
         (x-parallel (clad.selectors:select edges :parallel :x))
         (z-parallel (clad.selectors:select edges :parallel :z))
         ;; Parallel to X OR parallel to Z
         (selected (clad.selectors:select edges
                                          :or
                                          :parallel :x
                                          :parallel :z)))
    ;; OR should return union of both sets
    ;; Should be >= count of either individual selector
    (is (>= (length selected) (length x-parallel)))
    (is (>= (length selected) (length z-parallel)))
    ;; Should be <= sum (no more than that even with duplicates)
    (is (<= (length selected) (+ (length x-parallel) (length z-parallel))))))

(test or-combinator-single-match
  "OR combinator with only one criterion matching"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Cylindrical (none) OR top face
         (selected (clad.selectors:select faces
                                          :or
                                          :type :cylinder
                                          :direction :+z :extreme :max)))
    ;; Should get at least the top face
    (is (>= (length selected) 1))))

(test or-combinator-no-duplicates
  "OR combinator doesn't return duplicates"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Planar OR planar (same criterion twice)
         (selected (clad.selectors:select faces
                                          :or
                                          :type :plane
                                          :type :plane))
         (planar-once (clad.selectors:select faces :type :plane)))
    ;; Should have same count as selecting planar once
    (is (= (length selected) (length planar-once)))))

(test or-combinator-all-match
  "OR combinator when all criteria match same entities"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Type planar OR type planar (redundant)
         (selected (clad.selectors:select faces
                                          :or
                                          :type :plane
                                          :type :plane)))
    ;; All box faces are planar
    (is (= 6 (length selected)))))

;;; ============================================================================
;;; NOT Combinator Tests (TDD Cycle 1.3)
;;; ============================================================================

(test not-combinator-basic
  "NOT combinator inverts selection"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; All faces EXCEPT cylindrical (so all planar)
         (selected (clad.selectors:select faces
                                          :not
                                          :type :cylinder)))
    ;; Box has 6 planar faces, 0 cylindrical
    (is (= 6 (length selected)))))

(test not-combinator-with-direction
  "NOT combinator with direction selector"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; All faces except those pointing up
         (selected (clad.selectors:select faces
                                          :not
                                          :direction :+z :extreme :max)))
    ;; 6 faces - 1 top = 5 faces
    (is (= 5 (length selected)))))

(test not-combinator-empty-input
  "NOT combinator with empty input returns empty"
  (let ((selected (clad.selectors:select '()
                                         :not
                                         :type :plane)))
    (is (null selected))))

(test not-combinator-all-match
  "NOT combinator when all entities match returns empty"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; NOT planar (but all box faces ARE planar)
         (selected (clad.selectors:select faces
                                          :not
                                          :type :plane)))
    (is (null selected))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test combinator-empty-list
  "Combinators handle empty input list"
  (is (null (clad.selectors:select '() :and :type :plane)))
  (is (null (clad.selectors:select '() :or :type :plane)))
  (is (null (clad.selectors:select '() :not :type :plane))))

(test combinator-single-element
  "AND combinator works with single-element list"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (top-face (list (first faces))))
    ;; AND with single element
    (is (not (null (clad.selectors:select top-face :and :type :plane))))))
