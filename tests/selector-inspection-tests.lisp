;;;; tests/selector-inspection-tests.lisp --- Tests for selector inspection/debugging

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite selector-inspection-tests
    :description "Tests for selector debugging and inspection tools (Phase 3)"
    :in clad-tests)

(in-suite selector-inspection-tests)

;;; ============================================================================
;;; Phase 3.1: REPL Inspection Function (RED)
;;; ============================================================================

(test inspect-selection-basic
  "inspect-selection returns detailed information about selection"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (report (clad.selectors:inspect-selection faces :type :plane)))
    ;; Report should be a property list
    (is (listp report))
    ;; Should have required keys
    (is (not (null (getf report :count))))
    (is (not (null (getf report :shapes))))
    (is (not (null (getf report :types))))
    ;; Count should match number of planar faces
    (is (= 6 (getf report :count)))
    ;; Shapes should be a list
    (is (listp (getf report :shapes)))
    (is (= 6 (length (getf report :shapes))))))

(test inspect-selection-empty
  "inspect-selection handles empty selection gracefully"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (report (clad.selectors:inspect-selection faces :type :cylinder)))
    ;; Should still return valid report
    (is (listp report))
    ;; Count should be zero
    (is (zerop (getf report :count)))
    ;; Shapes should be empty
    (is (null (getf report :shapes)))))

(test inspect-selection-with-combinator
  "inspect-selection works with combinators"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (report (clad.selectors:inspect-selection faces
                                                   :and :type :plane
                                                        :at-z 25.0 :tolerance 1.0)))
    ;; Should find top face
    (is (>= (getf report :count) 1))
    ;; All should be planar
    (is (every (lambda (type) (eq type :plane))
               (getf report :types)))))

(test inspect-selection-with-position-selector
  "inspect-selection works with position selectors"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (report (clad.selectors:inspect-selection faces :at-z 25.0 :tolerance 1.0)))
    ;; Should have count
    (is (numberp (getf report :count)))
    ;; Should have shapes
    (is (listp (getf report :shapes)))
    ;; Should have centers
    (is (listp (getf report :centers)))))

(test inspect-selection-provides-descriptions
  "inspect-selection provides human-readable descriptions"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (report (clad.selectors:inspect-selection faces :type :plane)))
    ;; Should have descriptions
    (is (not (null (getf report :descriptions))))
    ;; Each description should be a string
    (is (every #'stringp (getf report :descriptions)))
    ;; Should have same number as shapes
    (is (= (length (getf report :descriptions))
           (length (getf report :shapes))))))

(test inspect-selection-with-edges
  "inspect-selection works with edges"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (edges (clad.shapes:edges wrapped))
         (report (clad.selectors:inspect-selection edges :type :line)))
    ;; Should find line edges
    (is (> (getf report :count) 0))
    ;; All should be lines
    (is (every (lambda (type) (eq type :line))
               (getf report :types)))))

(test inspect-selection-includes-centers
  "inspect-selection includes center points"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (report (clad.selectors:inspect-selection faces :type :plane)))
    ;; Should have centers
    (is (not (null (getf report :centers))))
    ;; Each center should be a 3-element list
    (is (every (lambda (center)
                 (and (listp center)
                      (= 3 (length center))
                      (every #'numberp center)))
               (getf report :centers)))))

;;; ============================================================================
;;; Phase 3.2: DSL Debug Forms (RED)
;;; ============================================================================

(test debug-selection-outputs-to-stream
  "debug-selection can output to a string stream"
  (let ((output (with-output-to-string (*standard-output*)
                  (let* ((box (clad.core:make-box 100 100 50))
                         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
                         (faces (clad.shapes:faces wrapped)))
                    (clad.selectors:debug-selection faces :type :plane)))))
    ;; Should have printed something
    (is (> (length output) 0))
    ;; Should mention selection count
    (is (search "Selection" output))
    (is (search "6" output))))

(test debug-selection-with-message
  "debug-selection accepts optional message"
  (let ((output (with-output-to-string (*standard-output*)
                  (let* ((box (clad.core:make-box 100 100 50))
                         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
                         (faces (clad.shapes:faces wrapped)))
                    (clad.selectors:debug-selection faces :type :plane
                                                    :message "Testing planar faces")))))
    ;; Should include custom message
    (is (search "Testing planar faces" output))))

(test debug-selection-with-empty-result
  "debug-selection handles empty selection"
  (let ((output (with-output-to-string (*standard-output*)
                  (let* ((box (clad.core:make-box 100 100 50))
                         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
                         (faces (clad.shapes:faces wrapped)))
                    (clad.selectors:debug-selection faces :type :cylinder)))))
    ;; Should indicate no matches
    (is (search "0" output))))

(test debug-selection-with-combinator
  "debug-selection works with complex selectors"
  (let ((output (with-output-to-string (*standard-output*)
                  (let* ((box (clad.core:make-box 100 100 50))
                         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
                         (faces (clad.shapes:faces wrapped)))
                    (clad.selectors:debug-selection faces
                                                    :and :type :plane
                                                         :at-z 25.0 :tolerance 1.0)))))
    ;; Should have output
    (is (> (length output) 0))))

;;; ============================================================================
;;; Phase 3.3: Viewer Highlighting (RED)
;;; ============================================================================

(test add-highlight-basic
  "add-highlight stores highlight data"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (selected (clad.selectors:select faces :type :plane)))
    ;; Clear any existing highlights
    (clad.selectors:clear-highlights)
    ;; Add highlight
    (clad.selectors:add-highlight selected :red)
    ;; Should have highlights
    (is (not (null (clad.selectors:get-highlights))))
    ;; Should have one highlight entry
    (is (= 1 (length (clad.selectors:get-highlights))))))

(test add-highlight-with-color
  "add-highlight accepts different colors"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (selected (clad.selectors:select faces :type :plane)))
    (clad.selectors:clear-highlights)
    ;; Test different colors
    (clad.selectors:add-highlight selected :red)
    (clad.selectors:add-highlight selected :green)
    (clad.selectors:add-highlight selected :blue)
    ;; Should have 3 highlight entries
    (is (= 3 (length (clad.selectors:get-highlights))))))

(test add-highlight-multiple-calls
  "Multiple add-highlight calls accumulate"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped)))
    (clad.selectors:clear-highlights)
    ;; Add multiple highlights with different selections
    (let ((top-faces (clad.selectors:select faces :at-z 25.0 :tolerance 1.0))
          (bottom-faces (clad.selectors:select faces :at-z -25.0 :tolerance 1.0)))
      (clad.selectors:add-highlight top-faces :red)
      (clad.selectors:add-highlight bottom-faces :blue)
      ;; Should have accumulated both (if they're not empty)
      ;; At minimum, should have 1 or more entries
      (is (>= (length (clad.selectors:get-highlights)) 1)))))

(test clear-highlights-works
  "clear-highlights removes all highlights"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (selected (clad.selectors:select faces :type :plane)))
    ;; Add highlights
    (clad.selectors:add-highlight selected :red)
    (is (not (null (clad.selectors:get-highlights))))
    ;; Clear
    (clad.selectors:clear-highlights)
    ;; Should be empty
    (is (null (clad.selectors:get-highlights)))))

(test get-highlights-returns-list
  "get-highlights returns a list of highlight specs"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (selected (clad.selectors:select faces :type :plane)))
    (clad.selectors:clear-highlights)
    (clad.selectors:add-highlight selected :red)
    (let ((highlights (clad.selectors:get-highlights)))
      ;; Should be a list
      (is (listp highlights))
      ;; Each entry should have :entities and :color
      (dolist (hl highlights)
        (is (not (null (getf hl :entities))))
        (is (not (null (getf hl :color))))))))
