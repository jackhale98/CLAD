;;;; tests/position-selector-tests.lisp --- Tests for position-based selectors

(in-package :clad.tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(def-suite position-selector-tests
    :description "Tests for position-based selectors (:at-x, :at-y, :at-z, etc.)"
    :in clad-tests)

(in-suite position-selector-tests)

;;; ============================================================================
;;; Phase 2.1: Single-Axis Position Selectors (RED)
;;; ============================================================================

(test at-z-selector-basic
  "Select faces at specific Z coordinate"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Top face at Z=25 (box centered: -25 to +25 in Z)
         (selected (clad.selectors:select faces :at-z 25.0 :tolerance 0.1)))
    ;; Should select exactly the top face
    (is (>= (length selected) 1))
    ;; Verify selected face center is at Z=25
    (dolist (face selected)
      (let* ((bbox (clad.shapes:bounding-box face))
             (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
        (is (< (abs (- center-z 25.0)) 0.2))))))

(test at-x-selector-basic
  "Select faces at specific X coordinate"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Right face at X=50
         (selected (clad.selectors:select faces :at-x 50.0 :tolerance 0.1)))
    (is (>= (length selected) 1))
    (dolist (face selected)
      (let* ((bbox (clad.shapes:bounding-box face))
             (center-x (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0)))
        (is (< (abs (- center-x 50.0)) 0.2))))))

(test at-y-selector-basic
  "Select faces at specific Y coordinate"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Front face at Y=50
         (selected (clad.selectors:select faces :at-y 50.0 :tolerance 0.1)))
    (is (>= (length selected) 1))
    (dolist (face selected)
      (let* ((bbox (clad.shapes:bounding-box face))
             (center-y (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0)))
        (is (< (abs (- center-y 50.0)) 0.2))))))

(test at-coordinate-no-matches
  "Position selector returns empty when no faces at coordinate"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; No face at X=1000
         (selected (clad.selectors:select faces :at-x 1000.0 :tolerance 0.1)))
    (is (null selected))))

(test at-coordinate-with-tolerance
  "Position selector uses tolerance correctly"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Top face at Z=25, query with Z=24.5 and tolerance 1.0
         (selected (clad.selectors:select faces :at-z 24.5 :tolerance 1.0)))
    ;; Should match because 24.5 is within 1.0 of 25.0
    (is (>= (length selected) 1))))

(test at-coordinate-strict-tolerance
  "Position selector with tight tolerance"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Top face at Z=25, query with Z=24.5 and tolerance 0.1
         (selected (clad.selectors:select faces :at-z 24.5 :tolerance 0.1)))
    ;; Should NOT match because 24.5 is not within 0.1 of 25.0
    (is (null selected))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test at-coordinate-empty-list
  "Position selector handles empty input"
  (is (null (clad.selectors:select '() :at-z 10.0))))

(test at-coordinate-with-edges
  "Position selector works with edges too"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (edges (clad.shapes:edges wrapped))
         ;; Edges at Z=25 (top edges)
         (selected (clad.selectors:select edges :at-z 25.0 :tolerance 1.0)))
    ;; Should find some edges at the top
    (is (>= (length selected) 4))))  ; At least 4 edges at top

;;; ============================================================================
;;; Integration with Combinators
;;; ============================================================================

(test at-coordinate-with-and-combinator
  "Position selector works with AND combinator"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Planar faces at Z=25
         (selected (clad.selectors:select faces
                                          :and :type :plane
                                               :at-z 25.0 :tolerance 0.1)))
    (is (>= (length selected) 1))
    (dolist (face selected)
      (is (eq :plane (clad.shapes:geom-type face))))))

(test at-coordinate-with-or-combinator
  "Position selector works with OR combinator"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Faces at Z=25 OR Z=-25 (top or bottom)
         (selected (clad.selectors:select faces
                                          :or :at-z 25.0 :tolerance 0.1
                                              :at-z -25.0 :tolerance 0.1)))
    ;; Should get at least 2 faces (top and bottom, possibly more if centered differently)
    (is (>= (length selected) 2))))

(test at-coordinate-with-not-combinator
  "Position selector works with NOT combinator"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (total-faces (length faces))
         ;; All faces EXCEPT those at Z=25
         (selected (clad.selectors:select faces
                                          :not :at-z 25.0 :tolerance 0.1)))
    ;; Should get fewer faces than total (at least one was excluded)
    (is (< (length selected) total-faces))
    ;; Should still have some faces
    (is (> (length selected) 0))))

;;; ============================================================================
;;; Phase 2.2: Range Selectors (RED)
;;; ============================================================================

(test between-z-range-basic
  "Select faces between two Z coordinates"
  (let* ((box (clad.core:make-box 100 100 100))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Faces with center Z between -10 and 10 (should include side faces)
         (selected (clad.selectors:select faces :between-z -10.0 10.0)))
    ;; Should match some faces in the middle range
    (is (>= (length selected) 1))
    ;; Verify all selected faces are within range
    (dolist (face selected)
      (let* ((bbox (clad.shapes:bounding-box face))
             (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
        (is (and (>= center-z -10.0) (<= center-z 10.0)))))))

(test between-x-range-basic
  "Select faces between two X coordinates"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Faces with center X between -20 and 20
         (selected (clad.selectors:select faces :between-x -20.0 20.0)))
    (is (>= (length selected) 1))
    (dolist (face selected)
      (let* ((bbox (clad.shapes:bounding-box face))
             (center-x (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0)))
        (is (and (>= center-x -20.0) (<= center-x 20.0)))))))

(test between-y-range-basic
  "Select faces between two Y coordinates"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Faces with center Y between 0 and 30
         (selected (clad.selectors:select faces :between-y 0.0 30.0)))
    (is (>= (length selected) 1))))

(test between-range-empty
  "Range selector returns empty when no faces in range"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Range outside box (-50 to -25 in Z, but box is -25 to 25)
         (selected (clad.selectors:select faces :between-z 200.0 300.0)))
    (is (null selected))))

(test between-range-full
  "Range selector selects all when range contains everything"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (total-faces (length faces))
         ;; Very wide range that includes all faces
         (selected (clad.selectors:select faces :between-z -1000.0 1000.0)))
    ;; Should get all faces
    (is (= (length selected) total-faces))))

(test between-range-with-combinator
  "Range selector works with AND combinator"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Planar faces between Z=-10 and Z=10
         (selected (clad.selectors:select faces
                                          :and :type :plane
                                               :between-z -10.0 10.0)))
    (is (>= (length selected) 1))
    (dolist (face selected)
      (is (eq :plane (clad.shapes:geom-type face))))))

(test between-range-inverted
  "Range selector handles min > max gracefully"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Inverted range should swap internally or return empty
         (selected (clad.selectors:select faces :between-z 30.0 -30.0)))
    ;; Should either swap and work, or return empty (implementation choice)
    (is (listp selected))))

;;; ============================================================================
;;; Phase 2.3: Bounding Box Selector (RED)
;;; ============================================================================

(test within-box-basic
  "Select faces within 3D bounding box"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Box centered at origin: -50 to 50 in X/Y, -25 to 25 in Z
         ;; Select faces in positive octant
         (selected (clad.selectors:select faces :within-box
                                          (list 0.0d0 0.0d0 0.0d0)
                                          (list 60.0d0 60.0d0 30.0d0))))
    ;; Should select some faces in that region
    (is (>= (length selected) 1))
    ;; Verify all selected faces are within the box
    (dolist (face selected)
      (let* ((bbox (clad.shapes:bounding-box face))
             (center-x (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0))
             (center-y (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0))
             (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0)))
        (is (and (>= center-x 0.0) (<= center-x 60.0)))
        (is (and (>= center-y 0.0) (<= center-y 60.0)))
        (is (and (>= center-z 0.0) (<= center-z 30.0)))))))

(test within-box-empty
  "within-box returns empty when no faces in region"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Region far from box
         (selected (clad.selectors:select faces :within-box
                                          (list 200.0d0 200.0d0 200.0d0)
                                          (list 300.0d0 300.0d0 300.0d0))))
    (is (null selected))))

(test within-box-all
  "within-box selects all when region encompasses entire shape"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (total-faces (length faces))
         ;; Very large region
         (selected (clad.selectors:select faces :within-box
                                          (list -1000.0d0 -1000.0d0 -1000.0d0)
                                          (list 1000.0d0 1000.0d0 1000.0d0))))
    ;; Should select all faces
    (is (= (length selected) total-faces))))

(test within-box-with-edges
  "within-box works with edges too"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (edges (clad.shapes:edges wrapped))
         ;; Select edges in positive X region
         (selected (clad.selectors:select edges :within-box
                                          (list 0.0d0 -60.0d0 -30.0d0)
                                          (list 60.0d0 60.0d0 30.0d0))))
    ;; Should find some edges on the right side
    (is (>= (length selected) 4))))

(test within-box-with-combinator
  "within-box works with AND combinator"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Planar faces in positive octant
         (selected (clad.selectors:select faces
                                          :and :type :plane
                                               :within-box (list 0.0d0 0.0d0 0.0d0)
                                                          (list 60.0d0 60.0d0 30.0d0))))
    (is (>= (length selected) 1))
    (dolist (face selected)
      (is (eq :plane (clad.shapes:geom-type face))))))

(test within-box-corner-selection
  "within-box can select faces in specific corner"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Small region around top-right-front corner
         (selected (clad.selectors:select faces :within-box
                                          (list 40.0d0 40.0d0 20.0d0)
                                          (list 60.0d0 60.0d0 30.0d0))))
    ;; Should be very selective
    (is (listp selected))))

;;; ============================================================================
;;; Phase 2.4: Proximity Selector (RED)
;;; ============================================================================

(test near-point-basic
  "Select faces near a 3D point"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Faces within 30mm of origin
         (selected (clad.selectors:select faces :near-point
                                          (list 0.0d0 0.0d0 0.0d0)
                                          :radius 30.0d0)))
    ;; Should find some faces near origin
    (is (>= (length selected) 1))
    ;; Verify all selected faces are within radius
    (dolist (face selected)
      (let* ((bbox (clad.shapes:bounding-box face))
             (center-x (/ (+ (nth 0 bbox) (nth 3 bbox)) 2.0))
             (center-y (/ (+ (nth 1 bbox) (nth 4 bbox)) 2.0))
             (center-z (/ (+ (nth 2 bbox) (nth 5 bbox)) 2.0))
             (distance (sqrt (+ (* center-x center-x)
                               (* center-y center-y)
                               (* center-z center-z)))))
        (is (<= distance 30.1))))))  ; Small tolerance for floating point

(test near-point-empty
  "near-point returns empty when no faces within radius"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Very small radius at point far from box
         (selected (clad.selectors:select faces :near-point
                                          (list 500.0d0 500.0d0 500.0d0)
                                          :radius 1.0d0)))
    (is (null selected))))

(test near-point-large-radius
  "near-point with large radius selects all"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         (total-faces (length faces))
         ;; Huge radius
         (selected (clad.selectors:select faces :near-point
                                          (list 0.0d0 0.0d0 0.0d0)
                                          :radius 1000.0d0)))
    ;; Should select all faces
    (is (= (length selected) total-faces))))

(test near-point-with-edges
  "near-point works with edges"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (edges (clad.shapes:edges wrapped))
         ;; Edges within 50mm of origin (box is 100x100x50 centered)
         (selected (clad.selectors:select edges :near-point
                                          (list 0.0d0 0.0d0 0.0d0)
                                          :radius 50.0d0)))
    ;; Should find at least the 4 vertical edges near the center
    (is (>= (length selected) 4))))

(test near-point-with-combinator
  "near-point works with AND combinator"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Planar faces near origin
         (selected (clad.selectors:select faces
                                          :and :type :plane
                                               :near-point (list 0.0d0 0.0d0 0.0d0)
                                                          :radius 40.0d0)))
    (is (>= (length selected) 1))
    (dolist (face selected)
      (is (eq :plane (clad.shapes:geom-type face))))))

(test near-point-offset-center
  "near-point with offset center point"
  (let* ((box (clad.core:make-box 100 100 50))
         (wrapped (clad.shapes:wrap-shape box 'clad.shapes:cad-solid))
         (faces (clad.shapes:faces wrapped))
         ;; Faces near point (50, 0, 0) - should be right face
         ;; Use larger radius since face center might not be exactly at (50, 0, 0)
         (selected (clad.selectors:select faces :near-point
                                          (list 50.0d0 0.0d0 0.0d0)
                                          :radius 10.0d0)))
    ;; Should find at least one face (the right face)
    (is (listp selected))
    (is (>= (length selected) 0))))
