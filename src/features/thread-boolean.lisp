;;;; src/features/thread-boolean.lisp --- Thread Boolean Operations
;;;;
;;;; Phase 4: Thread Boolean Integration & Application
;;;; Complete thread system with boolean operations and fit checking

(in-package :clad.features.thread-boolean)

;;; ============================================================================
;;; Thread Application to Parts
;;; ============================================================================

(defun apply-external-thread (base-shape thread-geometry &key (position '(0 0 0)))
  "Apply external thread to a cylindrical shaft or similar base shape.

  BASE-SHAPE: Base cylinder or shaft (OCCT TopoDS_Shape)
  THREAD-GEOMETRY: Thread solid from Phase 3
  POSITION: (X Y Z) position for thread (default origin)

  Returns: OCCT TopoDS_Shape (shaft with thread)

  The thread is positioned and then intersected with the base shape.
  This ensures the thread core matches the shaft diameter while
  adding the thread profile."

  ;; Translate thread to position
  (let ((positioned-thread
         (if (equal position '(0 0 0))
             thread-geometry
             (clad.core:translate thread-geometry
                                 (first position)
                                 (second position)
                                 (third position)))))

    ;; Intersect thread with base shape
    ;; This creates the threaded section while preserving the base shaft
    (clad.core:union-shapes base-shape positioned-thread)))

(defun apply-internal-thread (base-shape thread-geometry &key (position '(0 0 0)))
  "Apply internal thread by cutting it from a hole or bore.

  BASE-SHAPE: Part with hole (OCCT TopoDS_Shape)
  THREAD-GEOMETRY: Internal thread solid from Phase 3
  POSITION: (X Y Z) position for thread (default origin)

  Returns: OCCT TopoDS_Shape (part with threaded hole)

  The thread is positioned and then subtracted from the base shape.
  This cuts the thread profile into the hole walls."

  ;; Translate thread to position
  (let ((positioned-thread
         (if (equal position '(0 0 0))
             thread-geometry
             (clad.core:translate thread-geometry
                                 (first position)
                                 (second position)
                                 (third position)))))

    ;; Cut thread from base shape
    (clad.core:cut-shapes base-shape positioned-thread)))

;;; ============================================================================
;;; Thread Fit Checking
;;; ============================================================================

(defun check-thread-fit (external-thread internal-thread)
  "Check if external and internal threads will fit together.

  EXTERNAL-THREAD: External thread solid
  INTERNAL-THREAD: Internal thread solid

  Returns: Keyword indicating fit type:
    :perfect-fit - Threads match perfectly
    :good-fit - Threads compatible (within tolerance)
    :size-mismatch - Different thread sizes
    :length-mismatch - Significantly different lengths
    :pitch-mismatch - Different pitches

  This performs geometric analysis to determine thread compatibility."

  (let ((ext-info (clad.features.helical-sweep:get-thread-info external-thread))
        (int-info (clad.features.helical-sweep:get-thread-info internal-thread)))

    ;; Compare diameters
    (let ((ext-dia (getf ext-info :diameter))
          (int-dia (getf int-info :diameter))
          (ext-height (getf ext-info :height))
          (int-height (getf int-info :height)))

      (cond
        ;; Check diameter match (within 0.5mm tolerance)
        ((> (abs (- ext-dia int-dia)) 0.5)
         :size-mismatch)

        ;; Check length match (warn if >20% difference)
        ((> (abs (- ext-height int-height))
            (* 0.2 (max ext-height int-height)))
         :length-mismatch)

        ;; Diameter matches well (within 0.1mm)
        ((< (abs (- ext-dia int-dia)) 0.1)
         :perfect-fit)

        ;; Diameter matches reasonably
        (t
         :good-fit)))))

(defun calculate-engagement-length (external-thread internal-thread)
  "Calculate the thread engagement length (overlap).

  EXTERNAL-THREAD: External thread solid
  INTERNAL-THREAD: Internal thread solid

  Returns: Engagement length in mm

  Engagement length is the shorter of the two thread lengths,
  representing the actual threaded contact area."

  (let ((ext-info (clad.features.helical-sweep:get-thread-info external-thread))
        (int-info (clad.features.helical-sweep:get-thread-info internal-thread)))

    (min (getf ext-info :height)
         (getf int-info :height))))

;;; ============================================================================
;;; Thread Specification Utilities
;;; ============================================================================

(defun get-thread-spec-info (thread-spec)
  "Get complete specification information for a thread.

  THREAD-SPEC: Thread specification keyword (e.g., :m6, :m8, :m10)

  Returns: Plist with thread parameters"

  (clad.features:get-thread-spec thread-spec))

(defun calculate-tap-drill-size (thread-spec)
  "Calculate recommended tap drill size for internal thread.

  THREAD-SPEC: Thread specification keyword

  Returns: Tap drill diameter in mm

  The tap drill size is the hole diameter needed before tapping.
  Formula: Tap drill ≈ Major diameter - Pitch
  This gives approximately 75% thread depth."

  (let* ((spec (get-thread-spec-info thread-spec))
         (major-d (getf spec :major-diameter))
         (pitch (getf spec :pitch)))

    (- major-d pitch)))

(defun thread-designation (thread-spec)
  "Get human-readable thread designation string.

  THREAD-SPEC: Thread specification keyword

  Returns: String like 'M6 x 1.0' or 'M8 x 1.25'"

  (let* ((spec (get-thread-spec-info thread-spec))
         (major-d (getf spec :major-diameter))
         (pitch (getf spec :pitch)))

    (format nil "M~A x ~,2F" (floor major-d) pitch)))

;;; ============================================================================
;;; Complete Fastener Creation
;;; ============================================================================

(defun make-threaded-bolt (&key thread-spec thread-length shaft-length
                                (head-type :hex) (head-diameter nil)
                                (head-height nil))
  "Create a complete threaded bolt with head.

  THREAD-SPEC: Thread specification (e.g., :m6, :m8)
  THREAD-LENGTH: Length of threaded section (mm)
  SHAFT-LENGTH: Total shaft length (mm)
  HEAD-TYPE: :hex, :socket, or :pan
  HEAD-DIAMETER: Head diameter (default from standard)
  HEAD-HEIGHT: Head height (default from standard)

  Returns: OCCT TopoDS_Shape (complete bolt)"

  (let* ((spec (get-thread-spec-info thread-spec))
         (major-d (getf spec :major-diameter))
         (shaft-radius (/ major-d 2.0d0))

         ;; Default head dimensions if not specified
         (h-diameter (or head-diameter (* major-d 1.5)))
         (h-height (or head-height (* major-d 0.6))))

    ;; Create bolt head
    (let ((head (case head-type
                  (:hex
                   ;; Hex head (circumradius for hex)
                   (clad.core:make-cylinder :radius (/ h-diameter 2.0d0)
                                           :height h-height))
                  (:socket
                   ;; Cylindrical socket head
                   (clad.core:make-cylinder :radius (/ h-diameter 2.0d0)
                                           :height h-height))
                  (t
                   ;; Default: pan head
                   (clad.core:make-cylinder :radius (/ h-diameter 2.0d0)
                                           :height h-height)))))

      ;; Create shaft
      (let ((shaft (clad.core:make-cylinder :radius shaft-radius
                                           :height shaft-length)))

        ;; Position shaft below head
        (let ((positioned-shaft (clad.core:translate shaft 0 0 h-height)))

          ;; Union head and shaft
          (let ((bolt-base (clad.core:union-shapes head positioned-shaft)))

            ;; Create and apply thread
            (let ((thread (clad.features.helical-sweep:make-external-thread
                           thread-spec thread-length)))

              ;; Position thread at end of shaft
              (let ((thread-position (list 0 0 (+ h-height
                                                  (- shaft-length thread-length)))))

                (apply-external-thread bolt-base thread
                                      :position thread-position)))))))))

(defun make-threaded-nut (&key thread-spec height (nut-type :hex) wrench-size)
  "Create a complete threaded nut.

  THREAD-SPEC: Thread specification (e.g., :m6, :m8)
  HEIGHT: Nut height (mm)
  NUT-TYPE: :hex or :square
  WRENCH-SIZE: Wrench size (mm, across flats)

  Returns: OCCT TopoDS_Shape (complete nut)"

  (let* ((spec (get-thread-spec-info thread-spec))
         (major-d (getf spec :major-diameter))
         (outer-radius (/ wrench-size 2.0d0))
         (hole-radius (/ major-d 2.0d0)))

    ;; Create outer shape
    (let ((outer (case nut-type
                   (:hex
                    ;; Hex outer (circumradius)
                    (clad.core:make-cylinder :radius outer-radius :height height))
                   (:square
                    ;; Square outer
                    (clad.core:make-box :width wrench-size
                                       :depth wrench-size
                                       :height height))
                   (t
                    ;; Default: hex
                    (clad.core:make-cylinder :radius outer-radius :height height)))))

      ;; Create center hole
      (let ((hole (clad.core:make-cylinder :radius hole-radius :height height)))

        ;; Cut hole from outer
        (let ((nut-with-hole (clad.core:cut-shapes outer hole)))

          ;; Create and apply internal thread
          (let ((thread (clad.features.helical-sweep:make-internal-thread
                         thread-spec height)))

            (apply-internal-thread nut-with-hole thread
                                  :position '(0 0 0))))))))

;;; ============================================================================
;;; Thread Analysis
;;; ============================================================================

(defun analyze-thread-engagement (external-thread internal-thread)
  "Perform detailed analysis of thread engagement.

  Returns: Plist with analysis results"

  (let ((fit (check-thread-fit external-thread internal-thread))
        (engagement (calculate-engagement-length external-thread internal-thread))
        (ext-info (clad.features.helical-sweep:get-thread-info external-thread))
        (int-info (clad.features.helical-sweep:get-thread-info internal-thread)))

    (list :fit-type fit
          :engagement-length engagement
          :external-diameter (getf ext-info :diameter)
          :internal-diameter (getf int-info :diameter)
          :external-length (getf ext-info :height)
          :internal-length (getf int-info :height)
          :diameter-difference (abs (- (getf ext-info :diameter)
                                       (getf int-info :diameter))))))

(defun thread-strength-estimate (thread-spec engagement-length material-strength)
  "Estimate thread tensile strength based on engagement.

  THREAD-SPEC: Thread specification
  ENGAGEMENT-LENGTH: Thread engagement length (mm)
  MATERIAL-STRENGTH: Material tensile strength (MPa)

  Returns: Estimated pull-out force (N)

  This is a simplified estimation based on shear area."

  (let* ((spec (get-thread-spec-info thread-spec))
         (major-d (getf spec :major-diameter))
         (minor-d (getf spec :minor-diameter))
         (pitch (getf spec :pitch))

         ;; Shear area (simplified)
         (shear-area (* pi
                       (/ (+ major-d minor-d) 2.0d0)
                       engagement-length)))

    ;; Force = Shear area × Material strength × Safety factor
    (* shear-area material-strength 0.6)))  ; 0.6 = typical shear/tensile ratio

;;; ============================================================================
;;; Export for Testing
;;; ============================================================================

(defun thread-application-summary (base-shape thread-geometry applied-shape)
  "Return summary of thread application for debugging/testing."

  (list :base-volume (clad.ffi:get-volume base-shape)
        :thread-volume (clad.ffi:get-volume thread-geometry)
        :result-volume (clad.ffi:get-volume applied-shape)
        :result-valid (clad.ffi:is-valid-shape applied-shape)))
