;;;; src/features/threads.lisp --- Thread modeling and fastener features (TDD GREEN Phase)

(in-package :clad.features)

;;; ============================================================================
;;; Thread Database
;;; ============================================================================

(defparameter *thread-database*
  '(;; ========================================================================
    ;; ISO Metric Coarse Threads (ISO 68-1)
    ;; ========================================================================
    (:m1.6 . (:major-diameter 1.6 :pitch 0.35 :standard "ISO Metric"))
    (:m2 . (:major-diameter 2.0 :pitch 0.4 :standard "ISO Metric"))
    (:m2.5 . (:major-diameter 2.5 :pitch 0.45 :standard "ISO Metric"))
    (:m3 . (:major-diameter 3.0 :pitch 0.5 :standard "ISO Metric"))
    (:m3.5 . (:major-diameter 3.5 :pitch 0.6 :standard "ISO Metric"))
    (:m4 . (:major-diameter 4.0 :pitch 0.7 :standard "ISO Metric"))
    (:m5 . (:major-diameter 5.0 :pitch 0.8 :standard "ISO Metric"))
    (:m6 . (:major-diameter 6.0 :pitch 1.0 :standard "ISO Metric"))
    (:m7 . (:major-diameter 7.0 :pitch 1.0 :standard "ISO Metric"))
    (:m8 . (:major-diameter 8.0 :pitch 1.25 :standard "ISO Metric"))
    (:m10 . (:major-diameter 10.0 :pitch 1.5 :standard "ISO Metric"))
    (:m12 . (:major-diameter 12.0 :pitch 1.75 :standard "ISO Metric"))
    (:m14 . (:major-diameter 14.0 :pitch 2.0 :standard "ISO Metric"))
    (:m16 . (:major-diameter 16.0 :pitch 2.0 :standard "ISO Metric"))
    (:m18 . (:major-diameter 18.0 :pitch 2.5 :standard "ISO Metric"))
    (:m20 . (:major-diameter 20.0 :pitch 2.5 :standard "ISO Metric"))
    (:m22 . (:major-diameter 22.0 :pitch 2.5 :standard "ISO Metric"))
    (:m24 . (:major-diameter 24.0 :pitch 3.0 :standard "ISO Metric"))
    (:m27 . (:major-diameter 27.0 :pitch 3.0 :standard "ISO Metric"))
    (:m30 . (:major-diameter 30.0 :pitch 3.5 :standard "ISO Metric"))
    (:m33 . (:major-diameter 33.0 :pitch 3.5 :standard "ISO Metric"))
    (:m36 . (:major-diameter 36.0 :pitch 4.0 :standard "ISO Metric"))
    (:m39 . (:major-diameter 39.0 :pitch 4.0 :standard "ISO Metric"))
    (:m42 . (:major-diameter 42.0 :pitch 4.5 :standard "ISO Metric"))
    (:m45 . (:major-diameter 45.0 :pitch 4.5 :standard "ISO Metric"))
    (:m48 . (:major-diameter 48.0 :pitch 5.0 :standard "ISO Metric"))
    (:m52 . (:major-diameter 52.0 :pitch 5.0 :standard "ISO Metric"))
    (:m56 . (:major-diameter 56.0 :pitch 5.5 :standard "ISO Metric"))
    (:m60 . (:major-diameter 60.0 :pitch 5.5 :standard "ISO Metric"))
    (:m64 . (:major-diameter 64.0 :pitch 6.0 :standard "ISO Metric"))

    ;; ========================================================================
    ;; ISO Metric Fine Pitch Threads (ISO 68-1)
    ;; ========================================================================
    (:m3x0.35 . (:major-diameter 3.0 :pitch 0.35 :standard "ISO Metric Fine"))
    (:m4x0.5 . (:major-diameter 4.0 :pitch 0.5 :standard "ISO Metric Fine"))
    (:m5x0.5 . (:major-diameter 5.0 :pitch 0.5 :standard "ISO Metric Fine"))
    (:m6x0.75 . (:major-diameter 6.0 :pitch 0.75 :standard "ISO Metric Fine"))
    (:m8x1.0 . (:major-diameter 8.0 :pitch 1.0 :standard "ISO Metric Fine"))
    (:m10x1.25 . (:major-diameter 10.0 :pitch 1.25 :standard "ISO Metric Fine"))
    (:m10x1.0 . (:major-diameter 10.0 :pitch 1.0 :standard "ISO Metric Fine"))
    (:m12x1.5 . (:major-diameter 12.0 :pitch 1.5 :standard "ISO Metric Fine"))
    (:m12x1.25 . (:major-diameter 12.0 :pitch 1.25 :standard "ISO Metric Fine"))
    (:m14x1.5 . (:major-diameter 14.0 :pitch 1.5 :standard "ISO Metric Fine"))
    (:m16x1.5 . (:major-diameter 16.0 :pitch 1.5 :standard "ISO Metric Fine"))
    (:m18x2.0 . (:major-diameter 18.0 :pitch 2.0 :standard "ISO Metric Fine"))
    (:m18x1.5 . (:major-diameter 18.0 :pitch 1.5 :standard "ISO Metric Fine"))
    (:m20x2.0 . (:major-diameter 20.0 :pitch 2.0 :standard "ISO Metric Fine"))
    (:m20x1.5 . (:major-diameter 20.0 :pitch 1.5 :standard "ISO Metric Fine"))
    (:m22x2.0 . (:major-diameter 22.0 :pitch 2.0 :standard "ISO Metric Fine"))
    (:m22x1.5 . (:major-diameter 22.0 :pitch 1.5 :standard "ISO Metric Fine"))
    (:m24x2.0 . (:major-diameter 24.0 :pitch 2.0 :standard "ISO Metric Fine"))
    (:m27x2.0 . (:major-diameter 27.0 :pitch 2.0 :standard "ISO Metric Fine"))
    (:m30x2.0 . (:major-diameter 30.0 :pitch 2.0 :standard "ISO Metric Fine"))

    ;; ========================================================================
    ;; UNC - Unified National Coarse (ANSI/ASME B1.1)
    ;; ========================================================================
    ;; Format: major diameter in inches, TPI (threads per inch)
    ;; Pitch (mm) = 25.4 / TPI
    (:|#0-80| . (:major-diameter 1.524 :pitch 0.3175 :tpi 80 :standard "UNC"))
    (:|#1-64| . (:major-diameter 1.854 :pitch 0.3969 :tpi 64 :standard "UNC"))
    (:|#2-56| . (:major-diameter 2.184 :pitch 0.4536 :tpi 56 :standard "UNC"))
    (:|#3-48| . (:major-diameter 2.515 :pitch 0.5292 :tpi 48 :standard "UNC"))
    (:|#4-40| . (:major-diameter 2.845 :pitch 0.635 :tpi 40 :standard "UNC"))
    (:|#5-40| . (:major-diameter 3.175 :pitch 0.635 :tpi 40 :standard "UNC"))
    (:|#6-32| . (:major-diameter 3.505 :pitch 0.7938 :tpi 32 :standard "UNC"))
    (:|#8-32| . (:major-diameter 4.166 :pitch 0.7938 :tpi 32 :standard "UNC"))
    (:|#10-24| . (:major-diameter 4.826 :pitch 1.0583 :tpi 24 :standard "UNC"))
    (:|#12-24| . (:major-diameter 5.486 :pitch 1.0583 :tpi 24 :standard "UNC"))
    (:|1/4-20| . (:major-diameter 6.35 :pitch 1.27 :tpi 20 :standard "UNC"))
    (:|5/16-18| . (:major-diameter 7.9375 :pitch 1.4111 :tpi 18 :standard "UNC"))
    (:|3/8-16| . (:major-diameter 9.525 :pitch 1.5875 :tpi 16 :standard "UNC"))
    (:|7/16-14| . (:major-diameter 11.1125 :pitch 1.8143 :tpi 14 :standard "UNC"))
    (:|1/2-13| . (:major-diameter 12.7 :pitch 1.9538 :tpi 13 :standard "UNC"))
    (:|9/16-12| . (:major-diameter 14.2875 :pitch 2.1167 :tpi 12 :standard "UNC"))
    (:|5/8-11| . (:major-diameter 15.875 :pitch 2.3091 :tpi 11 :standard "UNC"))
    (:|3/4-10| . (:major-diameter 19.05 :pitch 2.54 :tpi 10 :standard "UNC"))
    (:|7/8-9| . (:major-diameter 22.225 :pitch 2.8222 :tpi 9 :standard "UNC"))
    (:|1-8| . (:major-diameter 25.4 :pitch 3.175 :tpi 8 :standard "UNC"))
    (:|1-1/8-7| . (:major-diameter 28.575 :pitch 3.6286 :tpi 7 :standard "UNC"))
    (:|1-1/4-7| . (:major-diameter 31.75 :pitch 3.6286 :tpi 7 :standard "UNC"))
    (:|1-3/8-6| . (:major-diameter 34.925 :pitch 4.2333 :tpi 6 :standard "UNC"))
    (:|1-1/2-6| . (:major-diameter 38.1 :pitch 4.2333 :tpi 6 :standard "UNC"))
    (:|1-3/4-5| . (:major-diameter 44.45 :pitch 5.08 :tpi 5 :standard "UNC"))
    (:|2-4.5| . (:major-diameter 50.8 :pitch 5.6444 :tpi 4.5 :standard "UNC"))

    ;; ========================================================================
    ;; UNF - Unified National Fine (ANSI/ASME B1.1)
    ;; ========================================================================
    (:|#0-80| . (:major-diameter 1.524 :pitch 0.3175 :tpi 80 :standard "UNF"))
    (:|#1-72| . (:major-diameter 1.854 :pitch 0.3528 :tpi 72 :standard "UNF"))
    (:|#2-64| . (:major-diameter 2.184 :pitch 0.3969 :tpi 64 :standard "UNF"))
    (:|#3-56| . (:major-diameter 2.515 :pitch 0.4536 :tpi 56 :standard "UNF"))
    (:|#4-48| . (:major-diameter 2.845 :pitch 0.5292 :tpi 48 :standard "UNF"))
    (:|#5-44| . (:major-diameter 3.175 :pitch 0.5773 :tpi 44 :standard "UNF"))
    (:|#6-40| . (:major-diameter 3.505 :pitch 0.635 :tpi 40 :standard "UNF"))
    (:|#8-36| . (:major-diameter 4.166 :pitch 0.7056 :tpi 36 :standard "UNF"))
    (:|#10-32| . (:major-diameter 4.826 :pitch 0.7938 :tpi 32 :standard "UNF"))
    (:|#12-28| . (:major-diameter 5.486 :pitch 0.9071 :tpi 28 :standard "UNF"))
    (:|1/4-28| . (:major-diameter 6.35 :pitch 0.9071 :tpi 28 :standard "UNF"))
    (:|5/16-24| . (:major-diameter 7.9375 :pitch 1.0583 :tpi 24 :standard "UNF"))
    (:|3/8-24| . (:major-diameter 9.525 :pitch 1.0583 :tpi 24 :standard "UNF"))
    (:|7/16-20| . (:major-diameter 11.1125 :pitch 1.27 :tpi 20 :standard "UNF"))
    (:|1/2-20| . (:major-diameter 12.7 :pitch 1.27 :tpi 20 :standard "UNF"))
    (:|9/16-18| . (:major-diameter 14.2875 :pitch 1.4111 :tpi 18 :standard "UNF"))
    (:|5/8-18| . (:major-diameter 15.875 :pitch 1.4111 :tpi 18 :standard "UNF"))
    (:|3/4-16| . (:major-diameter 19.05 :pitch 1.5875 :tpi 16 :standard "UNF"))
    (:|7/8-14| . (:major-diameter 22.225 :pitch 1.8143 :tpi 14 :standard "UNF"))
    (:|1-12| . (:major-diameter 25.4 :pitch 2.1167 :tpi 12 :standard "UNF"))
    (:|1-1/8-12| . (:major-diameter 28.575 :pitch 2.1167 :tpi 12 :standard "UNF"))
    (:|1-1/4-12| . (:major-diameter 31.75 :pitch 2.1167 :tpi 12 :standard "UNF"))
    (:|1-3/8-12| . (:major-diameter 34.925 :pitch 2.1167 :tpi 12 :standard "UNF"))
    (:|1-1/2-12| . (:major-diameter 38.1 :pitch 2.1167 :tpi 12 :standard "UNF")))
  "Comprehensive thread specification database with ISO Metric, UNC, and UNF standards")

(defun get-thread-spec (designation)
  "Get thread specification from database.

  Returns a plist with:
    :major-diameter - Outer diameter (mm)
    :pitch - Thread pitch (mm)
    :pitch-diameter - Effective diameter at thread flanks (mm)
    :minor-diameter - Inner diameter at thread root (mm)
    :standard - Thread standard name
    :tpi - Threads per inch (UNC/UNF only)

  The pitch-diameter and minor-diameter are computed using ISO 68-1 formulas:
    H = P × √3/2 (fundamental triangle height)
    pitch-diameter = major-diameter - 0.6495 × P
    minor-diameter = major-diameter - 1.0825 × P"
  (let ((raw-spec (cdr (assoc designation *thread-database*))))
    (unless raw-spec
      (error "Unknown thread specification: ~S" designation))
    ;; Compute derived values using ISO 68-1 formulas
    (let* ((major-d (getf raw-spec :major-diameter))
           (pitch (getf raw-spec :pitch))
           ;; ISO 68-1: pitch diameter = D - 0.6495P (where 0.6495 ≈ 3H/8, H = P×√3/2)
           (pitch-d (- major-d (* 0.6495d0 pitch)))
           ;; ISO 68-1: minor diameter = D - 1.0825P (where 1.0825 ≈ 5H/8, H = P×√3/2)
           (minor-d (- major-d (* 1.0825d0 pitch))))
      ;; Return complete spec with computed values
      (list :major-diameter major-d
            :pitch pitch
            :pitch-diameter pitch-d
            :minor-diameter minor-d
            :standard (getf raw-spec :standard)
            :tpi (getf raw-spec :tpi)))))

(defun list-thread-specs ()
  "List all available thread specifications"
  (mapcar #'car *thread-database*))

(defun list-threads-by-standard (standard)
  "List all threads of a specific standard.

  STANDARD: String to match (e.g., \"ISO Metric\", \"UNC\", \"UNF\")

  Returns: List of thread designation keywords"
  (loop for (designation . spec) in *thread-database*
        when (search standard (getf spec :standard))
        collect designation))

(defun thread-designation-string (designation)
  "Get human-readable thread designation string.

  Args:
    designation - Thread designation keyword

  Returns: String like 'M6 x 1.0' or '1/4-20 UNC'"
  (let ((spec (get-thread-spec designation)))
    (cond
      ;; ISO Metric threads
      ((search "ISO" (getf spec :standard))
       (format nil "M~,1F x ~,2F"
               (getf spec :major-diameter)
               (getf spec :pitch)))

      ;; UNC/UNF threads
      ((or (search "UNC" (getf spec :standard))
           (search "UNF" (getf spec :standard)))
       (format nil "~A (~A TPI)"
               (string-downcase (symbol-name designation))
               (getf spec :tpi)))

      ;; Default
      (t (format nil "~A" designation)))))

(defun print-thread-database (&optional (standard nil))
  "Print thread database in a formatted table.

  STANDARD: Optional standard filter (\"ISO Metric\", \"UNC\", \"UNF\")"
  (let ((threads (if standard
                     (list-threads-by-standard standard)
                     (list-thread-specs))))

    (format t "~%Thread Specifications~A:~%"
            (if standard (format nil " (~A)" standard) ""))
    (format t "~A~%" (make-string 80 :initial-element #\=))
    (format t "~25A ~12A ~12A ~20A~%"
            "Designation" "Major Ø" "Pitch" "Standard")
    (format t "~A~%" (make-string 80 :initial-element #\-))

    (dolist (designation threads)
      (let ((spec (get-thread-spec designation)))
        (format t "~25A ~12,3F ~12,4F ~20A~%"
                (thread-designation-string designation)
                (getf spec :major-diameter)
                (getf spec :pitch)
                (getf spec :standard))))

    (format t "~A~%" (make-string 80 :initial-element #\=))
    (format t "Total: ~A thread specifications~%~%" (length threads))))

(defun define-thread-spec (designation major-diameter pitch &key (standard "Custom"))
  "Define a custom thread specification"
  (push (cons designation (list :major-diameter major-diameter
                                :pitch pitch
                                :standard standard))
        *thread-database*))

;;; ============================================================================
;;; Helix Creation
;;; ============================================================================

(defun make-helix (&key radius pitch height (right-hand t))
  "Create a helical curve.

  Args:
    radius - Radius of the helix
    pitch - Vertical distance per revolution
    height - Total height of the helix
    right-hand - T for right-hand helix, NIL for left-hand (default: T)

  Returns: Edge shape representing the helix"

  (unless (plusp radius)
    (error "Helix radius must be positive, got ~A" radius))
  (unless (plusp pitch)
    (error "Helix pitch must be positive, got ~A" pitch))
  (unless (plusp height)
    (error "Helix height must be positive, got ~A" height))

  ;; Calculate number of revolutions
  (let* ((revolutions (/ height pitch))
         ;; Use more points for smoother helix (at least 20 points per revolution)
         (points-per-rev 20)
         (num-points (ceiling (* revolutions points-per-rev)))
         (points nil))

    ;; Generate helix points parametrically
    ;; x(t) = radius * cos(t)
    ;; y(t) = radius * sin(t)
    ;; z(t) = (pitch / 2π) * t
    (dotimes (i (1+ num-points))
      (let* ((t-param (* 2 pi revolutions (/ i num-points)))
             ;; Flip direction for left-hand helix
             (angle (if right-hand t-param (- t-param)))
             (x (* radius (cos angle)))
             (y (* radius (sin angle)))
             (z (* height (/ i num-points))))
        (push (list x y z) points)))

    ;; Reverse to get correct order
    (setf points (nreverse points))

    ;; Create spline through helix points
    (let ((helix-curve (clad.core:make-spline points :closed nil)))
      ;; Add metadata
      (clad.core:make-shape (clad.core:shape-handle helix-curve)
                             :metadata (list :type :helix
                                           :radius radius
                                           :pitch pitch
                                           :height height
                                           :right-hand right-hand)))))

;;; ============================================================================
;;; Thread Profile Creation
;;; ============================================================================

(defun make-thread-profile (&key type pitch)
  "Create thread profile shape.

  Args:
    type - Thread type (:iso-metric, :unified)
    pitch - Thread pitch

  Returns: Wire shape representing thread profile"

  (unless (member type '(:iso-metric :unified))
    (error "Thread type must be :iso-metric or :unified, got ~A" type))
  (unless (plusp pitch)
    (error "Thread pitch must be positive, got ~A" pitch))

  ;; Both ISO metric and unified use 60° thread angle
  ;; Thread height H = (√3/2) * pitch ≈ 0.866025 * pitch
  (let* ((thread-angle 60)
         (h (* 0.866025404 pitch))  ; Fundamental triangle height
         ;; Profile dimensions (simplified for now - not full detailed profile)
         (half-pitch (/ pitch 2.0))
         ;; Create triangular profile points
         ;; Profile is in XZ plane, will be swept around helix
         (profile-points
           (list
            (list 0 0 0)                    ; Bottom center
            (list half-pitch 0 h)           ; Top right
            (list (- half-pitch) 0 h)       ; Top left
            (list 0 0 0))))                 ; Back to start (closed)

    ;; Create wire from profile points
    (let ((profile-wire (clad.core:make-spline profile-points :closed t)))
      ;; Add metadata
      (clad.core:make-shape (clad.core:shape-handle profile-wire)
                             :metadata (list :type :thread-profile
                                           :thread-type type
                                           :pitch pitch
                                           :thread-angle thread-angle)))))

;;; ============================================================================
;;; Thread Creation
;;; ============================================================================
;;;
;;; These functions delegate to the Phase 1-4 implementation in:
;;;   - clad.features.thread-profile (Profile geometry)
;;;   - clad.features.helical-path (Helical path generation)
;;;   - clad.features.helical-sweep (3D thread geometry)
;;;
;;; For cosmetic threads (faster, for visualization), use :cosmetic t

(defun make-external-thread (designation &key length (cosmetic nil) (right-handed t))
  "Create external thread geometry.

  Args:
    designation - Thread designation keyword (e.g., :m6, :m8, :|1/4-20|)
    length - Thread length (mm)
    cosmetic - T for cosmetic cylinder, NIL for full 3D helical geometry (default: NIL)
    right-handed - T for right-handed thread, NIL for left-handed (default: T)

  Returns: Solid shape representing external thread

  For full 3D helical geometry, this delegates to Phase 1-4 implementation.
  For cosmetic representation, a simple cylinder is returned."

  (unless (plusp length)
    (error "Thread length must be positive, got ~A" length))

  (let* ((spec (get-thread-spec designation))
         (major-dia (getf spec :major-diameter))
         (pitch (getf spec :pitch)))

    (if cosmetic
        ;; Cosmetic: simple cylinder with metadata
        (let ((cylinder (clad.core:make-cylinder (/ major-dia 2.0) length)))
          (clad.core:make-shape (clad.core:shape-handle cylinder)
                                :metadata (list :type :external-thread
                                              :thread-spec designation
                                              :length length
                                              :turns (floor (/ length pitch))
                                              :representation :cosmetic)))
        ;; Full 3D: delegate to Phase 3 helical sweep
        (clad.features.helical-sweep:make-external-thread
         designation length :right-handed right-handed))))

(defun make-internal-thread (designation &key depth (cosmetic nil) (right-handed t))
  "Create internal thread geometry (for threaded holes).

  Args:
    designation - Thread designation keyword (e.g., :m6, :m8)
    depth - Thread depth (mm)
    cosmetic - T for cosmetic cylinder, NIL for full 3D helical geometry (default: NIL)
    right-handed - T for right-handed thread, NIL for left-handed (default: T)

  Returns: Solid shape for cutting internal thread

  For full 3D helical geometry, this delegates to Phase 1-4 implementation.
  For cosmetic representation, a simple cylinder is returned."

  (unless (plusp depth)
    (error "Thread depth must be positive, got ~A" depth))

  (let* ((spec (get-thread-spec designation))
         (major-dia (getf spec :major-diameter))
         (pitch (getf spec :pitch)))

    (if cosmetic
        ;; Cosmetic: simple cylinder with metadata
        (let ((cylinder (clad.core:make-cylinder (/ major-dia 2.0) depth)))
          (clad.core:make-shape (clad.core:shape-handle cylinder)
                                :metadata (list :type :internal-thread
                                              :thread-spec designation
                                              :depth depth
                                              :turns (floor (/ depth pitch))
                                              :representation :cosmetic)))
        ;; Full 3D: delegate to Phase 3 helical sweep
        (clad.features.helical-sweep:make-internal-thread
         designation depth :right-handed right-handed))))

;;; ============================================================================
;;; Thread Operations
;;; ============================================================================

(defun add-external-thread (cylinder designation)
  "Add external thread to existing cylinder.

  Args:
    cylinder - Cylindrical shape
    designation - Thread designation keyword

  Returns: Shape with thread added"

  (clad.core:ensure-shape cylinder)

  (let* ((spec (get-thread-spec designation))
         (bbox (clad.shapes:bounding-box cylinder))
         ;; Estimate cylinder height from bounding box
         (height (- (nth 5 bbox) (nth 2 bbox)))  ; zmax - zmin
         ;; Create thread of same length
         (thread (make-external-thread designation :length height :cosmetic nil))
         ;; Union cylinder with thread
         (result (clad.core:union-shapes cylinder thread)))
    result))

(defun cut-internal-thread (shape designation x y z)
  "Cut internal thread into shape.

  Args:
    shape - Shape to cut thread into
    designation - Thread designation keyword
    x, y, z - Thread position

  Returns: Shape with internal thread cut"

  (clad.core:ensure-shape shape)

  (let* ((spec (get-thread-spec designation))
         ;; Estimate depth (use a reasonable default)
         (depth 20)
         ;; Create internal thread geometry
         (thread (make-internal-thread designation :depth depth :cosmetic nil))
         ;; Translate to position
         (positioned-thread (clad.core:translate thread x y z))
         ;; Cut thread from shape
         (result (clad.core:cut-shapes shape positioned-thread)))
    result))

;;; ============================================================================
;;; Thread Calculations
;;; ============================================================================

(defun thread-major-diameter (designation)
  "Get major (outer) diameter for thread.

  Args:
    designation - Thread designation keyword

  Returns: Major diameter in mm"
  (getf (get-thread-spec designation) :major-diameter))

(defun thread-pitch-diameter (designation)
  "Get pitch diameter for thread.

  Args:
    designation - Thread designation keyword

  Returns: Pitch diameter in mm (computed using ISO 68-1 formula)

  The pitch diameter is the effective diameter at the thread flanks,
  used for thread engagement and helix generation."
  (getf (get-thread-spec designation) :pitch-diameter))

(defun thread-minor-diameter (designation)
  "Get minor diameter for thread.

  Args:
    designation - Thread designation keyword

  Returns: Minor diameter in mm (computed using ISO 68-1 formula)"
  (getf (get-thread-spec designation) :minor-diameter))

(defun tap-drill-size (designation)
  "Calculate recommended tap drill size.

  Args:
    designation - Thread designation keyword

  Returns: Drill diameter in mm"

  (let* ((spec (get-thread-spec designation))
         (major-dia (getf spec :major-diameter))
         (pitch (getf spec :pitch))
         ;; Tap drill is typically minor diameter + ~10% of thread depth
         ;; For 75% thread engagement (common standard)
         ;; Tap drill ≈ major_diameter - pitch
         (tap-drill (- major-dia pitch)))
    tap-drill))
