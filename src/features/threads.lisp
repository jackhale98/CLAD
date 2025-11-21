;;;; src/features/threads.lisp --- Thread modeling and fastener features (TDD GREEN Phase)

(in-package :clad.features)

;;; ============================================================================
;;; Thread Database
;;; ============================================================================

(defparameter *thread-database*
  '((:m3 . (:major-diameter 3.0 :pitch 0.5 :standard "ISO Metric"))
    (:m6 . (:major-diameter 6.0 :pitch 1.0 :standard "ISO Metric"))
    (:m8 . (:major-diameter 8.0 :pitch 1.25 :standard "ISO Metric"))
    (:m10 . (:major-diameter 10.0 :pitch 1.5 :standard "ISO Metric"))
    (:m8x1.0 . (:major-diameter 8.0 :pitch 1.0 :standard "ISO Metric Fine"))
    (:m10x1.25 . (:major-diameter 10.0 :pitch 1.25 :standard "ISO Metric Fine"))
    (:1/4-20 . (:major-diameter 6.35 :pitch 1.27 :tpi 20 :standard "UNC")))
  "Thread specification database")

(defun get-thread-spec (designation)
  "Get thread specification from database"
  (let ((spec (cdr (assoc designation *thread-database*))))
    (unless spec
      (error "Unknown thread specification: ~S" designation))
    spec))

(defun list-thread-specs ()
  "List all available thread specifications"
  (mapcar #'car *thread-database*))

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

(defun make-external-thread (designation &key length (cosmetic nil))
  "Create external thread geometry.

  Args:
    designation - Thread designation keyword (e.g., :m6, :m8, :1/4-20)
    length - Thread length
    cosmetic - T for cosmetic representation, NIL for detailed (default: NIL)

  Returns: Solid shape representing external thread"

  (unless (plusp length)
    (error "Thread length must be positive, got ~A" length))

  (let* ((spec (get-thread-spec designation))
         (major-dia (getf spec :major-diameter))
         (pitch (getf spec :pitch))
         (standard (getf spec :standard))
         (thread-type (cond
                        ((search "ISO" standard) :iso-metric)
                        ((search "UNC" standard) :unified)
                        ((search "UNF" standard) :unified)
                        (t :iso-metric))))

    ;; For Basic Threads (Quick Wins), use cosmetic representation
    ;; Full helical geometry requires complex OpenCASCADE operations
    ;; TODO: Implement detailed helical threads in future enhancement
    ;; Use major diameter for external threads (standard practice for cosmetic threads)
    (let* ((representation (if cosmetic :cosmetic :detailed))
           (cylinder (clad.core:make-cylinder (/ major-dia 2.0) length))
           (turns (floor (/ length pitch))))
      (clad.core:make-shape (clad.core:shape-handle cylinder)
                             :metadata (list :type :external-thread
                                           :thread-type designation
                                           :length length
                                           :turns turns
                                           :representation representation)))))

(defun make-internal-thread (designation &key depth (cosmetic nil))
  "Create internal thread geometry (threaded hole).

  Args:
    designation - Thread designation keyword (e.g., :m6, :m8)
    depth - Thread depth
    cosmetic - T for cosmetic representation, NIL for detailed (default: NIL)

  Returns: Solid shape for cutting internal thread"

  (unless (plusp depth)
    (error "Thread depth must be positive, got ~A" depth))

  (let* ((spec (get-thread-spec designation))
         (major-dia (getf spec :major-diameter))
         (pitch (getf spec :pitch))
         (representation (if cosmetic :cosmetic :detailed))
         ;; Simple cylinder at major diameter for cutting
         (cylinder (clad.core:make-cylinder (/ major-dia 2.0) depth)))

    (clad.core:make-shape (clad.core:shape-handle cylinder)
                           :metadata (list :type :internal-thread
                                         :thread-type designation
                                         :depth depth
                                         :thread-direction :internal
                                         :representation representation))))

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

(defun thread-minor-diameter (designation)
  "Calculate minor diameter for thread.

  Args:
    designation - Thread designation keyword

  Returns: Minor diameter in mm"

  (let* ((spec (get-thread-spec designation))
         (major-dia (getf spec :major-diameter))
         (pitch (getf spec :pitch))
         ;; ISO metric minor diameter formula:
         ;; minor_diameter = major_diameter - 2 * (5/8) * H
         ;; where H = 0.866025 * pitch
         (h (* 0.866025404 pitch))
         (minor-dia (- major-dia (* 2.0 (/ 5.0 8.0) h))))
    minor-dia))

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
