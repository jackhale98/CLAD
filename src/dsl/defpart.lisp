;;;; src/dsl/defpart.lisp --- defpart macro for declarative part definition

(in-package :clad.dsl)

;;; ============================================================================
;;; defpart Macro
;;; ============================================================================

(defmacro defpart (name params &body body)
  "Define a parametric part with declarative syntax.

  Syntax:
    (defpart name ((param1 default1) (param2 default2) ...) [docstring]
      (:body shape-form)
      (:on-face selector-spec
        (:cut|:add shape-form))
      ...)

  Example:
    (defpart bracket ((width 100) (thickness 10))
      \"A simple bracket\"
      (:body
        (make-box width 50 thickness))
      (:on-face :direction :+z :extreme :max
        (:cut (make-cylinder 5 20))))

  The macro expands to a function definition that uses the context API
  to build the part sequentially.

  Auto-rebuild integration:
    If this part is the current part (via SHOW) and *auto-rebuild* is T,
    redefining the part will automatically trigger a rebuild."

  ;; Extract docstring if present
  (let* ((docstring (when (and (stringp (first body)) (rest body))
                      (first body)))
         (forms (if docstring (rest body) body))
         ;; Convert ((param default) ...) to (&optional (param default) ...)
         (lambda-list (cons '&optional
                            (mapcar (lambda (p)
                                      (if (listp p) p (list p)))
                                    params))))

    ;; Validate that we have at least a :body form
    (unless (find-if (lambda (form) (eq (first form) :body)) forms)
      (error "defpart ~A must have at least a :body form" name))

    ;; Generate inline expansion of part forms
    (let ((expansion-code
           `(progn
              ;; Clear sketch registry for this part build
              (clear-sketch-registry)
              ;; Build the part
              (with-context ()
                ,@(mapcar #'expand-part-form-at-compile-time forms)
                (get-result)))))

      ;; Generate the function definition plus auto-rebuild hook
      `(progn
         ;; Define the function
         (defun ,name ,lambda-list
           ,@(when docstring (list docstring))
           ,expansion-code)

         ;; Mark as a part function
         (setf (get ',name 'clad-part-function) t)

         ;; Trigger auto-rebuild if this is the current part
         (when (find-package :clad.auto-rebuild)
           (funcall (intern "MAYBE-REBUILD-CURRENT-PART" :clad.auto-rebuild)
                   ',name))

         ;; Return the function name
         ',name))))

(defun expand-part-form-at-compile-time (form)
  "Expand a part form (:body, :on-face, etc.) at macro expansion time."
  (case (first form)
    (:body
     `(add ,(second form)))

    (:on-face
     ;; Parse selector spec and features
     (let* ((args (rest form))
            (selector-spec '())
            (feature-forms '()))
       ;; Split args into selector-spec (keywords) and feature-forms (lists)
       (dolist (arg args)
         (if (and (not feature-forms) (not (listp arg)))
             (push arg selector-spec)
             (push arg feature-forms)))
       (setf selector-spec (nreverse selector-spec))
       (setf feature-forms (nreverse feature-forms))

       ;; Generate code for face selection and features
       `(progn
          (select-faces ,@selector-spec)
          ,@(mapcar #'expand-feature-form-at-compile-time feature-forms))))

    (:on-edge
     ;; Similar to :on-face but for edges
     (let* ((args (rest form))
            (selector-spec '())
            (feature-forms '()))
       (dolist (arg args)
         (if (and (not feature-forms) (not (listp arg)))
             (push arg selector-spec)
             (push arg feature-forms)))
       (setf selector-spec (nreverse selector-spec))
       (setf feature-forms (nreverse feature-forms))

       `(progn
          (select-edges ,@selector-spec)
          ,@(mapcar #'expand-feature-form-at-compile-time feature-forms))))

    (:mirror
     ;; Mirror the current shape across a plane
     ;; Syntax: (:mirror :plane-origin (x y z) :plane-normal (nx ny nz))
     (let* ((args (rest form))
            (plane-origin nil)
            (plane-normal nil))
       ;; Parse keyword arguments
       (loop for (key value) on args by #'cddr
             do (case key
                  (:plane-origin (setf plane-origin value))
                  (:plane-normal (setf plane-normal value))
                  (t (error "Unknown :mirror parameter: ~S" key))))
       (unless (and plane-origin plane-normal)
         (error ":mirror requires :plane-origin and :plane-normal parameters"))
       `(let* ((current-shape (get-result))
               (core-shape (if (typep current-shape 'clad.shapes:cad-shape)
                               (clad.shapes::core-shape current-shape)
                               current-shape))
               (mirrored (clad.core:mirror-shape core-shape
                                                  ',plane-origin
                                                  ',plane-normal)))
          (add mirrored))))

    ;; Phase 9 Sketch DSL forms
    (:sketch
     (expand-sketch-form-at-compile-time (rest form) nil))

    (:solve-sketch
     (expand-solve-sketch-at-compile-time (rest form)))

    (:extrude-sketch
     (expand-extrude-sketch-at-compile-time (rest form) nil))

    (:cut-extrude-sketch
     (expand-cut-extrude-sketch-at-compile-time (rest form) nil))

    (t
     (error "Unknown part form: ~S" form))))

(defun expand-feature-form-at-compile-time (feature-form)
  "Expand a feature form (:cut, :add, :circular-pattern, :linear-pattern, :grid-pattern, or feature macro) at macro expansion time."
  ;; First try to macroexpand the form in case it's a feature macro (deffeature-defined)
  (let ((expanded-form (macroexpand-1 feature-form)))
    (if (not (eq feature-form expanded-form))
        ;; It was a macro (probably a deffeature), recurse on the expanded form
        (expand-feature-form-at-compile-time expanded-form)
        ;; Not a macro, handle as normal feature form
        (case (first feature-form)
          (:cut
           `(cut-op ,(second feature-form)))
          (:add
           `(union-op ,(second feature-form)))
          (:circular-pattern
           ;; Extract pattern parameters and nested feature
           (expand-circular-pattern-at-compile-time (rest feature-form)))
          (:linear-pattern
           ;; Extract pattern parameters and nested feature
           (expand-linear-pattern-at-compile-time (rest feature-form)))
          (:grid-pattern
           ;; Extract pattern parameters and nested feature
           (expand-grid-pattern-at-compile-time (rest feature-form)))
          (:fillet
           ;; Apply fillet to currently selected edges
           `(fillet-selected ,(second feature-form)))
          (:chamfer
           ;; Apply chamfer to currently selected edges
           `(chamfer-selected ,(second feature-form)))
          (:sweep
           ;; Sweep profile along path - Phase 8
           (expand-sweep-feature-at-compile-time (rest feature-form)))
          (:pipe
           ;; Create pipe along path - Phase 8
           (expand-pipe-feature-at-compile-time (rest feature-form)))
          (:loft
           ;; Create loft through sections - Phase 8
           (expand-loft-feature-at-compile-time (rest feature-form)))
          ;; Phase 9 Sketch DSL (within :on-face context)
          (:sketch
           (expand-sketch-form-at-compile-time (rest feature-form) t))
          (:extrude-sketch
           (expand-extrude-sketch-at-compile-time (rest feature-form) t))
          (:cut-extrude-sketch
           (expand-cut-extrude-sketch-at-compile-time (rest feature-form) t))
          (t
           (error "Unknown feature operation: ~S" (first feature-form)))))))

(defun expand-circular-pattern-at-compile-time (args)
  "Expand a circular pattern at compile time.

  Args: (:count N :radius R :center-x X :center-y Y [:angle-start A1] [:angle-end A2] feature)"

  (let ((count nil)
        (radius nil)
        (center-x 0)
        (center-y 0)
        (angle-start 0)
        (angle-end 360)
        (feature-form nil))

    ;; Parse keyword arguments
    (loop for (key value) on args by #'cddr
          while (keywordp key)
          do (case key
               (:count (setf count value))
               (:radius (setf radius value))
               (:center-x (setf center-x value))
               (:center-y (setf center-y value))
               (:angle-start (setf angle-start value))
               (:angle-end (setf angle-end value))
               (t (error "Unknown circular-pattern parameter: ~S" key))))

    ;; The last element should be the feature
    (setf feature-form (car (last args)))

    (unless (and count radius)
      (error "circular-pattern requires :count and :radius parameters"))

    ;; Generate code that creates the pattern at runtime
    ;; Use a runtime loop instead of compile-time iteration
    `(loop for i from 0 below ,count
           do (let* ((angle-range (- ,angle-end ,angle-start))
                     ;; For full circles (≈360°), divide by count to avoid overlap at 0°/360°
                     ;; For partial arcs, divide by (count-1) to include both endpoints
                     (is-full-circle-p (>= (abs angle-range) 359.99))
                     (angle-step (if (> ,count 1)
                                     (if is-full-circle-p
                                         (/ angle-range ,count)
                                         (/ angle-range (1- ,count)))
                                     0))
                     (angle (+ ,angle-start (* i angle-step)))
                     (angle-rad (* angle (/ pi 180.0)))
                     (x-offset (* ,radius (cos angle-rad)))
                     (y-offset (* ,radius (sin angle-rad)))
                     (total-x (+ ,center-x x-offset))
                     (total-y (+ ,center-y y-offset)))
                ,(expand-pattern-feature-at-compile-time
                  feature-form
                  'total-x 'total-y)))))

(defun expand-linear-pattern-at-compile-time (args)
  "Expand a linear pattern at compile time.

  Args: (:count N :spacing S :direction-x DX :direction-y DY :start-x X :start-y Y feature)"

  (let ((count nil)
        (spacing nil)
        (direction-x 1)
        (direction-y 0)
        (start-x 0)
        (start-y 0)
        (feature-form nil))

    ;; Parse keyword arguments
    (loop for (key value) on args by #'cddr
          while (keywordp key)
          do (case key
               (:count (setf count value))
               (:spacing (setf spacing value))
               (:direction-x (setf direction-x value))
               (:direction-y (setf direction-y value))
               (:start-x (setf start-x value))
               (:start-y (setf start-y value))
               (t (error "Unknown linear-pattern parameter: ~S" key))))

    ;; The last element should be the feature
    (setf feature-form (car (last args)))

    (unless (and count spacing)
      (error "linear-pattern requires :count and :spacing parameters"))

    ;; Generate code that creates the pattern at runtime
    ;; Normalize the direction vector first
    `(let* ((dir-length (sqrt (+ (* ,direction-x ,direction-x)
                                  (* ,direction-y ,direction-y))))
            (norm-dx (/ ,direction-x dir-length))
            (norm-dy (/ ,direction-y dir-length)))
       (loop for i from 0 below ,count
             do (let* ((offset (* i ,spacing))
                       (total-x (+ ,start-x (* offset norm-dx)))
                       (total-y (+ ,start-y (* offset norm-dy))))
                  ,(expand-pattern-feature-at-compile-time
                    feature-form
                    'total-x 'total-y))))))

(defun expand-grid-pattern-at-compile-time (args)
  "Expand a grid pattern at compile time.

  Args: (:count-x NX :count-y NY :spacing-x SX :spacing-y SY :start-x X :start-y Y feature)"

  (let ((count-x nil)
        (count-y nil)
        (spacing-x nil)
        (spacing-y nil)
        (start-x 0)
        (start-y 0)
        (feature-form nil))

    ;; Parse keyword arguments
    (loop for (key value) on args by #'cddr
          while (keywordp key)
          do (case key
               (:count-x (setf count-x value))
               (:count-y (setf count-y value))
               (:spacing-x (setf spacing-x value))
               (:spacing-y (setf spacing-y value))
               (:start-x (setf start-x value))
               (:start-y (setf start-y value))
               (t (error "Unknown grid-pattern parameter: ~S" key))))

    ;; The last element should be the feature
    (setf feature-form (car (last args)))

    (unless (and count-x count-y spacing-x spacing-y)
      (error "grid-pattern requires :count-x, :count-y, :spacing-x, and :spacing-y parameters"))

    ;; Generate code that creates the 2D grid pattern at runtime
    `(loop for ix from 0 below ,count-x
           do (loop for iy from 0 below ,count-y
                    do (let* ((total-x (+ ,start-x (* ix ,spacing-x)))
                              (total-y (+ ,start-y (* iy ,spacing-y))))
                         ,(expand-pattern-feature-at-compile-time
                           feature-form
                           'total-x 'total-y))))))

(defun expand-pattern-feature-at-compile-time (feature-form x-var y-var)
  "Expand a feature form with position translation for patterns."
  ;; First try to macroexpand in case it's a feature macro
  (let ((expanded-form (macroexpand-1 feature-form)))
    (if (not (eq feature-form expanded-form))
        ;; It was a macro, recurse on the expanded form
        (expand-pattern-feature-at-compile-time expanded-form x-var y-var)
        ;; Not a macro, handle as normal
        (let ((operation (first feature-form))
              (shape-form (second feature-form)))
          (case operation
            (:cut
             `(cut-op (clad.core:translate ,shape-form ,x-var ,y-var 0)))
            (:add
             `(union-op (clad.core:translate ,shape-form ,x-var ,y-var 0)))
            (t
             (error "Unknown feature operation in pattern: ~S" operation)))))))

;;; ============================================================================
;;; Part Form Expansion
;;; ============================================================================

(defun expand-part-forms (part-name forms)
  "Expand declarative part forms into executable code using context API.

  This function processes the list of forms (:body, :on-face, etc.) and
  generates code that builds the part using the context API."

  (with-context ()
    ;; Process each form in order
    (dolist (form forms)
      (case (first form)
        (:body
         (expand-body-form (second form)))

        (:on-face
         (expand-on-face-form (rest form)))

        (:on-edge
         (expand-on-edge-form (rest form)))

        (t
         (error "Unknown part form in ~A: ~S" part-name form))))

    ;; Return the final result
    (get-result)))

;;; ============================================================================
;;; Body Form Expansion
;;; ============================================================================

(defun expand-body-form (shape-form)
  "Expand a :body form - adds the initial shape to the context.

  The shape-form should evaluate to a core shape object."

  ;; Evaluate the shape form and add it to context
  (let ((shape (eval shape-form)))
    (add shape)))

;;; ============================================================================
;;; Face Feature Expansion
;;; ============================================================================

(defun expand-on-face-form (args)
  "Expand an :on-face form - selects faces and applies features.

  Args structure: (selector-spec feature-form1 feature-form2 ...)

  Example:
    (:on-face :direction :+z :extreme :max
      (:cut (make-cylinder 5 20))
      (:add (make-box 10 10 5)))"

  ;; Parse selector spec (everything up to the first list)
  (let ((selector-spec '())
        (feature-forms '())
        (parsing-selector t))

    ;; Split args into selector-spec and feature-forms
    (dolist (arg args)
      (if (and parsing-selector (not (listp arg)))
          (push arg selector-spec)
          (progn
            (setf parsing-selector nil)
            (push arg feature-forms))))

    (setf selector-spec (nreverse selector-spec))
    (setf feature-forms (nreverse feature-forms))

    ;; Select faces using the selector spec
    (apply #'select-faces selector-spec)

    ;; Apply each feature to the selected faces
    (dolist (feature-form feature-forms)
      (expand-feature-form feature-form))))

;;; ============================================================================
;;; Edge Feature Expansion
;;; ============================================================================

(defun expand-on-edge-form (args)
  "Expand an :on-edge form - selects edges and applies features.

  Similar to expand-on-face-form but for edges."

  (let ((selector-spec '())
        (feature-forms '())
        (parsing-selector t))

    (dolist (arg args)
      (if (and parsing-selector (not (listp arg)))
          (push arg selector-spec)
          (progn
            (setf parsing-selector nil)
            (push arg feature-forms))))

    (setf selector-spec (nreverse selector-spec))
    (setf feature-forms (nreverse feature-forms))

    ;; Select edges using the selector spec
    (apply #'select-edges selector-spec)

    ;; Apply each feature
    (dolist (feature-form feature-forms)
      (expand-feature-form feature-form))))

;;; ============================================================================
;;; Feature Expansion
;;; ============================================================================

(defun expand-feature-form (feature-form)
  "Expand a feature form like (:cut shape) or (:add shape).

  Feature types:
    :cut  - Cut (subtract) the shape
    :add  - Add (union) the shape"

  (let ((operation (first feature-form))
        (shape-form (second feature-form)))

    (case operation
      (:cut
       (let ((tool-shape (eval shape-form)))
         (cut-op tool-shape)))

      (:add
       (let ((add-shape (eval shape-form)))
         (union-op add-shape)))
      (:fillet
       (let ((radius (eval shape-form)))
         (fillet-selected radius)))
      (:chamfer
       (let ((distance (eval shape-form)))
         (chamfer-selected distance)))

      (t
       (error "Unknown feature operation: ~S" operation)))))

;;; ============================================================================
;;; Phase 8 Advanced Features Support
;;; ============================================================================

(defun expand-sweep-feature-at-compile-time (args)
  "Expand :sweep feature form at compile time.
  Syntax: (:sweep :profile profile-expr :path path-expr)"
  (let ((profile nil)
        (path nil))
    ;; Parse keyword arguments
    (loop for (key value) on args by #'cddr
          do (case key
               (:profile (setf profile value))
               (:path (setf path value))
               (t (error "Unknown :sweep parameter: ~S" key))))
    (unless (and profile path)
      (error ":sweep requires :profile and :path parameters"))
    `(union-op (clad.core:make-sweep ,profile ,path))))

(defun expand-pipe-feature-at-compile-time (args)
  "Expand :pipe feature form at compile time.
  Syntax: (:pipe :path path-expr :radius radius-expr)"
  (let ((path nil)
        (radius nil))
    ;; Parse keyword arguments
    (loop for (key value) on args by #'cddr
          do (case key
               (:path (setf path value))
               (:radius (setf radius value))
               (t (error "Unknown :pipe parameter: ~S" key))))
    (unless (and path radius)
      (error ":pipe requires :path and :radius parameters"))
    `(union-op (clad.core:make-pipe ,path ,radius))))

(defun expand-loft-feature-at-compile-time (args)
  "Expand :loft feature form at compile time.
  Syntax: (:loft :sections (sec1 sec2 ...) [:solid t] [:ruled nil])"
  (let ((sections nil)
        (solid t)
        (ruled nil))
    ;; Parse keyword arguments
    (loop for (key value) on args by #'cddr
          do (case key
               (:sections (setf sections value))
               (:solid (setf solid value))
               (:ruled (setf ruled value))
               (t (error "Unknown :loft parameter: ~S" key))))
    (unless sections
      (error ":loft requires :sections parameter"))
    `(union-op (clad.core:make-loft ,sections :solid ,solid :ruled ,ruled))))

;;; ============================================================================
;;; Phase 9 Sketch DSL Support (Week 11-12)
;;; ============================================================================

;;; Sketch Registry - stores named sketches during part construction
(defparameter *sketch-registry* (make-hash-table :test 'equal)
  "Registry for named sketches defined in DSL")

(defun register-sketch (name sketch plane)
  "Register a sketch and its plane by name in the sketch registry."
  (setf (gethash name *sketch-registry*) (cons sketch plane)))

(defun lookup-sketch (name)
  "Lookup a sketch by name from the sketch registry. Returns (values sketch plane)."
  (let ((entry (gethash name *sketch-registry*)))
    (if entry
        (values (car entry) (cdr entry))
        (values nil nil))))

(defun clear-sketch-registry ()
  "Clear all sketches from the registry (called at start of part build)."
  (clrhash *sketch-registry*))

(defun expand-sketch-entity-at-compile-time (entity-form sketch-var entity-registry-var)
  "Expand a single sketch entity form (like :point, :line, :circle, :constraint).

  Args:
    entity-form - The entity definition (e.g., (:point :name \"P1\" :at (0 0) :fixed t))
    sketch-var - Symbol referring to the sketch object
    entity-registry-var - Symbol referring to the hash table storing named entities

  Returns: Code that creates and adds the entity to the sketch"

  (let ((entity-type (first entity-form))
        (args (rest entity-form)))

    (case entity-type
      (:point
       ;; Parse :point :name "P1" :at (x y) [:fixed t]
       (let ((name nil)
             (at nil)
             (fixed nil))
         (loop for (key value) on args by #'cddr
               do (case key
                    (:name (setf name value))
                    (:at (setf at value))
                    (:fixed (setf fixed value))
                    (t (error "Unknown :point parameter: ~S" key))))
         (unless at
           (error ":point requires :at parameter"))
         (let ((x (first at))
               (y (second at)))
           `(let ((point (clad.sketch:make-point-2d (coerce ,x 'double-float)
                                                      (coerce ,y 'double-float)
                                                      ,@(when name `(:name ,name))
                                                      :fixed ,fixed)))
              (clad.sketch:add-entity ,sketch-var point)
              ,@(when name
                  `((setf (gethash ,name ,entity-registry-var) point)))))))

      (:line
       ;; Parse :line [:name "L1"] :from "P1" :to "P2"
       (let ((name nil)
             (from nil)
             (to nil))
         (loop for (key value) on args by #'cddr
               do (case key
                    (:name (setf name value))
                    (:from (setf from value))
                    (:to (setf to value))
                    (t (error "Unknown :line parameter: ~S" key))))
         (unless (and from to)
           (error ":line requires :from and :to parameters"))
         `(let ((line (clad.sketch:make-line-2d
                        (gethash ,from ,entity-registry-var)
                        (gethash ,to ,entity-registry-var))))
            (clad.sketch:add-entity ,sketch-var line)
            ,@(when name
                `((setf (gethash ,name ,entity-registry-var) line))))))

      (:circle
       ;; Parse :circle [:name "C1"] :center (x y) :radius r
       (let ((name nil)
             (center nil)
             (radius nil))
         (loop for (key value) on args by #'cddr
               do (case key
                    (:name (setf name value))
                    (:center (setf center value))
                    (:radius (setf radius value))
                    (t (error "Unknown :circle parameter: ~S" key))))
         (unless (and center radius)
           (error ":circle requires :center and :radius parameters"))
         (let ((cx (first center))
               (cy (second center)))
           `(let ((center-pt (clad.sketch:make-point-2d (coerce ,cx 'double-float)
                                                          (coerce ,cy 'double-float)))
                  (circle (clad.sketch:make-circle-2d
                            (clad.sketch:make-point-2d (coerce ,cx 'double-float)
                                                         (coerce ,cy 'double-float))
                            (coerce ,radius 'double-float))))
              (clad.sketch:add-entity ,sketch-var circle)
              ,@(when name
                  `((setf (gethash ,name ,entity-registry-var) circle)))))))

      (:constraint
       ;; Parse :constraint :type entity-ref1 [entity-ref2] [value]
       ;; Examples:
       ;;   (:constraint :horizontal "L1")
       ;;   (:constraint :vertical "L2")
       ;;   (:constraint :distance "P1" "P2" 20)
       (let ((constraint-type (first args))
             (remaining-args (rest args)))
         (case constraint-type
           (:horizontal
            (let ((line-name (first remaining-args)))
              `(let ((line (gethash ,line-name ,entity-registry-var)))
                 (clad.sketch:add-constraint ,sketch-var
                   (clad.sketch.constraints:make-horizontal-constraint line)))))
           (:vertical
            (let ((line-name (first remaining-args)))
              `(let ((line (gethash ,line-name ,entity-registry-var)))
                 (clad.sketch:add-constraint ,sketch-var
                   (clad.sketch.constraints:make-vertical-constraint line)))))
           (:distance
            (let ((entity1-name (first remaining-args))
                  (entity2-name (second remaining-args))
                  (distance (third remaining-args)))
              `(let ((e1 (gethash ,entity1-name ,entity-registry-var))
                     (e2 (gethash ,entity2-name ,entity-registry-var)))
                 (clad.sketch:add-constraint ,sketch-var
                   (clad.sketch.constraints:make-distance-constraint
                     e1 e2 (coerce ,distance 'double-float))))))
           (t (error "Unknown constraint type: ~S" constraint-type)))))

      (t (error "Unknown sketch entity type: ~S" entity-type)))))

(defun expand-sketch-form-at-compile-time (args in-on-face-p)
  "Expand a :sketch form at compile time.

  Syntax:
    (:sketch :name \"SketchName\" [:plane :xy|:yz|:xz]
      (:point :name \"P1\" :at (0 0) :fixed t)
      (:line :from \"P1\" :to \"P2\")
      (:circle :center (0 0) :radius 10)
      (:constraint :horizontal \"L1\"))

  When in-on-face-p is true, the sketch plane is created from the selected face.
  Otherwise, the :plane parameter specifies the plane type."

  (let ((name nil)
        (plane-type :xy)
        (entity-forms '()))

    ;; Parse keyword arguments and entity forms
    (loop while args
          for arg = (pop args)
          do (cond
               ((eq arg :name)
                (setf name (pop args)))
               ((eq arg :plane)
                (setf plane-type (pop args)))
               ((listp arg)
                (push arg entity-forms))))

    (setf entity-forms (nreverse entity-forms))

    (unless name
      (error ":sketch requires :name parameter"))

    ;; Generate code to create sketch and entities
    (let ((sketch-var (gensym "SKETCH-"))
          (plane-var (gensym "PLANE-"))
          (entity-registry-var (gensym "ENTITY-REGISTRY-")))

      `(let* ((,entity-registry-var (make-hash-table :test 'equal))
              ,@(if in-on-face-p
                    ;; In :on-face context - create plane from selected face
                    `((,plane-var (let ((selected-faces (clad.context:current-selection)))
                                    (if (null selected-faces)
                                        (error "No face selected for sketch ~A" ,name)
                                        (clad.sketch:make-sketch-plane-from-face (first selected-faces))))))
                    ;; Standalone sketch - use specified plane type
                    `((,plane-var (clad.sketch:make-sketch-plane :type ,plane-type))))
              (,sketch-var (clad.sketch:make-sketch :name ,name)))

         ;; Add all entities
         ,@(mapcar (lambda (entity-form)
                     (expand-sketch-entity-at-compile-time entity-form sketch-var entity-registry-var))
                   entity-forms)

         ;; Register the sketch with its plane
         (register-sketch ,name ,sketch-var ,plane-var)))))

(defun expand-solve-sketch-at-compile-time (args)
  "Expand a :solve-sketch form at compile time.
  Syntax: (:solve-sketch \"SketchName\")"

  (let ((name (first args)))
    (unless name
      (error ":solve-sketch requires a sketch name"))

    `(multiple-value-bind (sketch plane) (lookup-sketch ,name)
       (declare (ignore plane))
       (unless sketch
         (error "Sketch not found: ~A" ,name))
       (clad.sketch.solver:solve-sketch sketch))))

(defun expand-extrude-sketch-at-compile-time (args in-on-face-p)
  "Expand an :extrude-sketch form at compile time.
  Syntax: (:extrude-sketch \"SketchName\" distance)"

  (declare (ignore in-on-face-p))
  (let ((name (first args))
        (distance (second args)))

    (unless (and name distance)
      (error ":extrude-sketch requires sketch name and distance"))

    `(multiple-value-bind (sketch plane) (lookup-sketch ,name)
       (unless sketch
         (error "Sketch not found: ~A" ,name))
       ;; Extrude the sketch and add to current shape
       (union-op (clad.sketch:extrude-sketch sketch ,distance :plane plane)))))

(defun expand-cut-extrude-sketch-at-compile-time (args in-on-face-p)
  "Expand a :cut-extrude-sketch form at compile time.
  Syntax: (:cut-extrude-sketch \"SketchName\" distance)"

  (declare (ignore in-on-face-p))
  (let ((name (first args))
        (distance (second args)))

    (unless (and name distance)
      (error ":cut-extrude-sketch requires sketch name and distance"))

    `(multiple-value-bind (sketch plane) (lookup-sketch ,name)
       (unless sketch
         (error "Sketch not found: ~A" ,name))
       ;; Extrude the sketch and cut from current shape
       (cut-op (clad.sketch:extrude-sketch sketch ,distance :plane plane)))))

