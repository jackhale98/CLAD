;;;; src/dsl/deffeature.lisp --- deffeature macro for reusable features

(in-package :clad.dsl)

;;; ============================================================================
;;; Feature Definition and Registry (TDD Cycles 7-9)
;;; ============================================================================

;; Feature registry - will be used to store reusable features
(defvar *feature-registry* (make-hash-table :test 'eq)
  "Registry of defined features for reuse")

;;; ============================================================================
;;; Feature Registry (TDD Cycle 9)
;;; ============================================================================

(defun register-feature (name feature-function)
  "Register a feature function in the global registry.

  Args:
    name - Symbol name for the feature
    feature-function - Lambda that returns a feature form (e.g., (:cut shape))

  Example:
    (register-feature 'my-hole
      (lambda (&key (diameter 8))
        (list :cut (clad.core:make-cylinder (/ diameter 2) 20))))"
  (setf (gethash name *feature-registry*) feature-function)
  name)

(defun lookup-feature (name)
  "Look up a feature from the global registry.

  Args:
    name - Symbol name of the feature

  Returns:
    The feature function, or NIL if not found"
  (gethash name *feature-registry*))

(defun list-features ()
  "List all registered features.

  Returns:
    List of feature names (symbols)"
  (let ((features '()))
    (maphash (lambda (name fn)
               (declare (ignore fn))
               (push name features))
             *feature-registry*)
    (sort features #'string< :key #'symbol-name)))

(defun clear-features ()
  "Clear all features from the registry.

  Note: This only clears runtime-registered features, not deffeature-defined macros."
  (clrhash *feature-registry*)
  t)

;;; ============================================================================
;;; deffeature Macro (TDD Cycle 7)
;;; ============================================================================

(defun generate-feature-expansion (form param-names)
  "Generate a form-building expression that substitutes parameters at runtime.

  This walks the feature form and creates an expression that, when evaluated in
  the generated macro, produces a properly substituted feature form.

  For example, if form is (:cut (make-cylinder (/ diameter 2) 20))
  and param-names is (diameter), this generates:
  (list ':cut (list 'make-cylinder (list '/ diameter '2) '20))

  When the generated macro is called with diameter=6, this evaluates to:
  (:cut (make-cylinder (/ 6 2) 20))"
  (cond
    ;; If it's a parameter, include it directly for evaluation
    ((member form param-names)
     form)

    ;; If it's an atom (keyword, number, symbol, etc.), quote it
    ((atom form)
     `',form)

    ;; If it's a list, recurse on each element
    ((listp form)
     `(list ,@(mapcar (lambda (x) (generate-feature-expansion x param-names)) form)))))

(defmacro deffeature (name params &body body)
  "Define a reusable feature.

  Syntax:
    (deffeature feature-name ((param1 default1) (param2 default2) ...) [docstring]
      (:cut|:add shape-form))

  Example:
    (deffeature mounting-hole ((diameter 6) (depth 20))
      \"Standard mounting hole\"
      (:cut (make-cylinder (/ diameter 2) depth)))

  The macro generates a new macro that can be called from within defpart.
  Parameters are passed as keyword arguments when using the feature."

  ;; Extract docstring if present
  (let* ((docstring (when (and (stringp (first body)) (rest body))
                      (first body)))
         (forms (if docstring (rest body) body))
         ;; Convert ((param default) ...) to (&key (param default) ...)
         (lambda-list (cons '&key
                           (mapcar (lambda (p)
                                     (if (listp p) p (list p)))
                                   params))))

    ;; Validate that we have exactly one feature form
    (unless (= (length forms) 1)
      (error "deffeature ~A must have exactly one feature form" name))

    (let* ((feature-form (first forms))
           ;; Extract parameter names
           (param-names (mapcar (lambda (p) (if (listp p) (first p) p)) params)))
      ;; Validate that it's a valid feature form
      (unless (member (first feature-form) '(:cut :add))
        (error "deffeature ~A must contain :cut or :add form, got ~S"
               name (first feature-form)))

      ;; Generate a macro that expands to the feature form with parameter substitution
      `(defmacro ,name ,lambda-list
         ,@(when docstring (list docstring))
         ;; Use backquote to create a form with parameter substitution
         ,(generate-feature-expansion feature-form param-names)))))
