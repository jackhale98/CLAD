;;;; tests/ffi-tests.lisp --- FFI layer tests

(in-package :clad.tests)

(in-suite clad-tests)

;;; ============================================================================
;;; Memory Management Tests
;;; ============================================================================

(test test-handle-creation
  "Test OCCT handle creation and validation"
  (let* ((ptr (cffi:make-pointer 12345))
         (handle (clad.ffi:make-occt-handle ptr :type :solid)))
    (is (typep handle 'clad.ffi:occt-handle))
    (is (eq (clad.ffi:handle-type handle) :solid))
    (is (clad.ffi:handle-valid-p handle))
    (is (not (clad.ffi:handle-null-p handle)))))

(test test-handle-reference-counting
  "Test reference counting in stub mode"
  (let ((ptr (cffi:make-pointer 99999)))
    ;; Clear any existing counts
    (clad.ffi:clear-handle-cache)

    ;; Create handle - should increment ref count
    (let ((h1 (clad.ffi:make-occt-handle ptr :type :solid)))
      (is (= (clad.ffi:occt-handle-ref-count ptr) 1))

      ;; Copy handle - should increment ref count
      (let ((h2 (clad.ffi:copy-occt-handle h1)))
        (is (= (clad.ffi:occt-handle-ref-count ptr) 2))
        (is (cffi:pointer-eq (clad.ffi:handle-ptr h1)
                             (clad.ffi:handle-ptr h2)))))))

;;; ============================================================================
;;; Exception Handling Tests
;;; ============================================================================

(test test-error-conditions
  "Test error condition definitions"
  (signals clad.ffi:occt-error
    (error 'clad.ffi:occt-error :message "Test error"))

  (signals clad.ffi:occt-domain-error
    (error 'clad.ffi:occt-domain-error :message "Domain error"))

  (signals clad.ffi:occt-construction-error
    (error 'clad.ffi:occt-construction-error :message "Construction error"))

  (signals clad.ffi:occt-null-object-error
    (error 'clad.ffi:occt-null-object-error :message "Null object error")))

(test test-error-code-conversion
  "Test conversion of error codes to conditions"
  (let ((cond-domain (clad.ffi:error-code-to-condition
                      clad.ffi:+occt-error-domain+ "Test"))
        (cond-construction (clad.ffi:error-code-to-condition
                            clad.ffi:+occt-error-construction+ "Test"))
        (cond-null (clad.ffi:error-code-to-condition
                    clad.ffi:+occt-error-null-object+ "Test")))
    (is (typep cond-domain 'clad.ffi:occt-domain-error))
    (is (typep cond-construction 'clad.ffi:occt-construction-error))
    (is (typep cond-null 'clad.ffi:occt-null-object-error))))

;;; ============================================================================
;;; Primitive FFI Tests (Stub Mode)
;;; ============================================================================

(test test-ffi-make-box
  "Test FFI box creation (stub mode)"
  (let ((handle (clad.ffi:ffi-make-box 100.0 50.0 30.0)))
    (is (typep handle 'clad.ffi:occt-handle))
    (is (eq (clad.ffi:handle-type handle) :solid))
    (is (clad.ffi:handle-valid-p handle))))

(test test-ffi-make-box-invalid
  "Test FFI box creation with invalid dimensions"
  (signals clad.ffi:occt-domain-error
    (clad.ffi:ffi-make-box -10.0 50.0 30.0))

  (signals clad.ffi:occt-domain-error
    (clad.ffi:ffi-make-box 10.0 0.0 30.0)))

(test test-ffi-make-cylinder
  "Test FFI cylinder creation (stub mode)"
  (let ((handle (clad.ffi:ffi-make-cylinder 10.0 50.0)))
    (is (typep handle 'clad.ffi:occt-handle))
    (is (eq (clad.ffi:handle-type handle) :solid))))

(test test-ffi-make-sphere
  "Test FFI sphere creation (stub mode)"
  (let ((handle (clad.ffi:ffi-make-sphere 25.0)))
    (is (typep handle 'clad.ffi:occt-handle))
    (is (eq (clad.ffi:handle-type handle) :solid))))

;;; ============================================================================
;;; Boolean Operation FFI Tests
;;; ============================================================================

(test test-ffi-union
  "Test FFI union operation (stub mode)"
  (let* ((h1 (clad.ffi:ffi-make-box 10.0 10.0 10.0))
         (h2 (clad.ffi:ffi-make-box 10.0 10.0 10.0))
         (result (clad.ffi:ffi-union h1 h2)))
    (is (typep result 'clad.ffi:occt-handle))
    (is (clad.ffi:handle-valid-p result))))

(test test-ffi-cut
  "Test FFI cut operation (stub mode)"
  (let* ((h1 (clad.ffi:ffi-make-box 100.0 100.0 20.0))
         (h2 (clad.ffi:ffi-make-cylinder 10.0 30.0))
         (result (clad.ffi:ffi-cut h1 h2)))
    (is (typep result 'clad.ffi:occt-handle))
    (is (clad.ffi:handle-valid-p result))))

(test test-ffi-intersect
  "Test FFI intersect operation (stub mode)"
  (let* ((h1 (clad.ffi:ffi-make-box 20.0 20.0 20.0))
         (h2 (clad.ffi:ffi-make-sphere 15.0))
         (result (clad.ffi:ffi-intersect h1 h2)))
    (is (typep result 'clad.ffi:occt-handle))
    (is (clad.ffi:handle-valid-p result))))

;;; ============================================================================
;;; Transformation FFI Tests
;;; ============================================================================

(test test-ffi-translate
  "Test FFI translate operation (stub mode)"
  (let* ((h (clad.ffi:ffi-make-box 10.0 10.0 10.0))
         (result (clad.ffi:ffi-translate h 5.0 10.0 15.0)))
    (is (typep result 'clad.ffi:occt-handle))
    (is (clad.ffi:handle-valid-p result))))

(test test-ffi-rotate
  "Test FFI rotate operation (stub mode)"
  (let* ((h (clad.ffi:ffi-make-box 10.0 10.0 10.0))
         (result (clad.ffi:ffi-rotate h 0.0 0.0 1.0 45.0)))
    (is (typep result 'clad.ffi:occt-handle))
    (is (clad.ffi:handle-valid-p result))))

(test test-ffi-mirror
  "Test FFI mirror operation (stub mode)"
  (let* ((h (clad.ffi:ffi-make-box 10.0 10.0 10.0))
         (result (clad.ffi:ffi-mirror h 0.0 0.0 1.0)))
    (is (typep result 'clad.ffi:occt-handle))
    (is (clad.ffi:handle-valid-p result))))

(test test-ffi-scale
  "Test FFI scale operation (stub mode)"
  (let* ((h (clad.ffi:ffi-make-box 10.0 10.0 10.0))
         (result (clad.ffi:ffi-scale h 2.0)))
    (is (typep result 'clad.ffi:occt-handle))
    (is (clad.ffi:handle-valid-p result))))

(test test-ffi-scale-invalid
  "Test FFI scale with invalid factor"
  (let ((h (clad.ffi:ffi-make-box 10.0 10.0 10.0)))
    (signals clad.ffi:occt-domain-error
      (clad.ffi:ffi-scale h -1.0))
    (signals clad.ffi:occt-domain-error
      (clad.ffi:ffi-scale h 0.0))))
