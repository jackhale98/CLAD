;;;; src/ffi/exception-handling.lisp --- Exception handling for OCCT calls

(in-package :clad.ffi)

;;; ============================================================================
;;; Condition Definitions
;;; ============================================================================

(define-condition occt-error (error)
  ((message :initarg :message
            :reader occt-error-message
            :documentation "Error message from OCCT")
   (error-type :initarg :error-type
               :reader occt-error-type
               :initform :unknown
               :documentation "Type of OCCT error"))
  (:documentation "Condition signaled when an OCCT operation fails")
  (:report (lambda (condition stream)
             (format stream "OCCT Error (~A): ~A"
                     (occt-error-type condition)
                     (occt-error-message condition)))))

(define-condition occt-domain-error (occt-error)
  ()
  (:documentation "OCCT domain error (invalid parameter value)")
  (:default-initargs :error-type :domain))

(define-condition occt-construction-error (occt-error)
  ()
  (:documentation "OCCT construction error (cannot build shape)")
  (:default-initargs :error-type :construction))

(define-condition occt-null-object-error (occt-error)
  ()
  (:documentation "OCCT null object error (dereferencing null)")
  (:default-initargs :error-type :null-object))

;;; ============================================================================
;;; C++ Exception Bridge
;;; ============================================================================

;;; Since CFFI cannot directly catch C++ exceptions, we need a C wrapper layer
;;; that catches exceptions and returns error codes. This is a common pattern
;;; for C++/C FFI bridges.

;;; Error codes returned by C wrapper functions
(defconstant +occt-success+ 0 "Operation succeeded")
(defconstant +occt-error-unknown+ -1 "Unknown error")
(defconstant +occt-error-domain+ -2 "Domain error (invalid parameter)")
(defconstant +occt-error-construction+ -3 "Construction failed")
(defconstant +occt-error-null-object+ -4 "Null object dereferenced")
(defconstant +occt-error-io+ -5 "I/O error")

(defun error-code-to-condition (error-code message)
  "Convert an error code to the appropriate condition type"
  (case error-code
    (#.+occt-error-domain+
     (make-condition 'occt-domain-error :message message))
    (#.+occt-error-construction+
     (make-condition 'occt-construction-error :message message))
    (#.+occt-error-null-object+
     (make-condition 'occt-null-object-error :message message))
    (otherwise
     (make-condition 'occt-error :message message :error-type :unknown))))

;;; ============================================================================
;;; Error Handling Wrapper
;;; ============================================================================

(defvar *last-error-message* nil
  "Stores the last error message from OCCT C wrapper")

(defvar *last-error-code* 0
  "Stores the last error code from OCCT C wrapper")

(defmacro with-occt-error-handling (&body body)
  "Wrapper for OCCT FFI calls that handles errors.

  The wrapped code should return an error code (0 = success, negative = error).
  On error, signals an appropriate occt-error condition.

  Example:
    (with-occt-error-handling
      (ffi-make-box dx dy dz))"
  (let ((result-code (gensym "RESULT-CODE")))
    `(let ((,result-code (progn ,@body)))
       (if (zerop ,result-code)
           t  ; Success
           (error (error-code-to-condition
                   ,result-code
                   (or *last-error-message*
                       (format nil "OCCT error code: ~D" ,result-code))))))))

(defun check-occt-result (error-code &optional operation-name)
  "Check an OCCT error code and signal condition if necessary"
  (unless (zerop error-code)
    (error (error-code-to-condition
            error-code
            (format nil "~@[~A: ~]~A"
                    operation-name
                    (or *last-error-message*
                        (format nil "Error code ~D" error-code))))))
  t)

;;; ============================================================================
;;; C Wrapper Function Prototypes
;;; ============================================================================

;;; These will be implemented in a C/C++ wrapper library
;;; For now, we provide stubs for testing

(defcfun ("occt_get_last_error" %occt-get-last-error) :string
  "Get the last error message from OCCT wrapper")

(defcfun ("occt_clear_error" %occt-clear-error) :void
  "Clear the last error message")

;;; Stub implementations for when OCCT is not available
(defun occt-get-last-error ()
  "Get the last error message (stub implementation)"
  (or *last-error-message* "No error"))

(defun occt-clear-error ()
  "Clear the last error message (stub implementation)"
  (setf *last-error-message* nil
        *last-error-code* 0))

;;; ============================================================================
;;; Testing Utilities
;;; ============================================================================

(defun simulate-occt-error (error-code message)
  "Simulate an OCCT error for testing purposes"
  (setf *last-error-code* error-code
        *last-error-message* message)
  error-code)

(defun simulate-occt-success ()
  "Simulate a successful OCCT operation for testing"
  (occt-clear-error)
  +occt-success+)
