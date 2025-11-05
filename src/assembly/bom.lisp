;;;; bom.lisp --- Bill of materials generation

(in-package #:clad.assembly.bom)

;;; ==============================================================================
;;; BOM Entry Class (Week 15-16)
;;; ==============================================================================

(defclass bom-entry ()
  ((name :initarg :name
         :accessor bom-entry-name
         :documentation "Component/part name")
   (part :initarg :part
         :accessor bom-entry-part
         :documentation "Part shape or sub-assembly")
   (quantity :initarg :quantity
             :initform 1
             :accessor bom-entry-quantity
             :documentation "Total quantity in assembly")
   (metadata :initarg :metadata
             :initform '()
             :accessor bom-entry-metadata
             :documentation "Part metadata (part number, material, etc.)"))
  (:documentation "Entry in a bill of materials"))

;;; ==============================================================================
;;; BOM Generation (Week 15-16)
;;; ==============================================================================

(defun generate-bom (assembly &key (flatten t) (format :list))
  "Generate bill of materials from an assembly.

  Args:
    assembly - Assembly to generate BOM from
    :flatten - Flatten nested assemblies (default t)
    :format - Output format :list or :tree (default :list)

  Returns: List of bom-entry objects

  When flatten is t, traverses nested assemblies and accumulates quantities.
  When flatten is nil, treats sub-assemblies as single items.

  Example:
    (generate-bom asm :flatten t :format :list)"
  (let ((entries (make-hash-table :test 'equal)))

    ;; Helper to add/update entry
    (labels ((add-entry (name part qty metadata)
               (let* ((key (prin1-to-string name))
                      (existing (gethash key entries)))
                 (if existing
                     ;; Update existing entry quantity
                     (incf (bom-entry-quantity existing) qty)
                     ;; Create new entry
                     (setf (gethash key entries)
                           (make-instance 'bom-entry
                                          :name name
                                          :part part
                                          :quantity qty
                                          :metadata metadata)))))

             (process-component (comp parent-qty)
               (let ((name (clad.assembly:component-name comp))
                     (part (clad.assembly:component-part comp))
                     (qty (* (clad.assembly:component-quantity comp) parent-qty))
                     (metadata (clad.assembly:component-metadata comp)))

                 ;; Check if part is a sub-assembly
                 (if (and flatten (typep part 'clad.assembly:assembly))
                     ;; Recursively process sub-assembly components
                     (dolist (sub-comp (clad.assembly:list-components part))
                       (process-component sub-comp qty))
                     ;; Add this component to BOM
                     (add-entry name part qty metadata)))))

      ;; Process all top-level components
      (dolist (comp (clad.assembly:list-components assembly))
        (process-component comp 1)))

    ;; Convert hash table to list
    (let ((result '()))
      (maphash (lambda (key entry)
                 (declare (ignore key))
                 (push entry result))
               entries)

      ;; Sort by name for consistent output
      (sort result #'string< :key (lambda (e) (prin1-to-string (bom-entry-name e)))))))

;;; ==============================================================================
;;; BOM Export (Week 15-16)
;;; ==============================================================================

(defun export-bom-csv (bom filename)
  "Export BOM to CSV file.

  Args:
    bom - List of bom-entry objects
    filename - Output file path

  Returns: Filename

  CSV format:
    Name,Quantity,Part Number,Material,Vendor,Notes

  Example:
    (export-bom-csv (generate-bom asm) \"/tmp/assembly-bom.csv\")"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    ;; Write header
    (format out "Name,Quantity,Part Number,Material,Vendor,Notes~%")

    ;; Write entries
    (dolist (entry bom)
      (let ((name (bom-entry-name entry))
            (qty (bom-entry-quantity entry))
            (meta (bom-entry-metadata entry)))
        (format out "~A,~A,~A,~A,~A,~A~%"
                name
                qty
                (or (getf meta :part-number) "")
                (or (getf meta :material) "")
                (or (getf meta :vendor) "")
                (or (getf meta :notes) "")))))

  filename)

(defun export-bom-json (bom filename)
  "Export BOM to JSON file.

  Args:
    bom - List of bom-entry objects
    filename - Output file path

  Returns: Filename

  JSON format:
    [{\"name\": \"...\", \"quantity\": 1, \"metadata\": {...}}, ...]

  Example:
    (export-bom-json (generate-bom asm) \"/tmp/assembly-bom.json\")"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "[~%")

    (loop for entry in bom
          for first = t then nil
          do (progn
               (unless first (format out ",~%"))
               (let ((name (bom-entry-name entry))
                     (qty (bom-entry-quantity entry))
                     (meta (bom-entry-metadata entry)))
                 (format out "  {\"name\": ~S, \"quantity\": ~A"
                         (prin1-to-string name) qty)

                 ;; Add metadata fields
                 (when meta
                   (format out ", \"metadata\": {")
                   (loop for (key val) on meta by #'cddr
                         for meta-first = t then nil
                         do (progn
                              (unless meta-first (format out ", "))
                              (format out "~S: ~S"
                                      (string-downcase (symbol-name key))
                                      (if (stringp val) val (prin1-to-string val)))))
                   (format out "}"))

                 (format out "}"))))

    (format out "~%]~%"))

  filename)

;;; ==============================================================================
;;; BOM Utilities
;;; ==============================================================================

(defun print-bom (bom &optional (stream t))
  "Print BOM to stream in human-readable format.

  Args:
    bom - List of bom-entry objects
    stream - Output stream (default t for *standard-output*)

  Example:
    (print-bom (generate-bom asm))"
  (format stream "~%Bill of Materials~%")
  (format stream "~A~%" (make-string 60 :initial-element #\=))
  (format stream "~&~20A ~8A ~15A ~15A~%" "Name" "Qty" "Part Number" "Material")
  (format stream "~A~%" (make-string 60 :initial-element #\-))

  (dolist (entry bom)
    (let ((name (bom-entry-name entry))
          (qty (bom-entry-quantity entry))
          (meta (bom-entry-metadata entry)))
      (format stream "~&~20A ~8A ~15A ~15A~%"
              name
              qty
              (or (getf meta :part-number) "-")
              (or (getf meta :material) "-"))))

  (format stream "~A~%" (make-string 60 :initial-element #\=))
  (format stream "Total items: ~A~%~%" (length bom)))
