(in-package #:memory-protocol)

(defclass memory-store () ())

(defun memory-store-p (x)
  (typep x 'memory-store))

(defvar *memory-store* nil)

(defun %ensure-store (&optional (store *memory-store*))
  (or store
      (restart-case
          (error 'memory-missing-store
                 :message "*memory-store* is nil — call MAKE-IN-MEMORY-STORE")
        (use-value (supplied)
          :report "Use a supplied memory store"
          supplied))))

(defgeneric append-record (store record)
  (:documentation "Append RECORD. Never rewrite an existing id."))

(defgeneric query-memory (store query)
  (:documentation "Time-first recall. Returns MEMORY-RESULT with an honest layer."))

(defgeneric current-state (store &key identity tenant window)
  (:documentation "Derived LLL-shaped state index for IDENTITY/TENANT."))

(defgeneric render-state (state &key bound)
  (:documentation "Injection text for STATE. BOUND caps entries."))

(defgeneric forget-records (store query)
  (:documentation "Tombstone matching live records and drop their text."))

(defgeneric mark-priority (store record-id priority)
  (:documentation "Human-only priority mark. PRIORITY is a keyword or NIL."))

(defgeneric clear-priority (store record-id)
  (:documentation "Clear a human priority mark."))

(defgeneric mark-branch-dead (store session &key identity tenant)
  (:documentation "Exclude SESSION from live recall (discarded branch)."))

(defmethod append-record ((store null) record)
  (append-record (%ensure-store) record))

(defmethod query-memory ((store null) query)
  (query-memory (%ensure-store) query))

(defmethod current-state ((store null) &key identity tenant window)
  (current-state (%ensure-store) :identity identity :tenant tenant :window window))

(defmethod forget-records ((store null) query)
  (forget-records (%ensure-store) query))

(defmethod mark-priority ((store null) record-id priority)
  (mark-priority (%ensure-store) record-id priority))

(defmethod clear-priority ((store null) record-id)
  (clear-priority (%ensure-store) record-id))

(defmethod mark-branch-dead ((store null) session &key identity tenant)
  (mark-branch-dead (%ensure-store) session :identity identity :tenant tenant))

(defun %interval-contains-record (interval record)
  (dt:interval-contains-p interval (memory-record-ts record)))

(defun %text-match-p (needle haystack)
  (let ((n (string-downcase (or needle "")))
        (h (string-downcase (or haystack ""))))
    (or (zerop (length n))
        (search n h :test #'char=))))

(defun %scope-record-p (record query)
  (and (string= (memory-record-identity record) (memory-query-identity query))
       (string= (memory-record-tenant record) (memory-query-tenant query))
       (member (memory-record-kind record) (memory-query-kinds query) :test #'eq)
       (memory-record-live-p record)
       (or (memory-query-include-tombstones query)
           (not (memory-record-forgotten-p record)))))

(defun %chronological (records)
  (sort (copy-list records) #'dt:less :key #'memory-record-ts))

(defun %hits (records layer &key (score 1) (confidence 1))
  (mapcar (lambda (r)
            (make-memory-hit :record r :layer layer :score score :confidence confidence))
          records))

(defun %take (list n)
  (subseq list 0 (min (length list) n)))
