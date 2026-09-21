(in-package #:memory-protocol)

(defclass in-memory-store (memory-store)
  ((records :initform (make-hash-table :test 'equal) :accessor in-memory-store-table)
   (order :initform nil :accessor in-memory-store-order)
   (priorities :initform (make-hash-table :test 'equal) :accessor in-memory-store-priorities)
   (semantic-fn :initarg :semantic-fn :accessor in-memory-store-semantic-fn :initform nil)))

(defun in-memory-store-p (x)
  (typep x 'in-memory-store))

(defun make-in-memory-store (&key semantic-fn)
  (make-instance 'in-memory-store :semantic-fn semantic-fn))

(defun use-in-memory-store (&rest args &key &allow-other-keys)
  (setf *memory-store* (apply #'make-in-memory-store args)))

(defmethod append-record ((store in-memory-store) record)
  (let ((rec (coerce-memory-record record)))
    (when (nth-value 1 (gethash (memory-record-id rec) (in-memory-store-table store)))
      (error 'memory-immutable
             :id (memory-record-id rec)
             :message (format nil "id ~s already exists" (memory-record-id rec))))
    (setf (gethash (memory-record-id rec) (in-memory-store-table store)) rec)
    (setf (in-memory-store-order store)
          (nconc (in-memory-store-order store) (list (memory-record-id rec))))
    rec))

(defun %all-records (store)
  (mapcar (lambda (id) (gethash id (in-memory-store-table store)))
          (in-memory-store-order store)))

(defun %scoped (store query)
  (remove-if-not (lambda (r) (%scope-record-p r query)) (%all-records store)))

(defun %in-interval (records interval)
  (if interval
      (remove-if-not (lambda (r) (%interval-contains-record interval r)) records)
      records))

(defun %exact (records text)
  (if (or (null text) (zerop (length text)))
      records
      (remove-if-not (lambda (r) (%text-match-p text (memory-record-text r)))
                     records)))

(defun %semantic-fallback (store query scoped)
  (let ((fn (in-memory-store-semantic-fn store)))
    (when fn
      (funcall fn store query scoped))))

(defmethod query-memory ((store in-memory-store) query)
  (let* ((q (coerce-memory-query query))
         (k (memory-query-top-k q))
         (scoped (%scoped store q))
         (in-range (%in-interval scoped (memory-query-interval q)))
         (exact (%exact in-range (memory-query-text q)))
         (min-exact 3))
    (cond
      ((and (memory-query-interval q) exact (cl:>= (length exact) min-exact))
       (make-memory-result
        :hits (%take (%hits (%chronological exact) :exact) k)
        :layer :exact
        :note "exact match inside the time window"))
      ((and (memory-query-interval q) exact (plusp (length exact)))
       (make-memory-result
        :hits (%take (%hits (%chronological exact) :exact) k)
        :layer :exact
        :note "exact match inside the time window"))
      ((and (memory-query-interval q) (plusp (length in-range)))
       (make-memory-result
        :hits (%take (%hits (%chronological in-range) :range) k)
        :layer :range
        :fallback-p (and (memory-query-text q)
                         (plusp (length (memory-query-text q))))
        :note "few exact matches — showing the whole range in order"))
      ((and (null (memory-query-interval q)) exact (plusp (length exact)))
       (make-memory-result
        :hits (%take (%hits (%chronological exact) :exact) k)
        :layer :exact
        :note "exact match"))
      (t
       (let ((semantic (%semantic-fallback store q scoped)))
         (if semantic
             (make-memory-result
              :hits (%take (%hits semantic :semantic :score 0.4 :confidence 0.4) k)
              :layer :semantic
              :fallback-p t
              :note "nothing in that time range — pulled the closest match from the full history")
             (make-memory-result
              :hits nil
              :layer (if (memory-query-interval q) :range :exact)
              :note "no matching records")))))))

(defmethod forget-records ((store in-memory-store) query)
  (let* ((q (coerce-memory-query query))
         (targets (%exact (%in-interval (%scoped store q) (memory-query-interval q))
                          (memory-query-text q)))
         (tombstones '()))
    (dolist (rec targets)
      (setf (memory-record-forgotten-p rec) t
            (memory-record-text rec) "")
      (let ((tomb (make-memory-record
                   :ts (dt:now)
                   :actor (memory-record-actor rec)
                   :role :system
                   :kind :tombstone
                   :text (format nil "forgot ~a" (memory-record-id rec))
                   :session (memory-record-session rec)
                   :identity (memory-record-identity rec)
                   :tenant (memory-record-tenant rec)
                   :model (memory-record-id rec))))
        (append-record store tomb)
        (push tomb tombstones)))
    (nreverse tombstones)))

(defmethod mark-priority ((store in-memory-store) record-id priority)
  (unless (nth-value 1 (gethash record-id (in-memory-store-table store)))
    (error 'memory-not-found :ids (list record-id)
           :message (format nil "unknown record ~s" record-id)))
  (setf (gethash record-id (in-memory-store-priorities store))
        (%coerce-keyword priority))
  priority)

(defmethod clear-priority ((store in-memory-store) record-id)
  (remhash record-id (in-memory-store-priorities store))
  nil)

(defmethod mark-branch-dead ((store in-memory-store) session &key identity tenant)
  (let ((sid (%coerce-string session "default"))
        (id (%coerce-string identity "default"))
        (ten (%coerce-string tenant "default"))
        (n 0))
    (dolist (rec (%all-records store))
      (when (and (string= (memory-record-session rec) sid)
                 (string= (memory-record-identity rec) id)
                 (string= (memory-record-tenant rec) ten))
        (setf (memory-record-live-p rec) nil)
        (incf n)))
    n))
