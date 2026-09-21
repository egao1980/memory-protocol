(in-package #:memory-protocol)

(defparameter +state-gap-hours+ 5)
(defparameter +state-headline-chars+ 80)
(defparameter +state-recent-minutes+ 90)
(defparameter +state-fold-minutes+ 30)
(defparameter +state-inject-hours+ 6)
(defparameter +greeting-tokens+
  '("good morning" "good evening" "good afternoon" "i'm back" "im back"
    "hello again" "hey again"))

(defun %hours-between (a b)
  (/ (cl:- (dt:instant-seconds b) (dt:instant-seconds a)) 3600d0))

(defun %greeting-p (text)
  (let ((s (string-downcase (or text ""))))
    (some (lambda (g) (eql 0 (search g s :test #'char=))) +greeting-tokens+)))

(defun %mechanical-headline (text)
  (let* ((raw (string-trim '(#\Space #\Tab #\Newline) (or text "")))
         (n (min (length raw) +state-headline-chars+)))
    (if (zerop n)
        ""
        (subseq raw 0 n))))

(defun %chunk-boundary-p (prev rec)
  (let ((gap (%hours-between (memory-record-ts prev) (memory-record-ts rec))))
    (or (cl:>= gap +state-gap-hours+)
        (and (cl:>= gap 1)
             (eq (memory-record-role rec) :user)
             (%greeting-p (memory-record-text rec))))))

(defun %priority-of (store rec)
  (gethash (memory-record-id rec) (in-memory-store-priorities store)))

(defun %fold-entries (entries now)
  "Keep last 90 minutes in full; older → one per 30-minute bucket.
   Priority-marked entries always survive."
  (let ((cutoff (dt:make-instant
                 (cl:- (dt:instant-seconds now)
                       (cl:* +state-recent-minutes+ 60))))
        (kept '())
        (buckets (make-hash-table :test 'eql)))
    (dolist (e entries)
      (cond
        ((or (state-entry-priority e)
             (dt:less cutoff (state-entry-ts e))
             (dt:value= cutoff (state-entry-ts e)))
         (push e kept))
        (t
         (let* ((secs (dt:instant-seconds (state-entry-ts e)))
                (bucket (floor secs (cl:* +state-fold-minutes+ 60))))
           (unless (gethash bucket buckets)
             (setf (gethash bucket buckets) e)
             (push e kept))))))
    (sort kept #'dt:less :key #'state-entry-ts)))

(defun %window-entries (entries now)
  (let* ((cutoff (dt:make-instant
                  (cl:- (dt:instant-seconds now)
                        (cl:* +state-inject-hours+ 3600))))
         (in (remove-if (lambda (e)
                          (dt:less (state-entry-ts e) cutoff))
                        entries))
         (omitted (cl:- (length entries) (length in))))
    (values in omitted)))

(defmethod current-state ((store in-memory-store) &key identity tenant window)
  (let* ((identity (%coerce-string identity "default"))
         (tenant (%coerce-string tenant "default"))
         (now (dt:now))
         (win (or window (dt:make-duration (cl:* 48 3600))))
         (since (dt:make-instant (cl:- (dt:instant-seconds now)
                                       (dt:duration-seconds win))))
         (q (make-memory-query :identity identity :tenant tenant
                               :kinds '(:text :action)
                               :interval (dt:make-interval since now)
                               :top-k 10000))
         (recs (%chronological (%scoped store q)))
         (chunks '())
         (current nil))
    (dolist (rec recs)
      (when (eq (memory-record-role rec) :user)
        (cond
          ((null current)
           (setf current (list rec)))
          ((%chunk-boundary-p (first current) rec)
           (push (nreverse current) chunks)
           (setf current (list rec)))
          (t (push rec current)))))
    (when current
      (push (nreverse current) chunks))
    (setf chunks (nreverse chunks))
    (let ((entries '())
          (prev-end nil))
      (dolist (chunk chunks)
        (let* ((first (first chunk))
               (gap (and prev-end (%hours-between prev-end (memory-record-ts first))))
               (headline (%mechanical-headline (memory-record-text first)))
               (priority (%priority-of store first)))
          (push (make-state-entry
                 :ts (memory-record-ts first)
                 :headline headline
                 :session (memory-record-session first)
                 :gap-hours (and gap (cl:>= gap +state-gap-hours+) (round gap))
                 :priority priority)
                entries)
          (setf prev-end (memory-record-ts (car (last chunk))))))
      (setf entries (%fold-entries (nreverse entries) now))
      (multiple-value-bind (shown omitted) (%window-entries entries now)
        (make-state-index :entries shown
                          :omitted omitted
                          :identity identity
                          :tenant tenant)))))

(defmethod render-state ((state state-index) &key bound)
  (let* ((entries (state-index-entries state))
         (cut (if bound (%take entries bound) entries))
         (omitted (cl:+ (state-index-omitted state)
                        (cl:- (length entries) (length cut))))
         (lines '()))
    (push (format nil "where we are (~a/~a)"
                  (state-index-identity state)
                  (state-index-tenant state))
          lines)
    (dolist (e cut)
      (let ((ts (dt:print-rfc3339 (state-entry-ts e)))
            (gap (state-entry-gap-hours e))
            (pri (state-entry-priority e))
            (head (state-entry-headline e)))
        (push (format nil "~a~@[ +~dh~]~@[ [~a]~] ~a"
                      ts gap pri head)
              lines)))
    (when (plusp omitted)
      (push (format nil "~d earlier entries omitted" omitted) lines))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defmethod render-state ((state null) &key bound)
  (declare (ignore bound))
  "")
