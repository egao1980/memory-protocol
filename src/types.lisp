(in-package #:memory-protocol)

(defparameter *record-id-counter* 0)

(defun %fresh-record-id ()
  (format nil "mem-~d" (incf *record-id-counter*)))

(defun %coerce-keyword (x &optional default)
  (cond
    ((null x) default)
    ((keywordp x) x)
    ((symbolp x) (intern (string-upcase (string x)) :keyword))
    ((stringp x) (intern (string-upcase x) :keyword))
    (t default)))

(defun %coerce-string (x &optional (default "default"))
  (cond
    ((null x) default)
    ((stringp x) x)
    ((symbolp x) (string x))
    (t (princ-to-string x))))

(defun %coerce-instant (ts)
  (cond
    ((dt:instantp ts) ts)
    ((integerp ts) (dt:make-instant ts))
    ((stringp ts)
     (dt:zoned-moment-to-instant (dt:parse-rfc3339 ts)))
    (t (dt:now))))

(defclass memory-record ()
  ((id :initarg :id :accessor memory-record-id)
   (ts :initarg :ts :accessor memory-record-ts)
   (actor :initarg :actor :accessor memory-record-actor :initform "default")
   (role :initarg :role :accessor memory-record-role :initform :user)
   (kind :initarg :kind :accessor memory-record-kind :initform :text)
   (text :initarg :text :accessor memory-record-text :initform "")
   (model :initarg :model :accessor memory-record-model :initform nil)
   (session :initarg :session :accessor memory-record-session :initform "default")
   (identity :initarg :identity :accessor memory-record-identity :initform "default")
   (tenant :initarg :tenant :accessor memory-record-tenant :initform "default")
   (forgotten-p :initarg :forgotten-p :accessor memory-record-forgotten-p :initform nil)
   (live-p :initarg :live-p :accessor memory-record-live-p :initform t)))

(defun memory-record-p (x)
  (typep x 'memory-record))

(defun make-memory-record (&key id ts actor role kind text model session
                             identity tenant forgotten-p (live-p t))
  (make-instance 'memory-record
                 :id (or id (%fresh-record-id))
                 :ts (%coerce-instant ts)
                 :actor (%coerce-string actor "default")
                 :role (%coerce-keyword role :user)
                 :kind (%coerce-keyword kind :text)
                 :text (or text "")
                 :model model
                 :session (%coerce-string session "default")
                 :identity (%coerce-string identity "default")
                 :tenant (%coerce-string tenant "default")
                 :forgotten-p (and forgotten-p t)
                 :live-p (if live-p t nil)))

(defun coerce-memory-record (x)
  (etypecase x
    (memory-record x)
    (string (make-memory-record :text x :ts (dt:now)))
    (list (apply #'make-memory-record x))))

(defclass memory-query ()
  ((text :initarg :text :accessor memory-query-text :initform nil)
   (interval :initarg :interval :accessor memory-query-interval :initform nil)
   (identity :initarg :identity :accessor memory-query-identity :initform "default")
   (tenant :initarg :tenant :accessor memory-query-tenant :initform "default")
   (kinds :initarg :kinds :accessor memory-query-kinds :initform '(:text :action))
   (top-k :initarg :top-k :accessor memory-query-top-k :initform 32)
   (include-tombstones :initarg :include-tombstones
                       :accessor memory-query-include-tombstones
                       :initform nil)))

(defun memory-query-p (x)
  (typep x 'memory-query))

(defun make-memory-query (&key text interval identity tenant
                            (kinds '(:text :action)) (top-k 32)
                            include-tombstones now zone)
  (let ((text (or text "")))
    (unless interval
      (multiple-value-bind (iv rest matched)
          (dt:parse-time-range text :now now :zone (or zone dt:+utc+))
        (when matched
          (setf interval iv
                text rest))))
    (make-instance 'memory-query
                   :text text
                   :interval interval
                   :identity (%coerce-string identity "default")
                   :tenant (%coerce-string tenant "default")
                   :kinds (mapcar #'%coerce-keyword (or kinds '(:text :action)))
                   :top-k top-k
                   :include-tombstones include-tombstones)))

(defun coerce-memory-query (x &key identity tenant now zone)
  (etypecase x
    (memory-query x)
    (string (make-memory-query :text x :identity identity :tenant tenant
                               :now now :zone zone))
    (list (apply #'make-memory-query
                 (append x (list :identity identity :tenant tenant
                                 :now now :zone zone))))))

(defclass memory-hit ()
  ((record :initarg :record :accessor memory-hit-record)
   (layer :initarg :layer :accessor memory-hit-layer :initform :exact)
   (score :initarg :score :accessor memory-hit-score :initform 1)
   (confidence :initarg :confidence :accessor memory-hit-confidence :initform 1)))

(defun memory-hit-p (x)
  (typep x 'memory-hit))

(defun make-memory-hit (&key record (layer :exact) (score 1) (confidence 1))
  (make-instance 'memory-hit
                 :record record :layer layer :score score :confidence confidence))

(defclass memory-result ()
  ((hits :initarg :hits :accessor memory-result-hits :initform nil)
   (layer :initarg :layer :accessor memory-result-layer :initform :exact)
   (fallback-p :initarg :fallback-p :accessor memory-result-fallback-p :initform nil)
   (note :initarg :note :accessor memory-result-note :initform nil)))

(defun memory-result-p (x)
  (typep x 'memory-result))

(defun make-memory-result (&key hits (layer :exact) fallback-p note)
  (make-instance 'memory-result
                 :hits hits :layer layer :fallback-p fallback-p :note note))

(defclass state-entry ()
  ((ts :initarg :ts :accessor state-entry-ts)
   (headline :initarg :headline :accessor state-entry-headline :initform "")
   (session :initarg :session :accessor state-entry-session :initform nil)
   (gap-hours :initarg :gap-hours :accessor state-entry-gap-hours :initform nil)
   (priority :initarg :priority :accessor state-entry-priority :initform nil)
   (done-p :initarg :done-p :accessor state-entry-done-p :initform nil)))

(defun state-entry-p (x)
  (typep x 'state-entry))

(defun make-state-entry (&key ts headline session gap-hours priority done-p)
  (make-instance 'state-entry
                 :ts ts :headline (or headline "")
                 :session session :gap-hours gap-hours
                 :priority priority :done-p done-p))

(defclass state-index ()
  ((entries :initarg :entries :accessor state-index-entries :initform nil)
   (omitted :initarg :omitted :accessor state-index-omitted :initform 0)
   (identity :initarg :identity :accessor state-index-identity :initform "default")
   (tenant :initarg :tenant :accessor state-index-tenant :initform "default")))

(defun state-index-p (x)
  (typep x 'state-index))

(defun make-state-index (&key entries (omitted 0) identity tenant)
  (make-instance 'state-index
                 :entries entries :omitted omitted
                 :identity (%coerce-string identity "default")
                 :tenant (%coerce-string tenant "default")))
