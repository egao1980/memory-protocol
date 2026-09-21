(in-package #:memory-protocol)

(define-condition memory-error (error)
  ((message :initarg :message :reader memory-error-message :initform nil))
  (:report (lambda (c s)
             (format s "memory error~@[: ~a~]" (memory-error-message c)))))

(define-condition memory-missing-store (memory-error) ()
  (:report (lambda (c s)
             (format s "memory store missing~@[: ~a~]" (memory-error-message c)))))

(define-condition memory-immutable (memory-error)
  ((id :initarg :id :reader memory-immutable-id :initform nil))
  (:report (lambda (c s)
             (format s "memory record is append-only~@[ (~a)~]~@[: ~a~]"
                     (memory-immutable-id c) (memory-error-message c)))))

(define-condition memory-not-found (memory-error)
  ((ids :initarg :ids :reader memory-not-found-ids :initform nil))
  (:report (lambda (c s)
             (format s "memory records not found: ~s~@[: ~a~]"
                     (memory-not-found-ids c) (memory-error-message c)))))
