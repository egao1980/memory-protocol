(in-package #:memory-protocol/tests)

(deftest forget-tombstones-and-drops-text
  (let ((store (%store (%rec :id "a" :text "secret token"))))
    (let ((tombs (memory-protocol:forget-records
                  store
                  (memory-protocol:make-memory-query
                   :text "secret" :identity "ada" :tenant "acme"))))
      (ok (= 1 (length tombs)))
      (ok (eq :tombstone
              (memory-protocol:memory-record-kind (first tombs)))))
    (let ((gone (memory-protocol:query-memory
                 store
                 (memory-protocol:make-memory-query
                  :text "secret" :identity "ada" :tenant "acme"))))
      (ok (null (memory-protocol:memory-result-hits gone))))
    (let ((seen (memory-protocol:query-memory
                 store
                 (memory-protocol:make-memory-query
                  :text ""
                  :identity "ada" :tenant "acme"
                  :kinds '(:text :tombstone)
                  :include-tombstones t))))
      (ok (some (lambda (hit)
                  (and (memory-protocol:memory-record-forgotten-p
                        (memory-protocol:memory-hit-record hit))
                       (string= "" (memory-protocol:memory-record-text
                                    (memory-protocol:memory-hit-record hit)))))
                (memory-protocol:memory-result-hits seen)))
      (ok (some (lambda (hit)
                  (eq :tombstone
                      (memory-protocol:memory-record-kind
                       (memory-protocol:memory-hit-record hit))))
                (memory-protocol:memory-result-hits seen))))))
