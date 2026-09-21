(in-package #:memory-protocol/tests)

(defun %ts (unix)
  (datetime-protocol:make-instant unix))

(defun %rec (&key (id nil) (unix 1700000000) text (kind :text) (role :user)
               (identity "ada") (tenant "acme") (session "s1") actor)
  (memory-protocol:make-memory-record
   :id id
   :ts (%ts unix)
   :text (or text "hello")
   :kind kind
   :role role
   :identity identity
   :tenant tenant
   :session session
   :actor (or actor identity)))

(defun %store (&rest recs)
  (let ((store (memory-protocol:make-in-memory-store)))
    (dolist (r recs)
      (memory-protocol:append-record store r))
    store))

(deftest append-is-immutable
  (let ((store (%store (%rec :id "a" :text "one"))))
    (ok (signals (memory-protocol:append-record
                  store (%rec :id "a" :text "two"))
                 'memory-protocol:memory-immutable))))

(deftest query-time-first-exact
  (let* ((store (%store
                 (%rec :id "a" :unix 1700000000 :text "budget numbers")
                 (%rec :id "b" :unix 1700003600 :text "weather")
                 (%rec :id "c" :unix 1800000000 :text "budget later")))
         (q (memory-protocol:make-memory-query
             :text "budget"
             :identity "ada" :tenant "acme"
             :interval (datetime-protocol:make-interval
                        (%ts 1699990000) (%ts 1700010000))))
         (result (memory-protocol:query-memory store q)))
    (ok (eq :exact (memory-protocol:memory-result-layer result)))
    (ok (= 1 (length (memory-protocol:memory-result-hits result))))
    (ok (equal "a" (memory-protocol:memory-record-id
                    (memory-protocol:memory-hit-record
                     (first (memory-protocol:memory-result-hits result))))))))

(deftest query-range-fallback-announced
  (let* ((store (%store
                 (%rec :id "a" :unix 1700000000 :text "alpha")
                 (%rec :id "b" :unix 1700003600 :text "beta")))
         (q (memory-protocol:make-memory-query
             :text "zzz"
             :identity "ada" :tenant "acme"
             :interval (datetime-protocol:make-interval
                        (%ts 1699990000) (%ts 1700010000))))
         (result (memory-protocol:query-memory store q)))
    (ok (eq :range (memory-protocol:memory-result-layer result)))
    (ok (memory-protocol:memory-result-fallback-p result))
    (ok (search "whole range" (memory-protocol:memory-result-note result)))
    (ok (= 2 (length (memory-protocol:memory-result-hits result))))
    (ok (equal "a" (memory-protocol:memory-record-id
                    (memory-protocol:memory-hit-record
                     (first (memory-protocol:memory-result-hits result))))))))

(deftest query-parses-relative-time
  (let* ((now (%ts 1700007200))
         (store (%store
                 (%rec :id "a" :unix 1700000000 :text "budget")
                 (%rec :id "b" :unix 1800000000 :text "budget")))
         (today (datetime-protocol:zoned-moment-date
                 (datetime-protocol:instant-in-zone now datetime-protocol:+utc+)))
         (start (datetime-protocol:zoned-moment-to-instant
                 (datetime-protocol:moment-in-zone
                  (datetime-protocol:make-moment today datetime-protocol:+midnight+)
                  datetime-protocol:+utc+)))
         ;; pin records onto "today"
         (store2 (%store
                  (%rec :id "today-hit"
                        :unix (datetime-protocol:instant-seconds start)
                        :text "budget")
                  (%rec :id "old" :unix 1600000000 :text "budget")))
         (result (memory-protocol:query-memory
                  store2
                  (memory-protocol:make-memory-query
                   :text "today budget"
                   :identity "ada" :tenant "acme"
                   :now now))))
    (declare (ignore store))
    (ok (eq :exact (memory-protocol:memory-result-layer result)))
    (ok (= 1 (length (memory-protocol:memory-result-hits result))))
    (ok (equal "today-hit"
               (memory-protocol:memory-record-id
                (memory-protocol:memory-hit-record
                 (first (memory-protocol:memory-result-hits result))))))))

(deftest query-excludes-documents-from-conversation-view
  (let* ((store (%store
                 (%rec :id "t" :unix 1700000000 :text "budget" :kind :text)
                 (%rec :id "d" :unix 1700000100 :text "budget" :kind :document)))
         (result (memory-protocol:query-memory
                  store
                  (memory-protocol:make-memory-query
                   :text "budget" :identity "ada" :tenant "acme"))))
    (ok (= 1 (length (memory-protocol:memory-result-hits result))))
    (ok (equal "t" (memory-protocol:memory-record-id
                    (memory-protocol:memory-hit-record
                     (first (memory-protocol:memory-result-hits result))))))))

(deftest query-identity-scope
  (let* ((store (%store
                 (%rec :id "ada" :identity "ada" :text "note")
                 (%rec :id "bob" :identity "bob" :text "note")))
         (result (memory-protocol:query-memory
                  store
                  (memory-protocol:make-memory-query
                   :text "note" :identity "ada" :tenant "acme"))))
    (ok (= 1 (length (memory-protocol:memory-result-hits result))))
    (ok (equal "ada" (memory-protocol:memory-record-id
                      (memory-protocol:memory-hit-record
                       (first (memory-protocol:memory-result-hits result))))))))

(deftest dead-branch-excluded
  (let ((store (%store
                (%rec :id "live" :session "s1" :text "keep")
                (%rec :id "dead" :session "s2" :text "keep"))))
    (ok (= 1 (memory-protocol:mark-branch-dead store "s2"
                                              :identity "ada" :tenant "acme")))
    (let ((result (memory-protocol:query-memory
                   store
                   (memory-protocol:make-memory-query
                    :text "keep" :identity "ada" :tenant "acme"))))
      (ok (= 1 (length (memory-protocol:memory-result-hits result))))
      (ok (equal "live" (memory-protocol:memory-record-id
                         (memory-protocol:memory-hit-record
                          (first (memory-protocol:memory-result-hits result)))))))))

(deftest missing-store-restart
  (let ((memory-protocol:*memory-store* nil))
    (ok (signals (memory-protocol:query-memory nil "x")
                 'memory-protocol:memory-missing-store))))
