;;;; Offline memory-protocol demo. Load at compile time so tests can
;;;; asdf:load-system this file's package.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:memory-protocol)
    (asdf:load-system "memory-protocol")))

(defpackage #:memory-protocol/demo
  (:use #:cl)
  (:export #:run-recall-demo))

(in-package #:memory-protocol/demo)

(defun run-recall-demo ()
  "Append a few lines, time-first query, state inject, forget."
  (let* ((now (datetime-protocol:make-instant 1700007200))
         (datetime-protocol:*clock* (datetime-protocol:make-fixed-clock now))
         (store (memory-protocol:make-in-memory-store))
         (today (datetime-protocol:zoned-moment-date
                 (datetime-protocol:instant-in-zone now datetime-protocol:+utc+)))
         (start (datetime-protocol:instant-seconds
                 (datetime-protocol:zoned-moment-to-instant
                  (datetime-protocol:moment-in-zone
                   (datetime-protocol:make-moment today datetime-protocol:+midnight+)
                   datetime-protocol:+utc+)))))
    (memory-protocol:append-record
     store (memory-protocol:make-memory-record
            :id "u1" :ts (datetime-protocol:make-instant start)
            :text "we should freeze the budget" :identity "ada" :tenant "acme"
            :session "s1" :actor "ada"))
    (memory-protocol:append-record
     store (memory-protocol:make-memory-record
            :id "a1" :ts (datetime-protocol:make-instant (cl:+ start 60))
            :text "agreed" :role :assistant :identity "ada" :tenant "acme"
            :session "s1" :actor "cece"))
    (let* ((result (memory-protocol:query-memory
                    store
                    (memory-protocol:make-memory-query
                     :text "today budget" :identity "ada" :tenant "acme"
                     :now now)))
           (state (memory-protocol:current-state store
                                                 :identity "ada" :tenant "acme")))
      (list :layer (memory-protocol:memory-result-layer result)
            :n (length (memory-protocol:memory-result-hits result))
            :state (memory-protocol:render-state state)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*demo-ran*)
    (defparameter *demo-ran* t)
    (run-recall-demo)))
