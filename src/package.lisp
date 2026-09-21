(defpackage #:memory-protocol
  (:use #:cl)
  (:nicknames #:stack-memory)
  (:local-nicknames (#:dt #:datetime-protocol))
  (:export #:memory-error
           #:memory-error-message
           #:memory-missing-store
           #:memory-immutable
           #:memory-immutable-id
           #:memory-not-found
           #:memory-not-found-ids

           #:memory-record
           #:memory-record-p
           #:make-memory-record
           #:memory-record-id
           #:memory-record-ts
           #:memory-record-actor
           #:memory-record-role
           #:memory-record-kind
           #:memory-record-text
           #:memory-record-model
           #:memory-record-session
           #:memory-record-identity
           #:memory-record-tenant
           #:memory-record-forgotten-p
           #:memory-record-live-p
           #:coerce-memory-record

           #:memory-query
           #:memory-query-p
           #:make-memory-query
           #:coerce-memory-query
           #:memory-query-text
           #:memory-query-interval
           #:memory-query-identity
           #:memory-query-tenant
           #:memory-query-kinds
           #:memory-query-top-k
           #:memory-query-include-tombstones

           #:memory-hit
           #:memory-hit-p
           #:make-memory-hit
           #:memory-hit-record
           #:memory-hit-layer
           #:memory-hit-score
           #:memory-hit-confidence

           #:memory-result
           #:memory-result-p
           #:make-memory-result
           #:memory-result-hits
           #:memory-result-layer
           #:memory-result-fallback-p
           #:memory-result-note

           #:state-entry
           #:state-entry-p
           #:make-state-entry
           #:state-entry-ts
           #:state-entry-headline
           #:state-entry-session
           #:state-entry-gap-hours
           #:state-entry-priority
           #:state-entry-done-p

           #:state-index
           #:state-index-p
           #:make-state-index
           #:state-index-entries
           #:state-index-omitted
           #:state-index-identity
           #:state-index-tenant

           #:memory-store
           #:memory-store-p
           #:*memory-store*

           #:append-record
           #:query-memory
           #:current-state
           #:render-state
           #:forget-records
           #:mark-priority
           #:clear-priority
           #:mark-branch-dead

           #:in-memory-store
           #:in-memory-store-p
           #:make-in-memory-store
           #:use-in-memory-store))

(in-package #:memory-protocol)
