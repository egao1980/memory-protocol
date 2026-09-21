(in-package #:memory-protocol/tests)

(deftest state-mechanical-headline-and-gap
  (let* ((t0 1700000000)
         (store (%store
                 (%rec :id "a" :unix t0 :text "talking about the budget numbers today"
                       :session "s1")
                 (%rec :id "b" :unix (cl:+ t0 600) :text "ok" :role :assistant
                       :session "s1")
                 (%rec :id "c" :unix (cl:+ t0 (* 6 3600))
                       :text "good morning — shipping the patch"
                       :session "s2")))
         (state (let ((datetime-protocol:*clock*
                        (datetime-protocol:make-fixed-clock
                         (%ts (cl:+ t0 (* 6 3600) 60)))))
                  (memory-protocol:current-state store
                                                 :identity "ada" :tenant "acme")))
         (entries (memory-protocol:state-index-entries state)))
    ;; 6h inject window drops the first chunk and says so.
    (ok (= 1 (length entries)))
    (ok (plusp (memory-protocol:state-index-omitted state)))
    (ok (search "shipping the patch"
                (memory-protocol:state-entry-headline (first entries))))
    (ok (memory-protocol:state-entry-gap-hours (first entries)))
    (ok (search "earlier entries omitted"
                (memory-protocol:render-state state)))))

(deftest state-human-priority-survives-fold
  (let* ((t0 1700000000)
         (store (%store
                 (%rec :id "old" :unix t0 :text "old topic")
                 (%rec :id "new" :unix (cl:+ t0 4000) :text "new topic"))))
    (memory-protocol:mark-priority store "old" :high)
    (let* ((state (let ((datetime-protocol:*clock*
                          (datetime-protocol:make-fixed-clock
                           (%ts (cl:+ t0 4000)))))
                    (memory-protocol:current-state store
                                                   :identity "ada" :tenant "acme")))
           (text (memory-protocol:render-state state)))
      (ok (search "old topic" text))
      (ok (search "[HIGH]" text))
      (ok (search "where we are" text)))))

(deftest state-render-announces-omitted
  (let ((idx (memory-protocol:make-state-index
              :entries (list (memory-protocol:make-state-entry
                              :ts (%ts 1700000000)
                              :headline "one")
                             (memory-protocol:make-state-entry
                              :ts (%ts 1700001000)
                              :headline "two"))
              :omitted 4
              :identity "ada" :tenant "acme")))
    (ok (search "4 earlier entries omitted"
                (memory-protocol:render-state idx :bound 2)))
    (ok (search "one" (memory-protocol:render-state idx :bound 1)))))
