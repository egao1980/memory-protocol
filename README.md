# memory-protocol

Lispy **CLOS** lossless memory for [cl-stack](https://github.com/egao1980/cl-stack) — append-only identity-scoped log, time-first recall, derived “where we are” index.

Not a session window (that is [`conversation-protocol`](https://github.com/egao1980/conversation-protocol)). Not RAG GFs (indexes stay [`rag-protocol`](https://github.com/egao1980/rag-protocol)). Time phrases live in [`datetime-protocol`](https://github.com/egao1980/datetime-protocol) `parse-time-range`.

Ideas from [lossless-memory](https://github.com/aru-labs/lossless-memory); this is not a port of their daemon.

```lisp
(asdf:load-system "memory-protocol")

(let ((store (stack-memory:make-in-memory-store)))
  (stack-memory:append-record
   store (stack-memory:make-memory-record
          :text "freeze the budget" :identity "ada" :tenant "acme"))
  (stack-memory:query-memory store "budget"))
```

| GF | Role |
|----|------|
| `append-record` | Write path. Immutable. No `generate`. |
| `query-memory` | Time first, then exact text. Layer tag + fallback note. |
| `current-state` / `render-state` | Mechanical headlines. Human `mark-priority`. |
| `forget-records` | Tombstone + drop text. Testable. |

Default conversation views exclude `:document`. `session` is provenance; identity+tenant is the key.

```lisp
(asdf:test-system "memory-protocol")
```

Offline demo:

```bash
sbcl --load examples/recall.lisp
```

## License

MIT
