(defsystem "memory-protocol"
  :version "0.1.0"
  :description "CLOS lossless memory protocol for cl-stack (append-only identity-scoped log)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("datetime-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "protocol")
               (:file "store")
               (:file "state"))
  :in-order-to ((test-op (test-op "memory-protocol/tests"))))

(defsystem "memory-protocol/tests"
  :depends-on ("memory-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "state-test")
               (:file "forget-test")
               (:file "demo-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
