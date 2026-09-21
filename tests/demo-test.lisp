(in-package #:memory-protocol/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "../examples/recall.lisp"
                         (or *compile-file-truename* *load-truename*))))

(deftest recall-demo-runs
  (let ((out (memory-protocol/demo:run-recall-demo)))
    (ok (eq :exact (getf out :layer)))
    (ok (plusp (getf out :n)))
    (ok (search "where we are" (getf out :state)))))
