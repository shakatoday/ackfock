(defsystem "ackfock-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Shaka Chen"
  :license ""
  :depends-on ("ackfock"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "ackfock"))))
  :description "Test system for ackfock"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
