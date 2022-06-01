(defsystem "ackfock"
  :version "0.1.0"
  :author "Shaka Chen"
  :license ""
  :depends-on ("envy"
               "uiop"
               "str"
               "alexandria"
               "uuid"
               "local-time"

               ;; for DB
               "datafly"
               "sxql"

               ;; for DB migration
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"

               ;; GUI framework
               "clog"

               ;; password hashing and verification library
               "cl-pass"

               ;; email and more validators
               "clavier"

               ;; email delivery
               "mailgun")
  :components ((:module "src"
                :components
                ((:file "ackfock")
                 (:file "model" :depends-on ("db" "utils" "model-definition"))
                 (:file "utils")
                 (:file "model-definition" :depends-on ("db"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "ackfock-test"))))
