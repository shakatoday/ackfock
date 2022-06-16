(defsystem "ackfock"
  :version "0.1.0"
  :author "Shaka Chen"
  :license ""
  :depends-on ("envy"
               "uiop"
               "str"
               "alexandria"
               "rutils"
               "uuid"
               "local-time"
               "spinneret"

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
                ((:file "ackfock" :depends-on ("view" "theme" "auth" "model-definition" "db"))
                 (:file "view" :depends-on ("model" "model-definition"))
                 (:file "theme" :depends-on ("auth" "model-definition"))
                 (:file "auth" :depends-on ("authenticate-user-email" "utils" "model-definition" "db"))
                 (:file "authenticate-user-email" :depends-on ("model-definition" "utils" "db"))
                 (:file "model" :depends-on ("model-definition" "utils" "db"))
                 (:file "utils")
                 (:file "model-definition" :depends-on ("db"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "ackfock-test"))))
