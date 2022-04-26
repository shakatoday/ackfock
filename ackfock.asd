(defsystem "ackfock"
  :version "0.1.0"
  :author "Shaka Chen"
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"
               "str"
               "alexandria"
               "uuid"
               "local-time"
               
               ;; HTML Template
               "djula"
               "cl-who"

               ;; for DB
               "datafly"
               "sxql"

               ;; for DB migration
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"

               ;; password hashing and verification library
               "cl-pass"

               ;; email and more validators
               "clavier"

               ;; email delivery
               "mailgun")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("config" "utils" "model" "page"))
                 (:file "page" :depends-on ("utils" "model" "model-definition" "view"))
                 (:file "view" :depends-on ("config" "utils" "model-definition"))
                 (:file "model" :depends-on ("db" "utils" "model-definition"))
                 (:file "utils")
                 (:file "model-definition" :depends-on ("db"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "ackfock-test"))))
