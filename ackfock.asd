(defsystem "ackfock"
  :version "1.2.0-alpha"
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
               "cl-ppcre"

               ;; for DB
               "datafly"
               "sxql"

               ;; for DB migration
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"

               ;; GUI framework
               "clog"

               ;; lack middlewares
               "lack-middleware-session"

               ;; password hashing and verification library
               "cl-pass"

               ;; email and more validators
               "clavier"

               ;; email delivery
               "mailgun")
  :components ((:module "www"
                :components
                ((:static-file "index.html")))
               (:module "src"
                :depends-on ("www")
                :components
                ((:file "ackfock" :depends-on ("main-page" "view" "theme" "auth" "db"))
                 (:file "main-page" :depends-on ("view" "theme" "model" "model-definition"))
                 (:file "view" :depends-on ("theme" "invitation" "model" "model-definition"))
                 (:file "theme" :depends-on ("auth" "model-definition"))
                 (:file "auth" :depends-on ("authenticate-user-email" "model-definition" "utils" "db"))
                 (:file "authenticate-user-email" :depends-on ("model-definition" "utils" "db"))
                 (:file "invitation" :depends-on ("model" "model-definition" "utils" "db"))
                 (:file "model" :depends-on ("model-definition" "utils" "db" "config"))
                 (:file "utils")
                 (:file "model-definition" :depends-on ("db"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "ackfock-test"))))
