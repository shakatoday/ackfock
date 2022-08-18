(defsystem "ackfock"
  :version "1.2.3-alpha"
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
               "sendgrid")
  :components ((:module "www"
                :components
                ((:static-file "index.html")))
               (:module "src"
                :depends-on ("www")
                :components
                ((:file "ackfock" :depends-on ("games" "auth"))
                 (:module "games"
                  :depends-on ("model"
                               "model-definition"
                               "invitation"
                               "auth"
                               "authenticate-user-email"
                               "db")
                  :components ((:file "pages" :depends-on ("main-page" "game" "theme"))
                               (:file "main-page" :depends-on ("game"))
                               (:file "game" :depends-on ("theme"))
                               (:file "theme")))
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
