(defsystem "ackfock"
  :version "1.3.0"
  :author "Shaka Chen"
  :license ""
  :depends-on ("envy"
               "uiop"
               "str"
               "alexandria"
               "serapeum"
               "rutils"
               "uuid"
               "local-time"
               "spinneret"
               "cl-ppcre"

               ;; for DB
               "mito"
               "datafly"
               "sxql"

               ;; for DB migration
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"
               "ackfock-db-migrations"

               ;; GUI framework
               "clog"

               ;; server-side sessions
               "lack-middleware-session"
               "clog-lack-session"

               ;; routing library
               "myway"

               ;; password hashing and verification library
               "cl-pass"

               ;; email delivery
               "sendgrid")
  :components ((:module "src"
                :depends-on ("www" "db")
                :components
                ((:file "ackfock" :depends-on ("games" "config"))
                 (:module "games"
                  :depends-on ("features"
                               "models"
                               "db")
                  :components ((:file "game")
                               (:file "entries" :depends-on ("main-entry" "game" "auth" "theme"))
                               (:file "main-entry" :depends-on ("game"))
                               (:file "auth" :depends-on ("theme"))
                               (:file "memo" :depends-on ("channel" "game"))
                               (:file "channel" :depends-on ("game"))
                               (:file "theme")))
                 (:module "features"
                  :depends-on ("models" "db" "config")
                  :components ((:file "features" :depends-on ("auth"))
                               (:file "auth" :depends-on ("email-activation"))
                               (:file "email-activation")
                               (:file "search" :depends-on ("auth"))
                               (:file "channel-invitation" :depends-on ("auth"))))
                 (:module "models"
                  :depends-on ("db")
                  :components ((:file "model")
                               (:file "relationships" :depends-on ("model"))))
                 (:file "db" :depends-on ("config"))
                 (:file "config")))
               (:module "www"
                :components
                ((:static-file "index.html")))
               (:module "db"
                :components
                ((:static-file "search_query.sql")
                 (:static-file "memo_latest_ackfocks_per_user.sql"))))
  :description ""
  :in-order-to ((test-op (test-op "ackfock-test"))))
