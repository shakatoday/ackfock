(defsystem "ackfock"
  :version "1.2.3"
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
               "datafly"
               "sxql"

               ;; for DB migration
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"

               ;; GUI framework
               "clog"

               ;; server-side sessions
               "lack-middleware-session"
               "clog-lack-session"

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
                               (:file "pages" :depends-on ("main-page" "game" "theme"))
                               (:file "main-page" :depends-on ("game"))
                               (:file "memo" :depends-on ("channel" "game"))
                               (:file "channel" :depends-on ("game"))
                               (:file "theme")))
                 (:module "features"
                  :depends-on ("models" "db" "config")
                  :components ((:file "features")
                               (:file "search")
                               (:file "auth" :depends-on ("email-activation"))
                               (:file "email-activation")
                               (:file "channel-invitation")))
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
