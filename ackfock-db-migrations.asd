(defsystem "ackfock-db-migrations"
  :version "0.1.0"
  :author "Shaka Chen"
  :license ""
  :depends-on ("envy" ;for config

               "mito"
               "sxql"
               "datafly"


               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"

               "serapeum")
  :components ((:module "src"
                :components
                ((:file "db/migrations" :depends-on ("db"))
                 (:file "db" :depends-on ("config"))
                 (:file "config")))))
