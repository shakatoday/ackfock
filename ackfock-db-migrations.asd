(defsystem "ackfock-db-migrations"
  :version "0.1.0"
  :author "Shaka Chen"
  :license ""
  :depends-on ("str"
               "mito"
               "sxql"

               "cl-migratum.driver.dbi")
  :components ((:module "src/db"
                :components
                ((:file "migrations")))))
