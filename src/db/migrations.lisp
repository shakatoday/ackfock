(in-package :cl-user)
(defpackage ackfock.db.migrations
  (:use :cl :sxql)
  (:export #:rename-user-table-for-using-mito/downgrade
           #:rename-user-table-for-using-mito/upgrade))
(in-package :ackfock.db.migrations)

(defun rename-user-table-for-using-mito/upgrade (migration-driver)
  "To change users to user. And ensuring it's letter case is compatible"
  (let ((mito:*connection* (migratum.driver.dbi:dbi-driver-connection migration-driver)))
    (mito:execute-sql
     (alter-table :users
       (rename-to :user)))))

(defun rename-user-table-for-using-mito/downgrade (migration-driver)
  "To change table user back to users."
  (let ((mito:*connection* (migratum.driver.dbi:dbi-driver-connection migration-driver)))
    (mito:execute-sql
     (alter-table :user
       (rename-to :users)))))
