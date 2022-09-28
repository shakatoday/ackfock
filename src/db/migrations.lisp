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

(defun id-and-primary-key-compatibility/upgrade (migration-driver)
  "Alter all tables' uuid column names to id, and alter types to varchar(36)."
  (let ((mito:*connection* (migratum.driver.dbi:dbi-driver-connection migration-driver)))
    (mapc (lambda (table-name)
            (mito:execute-sql
             (alter-table table-name
               (rename-column :uuid :id))))
          '(:user :channel :memo))
    (mapc (lambda (table-name constraint-name)
            (mito:execute-sql
             (format nil "ALTER TABLE ~(~a~) ALTER COLUMN DROP CONSTRAINT ~(~a~)"
                     table-name
                     constraint-name)))
          '(:user_ackfock :user_ackfock
            '(:user_ackfock_memo_id_fkey :user_ackfock_user_id_fkey

(defun id-and-primary-key-compatibility/downgrade (migration-driver)
  "Alter all tables' id column names back to uuid, and alter the types back to uuid")
