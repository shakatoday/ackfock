(in-package :cl-user)
(defpackage ackfock.db.migrations
  (:use :cl :sxql)
  (:export #:rename-user-table-for-using-mito))
(in-package :ackfock.db.migrations)

(defun rename-user-table-for-using-mito (migration-driver)
  "To change users to user. And ensuring it's letter case is compatible"
  (let ((mito:*connection* (ackfock.db:db)))
    (mito:execute-sql
     (alter-table :users
       (rename-to :user)))))
