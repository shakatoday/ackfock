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
  (let ((mito:*connection* (migratum.driver.dbi:dbi-driver-connection migration-driver))
        (foreign-key-tables '(:user_ackfock :user_ackfock :memo :memo :user_channel_access :user_channel_access :invitation-code :invitation-code :invitation-code))
        (foreign-key-columns '(:memo_id :user_id :archive_id :creator_id :archive_id :user_id :channel_id :source_user_id :used_by_user_id)))
    ;;
    ;; rename uuid column to id
    (mapc (lambda (table-name)
            (mito:execute-sql
             (alter-table table-name
               (rename-column :uuid :id))))
          '(:user :channel :memo))
    ;;
    ;; drop foriegn keys
    (mapc (lambda (table-name reference-column-name)
            (mito:execute-sql
             (format nil "ALTER TABLE ~(~a~) ALTER COLUMN DROP CONSTRAINT ~(~a~)_~(~a~)_fkey"
                     table-name
                     (or constraint-table-name
                         table-name)
                     reference-column-name)))
          foreign-key-tables
          '(nil nil nil nil :user_archive_access :user_archive_access nil nil nil)
          foreign-key-columns)
    ;;
    ;; change :uuid type to varchar(36)
    (mapc (lambda (table-name column-name)
            (mito:execute-sql
             (alter-table table-name
               (alter-column column-name :type '(:varchar 36)))))
          (append '(:user :channel :memo)
                  foreign-key-tables)
          (append '(:id :id :id)
                  foreign-key-columns))
    ;;
    ;; re-construct foreign key constraints
    (mapc (lambda (table-name column-name)
            (let ((reference-table-name (or (case column-name
                                              (:memo_id :memo)
                                              ((:archive_id :channel_id) :channel)
                                              ((:user_id :creator_id :source_user_id :used_by_user_id) :user))
                                            (error "Column: ~(~a~) doesn't have corresponding reference-table-name" column-name))))
              (mito:execute-sql
               (format nil "ALTER TABLE ~(~a~) ADD ~a"
                       table-name
                       (yield
                        (foreign-key `(,column-name) :references `(,reference-table-name :id))))))
            foreign-key-tables
            foreign-key-columns))))

(defun id-and-primary-key-compatibility/downgrade (migration-driver)
  "Alter all tables' id column names back to uuid, and alter the types back to uuid")
