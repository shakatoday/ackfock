(in-package :cl-user)
(defpackage ackfock.db.migrations
  (:use :cl :sxql)
  (:export #:rename-user-table-for-using-mito/downgrade
           #:rename-user-table-for-using-mito/upgrade
           #:id-and-primary-key-compatibility/upgrade
           #:id-and-primary-key-compatibility/downgrade))
(in-package :ackfock.db.migrations)

(defparameter *foreign-key-tables*
  '(:user_ackfock :user_ackfock :memo :memo :memo :user_channel_access :user_channel_access :invitation_code :invitation_code :invitation_code))

(defparameter *foreign-key-columns*
  '(:memo_id :user_id :archive_id :creator_id :parent_memo_id :archive_id :user_id :channel_id :source_user_id :used_by_user_id))

(defun rename-user-table-for-using-mito/upgrade (migration-driver)
  "To change users to user. And ensuring it's letter case is compatible"
  (let ((mito:*connection* (migratum.driver.dbi:dbi-driver-connection migration-driver)))
    (mito:execute-sql
     (alter-table :users
       (rename-to :account)))))

(defun rename-user-table-for-using-mito/downgrade (migration-driver)
  "To change table user back to users."
  (let ((mito:*connection* (migratum.driver.dbi:dbi-driver-connection migration-driver)))
    (mito:execute-sql
     (alter-table :account
       (rename-to :users)))))

(defun id-and-primary-key-compatibility/upgrade (migration-driver)
  "Alter all tables' uuid column names to id, and alter types to varchar(36)."
  (let ((mito:*connection* (migratum.driver.dbi:dbi-driver-connection migration-driver)))
    ;;
    ;; rename uuid column to id
    (mapc (lambda (table-name)
            (mito:execute-sql
             (alter-table table-name
               (rename-column :uuid :id))))
          '(:user :channel :memo))
    ;;
    ;; drop foriegn keys
    (mapc (lambda (table-name constraint-table-name reference-column-name)
            (mito:execute-sql
             (format nil "ALTER TABLE ~(~a~) DROP CONSTRAINT ~(~a~)_~(~a~)_fkey"
                     table-name
                     (or constraint-table-name
                         table-name)
                     reference-column-name)))
          *foreign-key-tables*
          '(nil nil nil nil nil :user_archive_access :user_archive_access nil nil nil)
          *foreign-key-columns*)
    ;;
    ;; change :uuid type to varchar(36)
    (mapc (lambda (table-name column-name)
            (mito:execute-sql
             (alter-table table-name
               (alter-column column-name :type '(:varchar 36)))))
          (append '(:user :channel :memo)
                  *foreign-key-tables*)
          (append '(:id :id :id)
                  (substitute :channel_id
                              :archive_id
                              *foreign-key-columns*)))
    ;;
    ;; re-construct foreign key constraints
    (mapc (lambda (table-name column-name)
            (let* ((column-name (if (eq column-name :archive_id)
                                    :channel_id
                                    column-name))
                   (reference-table-name (or (case column-name
                                              ((:memo_id :parent_memo_id) :memo)
                                              (:channel_id :channel)
                                              ((:user_id :creator_id :source_user_id :used_by_user_id) :user))
                                            (error "Column: ~(~a~) doesn't have corresponding reference-table-name" column-name))))
              (mito:execute-sql
               (format nil "ALTER TABLE ~(~a~) ADD ~a"
                       table-name
                       (yield
                        (foreign-key `(,column-name) :references `(,reference-table-name :id)))))))
          *foreign-key-tables*
          *foreign-key-columns*)))

(defun id-and-primary-key-compatibility/downgrade (migration-driver)
  "Alter all tables' id column names back to uuid, and alter the types back to uuid")
