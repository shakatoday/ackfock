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
  '(:memo_id :user_id :archive_id :creator_id :parent_memo_id :archive_id :user_id :channel_id :source_user_id :used_by_user_id)
  "The foreign key constraint names have migration history side. Thus, the name archive_id is here rather than channel_id")

(defmacro define-migration-handler (name &body body)
  (let ((docstring (and (stringp (first body))
                        (pop body))))
    `(defun ,name (migration-driver)
       ,@(serapeum:unsplice docstring)
       (let ((mito:*connection* (migratum.driver.dbi:dbi-driver-connection migration-driver)))
         ,@body))))

(define-migration-handler rename-user-table-for-using-mito/upgrade
  "To change users to user. And ensuring it's letter case is compatible"
  (mito:execute-sql
   (alter-table :users
     (rename-to :account))))

(define-migration-handler rename-user-table-for-using-mito/downgrade
  "To change table user back to users."
  (mito:execute-sql
   (alter-table :account
     (rename-to :users))))

(define-migration-handler rename-all-db-object-names-from-user-to-account/upgrade)

(define-migration-handler rename-all-db-object-names-from-user-to-account/downgrade)

(define-migration-handler id-and-primary-key-compatibility/upgrade
  "Alter all tables' uuid column names to id, and alter types to varchar(36)."
  ;;
  ;; rename uuid column to id
  (mapc (lambda (table-name)
          (mito:execute-sql
           (alter-table table-name
             (rename-column :uuid :id))))
        '(:account :channel :memo))
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
        (append '(:account :channel :memo)
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
                                             ((:user_id :creator_id :source_user_id :used_by_user_id) :account))
                                           (error "Column: ~(~a~) doesn't have corresponding reference-table-name" column-name))))
            (mito:execute-sql
             (format nil "ALTER TABLE ~(~a~) ADD ~a"
                     table-name
                     (yield
                      (foreign-key `(,column-name) :references `(,reference-table-name :id)))))))
        *foreign-key-tables*
        *foreign-key-columns*))

(define-migration-handler id-and-primary-key-compatibility/downgrade
  "Alter all tables' id column names back to uuid, and alter the types back to uuid"

  ;;
  ;; drop foreign key constraints
  (mapc (lambda (table-name reference-column-name)
          (mito:execute-sql
           (format nil "ALTER TABLE ~(~a~) DROP CONSTRAINT ~(~a~)_~(~a~)_fkey"
                   table-name
                   table-name
                   reference-column-name)))
        *foreign-key-tables*
        (substitute :channel_id
                    :archive_id
                    *foreign-key-columns*))
  ;;
  ;; change varchar(36) type to :uuid
  (mapc (lambda (table-name column-name)
          (mito:execute-sql
           (alter-table table-name
             (alter-column column-name :type :uuid))))
        (append '(:account :channel :memo)
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
                                             ((:user_id :creator_id :source_user_id :used_by_user_id) :account))
                                           (error "Column: ~(~a~) doesn't have corresponding reference-table-name" column-name))))
            (mito:execute-sql
             (format nil "ALTER TABLE ~(~a~) ADD ~a"
                     table-name
                     (yield
                      (foreign-key `(,column-name) :references `(,reference-table-name :id)))))))
        *foreign-key-tables*
        *foreign-key-columns*)
  ;;
  ;; rename constraints for migration history side effect
  (mapc (lambda (table-name column-name)
          (let ((historical-table-name (when (eq table-name :user_channel_access)
                                         :user_archive_access))
                (constraint-column-name-changed-after-upgrade (when (eq column-name :archive_id)
                                                                :channel_id)))
            (mito:execute-sql
             (format nil "ALTER TABLE ~(~a~) RENAME CONSTRAINT ~(~a~)_~(~a~)_fkey to ~(~a~)_~(~a~)_fkey"
                     table-name
                     table-name
                     (or constraint-column-name-changed-after-upgrade
                         column-name)
                     (or historical-table-name
                         table-name)
                     column-name))))
        '(:memo :user_channel_access :user_channel_access)
        '(:archive_id :archive_id :user_id))
  ;;
  ;; rename id column to uuid
  (mapc (lambda (table-name)
          (mito:execute-sql
           (alter-table table-name
             (rename-column :id :uuid))))
        '(:account :channel :memo)))
