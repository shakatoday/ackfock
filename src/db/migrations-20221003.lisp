(in-package ackfock.db.migrations)

(let ((table-names '(:user_ackfock :user_channel_access :invitation_code :invitation_code))
      (old-column-names '(:user_id :user_id :source_user_id :used_by_user_id))
      (new-column-names '(:account_id :account_id :source_account_id :used_by_account_id)))
  (define-migration-handler rename-all-column-names-containing-\"user\"-to-\"account\"/upgrade
    (mapc (lambda (table-name old-column-name new-column-name)
            (mito:execute-sql
             (alter-table table-name
               (rename-column old-column-name new-column-name))))
          table-names
          old-column-names
          new-column-names))

  (define-migration-handler rename-all-column-names-containing-\"user\"-to-\"account\"/downgrade
    (mapc (lambda (table-name old-column-name new-column-name)
            (mito:execute-sql
             (alter-table table-name
               (rename-column old-column-name new-column-name))))
          table-names
          new-column-names
          old-column-names)))

(define-migration-handler rename-table-name-containing-\"user\"-to-\"account\"/upgrade
  (mito:execute-sql
   (alter-table :user_ackfock
     (rename-to :account_ackfock)))
  (mito:execute-sql
   (alter-table :user_channel_access
     (rename-to :account_channel_access))))

(define-migration-handler rename-table-name-containing-\"user\"-to-\"account\"/downgrade
  (mito:execute-sql
   (alter-table :account_ackfock
     (rename-to :user_ackfock)))
  (mito:execute-sql
   (alter-table :account_channel_access
     (rename-to :user_channel_access))))
