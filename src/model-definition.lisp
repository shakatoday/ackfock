(in-package :cl-user)
(defpackage ackfock.model-definition
  (:use :cl :ackfock.db :datafly :sxql)
  (:export #:user
           #:user-uuid
           #:user-email
           #:user-username
           #:user-p
           #:make-user
           #:memo
           #:memo-uuid
           #:memo-source-user
           #:memo-target-user
           #:memo-content
           #:memo-source-user-ackfock
           #:memo-target-user-ackfock
           #:authentication-code-code
           #:authentication-code-email
           #:authentication-code-valid-until
           #:authentication-code))
(in-package :ackfock.model-definition)

(deftype ackfock () '(member :ACK :FOCK)) ; the enum type in DB uses uppercase. we capitalize :ACK :FOCK as a reminder even if symbols in CL are uppercase by default.

(defun string-to-ackfock (string)
  "If the STRING is \"ACK\" or \"FOCK\", this function returns :ACK or :FOCK. All the other cases it returns nil"
  (and (stringp string)
       (member string
               '("ACK" "FOCK")
               :test #'string=)
       (alexandria:make-keyword string)))

(defmodel (user (:inflate created-at #'datetime-to-timestamp)
                (:has-many (archives archive)
                           (select :*
                             (from :archive)
                             (inner-join (:as (select :*
                                                (from :user_archive_access)
                                                (where (:= :user_archive_access.user_id uuid)))
                                          :user_archive_access_join)
                                         :on (:= :archive.uuid :user_archive_access_join.archive_id))
                             (order-by (:desc :user_archive_access_join.created_at))))
                (:has-many (private-memos memo)
                           (select :*
                             (from :memo)
                             (where (:and (:= :memo.user_id uuid)
                                          (:= :memo.archive_id :null))))))
  uuid
  email
  username
  created-at)

(defmodel (archive (:has-many (users user)
                              (select :*
                                (from :users)
                                (inner-join (:as (select :*
                                                   (from :user_archive_access)
                                                   (where (:= :user_archive_access.archive_id uuid)))
                                             :user_archive_access_join)
                                            :on (:= :users.uuid :user_archive_access_join.user_id))
                                (order-by (:desc :user_archive_access_join.created_at))))
                   (:has-many (memos memo)
                              (select :*
                                (from :memo)
                                (where (:= :memo.archive_id uuid)))))
  uuid
  name)

(defmodel (memo (:inflate created-at #'datetime-to-timestamp)
                (:has-a archive (where (:= :uuid archive-id))))
  uuid
  content
  archive-id
  created-at)

;; memo-user-ackfocks should be a function in model.lisp

;; defmodel :has-one doesn't support query from table with different names, so we have to defun memo-source-user and memo-target-user
(defun-with-db-connection memo-source-user (memo)
  (retrieve-one
   (select :*
     (from :users)
     (where (:= :uuid (memo-source-user-id memo))))
   :as 'user))

;; defmodel :has-one doesn't support query from table with different names, so we have to defun memo-source-user and memo-target-user
(defun-with-db-connection memo-target-user (memo)
  (retrieve-one
   (select :*
     (from :users)
     (where (:= :uuid (or (memo-target-user-id memo)
                          :null))))
   :as 'user))

(defmodel (authentication-code (:inflate created-at #'datetime-to-timestamp)
                               (:inflate valid-until #'datetime-to-timestamp))
  email
  code
  created-at
  valid-until)
