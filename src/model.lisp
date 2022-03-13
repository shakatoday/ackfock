(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :ackfock.db :datafly :sxql)
  (:export #:new-user
           #:user-memos))
(in-package :ackfock.model)

(defconstant +DUMMY-UUID+ "A2543078-7D5B-4F40-B6FD-DBC58E863752")

(defmodel (user (:inflate created-at #'datetime-to-timestamp))
  uuid
  email
  username
  created-at)

(defmodel (memo (:inflate created-at #'datetime-to-timestamp)
                (:has-a (source-user user)
                        (where (:= :uuid source-user-id)))
                (:has-a (target-user user)
                        (where (:= :uuid (or target-user-id :null)))))
  content
  source-user-id
  target-user-id
  source-user-ackfock
  target-user-ackfock
  created-at)

(defun new-user (email username password)
  "Insert a new user into database and return an ACKFOCK.MODEL::USER instance"
  (with-connection (db)
    (retrieve-one 
     (insert-into :user
       (set= :email email
             :username username
             :password_salted (cl-pass:hash password))
       (returning :*))
     :as 'user)))

(defmethod user-memos ((user user) &key (as-source-user t) as-target-user (limit 20) (offset 0))
  (with-connection (db)
    (retrieve-all
     (select :*
       (from :memo)
       (where (:or (:= :source_user_id (case as-source-user
                                         ((t) (user-uuid user))
                                         ((nil) +DUMMY-UUID+))) ; for uuid type consistency in postgresql
                   (:= :target_user_id (case as-target-user
                                         ((t) (user-uuid user))
                                         ((nil) +DUMMY-UUID+))))) ; for uuid type consistency in postgresql
       (order-by (:desc :updated_at))
       (limit limit)
       (offset offset))
     :as 'memo)))
