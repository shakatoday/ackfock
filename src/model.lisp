(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :datafly :sxql :ackfock.model-definition)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:new-user
           #:user-memos
           #:authenticate
           #:new-memo
           #:ackfock-memo
           #:*current-user*
           #:get-user-by-email
           #:send-memo))
(in-package :ackfock.model)

(defconstant +DUMMY-UUID+ :A2543078-7D5B-4F40-B6FD-DBC58E863752)
(defvar *current-user*) ; where should this variable be?

(defun dummy-uuid ()
  (string +DUMMY-UUID+))

(defun-with-db-connection new-user (email username password)
  "Insert a new user into database and return an ACKFOCK.MODEL::USER instance"
  (retrieve-one 
   (insert-into :user
     (set= :email email
           :username username
           :password_salted (cl-pass:hash password))
     (returning :*))
   :as 'user))

(defun-with-db-connection user-memos (user &key (as-source-user t) as-target-user (limit 20) (offset 0))
  (retrieve-all
   (select :*
     (from :memo)
     (where (:or (:= :source_user_id (cond ((null as-source-user) (dummy-uuid)) ; for uuid type consistency in postgresql
                                           (t (user-uuid user))))
                 (:= :target_user_id (cond ((null as-target-user) (dummy-uuid)) ; for uuid type consistency in postgresql
                                           (t (user-uuid user))))))
     (order-by (:desc :updated_at))
     (limit limit)
     (offset offset))
   :as 'memo))

(defun-with-db-connection authenticate (email password)
  (let ((user-data (retrieve-one
                    (select :*
                      (from :user)
                      (where (:= :email email))))))
    (and user-data
         (cl-pass:check-password password
                                 (getf user-data :password-salted))
         (apply #'make-user
                user-data))))

(defun-with-db-connection new-memo (user content)
  (when (user-p user)
    (execute
     (insert-into :memo
       (set= :source_user_id (user-uuid user)
             :content content)))))

(defun-with-db-connection ackfock-memo (memo-uuid ackfock &key as-target-user-ackfock)
  "Ackfock the memo with the given MEMO-UUID. This memo has to be either created by current user or shared by the creator."
  (unless (or (str:emptyp ackfock) (null *current-user*))
    (execute
     (update :memo
       (set= (if as-target-user-ackfock
                 :target_user_ackfock
                 :source_user_ackfock)
             ackfock)
       (where (:and (:= :uuid memo-uuid)
                    (:or (:= :source_user_id (user-uuid *current-user*))
                         (:= :target_user_id (user-uuid *current-user*)))))))))

(defun-with-db-connection send-memo (memo-uuid recipient)
  (execute
   (update :memo
     (set= :target_user_id (user-uuid recipient))
     (where (:and (:= :uuid memo-uuid)
                  (:= :source_user_id (user-uuid *current-user*)))))))

(defun-with-db-connection get-user-by-email (email)
  (retrieve-one
   (select :*
     (from :user)
     (where (:= :email email)))
   :as 'user))
