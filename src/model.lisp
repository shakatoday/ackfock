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
           #:get-user-by-email
           #:send-memo))
(in-package :ackfock.model)

(defconstant +dummy-uuid+ :A2543078-7D5B-4F40-B6FD-DBC58E863752)

(defun dummy-uuid ()
  (string +dummy-uuid+))

(defun-with-db-connection new-user (email username password)
  "Insert a new user into database and return an ACKFOCK.MODEL::USER instance"
  (retrieve-one 
   (insert-into :users
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
                      (from :users)
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
  (unless (or (str:emptyp ackfock) (null (ackfock.utils:current-user)))
    (execute
     (update :memo
       (set= (if as-target-user-ackfock
                 :target_user_ackfock
                 :source_user_ackfock)
             ackfock)
       (where (:and (:= :uuid memo-uuid)
                    (:or (:= :source_user_id (user-uuid (ackfock.utils:current-user)))
                         (:= :target_user_id (user-uuid (ackfock.utils:current-user))))))))))

(defun-with-db-connection send-memo (memo-uuid recipient)
  (execute
   (update :memo
     (set= :target_user_id (user-uuid recipient))
     (where (:and (:= :uuid memo-uuid)
                  (:= :source_user_id (user-uuid (ackfock.utils:current-user))))))))

(defun-with-db-connection get-user-by-email (email)
  (retrieve-one
   (select :*
     (from :users)
     (where (:= :email email)))
   :as 'user))

(defun-with-db-connection create-authentication-code (email &key (ttl-in-sec (* 60 60))) ; by default code will expire in 1 hour
  "Create an authentication code for EMAIL with TTL-IN-SEC, insert it into database, and return an AUTHENTICATION-CODE object if success."
  (let ((valid-until (local-time:timestamp+ (local-time:now)
                                            ttl-in-sec
                                            :sec))
        (code (uuid:print-bytes nil (uuid:make-v4-uuid))))
    (retrieve-one
     (insert-into :authentication_code
       (set= :email email
             :code code
             :valid_until valid-until)
       (returning :*))
     :as 'authentication-code)))
