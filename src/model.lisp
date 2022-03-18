(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :ackfock.db :datafly :sxql)
  (:export #:new-user
           #:user-memos
           #:authenticate
           #:user-email
           #:memo
           #:memo-target-user-id
           #:memo-target-user
           #:memo-content
           #:memo-source-user-ackfock
           #:memo-target-user-ackfock))
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

(defmacro defun-with-db-connection (name lambda-list &body body)
  "Define a function by DEFUN and put the BODY inside (WITH-CONNECTION (DB)). Doc-string will be safely processed."
  (let* ((doc-string-list (when (and (stringp (first body))
                                     (> (length body) 1))
                            (list (first body))))
         (body (if (null doc-string-list)
                   body
                   (subseq body 1))))
    `(defun ,name ,lambda-list
       ,@doc-string-list
       (with-connection (db)
         ,@body))))

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
     (where (:or (:= :source_user_id (cond ((null as-source-user) +DUMMY-UUID+) ; for uuid type consistency in postgresql
                                           (t (user-uuid user))))
                 (:= :target_user_id (cond ((null as-target-user) +DUMMY-UUID+) ; for uuid type consistency in postgresql
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
