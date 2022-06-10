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
           #:send-memo
           #:create-authentication-code
           #:authenticate-user-email))
(in-package :ackfock.model)

(defconstant +dummy-uuid+ :A2543078-7D5B-4F40-B6FD-DBC58E863752)

(defun dummy-uuid ()
  (string +dummy-uuid+))

(defun string-to-ackfock (string)
  "If the STRING is \"ACK\" or \"FOCK\", this function returns :ACK or :FOCK. All the other cases it returns nil"
  (and (stringp string)
       (member string
               '("ACK" "FOCK")
               :test #'string=)
       (alexandria:make-keyword string)))

(defmacro defun-with-user-id-bind-from-token (name lambda-list &body body)
  (let ((lambda-list (cons 'user-token lambda-list)))
    `(defun-with-db-connection ,name ,lambda-list
       (handler-case
           ;; race condition notice below!
           (let* ((user-id (user-uuid (user-by-user-token user-token))))
             ,@body)
         (type-error () nil)))))

(defun-with-user-id-bind-from-token new-memo (archive-id content)
  (execute
   (if (retrieve-one
        (select :*
          (from :user_archive_access)
          (where #.(utils-ackfock:ensure-plist '(:=
                                                 user-id
                                                 archive-id)))))
       (insert-into :memo
         #.(utils-ackfock:ensure-plist '(:=
                                         :creator_id user-id
                                         content
                                         archive-id)))
       (insert-into :memo
         #.(utils-ackfock:ensure-plist '(:=
                                         :creator_id user-id
                                         content))))))

(defun-with-user-id-bind-from-token new-archive (archive-name)
  (let ((archive-id (retrieve-one
                     (insert-into :archive
                       (set= :name archive-name)
                       (returning :uuid)))))
    (execute
     (insert-into :user_archive_access
       #.(utils-ackfock:ensure-plist '(set=
                                       user-id
                                       archive-id))))
    archive-id))

(defun-with-db-connection invite-to-archive (source-user-token target-user-email archive-id))

(defun-with-db-connection add-memo-to-archive (source-user-token memo-id archive-id))

(defun-with-db-connection memo-user-ackfocks (user-token memo-id))

(defun-with-db-connection ackfock-memo (user-token memo-id ackfock))

(defun-with-db-connection user-by-user-token (user-token)
  (retrieve-one
   (select :*
     (from :users)
     (where (:= :token user-token)))
   :as 'user))
