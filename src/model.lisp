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

(defun-with-db-connection new-memo (user archive content))

(defun-with-db-connection new-archive (user))

(defun-with-db-connection invite-to-archive (source-user target-user))

(defun-with-db-connection memo-user-ackfocks (memo))

(defun-with-db-connection get-user-by-email (email)
  (retrieve-one
   (select :*
     (from :users)
     (where #.(ackfock.utils:ensure-plist '(:= email))))
   :as 'user))
