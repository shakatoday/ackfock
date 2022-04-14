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
           #:memo-target-user-ackfock))
(in-package :ackfock.model-definition)

(deftype ackfock () '(member :ACK :FOCK nil)) ; the enum type in DB uses uppercase. we capitalize :ACK :FOCK as a reminder even if symbols in CL are uppercase by default.

(defun string-to-ackfock (string)
  "If the STRING is \"ACK\" or \"FOCK\", this function returns :ACK or :FOCK. All the other cases it returns nil"
  (and (stringp string)
       (member string
               '("ACK" "FOCK")
               :test #'string=)
       (alexandria:make-keyword string)))

(defmodel (user (:inflate created-at #'datetime-to-timestamp))
  uuid
  email
  username
  created-at)

(defmodel (memo (:inflate created-at #'datetime-to-timestamp)
                (:inflate source-user-ackfock #'string-to-ackfock)
                (:inflate target-user-ackfock #'string-to-ackfock))
                                        ; :has-one doesn't support query from table with different names, so we'll defun memo-source-user and memo-target-user after defmodel
  uuid
  content
  source-user-id
  target-user-id
  (source-user-ackfock nil :type ackfock)
  (target-user-ackfock nil :type ackfock)
  created-at)

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

