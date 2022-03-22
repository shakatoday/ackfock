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
           #:memo-target-user-id
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
                (:inflate target-user-ackfock #'string-to-ackfock)
                (:has-a (source-user-func user)
                        (where (:= :uuid source-user-id)))
                (:has-a (target-user-func user)
                        (where (:= :uuid (or target-user-id :null)))))
  uuid
  content
  source-user-id
  target-user-id
  (source-user-ackfock nil :type ackfock)
  (target-user-ackfock nil :type ackfock)
  created-at)

(defun memo-source-user (memo)
  (with-connection (db)
    (memo-source-user-func memo)))

(defun memo-target-user (memo)
  (with-connection (db)
    (memo-target-user-func memo)))
