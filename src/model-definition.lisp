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
                (:inflate target-user-ackfock #'string-to-ackfock)
                ;; only one user, but :has-one doesn't support query from table with different names
                (:has-many (source-users-func user)
                           (select :*
                             (from :users)
                             (where (:= :uuid source-user-id))
                             (limit 1)))
                ;; only one user, but :has-one doesn't support query from table with different names
                (:has-many (target-users-func user)
                           (select :*
                             (from :users)
                             (where (:= :uuid (or target-user-id :null)))
                             (limit 1))))
  uuid
  content
  source-user-id
  target-user-id
  (source-user-ackfock nil :type ackfock)
  (target-user-ackfock nil :type ackfock)
  created-at)

(defun memo-source-user (memo)
  (with-connection (db)
    ;; only one user, but :has-one doesn't support query from table with different names
    (first (memo-source-users-func memo))))

(defun memo-target-user (memo)
  (with-connection (db)
    ;; only one user, but :has-one doesn't support query from table with different names
    (first (memo-target-users-func memo))))
