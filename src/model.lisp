(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :ackfock.db :datafly :sxql))
(in-package :ackfock.model)

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
