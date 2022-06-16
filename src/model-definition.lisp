(in-package :cl-user)
(defpackage ackfock.model-definition
  (:use :cl :ackfock.db :datafly :sxql)
  (:export #:ackfock
           #:user-ackfock
           #:user-ackfock-user
           #:user-ackfock-ackfock
           #:make-user-ackfock
           #:user
           #:user-uuid
           #:user-email
           #:user-username
           #:user-p
           #:make-user
           #:user-channels
           #:user-private-memos
           #:channel
           #:channel-uuid
           #:channel-name
           #:channel-users
           #:channel-memos
           #:memo
           #:memo-uuid
           #:memo-content
           #:memo-channel
           #:memo-channel-id
           #:memo-creator
           #:memo-creator-id
           #:authentication-code-code
           #:authentication-code-email
           #:authentication-code-valid-until
           #:authentication-code))
(in-package :ackfock.model-definition)

(deftype ackfock () '(member :ACK :FOCK)) ; the enum type in DB uses uppercase. we capitalize :ACK :FOCK as a reminder even if symbols in CL are uppercase by default.

;; memo-user-ackfocks should be a function in model.lisp
(defstruct user-ackfock
  user ackfock created-at)

(defmodel (user (:inflate created-at #'datetime-to-timestamp))
  uuid
  email
  username
  created-at)

(defmodel (channel)
  uuid
  name)

(defmodel (memo (:inflate created-at #'datetime-to-timestamp))
  uuid
  content
  creator-id
  channel-id
  created-at)

(defmodel (authentication-code (:inflate created-at #'datetime-to-timestamp)
                               (:inflate valid-until #'datetime-to-timestamp))
  email
  code
  created-at
  valid-until)

(defun-with-db-connection user-channels (user)
  (retrieve-all
   (select :*
     (from :channel)
     (inner-join (:as (select :*
                        (from :user_channel_access)
                        (where (:= :user_channel_access.user_id (user-uuid user))))
                  :user_channel_access_join)
                 :on (:= :channel.uuid :user_channel_access_join.channel_id))
     (order-by (:desc :user_channel_access_join.created_at)))
   :as 'channel))

(defun-with-db-connection user-private-memos (user)
  (retrieve-all
   (select :*
     (from :memo)
     (where (:and (:= :memo.creator_id (user-uuid user))
                  (:is-null :memo.channel_id))))
   :as 'memo))

(defun-with-db-connection channel-users (channel)
  (retrieve-all
   (select :*
     (from :users)
     (inner-join (:as (select :*
                        (from :user_channel_access)
                        (where (:= :user_channel_access.channel_id (channel-uuid channel))))
                  :user_channel_access_join)
                 :on (:= :users.uuid :user_channel_access_join.user_id))
     (order-by (:desc :user_channel_access_join.created_at)))
   :as 'user))

(defun-with-db-connection channel-memos (channel)
  (retrieve-all
   (select :*
     (from :memo)
     (where (:= :memo.channel_id (channel-uuid channel))))
   :as 'memo))

(defun-with-db-connection memo-channel (memo)
  (when (memo-channel-id memo)
    (retrieve-one
     (select :*
       (from :channel)
       (where (:= :channel.uuid (memo-channel-id memo))))
     :as 'channel)))

(defun-with-db-connection memo-creator (memo)
  (retrieve-one
   (select :*
     (from :users)
     (where (:= :users.uuid (memo-creator-id memo))))
   :as 'user))
