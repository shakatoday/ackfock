(in-package :cl-user)
(defpackage ackfock.model-definition
  (:use :cl :ackfock.db :datafly :sxql)
  (:export #:ackfock
           #:user-ackfock
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
           #:authentication-code-code
           #:authentication-code-email
           #:authentication-code-valid-until
           #:authentication-code))
(in-package :ackfock.model-definition)

(deftype ackfock () '(member :ACK :FOCK)) ; the enum type in DB uses uppercase. we capitalize :ACK :FOCK as a reminder even if symbols in CL are uppercase by default.

;; memo-user-ackfocks should be a function in model.lisp
(defstruct user-ackfock
  user ackfock created-at)

(defmodel (user (:inflate created-at #'datetime-to-timestamp)
                (:has-many (channels channel)
                           (select :*
                             (from :channel)
                             (inner-join (:as (select :*
                                                (from :user_channel_access)
                                                (where (:= :user_channel_access.user_id uuid)))
                                          :user_channel_access_join)
                                         :on (:= :channel.uuid :user_channel_access_join.channel_id))
                             (order-by (:desc :user_channel_access_join.created_at))))
                (:has-many (private-memos memo)
                           (select :*
                             (from :memo)
                             (where (:and (:= :memo.user_id uuid)
                                          (:= :memo.channel_id :null))))))
  uuid
  email
  username
  created-at)

(defmodel (channel (:has-many (users user)
                              (select :*
                                (from :users)
                                (inner-join (:as (select :*
                                                   (from :user_channel_access)
                                                   (where (:= :user_channel_access.channel_id uuid)))
                                             :user_channel_access_join)
                                            :on (:= :users.uuid :user_channel_access_join.user_id))
                                (order-by (:desc :user_channel_access_join.created_at))))
                   (:has-many (memos memo)
                              (select :*
                                (from :memo)
                                (where (:= :memo.channel_id (or uuid :null))))))
  uuid
  name)

(defmodel (memo (:inflate created-at #'datetime-to-timestamp)
                (:has-a channel (where (:= :uuid channel-id))))
  uuid
  content
  channel-id
  created-at)

(defmodel (authentication-code (:inflate created-at #'datetime-to-timestamp)
                               (:inflate valid-until #'datetime-to-timestamp))
  email
  code
  created-at
  valid-until)
