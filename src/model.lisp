(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :ackfock.db :datafly :sxql)
  (:export #:ackfock
           #:user-ackfock
           #:user-ackfock-user
           #:user-ackfock-ackfock
           #:user-ackfock-created-at
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
           #:channel-p
           #:channel-uuid
           #:channel-name
           #:channel-users
           #:channel-memos
           #:make-private-channel
           #:private-channel-p
           #:memo
           #:memo-p
           #:make-memo
           #:memo-uuid
           #:memo-content
           #:memo-channel
           #:memo-channel-id
           #:memo-creator
           #:memo-creator-id
           #:memo-parent-memo-id
           #:activation-code-code
           #:activation-code-email
           #:activation-code-valid-until
           #:activation-code
           #:memo-parent-memo
           #:invitation-code
           #:invitation-code-code
           #:invitation-code-valid-until
           #:invitation-code-used-by-user-id
           #:invitation-code-channel-id
           #:user-from-plist
           #:has-access-p))
(in-package :ackfock.model)

(deftype ackfock () '(member :ACK :FOCK)) ; the enum type in DB uses uppercase. we capitalize :ACK :FOCK as a reminder even if symbols in CL are uppercase by default.

(defstruct user-ackfock
  user
  (ackfock nil :type (or ackfock nil))
  created-at)

(defmodel (user (:inflate created-at #'datetime-to-timestamp))
  uuid
  email
  username
  created-at)

(defmodel (channel)
  uuid
  name)

(defun make-private-channel ()
  (make-channel :uuid nil
                :name "My private memos"))

(defun private-channel-p (channel)
  (null (channel-uuid channel)))

(defmodel (memo (:inflate created-at #'datetime-to-timestamp))
  uuid
  content
  (as-an-update nil :type boolean)
  creator-id
  parent-memo-id
  channel-id
  created-at)

(defmodel (activation-code (:inflate created-at #'datetime-to-timestamp)
                           (:inflate valid-until #'datetime-to-timestamp))
  email
  code
  created-at
  valid-until)

(defmodel (invitation-code (:inflate created-at #'datetime-to-timestamp)
                           (:inflate valid-until #'datetime-to-timestamp)
                           (:has-a channel (where (:= :uuid channel-id))))
  code
  source-user-id
  used-by-user-id
  channel-id
  created-at
  valid-until)

(defmacro user-from-plist (plist)
  (cons 'make-user
        (reduce #'append
                (mapcar (lambda (keyword-arg)
                          `(,keyword-arg (getf ,plist ,keyword-arg))) ; use the later created-at so we need a copy and remf
                                        ; TODO: created-at needs an inflation-function
                        '(:uuid :email :username :created-at)))))

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
                  (:is-null :memo.channel_id)))
     (order-by (:asc :created_at)))
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
     (where (:= :memo.channel_id (channel-uuid channel)))
     (order-by (:asc :created_at)))
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

(defun-with-db-connection memo-parent-memo (memo)
  (when (memo-parent-memo-id memo)
    (retrieve-one
     (select :*
       (from :memo)
       (where (:= :memo.uuid (memo-parent-memo-id memo))))
     :as 'memo)))

(defmethod has-access-p ((user user) (model-obj channel))
  (let ((channel-id (channel-uuid model-obj))
        (user-id (user-uuid user)))
    (when (and channel-id
               user-id)
      (retrieve-one
       (select :*
         (from :user_channel_access)
         (where (:and (:= :user_id user-id )
                      (:= :channel_id channel-id))))))))

(defmethod has-access-p ((user user) (model-obj memo))
  (when (and (user-uuid user)
             (memo-uuid model-obj))
    (or (string= (memo-creator-id model-obj) (user-uuid user))
        (retrieve-one
         (select :*
           (from :user_channel_access)
           (inner-join :memo
                       :on (:= :user_channel_access.channel_id :memo.channel_id))
           (where (:and (:= :memo.uuid (memo-uuid model-obj)) ; TODO: handling type error
                        (:= :user_id (user-uuid user))))))))) ; TODO: handling type error
