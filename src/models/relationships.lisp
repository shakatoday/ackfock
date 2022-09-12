(in-package :cl-user)
(defpackage ackfock.model.relationships
  (:use :cl :datafly :sxql :ackfock.db :ackfock.model)
  (:export #:user-channels
           #:user-private-memos
           #:channel-users
           #:channel-memos
           #:memo-creator
           #:memo-channel
           #:memo-parent-memo
           #:has-access-p))
(in-package :ackfock.model.relationships)

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

(defgeneric has-access-p (user model-obj))

(defmethod has-access-p ((user user) (model-obj channel))
  (let ((channel-id (channel-uuid model-obj))
        (user-id (user-uuid user)))
    (when (and channel-id
               user-id)
      (retrieve-one
       (select :*
         (from :user_channel_access)
         (where (:and (:= :user_id user-id)
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
