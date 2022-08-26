(in-package :cl-user)
(defpackage ackfock.features
  (:use :cl :ackfock.db :datafly :sxql)
  (:export #:new-memo
           #:reply-memo
           #:new-channel
           #:rename-channel
           #:invite-to-channel
           #:memo-latest-ackfocks-per-user-by-ackfock
           #:ackfock-memo
           #:memo-user-ackfocks))
(in-package :ackfock.features)

(defparameter *memo-latest-ackfocks-per-user-sql-query*
  (rutils.string:read-file (merge-pathnames #p"db/memo_latest_ackfocks_per_user.sql"
                                            ackfock.config:*application-root*)))

(defmacro defun-with-db-connection-and-current-user-uuid (name lambda-list &body body)
  "Define a function by DEFUN and put the BODY inside (WITH-CONNECTION (DB)). Bound CURRENT-USER-ID to (ackfock.model:user-uuid ackfock.feature.auth:*current-user*). Check the source to see how docstring is extracted"
  (let ((docstring (when (stringp (first body))
                     (pop body))))
    `(defun ,name ,lambda-list
       ,@(serapeum:unsplice docstring)
       (ackfock.db:with-connection (ackfock.db:db)
         (let ((current-user-id (ackfock.model:user-uuid ackfock.feature.auth:*current-user*)))
           ,@body)))))

(defun-with-db-connection-and-current-user-uuid new-memo (channel content)
  (execute
   (if (ackfock.model:private-channel-p channel)
       (insert-into :memo
         (set= :creator_id current-user-id
               :content content))
       (let ((channel-id (ackfock.model:channel-uuid channel)))
         (when (ackfock.feature.auth:current-user-has-access-p channel)
           (insert-into :memo
             (set= :creator_id current-user-id
                   :content content
                   :channel_id channel-id)))))))

(defun-with-db-connection-and-current-user-uuid reply-memo (memo new-content &key as-an-update-p)
  (retrieve-one
   ;; TODO: should check channel access
   (insert-into :memo
     (set= :creator_id current-user-id
           :parent_memo_id (ackfock.model:memo-uuid memo)
           :as_an_update (if (and as-an-update-p
                                  (string= (ackfock.model:memo-creator-id memo) current-user-id))
                             "true"
                             "false")
           :content new-content
           :channel_id (or (ackfock.model:memo-channel-id memo)
                           :null))
     (returning :*))
   :as 'ackfock.model:memo))

(defun-with-db-connection-and-current-user-uuid new-channel (channel-name)
  (let ((channel (retrieve-one
                  (insert-into :channel
                    (set= :name channel-name)
                    (returning :*))
                  :as 'ackfock.model:channel)))
    (execute
     (insert-into :user_channel_access
       (set= :user_id current-user-id
             :channel_id (ackfock.model:channel-uuid channel))))
    channel))

(defun-with-db-connection invite-to-channel (target-user-email channel)
  (when (ackfock.feature.auth:current-user-has-access-p channel)
    (let ((target-user-id (ackfock.model:user-uuid (retrieve-one
                                                    (select :uuid
                                                      (from :users)
                                                      (where (:= :email target-user-email)))
                                                    :as 'ackfock.model:user))))
      (execute
       (insert-into :user_channel_access
         (set= :user_id target-user-id
               :channel_id (ackfock.model:channel-uuid channel))
         (on-conflict-do-nothing))))))

(defun-with-db-connection-and-current-user-uuid memo-latest-ackfocks-per-user-by-ackfock (memo)
  "Return an alist by \"ACK\" and \"FOCK\" associated with corresponding USER-ACKFOCK"
  (let ((memo-id (ackfock.model:memo-uuid memo)))
    (if (null (ackfock.model:memo-channel-id memo))
        ;; private memo
        (when (string= (ackfock.model:memo-creator-id memo) current-user-id)
          (let ((current-user-ackfock (ackfock.model:plist-to-user-ackfock
                                       (retrieve-one
                                        (select :*
                                          (from :user_ackfock)
                                          (inner-join :users
                                                      :on (:= :users.uuid :user_ackfock.user_id))
                                          (where (:and (:= :memo_id memo-id)
                                                       (:= :users.uuid current-user-id)))
                                          (order-by (:desc :user_ackfock.created_at))
                                          (limit 1))))))
            (ackfock.model:user-ackfock-list-to-alist-by-ackfock (serapeum:unsplice current-user-ackfock))))
        ;; memo in a channel
        (when (retrieve-one
               (select :*
                 (from :user_channel_access)
                 (where (:and (:= :channel_id (ackfock.model:memo-channel-id memo))
                              (:= :user_id current-user-id)))))
          (let ((data-plist-list (mapcar #'datafly.db::convert-row
                                         (dbi:fetch-all
                                          (dbi:execute
                                           (dbi:prepare *connection*
                                                        *memo-latest-ackfocks-per-user-sql-query*)
                                           (list memo-id))))))
            (ackfock.model:user-ackfock-list-to-alist-by-ackfock (mapcar #'ackfock.model:plist-to-user-ackfock
                                                                         data-plist-list)))))))

(defun-with-db-connection-and-current-user-uuid ackfock-memo (memo ackfock)
  "Return an ACKFOCK.MODEL::USER-ACKFOCK if success. Nil otherwise."
  (let ((memo-id (ackfock.model:memo-uuid memo)))
    (when (ackfock.feature.auth:current-user-has-access-p memo)
      ;; race condition gap notice!
      (ackfock.model:user-ackfock ackfock.feature.auth:*current-user*
                    (ackfock.model:string-to-ackfock ackfock)
                    (datetime-to-timestamp (getf (retrieve-one
                                                  (insert-into :user_ackfock
                                                    (set= :memo_id memo-id
                                                          :user_id current-user-id
                                                          :ackfock ackfock)
                                                    (returning :created_at)))
                                                 :created-at))))))

(defun-with-db-connection rename-channel (channel new-name)
  (when (and (ackfock.feature.auth:current-user-has-access-p channel)
             (str:non-blank-string-p new-name))
    (execute
     (update :channel
       (set= :name new-name)
       (where (:= :uuid (ackfock.model:channel-uuid channel)))))))

(defun-with-db-connection memo-user-ackfocks (memo)
  (when (ackfock.feature.auth:current-user-has-access-p memo)
    (mapcar #'ackfock.model:plist-to-user-ackfock
            (retrieve-all
             (select :*
               (from :user_ackfock)
               (inner-join :users
                           :on (:= :users.uuid :user_ackfock.user_id))
               (where (:= :memo_id (ackfock.model:memo-uuid memo)))
               (order-by (:asc :user_ackfock.created_at)))))))
