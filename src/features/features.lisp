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

(defmacro defun-with-db-connection-and-current-user (name lambda-list &body body)
  "Wrap with WITH-CONNECTION (DB) and handler-case. bound USER-ID according to CURRENT-USER"
  (let ((docstring (when (stringp (first body))
                     (pop body)))
        (lambda-list (cons 'current-user lambda-list)))
    `(defun ,name ,lambda-list
       ,@(serapeum:unsplice docstring)
       (with-connection (db)
         (handler-case
             ;; race condition notice below!
             (let ((user-id (ackfock.model:user-uuid current-user)))
               ,@body)
           (type-error (condition)
             (when (ackfock.config:developmentp)
               (print condition))
             nil)
           (sb-pcl::no-primary-method-error (condition)
             (when (ackfock.config:developmentp)
               (print condition))
             nil))))))

(defun-with-db-connection-and-current-user new-memo (channel content)
  (execute
   (if (ackfock.model:private-channel-p channel)
       (insert-into :memo
         (set= :creator_id user-id
               :content content))
       (let ((channel-id (ackfock.model:channel-uuid channel)))
         (when (ackfock.model.relationships:has-access-p current-user channel)
           (insert-into :memo
             (set= :creator_id user-id
                   :content content
                   :channel_id channel-id)))))))

(defun-with-db-connection-and-current-user reply-memo (memo new-content &key as-an-update-p)
  (retrieve-one
   ;; TODO: should check channel access
   (insert-into :memo
     (set= :creator_id user-id
           :parent_memo_id (ackfock.model:memo-uuid memo)
           :as_an_update (if (and as-an-update-p
                                  (string= (ackfock.model:memo-creator-id memo) user-id))
                             "true"
                             "false")
           :content new-content
           :channel_id (or (ackfock.model:memo-channel-id memo)
                           :null))
     (returning :*))
   :as 'ackfock.model:memo))

(defun-with-db-connection-and-current-user new-channel (channel-name)
  (let ((channel (retrieve-one
                  (insert-into :channel
                    (set= :name channel-name)
                    (returning :*))
                  :as 'ackfock.model:channel)))
    (execute
     (insert-into :user_channel_access
       (set= :user_id user-id
             :channel_id (ackfock.model:channel-uuid channel))))
    channel))

(defun-with-db-connection-and-current-user invite-to-channel (target-user-email channel)
  (declare (ignore user-id))
  (when (ackfock.model.relationships:has-access-p current-user channel)
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

(defun-with-db-connection-and-current-user memo-latest-ackfocks-per-user-by-ackfock (memo)
  "Return an alist by \"ACK\" and \"FOCK\" associated with corresponding USER-ACKFOCK"
  (let ((memo-id (ackfock.model:memo-uuid memo)))
    (if (null (ackfock.model:memo-channel-id memo))
        ;; private memo
        (when (string= (ackfock.model:memo-creator-id memo) user-id)
          (let ((current-user-ackfock (ackfock.model:plist-to-user-ackfock
                                       (retrieve-one
                                        (select :*
                                          (from :user_ackfock)
                                          (inner-join :users
                                                      :on (:= :users.uuid :user_ackfock.user_id))
                                          (where (:and (:= :memo_id memo-id)
                                                       (:= :users.uuid user-id)))
                                          (order-by (:desc :user_ackfock.created_at))
                                          (limit 1))))))
            (ackfock.model:user-ackfock-list-to-alist-by-ackfock (list current-user-ackfock))))
        ;; memo in a channel
        (when (retrieve-one
               (select :*
                 (from :user_channel_access)
                 (where (:and (:= :channel_id (ackfock.model:memo-channel-id memo))
                              (:= :user_id user-id)))))
          (let ((data-plist-list (mapcar #'datafly.db::convert-row
                                         (dbi:fetch-all
                                          (dbi:execute
                                           (dbi:prepare *connection*
                                                        *memo-latest-ackfocks-per-user-sql-query*)
                                           (list memo-id))))))
            (ackfock.model:user-ackfock-list-to-alist-by-ackfock (mapcar #'ackfock.model:plist-to-user-ackfock
                                                                         data-plist-list)))))))

(defun-with-db-connection-and-current-user ackfock-memo (memo ackfock)
  "Return an ACKFOCK.MODEL::USER-ACKFOCK if success. Nil otherwise."
  (let ((memo-id (ackfock.model:memo-uuid memo)))
    (when (ackfock.model.relationships:has-access-p current-user memo)
      ;; race condition gap notice!
      (ackfock.model:user-ackfock current-user
                    (ackfock.model:string-to-ackfock ackfock)
                    (datetime-to-timestamp (getf (retrieve-one
                                                  (insert-into :user_ackfock
                                                    (set= :memo_id memo-id
                                                          :user_id user-id
                                                          :ackfock ackfock)
                                                    (returning :created_at)))
                                                 :created-at))))))

(defun-with-db-connection-and-current-user rename-channel (channel new-name)
  (declare (ignore user-id))
  (when (and (ackfock.model.relationships:has-access-p current-user channel)
             (str:non-blank-string-p new-name))
    (execute
     (update :channel
       (set= :name new-name)
       (where (:= :uuid (ackfock.model:channel-uuid channel)))))))

(defun-with-db-connection-and-current-user memo-user-ackfocks (memo)
  (declare (ignore user-id))
  (when (ackfock.model.relationships:has-access-p current-user memo)
    (mapcar #'ackfock.model:plist-to-user-ackfock
            (retrieve-all
             (select :*
               (from :user_ackfock)
               (inner-join :users
                           :on (:= :users.uuid :user_ackfock.user_id))
               (where (:= :memo_id (ackfock.model:memo-uuid memo)))
               (order-by (:asc :user_ackfock.created_at)))))))
