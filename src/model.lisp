(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :ackfock.db :datafly :sxql :ackfock.model-definition)
  (:export #:new-memo
           #:reply-memo
           #:new-channel
           #:rename-channel
           #:invite-to-channel
           #:memo-latest-ackfocks-per-user-by-ackfock
           #:user-by-email
           #:ackfock-memo))
(in-package :ackfock.model)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\$ nil)
  (set-macro-character #\$
                       (lambda (stream char)
                         (declare (ignore char))
                         (ackfock.utils:ensure-plist (read stream)))))

(defconstant +dummy-uuid+ :A2543078-7D5B-4F40-B6FD-DBC58E863752)

(defun dummy-uuid ()
  (string +dummy-uuid+))

(defun string-to-ackfock (string)
  "If the STRING is \"ACK\" or \"FOCK\", this function returns :ACK or :FOCK. All the other cases it returns nil"
  (and (stringp string)
       (member string
               '("ACK" "FOCK")
               :test #'string=)
       (alexandria:make-keyword string)))

(defun plist-to-user-ackfock (plist)
  (let ((copy-plist (copy-list plist)))
    (remf copy-plist :created-at)
    (make-user-ackfock :user #.(cons 'make-user
                                     (reduce #'append
                                             (mapcar (lambda (keyword-arg)
                                                       `(,keyword-arg (getf copy-plist ,keyword-arg))) ; use the later created-at so we need a copy and remf
                                                     '(:uuid :email :username :created-at))))
                       :ackfock (getf plist :ackfock)
                       :created-at (getf plist :created-at))))

(defun user-ackfock-list-to-alist-by-ackfock (user-ackfock-list)
  (list (cons "ACK"
              (remove-if-not (lambda (ackfock) (string= ackfock "ACK"))
                             user-ackfock-list
                             :key #'user-ackfock-ackfock))
        (cons "FOCK"
              (remove-if-not (lambda (ackfock) (string= ackfock "FOCK"))
                             user-ackfock-list
                             :key #'user-ackfock-ackfock))))

(defmacro defun-with-db-connection-and-current-user (name lambda-list &body body)
  "Wrap with WITH-CONNECTION (DB) and handler-case. bound USER-ID according to CURRENT-USER"
  (let* ((docstring-list (when (and (stringp (first body))
                                    (cdr body)) ; which means (> (length body) 1))
                           (list (first body))))
         (body (if (null docstring-list)
                   body
                   (subseq body 1)))
         (lambda-list (cons 'current-user lambda-list)))
    `(flet ((user-by-user-token (user-token)
              (retrieve-one
               (select :*
                 (from :users)
                 (where (:= :token user-token)))
               :as 'user)))
       (defun ,name ,lambda-list
         ,@docstring-list
         (with-connection (db)
           (handler-case
               ;; race condition notice below!
               (let ((user-id (user-uuid current-user)))
                 ,@body)
             (type-error (condition)
               (when (ackfock.config:developmentp)
                 (print condition))
               nil)
             (sb-pcl::no-primary-method-error (condition)
               (when (ackfock.config:developmentp)
                 (print condition))
               nil)))))))

(defmethod has-access-p ((user user) (model-obj channel))
  (let ((channel-id (channel-uuid model-obj))
        (user-id (user-uuid user)))
    (when (and channel-id
               user-id)
      (retrieve-one
       (select :*
         (from :user_channel_access)
         (where (:and $(:= user-id )
                      $(:= channel-id))))))))

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

(defun-with-db-connection-and-current-user new-memo (channel content)
  (execute
   (if (private-channel-p channel)
       (insert-into :memo
         $(set= :creator_id user-id
                content))
       (let ((channel-id (channel-uuid channel)))
         (when (has-access-p current-user channel)
           (insert-into :memo
             $(set= :creator_id user-id
                    content
                    channel-id)))))))

(defun-with-db-connection-and-current-user reply-memo (memo new-content &key as-an-update-p)
  (retrieve-one
   (if (memo-channel-id memo)
       ;; TODO: should check channel access
       ;; TODO: resolve code duplication
       (insert-into :memo
         (set= :creator_id user-id
               :parent_memo_id (memo-uuid memo)
               :as_an_update (if (and as-an-update-p
                                      (string= (memo-creator-id memo) user-id))
                                 :true
                                 :false)
               :content new-content
               :channel_id (memo-channel-id memo))
         (returning :*))
       (insert-into :memo
         (set= :creator_id user-id
               :parent_memo_id (memo-uuid memo)
               :as_an_update (if (and as-an-update-p
                                      (string= (memo-creator-id memo) user-id))
                                 :true
                                 :false)
               :content new-content)
         (returning :*)))
   :as 'memo))

(defun-with-db-connection-and-current-user new-channel (channel-name)
  (let ((channel (retrieve-one
                  (insert-into :channel
                    (set= :name channel-name)
                    (returning :*))
                  :as 'channel)))
    (execute
     (insert-into :user_channel_access
       $(set= user-id
              :channel_id (channel-uuid channel))))
    channel))

(defun-with-db-connection-and-current-user invite-to-channel (target-user-email channel)
  (declare (ignore user-id))
  (when (has-access-p current-user channel)
    (let ((target-user-id (user-uuid (retrieve-one
                                      (select :uuid
                                        (from :users)
                                        (where (:= :email target-user-email)))
                                      :as 'user))))
      (execute
       (insert-into :user_channel_access
         (set= :user_id target-user-id
               :channel-id (channel-uuid channel))
         (on-conflict-do-nothing))))))

(defun-with-db-connection-and-current-user memo-latest-ackfocks-per-user-by-ackfock (memo)
  "Return an alist by \"ACK\" and \"FOCK\" associated with corresponding USER-ACKFOCK"
  (let ((memo-id (memo-uuid memo)))
    (if (null (memo-channel-id memo))
        ;; private memo
        (when (string= (memo-creator-id memo) user-id)
          (let ((current-user-ackfock (plist-to-user-ackfock
                                       (retrieve-one
                                        (select :*
                                          (from :user_ackfock)
                                          (inner-join :users
                                                      :on (:= :users.uuid :user_ackfock.user_id))
                                          (where (:and $(:= memo-id)
                                                       (:= :users.uuid user-id)))
                                          (order-by (:desc :user_ackfock.created_at))
                                          (limit 1))))))
            (user-ackfock-list-to-alist-by-ackfock (list current-user-ackfock))))
        ;; memo in a channel
        (when (retrieve-one
               (select :*
                 (from :user_channel_access)
                 (where (:and (:= :channel_id (memo-channel-id memo))
                              $(:= user-id)))))
          (let ((data-plist-list (mapcar #'datafly.db::convert-row
                                  (dbi:fetch-all
                                   (dbi:execute
                                    (dbi:prepare *connection*
                                                 "select distinct on (user_ackfock.user_id) *
                                                 from user_ackfock
                                                 inner join users on users.uuid = user_ackfock.user_id
                                                 where memo_id = ?
                                                 order by user_ackfock.user_id, user_ackfock.created_at desc")
                                    (list memo-id))))))
            (user-ackfock-list-to-alist-by-ackfock (mapcar #'plist-to-user-ackfock
                                                           data-plist-list)))))))

(defun-with-db-connection-and-current-user ackfock-memo (memo ackfock)
  "Return an ACKFOCK.MODEL-DEFINITION::USER-ACKFOCK if success. Nil otherwise."
  (let ((memo-id (memo-uuid memo)))
    (when (has-access-p current-user memo)
      ;; race condition gap notice!
      (apply #'make-user-ackfock
             (append (retrieve-one
                      (insert-into :user_ackfock
                        $(set= memo-id
                               user-id
                               ackfock)
                        (returning :created_at)))
                     (list :user current-user
                           :ackfock ackfock))))))

(defun-with-db-connection user-by-email (email)
  (when (and (str:non-blank-string-p email)
             (clavier:validate ackfock.utils:*email-validator* email))
    (retrieve-one
     (select :*
       (from :users)
       (where $(:= email)))
     :as 'user)))

(defun-with-db-connection-and-current-user rename-channel (channel new-name)
  (declare (ignore user-id))
  (when (and (has-access-p current-user channel)
             (str:non-blank-string-p new-name))
    (execute
     (update :channel
       (set= :name new-name)
       (where (:= :uuid (channel-uuid channel)))))))
