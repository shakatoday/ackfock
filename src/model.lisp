(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :ackfock.db :datafly :sxql :ackfock.model-definition)
  (:export #:new-memo
           #:new-channel
           #:invite-to-channel
           #:add-memo-to-channel
           #:memo-user-ackfocks
           #:memo-latest-ackfocks-per-user-by-ackfock
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
  (make-user-ackfock :user #.(cons 'make-user
                                   (reduce #'append
                                           (mapcar (lambda (keyword-arg)
                                                     `(,keyword-arg (getf (reverse plist) ,keyword-arg))) ; use the later created-at so we need a reverse
                                                   '(:uuid :email :username :created-at))))
                     :ackfock (getf plist :ackfock)
                     :created-at (getf plist :ackfock)))

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

(defun-with-db-connection-and-current-user new-memo (channel-id content)
  (execute
   (if (retrieve-one
        (select :*
          (from :user_channel_access)
          (where (:and $(:= user-id)
                       $(:= channel-id)))))
       (insert-into :memo
         $(set= :creator_id user-id
                content
                channel-id))
       (insert-into :memo
         $(set= :creator_id user-id
                content)))))

(defun-with-db-connection-and-current-user new-channel (channel-name)
  (let ((channel-id (channel-uuid (retrieve-one
                                   (insert-into :channel
                                     (set= :name channel-name)
                                     (returning :*))
                                   :as 'channel))))
    (execute
     (insert-into :user_channel_access
       $(set= user-id
              channel-id)))
    channel-id))

(defun-with-db-connection-and-current-user invite-to-channel (target-user-email channel-id)
  ;; make sure current user got the access
  (when (and channel-id
             (retrieve-one
              (select :*
                (from :user_channel_access)
                (where (:and $(:= user-id)
                             $(:= channel-id))))))
    (let ((target-user-id (user-uuid (retrieve-one
                                      (select :uuid
                                        (from :users)
                                        (where (:= :email target-user-email)))
                                      :as 'user))))
      (execute
       (insert-into :user_channel_access
         $(set= :user_id target-user-id
                channel-id)
         (on-conflict-do-nothing))))))

(defun-with-db-connection-and-current-user add-memo-to-channel (memo-id channel-id)
  (when (and channel-id
             (retrieve-one
              (select :*
                (from :memo)
                (where (:and (:= :uuid memo-id)
                             (:= :creator_id user-id)
                             (:is-null :channel_id))))))
    (execute
     (update :memo
       $(set= channel-id)
       (where (:= :uuid memo-id))))))

(defun-with-db-connection-and-current-user memo-user-ackfocks (memo-id)
  (when (or (retrieve-one
             (select :*
               (from :user_channel_access)
               (where (:and $(:= memo-id)
                            $(:= user-id)))))
            (retrieve-one
             (select :*
               (from :memo)
               (where (:and (:= :memo.uuid memo-id)
                            (:= :creator_id user-id))))))
    ;; race condition gap notice!
    (let ((data-plist-list (retrieve-all
                            (select :*
                              (from :user_ackfock)
                              (inner-join :users
                                          :on (:= :users.uuid :user_ackfock.user_id))
                              (where $(:= memo-id))))))
      (mapcar #'plist-to-user-ackfock
              data-plist-list))))

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
                 (where (:and $(:= memo-id)
                              $(:= user-id)))))
          (let ((data-plist-list (retrieve-all
                                  (select :*
                                    (from :user_ackfock)
                                    (inner-join :users
                                                :on (:= :users.uuid :user_ackfock.user_id))
                                    (where $(:= memo-id))))))
            (user-ackfock-list-to-alist-by-ackfock (mapcar #'plist-to-user-ackfock
                                                           data-plist-list)))))))

(defun-with-db-connection-and-current-user ackfock-memo (memo-id ackfock)
  "Return an ACKFOCK.MODEL-DEFINITION::USER-ACKFOCK if success. Nil otherwise."
  (when (or (retrieve-one
             (select :*
               (from :user_channel_access)
               (where (:and $(:= memo-id)
                            $(:= user-id)))))
            (retrieve-one
             (select :*
               (from :memo)
               (where (:and $(:= memo-id)
                            $(:= :creator_id user-id))))))
    ;; race condition gap notice!
    (plist-to-user-ackfock
     (retrieve-one
      (select :*
        (from (insert-into :user_ackfock
                $(set= memo-id
                       user-id
                       ackfock)
                (returning :*)
                :as :new_user_ackfock))
        (inner-join :users
                    :on (:= :users.uuid :new_user_ackfock.user_id)))))))
