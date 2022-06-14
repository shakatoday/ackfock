(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :ackfock.db :datafly :sxql :ackfock.model-definition)
  (:export #:new-user
           #:user-memos
           #:authenticate
           #:new-memo
           #:ackfock-memo
           #:get-user-by-email
           #:send-memo
           #:create-authentication-code
           #:authenticate-user-email))
(in-package :ackfock.model)

(eval-when (:compile-toplevel :load-toplevel)
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

(defmacro defun-with-db-connection-and-current-user (name lambda-list &body body)
  "Wrap with WITH-CONNECTION (DB) and handler-case. bound CURRENT-USER and USER-ID according to USER-TOKEN"
  (let* ((docstring-list (when (and (stringp (first body))
                                    (cdr body)) ; which means (> (length body) 1))
                           (list (first body))))
         (body (if (null docstring-list)
                   body
                   (subseq body 1)))
         (lambda-list (cons 'user-token lambda-list)))
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
               (let* ((current-user (user-by-user-token user-token))
                      (user-id (user-uuid current-user)))
                 ,@body)
             (type-error (condition)
               (when (ackfock.config:developmentp)
                 (print condition))
               nil)
             (sb-pcl::no-primary-method-error (condition)
               (when (ackfock.config:developmentp)
                 (print condition))
               nil)))))))

(defun-with-db-connection-and-current-user new-memo (archive-id content)
  (execute
   (if (retrieve-one
        (select :*
          (from :user_archive_access)
          (where (:and $(:= user-id)
                       $(:= archive-id)))))
       (insert-into :memo
         $(set= :creator_id user-id
                content
                archive-id))
       (insert-into :memo
         $(set= :creator_id user-id
                content)))))

(defun-with-db-connection-and-current-user new-archive (archive-name)
  (let ((archive-id (archive-uuid (retrieve-one
                                   (insert-into :archive
                                     (set= :name archive-name)
                                     (returning :*))
                                   :as 'archive))))
    (execute
     (insert-into :user_archive_access
       $(set= user-id
              archive-id)))
    archive-id))

(defun-with-db-connection-and-current-user invite-to-archive (target-user-email archive-id)
  ;; make sure current user got the access
  (when (and archive-id
             (retrieve-one
              (select :*
                (from :user_archive_access)
                (where (:and $(:= user-id)
                             $(:= archive-id))))))
    (let ((target-user-id (user-uuid (retrieve-one
                                      (select :uuid
                                        (from :users)
                                        (where (:= :email target-user-email)))
                                      :as 'user))))
      (execute
       (insert-into :user_archive_access
         $(set= :user_id target-user-id
                archive-id)
         (on-conflict-do-nothing))))))

(defun-with-db-connection-and-current-user add-memo-to-archive (memo-id archive-id)
  (when (and archive-id
             (retrieve-one
              (select :*
                (from :memo)
                (where (:and (:= :uuid memo-id)
                             (:= :creator_id user-id)
                             (:= :archive_id :null))))))
    (execute
     (update :memo
       $(set= archive-id)
       (where (:= :uuid memo-id))))))

(defun-with-db-connection-and-current-user memo-user-ackfocks (memo-id)
  (when (or (retrieve-one
             (select :*
               (from :user_archive_access)
               (where (:and $(:= memo-id)
                            $(:= user-id)))))
            (retrieve-one
             (select :*
               (from :memo)
               (where (:and $(:= memo-id)
                            $(:= :creator_id user-id))))))
    ;; race condition gap notice!
    (let ((data-plist-list (retrieve-all
                            (select :*
                              (from :user_ackfock)
                              (inner-join :users
                                          :on (:= :users.uuid :user_ackfock.user_id))
                              (where $(:= memo-id))))))
      (mapcar #'plist-to-user-ackfock
              data-plist-list))))

 (defun-with-db-connection-and-current-user ackfock-memo (memo-id ackfock)
  "Return an ACKFOCK::USER-ACKFOCK if success. Nil otherwise."
  (when (or (retrieve-one
             (select :*
               (from :user_archive_access)
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

(eval-when (:compile-toplevel :load-toplevel)
  (set-macro-character #\$ nil))
