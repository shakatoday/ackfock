(in-package :cl-user)
(defpackage ackfock.feature.channel-invitation
  (:use :cl :datafly :sxql)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:*invitation-code-expiration-message*
           #:*invitation-code-had-been-consumed-message*
           #:consume-invitation-code
           #:create-invitation-code
           #:no-such-code
           #:invalid-code
           #:condition-message))
(in-package :ackfock.feature.channel-invitation)

(defparameter *invitation-code-expiration-message* "Invitation link has already expired.")

(defparameter *invitation-code-had-been-consumed-message* "One-time invitation link had been consumed. (Visiting without login won't consume the link")

(defun-with-db-connection create-invitation-code (current-user channel &key (ttl-in-hour (* 24 3)))
  (when (ackfock.model.relationships:has-access-p current-user channel)
    (let ((valid-until (local-time:format-timestring nil
                                                     (local-time:timestamp+ (local-time:now)
                                                                            ttl-in-hour
                                                                            :hour)))
          (code (str:downcase (uuid:print-bytes nil (uuid:make-v4-uuid)))))
      (retrieve-one
       (insert-into :invitation_code
         (set= :code code
               :valid_until valid-until
               :source_user_id (ackfock.model:user-uuid current-user)
               :channel_id (ackfock.model:channel-uuid channel))
         (returning :*))
       :as 'invitation-code))))

(define-condition invitation-code-consumption-error (error)
  ((text :initarg :condition-message :reader condition-message)))

(define-condition no-such-code (invitation-code-consumption-error)
  ((text :initarg :condition-message :reader condition-message)))

(define-condition invalid-code (invitation-code-consumption-error)
  ;; expired or already consumed
  ((text :initarg :condition-message :reader condition-message)))

(defun get-invitation-code-by-code (code)
  (retrieve-one
   (select :*
     (from :invitation_code)
     (where (:= :code code)))
   :as 'invitation-code))

(defun-with-db-connection consume-invitation-code (current-user code)
  (when (and (ackfock.model:user-p current-user)
             (ackfock.model:user-uuid current-user))
    (let ((invitation-code (get-invitation-code-by-code code))
          (now (local-time:now)))
      (unless invitation-code
        (error 'no-such-code :condition-message (format nil "No such code: ~a." code)))
      (unless (local-time:timestamp<= now ; otherwise the code is timeout
                                      (ackfock.model:invitation-code-valid-until invitation-code))
        (error 'invalid-code :condition-message *invitation-code-expiration-message*))
      (when (ackfock.model:invitation-code-used-by-user-id invitation-code)
        (error 'invalid-code :condition-message *invitation-code-had-been-consumed-message*))
      (execute
       (insert-into :user_channel_access
         (set= :user_id (ackfock.model:user-uuid current-user)
               :channel_id (ackfock.model:invitation-code-channel-id invitation-code))
         (on-conflict-do-nothing)))
      (execute
       (update :invitation_code
         (set= :used_by_user_id (ackfock.model:user-uuid current-user))
         (where (:= :code code)))))))
