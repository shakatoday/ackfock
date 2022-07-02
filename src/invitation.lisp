(in-package :cl-user)
(defpackage ackfock.invitation
  (:use :cl :datafly :sxql :ackfock.model-definition)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:import-from :ackfock.model
                #:has-access-p)
  (:export #:*invitation-code-expiration-message*
           #:*invitation-code-had-been-consumed-message*
           #:consume-invitation-code
           #:create-invitation-code))
(in-package :ackfock.invitation)

(defparameter *invitation-code-expiration-message* "Invitation code has already expired.")

(defparameter *invitation-code-had-been-consumed-message* "Invitation code had been used.")

(defun-with-db-connection create-invitation-code (current-user channel &key (ttl-in-hour (* 24 3)))
  (when (has-access-p current-user channel)
    (let ((valid-until (local-time:format-timestring nil
                                                     (local-time:timestamp+ (local-time:now)
                                                                            ttl-in-hour
                                                                            :hour)))
          (code (str:downcase (uuid:print-bytes nil (uuid:make-v4-uuid)))))
      (retrieve-one
       (insert-into :invitation_code
         (set= :code code
               :valid_until valid-until
               :source_user_id (user-uuid current-user)
               :channel_id (channel-uuid channel)
               (returning :*))
         :as 'invitation-code)))))

(define-condition invitation-code-consumption-error (error)
  ((text :initarg :text :reader text)))

(define-condition no-such-code (invitation-code-consumption-error)
  ((text :initarg :text :reader text)))

(define-condition invalid-code (invitation-code-consumption-error)
  ;; expired or already consumed
  ((text :initarg :text :reader text)))

(defun get-invitation-code-by-code (code)
  (retrieve-one
   (select :*
     (from :invitation_code)
     (where #.(ackfock.utils:ensure-plist '(:= code))))
   :as 'inviation-code))

(defun-with-db-connection consume-invitation-code (current-user code)
  (when (and (user-p current-user)
             (user-uuid current-user))
    (let ((invitation-code (get-invitation-code-by-code code))
          (now (local-time:now)))
      (unless invitation-code
        (error 'no-such-code :text (format nil "No such code: ~a." code)))
      (unless (local-time:timestamp<= now ; otherwise the code is timeout
                                      (invitation-code-valid-until invitation-code))
        (error 'invalid-code :text *invitation-code-expiration-message*))
      (when (invitation-code-used-by-user-id invitation-code)
        (error 'invalid-code :text *invitation-code-had-been-consumed-message*))
      (execute
       (insert-into :user_channel_access
         (set= :user_id (user-uuid current-user)
               :channel_id (invitation-code-channel-id invitation-code))
         (on-conflict-do-nothing))))))
