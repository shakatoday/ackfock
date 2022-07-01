(in-package :cl-user)
(defpackage ackfock.invitation
  (:use :cl :datafly :sxql :ackfock.model-definition)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:import-from :ackfock.model
                #:has-access-p)
  (:export #:email-invite-to-channel))
(in-package :ackfock.invitation)

(defun-with-db-connection email-invite-to-channel (current-user channel email &key (ttl-in-hour (* 24 3)))
  (when (has-access-p current-user channel)
    (let ((valid-until (local-time:format-timestring nil
                                                   (local-time:timestamp+ (local-time:now)
                                                                          ttl-in-hour
                                                                          :hour))))
      (when (retrieve-one
             (insert-into :invitation
               #.(ackfock.utils:ensure-plist '(set=
                                               email
                                               valid-until
                                               :source_user_id (user-uuid current-user)
                                               :channel_id (channel-uuid channel)))
               (returning :*)))
        (ackfock.utils:send-invitation-email email
                                             (user-username current-user)
                                             (channel-name channel))))))
