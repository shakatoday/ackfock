(in-package :cl-user)
(defpackage ackfock.feature.email-activation
  (:use :cl :datafly :sxql :ackfock.model-definition)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:create-activation-code
           #:activate-user-email))
(in-package :ackfock.feature.email-activation)

(defun-with-db-connection create-activation-code (email &key (ttl-in-sec (* 60 60))) ; by default code will expire in 1 hour
  "Create an activation code for EMAIL with TTL-IN-SEC, insert it into database, and return an ACTIVATION-CODE object if success."
  (let ((valid-until (local-time:format-timestring nil
                                                   (local-time:timestamp+ (local-time:now)
                                                                          ttl-in-sec
                                                                          :sec)))
        (code (str:downcase (uuid:print-bytes nil (uuid:make-v4-uuid)))))
    (retrieve-one
     (insert-into :activation_code
       #.(ackfock.utils:ensure-plist '(set= email code valid-until))
       (returning :*))
     :as 'activation-code)))

(defun-with-db-connection get-activation-code-by-code (code)
  (retrieve-one
   (select :*
     (from :activation_code)
     (where #.(ackfock.utils:ensure-plist '(:= code))))
   :as 'activation-code))

(defun-with-db-connection update-user-email-activated-at (email email-activated-at)
  (retrieve-one
   (update :users
     #.(ackfock.utils:ensure-plist '(set= email-activated-at))
     (where #.(ackfock.utils:ensure-plist '(:= email)))
     (returning :*))
   :as 'user))

(defun activate-user-email (code)
  "Return corresponding user model when success, return nil otherwise"
  (let ((activation-code (get-activation-code-by-code code)) ; Race condition?
        (now (local-time:now)))
    (when (and activation-code
               (local-time:timestamp<= now ; otherwise the code is timeout
                                       (activation-code-valid-until activation-code)))
      (update-user-email-activated-at (activation-code-email activation-code)
                                      now))))
