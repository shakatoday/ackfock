(in-package :cl-user)
(defpackage ackfock.authenticate-user-email
  (:use :cl :datafly :sxql :ackfock.model-definition)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:create-authentication-code
           #:authenticate-user-email))
(in-package :ackfock.authenticate-user-email)

(defun-with-db-connection create-authentication-code (email &key (ttl-in-sec (* 60 60))) ; by default code will expire in 1 hour
  "Create an authentication code for EMAIL with TTL-IN-SEC, insert it into database, and return an AUTHENTICATION-CODE object if success."
  (let ((valid-until (local-time:format-timestring nil
                                                   (local-time:timestamp+ (local-time:now)
                                                                          ttl-in-sec
                                                                          :sec)))
        (code (str:downcase (uuid:print-bytes nil (uuid:make-v4-uuid)))))
    (retrieve-one
     (insert-into :authentication_code
       #.(ackfock.utils:ensure-plist '(set= email code valid-until))
       (returning :*))
     :as 'authentication-code)))

(defun-with-db-connection get-authentication-code-by-code (code)
  (retrieve-one
   (select :*
     (from :authentication_code)
     (where #.(ackfock.utils:ensure-plist '(:= code))))
   :as 'authentication-code))

(defun-with-db-connection update-user-email-authenticated-at (email email-authenticated-at)
  (retrieve-one
   (update :users
     #.(ackfock.utils:ensure-plist '(set= email-authenticated-at))
     (where #.(ackfock.utils:ensure-plist '(:= email)))
     (returning :*))
   :as 'user))
   
(defun authenticate-user-email (code)
  "Return corresponding user token when success, return nil otherwise"
  (let ((authentication-code (get-authentication-code-by-code code)) ; Race condition?
        (now (local-time:now)))
    (when (and authentication-code
               (local-time:timestamp<= now ; otherwise the code is timeout
                                       (authentication-code-valid-until authentication-code)))
      (update-user-email-authenticated-at (authentication-code-email authentication-code)
                                          now))))
