(in-package :cl-user)
(defpackage ackfock.feature.email-activation
  (:use :cl :datafly :sxql)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:create-code
           #:activate
           #:send-email))
(in-package :ackfock.feature.email-activation)

(defparameter *send-email-from*
  "noreply@ackfock.com")

(defun-with-db-connection create-code (email &key (ttl-in-sec (* 60 60))) ; by default code will expire in 1 hour
  "Create an activation code for EMAIL with TTL-IN-SEC, insert it into database, and return an ACTIVATION-CODE object if success."
  (let ((valid-until (local-time:format-timestring nil
                                                   (local-time:timestamp+ (local-time:now)
                                                                          ttl-in-sec
                                                                          :sec)))
        (code (str:downcase (uuid:print-bytes nil (uuid:make-v4-uuid)))))
    (retrieve-one
     (insert-into :activation_code
       (set= :email email
             :code code
             :valid_until valid-until)
       (returning :*))
     :as 'ackfock.model:activation-code)))

(defun-with-db-connection get-activation-code-by-code (code)
  (retrieve-one
   (select :*
     (from :activation_code)
     (where (:= :code code)))
   :as 'ackfock.model:activation-code))

(defun-with-db-connection update-user-email-activated-at (email email-activated-at)
  (retrieve-one
   (update :users
     (set= :email_activated_at email-activated-at)
     (where (:= :email email))
     (returning :*))
   :as 'ackfock.model:user))

(defun activate (code)
  "Return corresponding user model when success, return nil otherwise"
  (let ((activation-code (get-activation-code-by-code code)) ; Race condition?
        (now (local-time:now)))
    (when (and activation-code
               (local-time:timestamp<= now ; otherwise the code is timeout
                                       (ackfock.model:activation-code-valid-until activation-code)))
      (update-user-email-activated-at (ackfock.model:activation-code-email activation-code)
                                      now))))

(defun send-email (email recipient-name link)
  (sendgrid:send-email :to email
                       :from *send-email-from*
                       :subject (format nil "Hi ~a, please verify your Ackfock account"
                                        recipient-name)
                       :content-type "text/html"
                       :content (spinneret:with-html-string
                                  (:doctype)
                                  (:html
                                   (:body
                                    (:h1 "Account Verification")
                                    (:p "Howdy,")
                                    (:p "Welcome to Ackfock!. Please confirm your email address by clicking the link below")
                                    (:a :href link link)
                                    (:p "If you did not sign up for an Ackfock account, you can simply disregard this email.")
                                    (:p "Happy Ack & Fock!")
                                    (:p "The Ackfock Team"))))))
