(in-package :cl-user)
(defpackage ackfock.utils
  (:use :cl)
  (:export #:current-user
           #:*email-validator*
           #:send-authentication-email))
(in-package :ackfock.utils)

(defvar *email-validator* (make-instance 'clavier:email-validator))

(defun send-authentication-email (email recipient-name link)
  (let ((mailgun:*domain* (uiop:getenv "MAILGUN_DOMAIN"))
        (mailgun:*api-key* (uiop:getenv "MAILGUN_API_KEY")))
    (mailgun:send ((concatenate 'string "noreply@" mailgun:*domain*)
                   email
                   (format nil "Hi ~a, please verify your Ackfock account" recipient-name))
      (:h1 "Account Verification")
      (:p "Howdy,")
      (:p "Welcome to Ackfock!. Please confirm your email address by clicking the link below")
      (:a :href link link)
      (:p "If you did not sign up for an Ackfock account, you can simply disregard this email.")
      (:p "Happy Ack & Fock!")
      (:p "The Ackfock Team"))))
