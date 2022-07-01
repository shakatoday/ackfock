(in-package :cl-user)
(defpackage ackfock.utils
  (:use :cl)
  (:export #:*email-validator*
           #:send-authentication-email
           #:ensure-plist
           #:send-invitation-email))
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

(defun send-invitation-email (email from-name channel-name)
  (let ((mailgun:*domain* (uiop:getenv "MAILGUN_DOMAIN"))
        (mailgun:*api-key* (uiop:getenv "MAILGUN_API_KEY")))
    (mailgun:send ((concatenate 'string "noreply@" mailgun:*domain*)
                   email
                   (format nil "Hi, ~a invite you to a channel ~a on Ackfock."
                           from-name
                           channel-name))
      (:h1 "Invitation")
      (:p "Howdy,")
      (:p ("~a invite you to a channel," from-name))
      (:a :href ackfock.config:*application-url* ("~a, on Ackfock.com" channel-name))
      (:p "Happy Ack & Fock!")
      (:p "The Ackfock Team"))))

(defun ensure-plist (list)
  (labels ((recur-gen-args (args parse-keyword-p) ;  If any arg in args doesn't have a keyword nor a list before it, append a keyword with the same symbol name as the arg. ; <- not a correct comment!
             (when args
               (let ((first-arg (car args)))
                 (if (and parse-keyword-p
                          (symbolp first-arg)
                          (not (keywordp first-arg)))
                     (append (list (alexandria:make-keyword (str:upcase (str:snake-case (symbol-name first-arg))))
                                   first-arg)
                             (recur-gen-args (cdr args) parse-keyword-p))
                     (cons first-arg
                           (recur-gen-args (cdr args) (not parse-keyword-p))))))))
    (if (trivial-types:proper-list-p list)
        (cons (first list)
              (recur-gen-args (cdr list)
                              t))
        list)))
