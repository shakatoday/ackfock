(in-package :cl-user)
(defpackage ackfock.utils
  (:use :cl)
  (:export #:current-user
           #:*email-validator*))
(in-package :ackfock.utils)

(defvar *email-validator* (make-instance 'clavier:email-validator))

(defun current-user ()
  (gethash :user caveman2:*session*))

(defun (setf current-user) (new-value)
  (setf (gethash :user caveman2:*session*) new-value))
