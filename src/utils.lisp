(in-package :cl-user)
(defpackage ackfock.utils
  (:use :cl)
  (:export #:current-user))
(in-package :ackfock.utils)

(defun current-user ()
  (gethash :user caveman2:*session*))

(defun (setf current-user) (new-value)
  (setf (gethash :user caveman2:*session*) new-value))
