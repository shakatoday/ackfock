(in-package :cl-user)
(defpackage ackfock.game.page
  (:use :cl)
  (:export #:clog-web-page
           #:name
           #:clog-body))
(in-package :ackfock.game.page)

(defclass clog-web-page ()
  ((clog-body
    :reader clog-body)
   (name
    :reader name)
   path
   renderer))
