(in-package :cl-user)
(defpackage #:ackfock
  (:use #:cl)
  (:export #:operation-start
           #:operation-stop))
(in-package :ackfock)

(defun operation-start ()
  (ackfock.game:init (getf ackfock.game:*environment*
                           :top-context)
                     :port (getf ackfock.game:*environment*
                                 :port))
  (ackfock.game:gamify 'ackfock.game.entries:all-entries
                       (getf ackfock.game:*environment*
                             :top-context)))

(defun operation-stop ()
  (ackfock.game:operation-stop (getf ackfock.game:*environment*
                                     :top-context)))
