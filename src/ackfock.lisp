(in-package :cl-user)
(defpackage #:ackfock
  (:use #:cl)
  (:export #:start-app
           #:stop))
(in-package :ackfock)

(defun operation-start ())

(defun start-app (&key (port 8080) (open-browser-p nil))
  ;; Setup clog
  
  (ackfock.game:gamify 'ackfock.game.entries:all-entries
                       (asdf:find-system :clog))
  (when open-browser-p
    (clog:open-browser :url (format nil "http://127.0.0.1:~a"
                                    port))))

(defun operation-stop)

(defun stop ()
  (clog:shutdown))
