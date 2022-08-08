(in-package :cl-user)
(defpackage #:ackfock
  (:use #:cl)
  (:export #:start-app))
(in-package :ackfock)

(defun start-app (&key (port 8080) (open-browser-p nil))
  (clog-auth:add-authorization '(:guest)
                               '(:login :signup))
  (clog-auth:add-authorization '(:member)
                               '(:logout
                                 :search
		                 :change-password))
  ;; Setup clog
  (clog:initialize 'ackfock.route:on-main
                   :port port
                   :lack-middleware-list `(,lack.middleware.session:*lack-middleware-session*
                                           ,(lambda (app)
                                              (lambda (env)
                                                (print env)
                                                (funcall app env))))
	           :extended-routing t
                   :static-root (merge-pathnames "./www/"
	                                         (asdf:system-source-directory :ackfock))
	           :boot-function (clog-web:clog-web-meta
			           "Ackfock is a platform of mini agreements and mini memos of understanding."))
  (clog-web:clog-web-routes-from-menu ackfock.route:*clog-routes*)
  (when open-browser-p
    (clog:open-browser)))

(defun shutdown ()
  (clog:shutdown))
