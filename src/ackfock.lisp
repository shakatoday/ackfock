(in-package :cl-user)
(defpackage #:ackfock
  (:use #:cl #:clog #:clog-web)
  (:export #:start-app))
(in-package :ackfock)

(defparameter *routes* `(("Account" (("Login"
                                      "/login"
                                      on-login)
				     ("Signup"
                                      "/signup"
                                      on-signup)
                                     ("Activate"
                                      "/activate"
                                      on-activate)
				     ("Change Password"
                                      "/pass"
                                      on-new-pass)
				     ("Logout"
                                      "/logout"
                                      on-logout)))
                         ("Content" (("Search"
                                      "/search"
                                      on-search)))
                         ("Channel" (("Invitation"
                                      "/i"
                                      on-invitation))))
  "Setup website routes")

(defun start-app (&key (port 8080) (open-browser-p nil))
  ;; Setup clog
  (initialize 'on-main
              :port port
              :lack-middleware-list `(,lack.middleware.session:*lack-middleware-session*
                                      ,(lambda (app)
                                         (lambda (env)
                                           (cond ((and (string= (getf env
                                                                      :path-info)
                                                                "/")
                                                       (null (gethash :current-user
                                                                      (getf env
                                                                            :lack.session))))
                                                  `(200 (:content-type "text/html")
                                                        (,ackfock.main-page:*landing-page*)))
                                                 (t
                                                  (ackfock.auth:current-session-from-lack-session env)
                                                  (funcall app env))))))
	      :extended-routing t
              :static-root (merge-pathnames "./www/"
	                                    (asdf:system-source-directory :ackfock))
	      :boot-function (clog-web-meta
			      "Ackfock is a platform of mini agreements and mini memos of understanding."))
  (clog-web-routes-from-menu *routes*)
  (when open-browser-p
    (open-browser)))
