(in-package :cl-user)
(defpackage ackfock.game.pages
  (:use :cl #:clog #:clog-web)
  (:export #:define-all))
(in-package :ackfock.game.pages)

(defun defpage (name &key path renderer)
  (set-on-new-window renderer
                     :path (or path
                               (str:concat "/" name))))

(defun define-all ()
  (defpage "channel-invitation"
    :path "/i"
    :renderer (lambda (body)
                (let* ((web-site (ackfock.game.theme:init-site body))
                       (path-name (path-name (location body)))
                       (code (when (> (length path-name) (length "/i/")) ; TODO: 1. return 404 when failed 2. check type/length or other ways to avoid from db access and improve performance.
                               (subseq path-name (length "/i/")))))
                  (handler-case
                      (cond ((str:blankp code)
                             (url-replace (location body) "/"))
                            ((find :guest (roles web-site))
                             (clog-web-alert body
                                             "Login Required"
                                             "You have to login first and re-visit the link to accept invitation"
                                             :color-class "w3-red")
                             (sb-ext:schedule-timer (sb-ext:make-timer (lambda ()
                                                                         (url-replace (location body) "/login")))
                                                    3))
                            (t
                             (ackfock.invitation:consume-invitation-code (profile web-site)
                                                                         code)
                             (url-replace (location body) "/")))
                    (ackfock.invitation:no-such-code ()
                      (create-web-page body
                                       :index
                                       `(:content ,(lambda (body)
                                                     (clog-web-alert body
                                                                     "Not Exists"
                                                                     "No such invitation link"
                                                                     :color-class "w3-red")))))
                    (ackfock.invitation:invalid-code (invalid-code-condition)
                      (create-web-page body
                                       :index
                                       `(:content ,(lambda (body)
                                                     (clog-web-alert body
                                                                     "Invalid"
                                                                     (ackfock.invitation:text invalid-code-condition)
                                                                     :color-class "w3-red")))))))))

  (defpage "account-activation"
    :path "/activate"
    :renderer (lambda (body)
                (let* ((path-name (path-name (location body)))
                       (user (when (> (length path-name) (length "/activate/"))
                               (ackfock.feature.email-activation:activate-user-email (subseq path-name (length "/activate/")))))) ; TODO: 1. return 404 when failed 2. check type/length or other ways to avoid from db access and improve performance.
                  (cond (user
                         (setf (ackfock.feature.auth:current-user body) user)
                         (clog-web-initialize body)
                         (clog-web-alert body
                                         "Success"
                                         "Email verification success"
                                         :color-class "w3-green")
                         (sb-ext:schedule-timer (sb-ext:make-timer (lambda ()
                                                                     (url-replace (location body) "/")))
                                                3))
                        (t
                         (url-replace (location body) "/"))))))

  (defpage "search"
    :renderer (lambda (body)
                (let ((web-site (ackfock.game.theme:init-site body)))
                  (if (profile web-site)
                      (create-web-page
                       body
                       :search
                       `(:content ,(lambda (body)
                                     (let ((search-input (form-data-item (form-get-data body)
                                                                         "q")))
                                       (if (str:blankp search-input)
                                           (create-div body :content "Empty search input")
                                           (loop for memo in (ackfock.model:search-memo (profile web-site)
                                                                                        search-input)
                                                 do (ackfock.game:gamify memo
                                                                         (profile web-site)
                                                                         body)))))))
                      (url-replace (location body) "/")))))

  (defpage "login"
    :renderer (lambda (body)
                (if (profile (ackfock.game.theme:init-site body))
                    (url-replace (location body) "/")
                    (create-web-page
                     body
                     :login `(:menu      ,'(())
	                      :on-submit ,(lambda (obj)
			                    (if (ackfock.feature.auth:login body
                                                                            (ackfock.db:db)
				                                            (name-value obj "email")
				                                            (name-value obj "password"))
			                        (url-replace (location body) "/")
			                        (clog-web-alert obj "Invalid" "The email and password are invalid."
					                        :time-out 3
					                        :place-top t))))))))

  (defpage "logout"
    :renderer (lambda (body)
                (ackfock.feature.auth:logout body)
                (url-replace (location body) "/")))

  (defpage "signup"
    :renderer (lambda (body)
                (if (profile (ackfock.game.theme:init-site body))
                    (url-replace (location body) "/")
                    (create-web-page body
		                     :signup `(:menu    ,'(())
			                       :content ,(lambda (body)
					                   (ackfock.feature.auth:sign-up body (ackfock.db:db))))))))

  (defpage "change-password"
    :path "/pass"
    :renderer (lambda (body)
                (if (profile (ackfock.game.theme:init-site body))
                    (create-web-page body
		                     :change-password `(:menu    ,'(())
				                        :content ,(lambda (body)
						                    (ackfock.feature.auth:change-password body (ackfock.db:db)))))
                    (url-replace (location body) "/"))))

  (defpage "main"
    :path "/"
    :renderer (lambda (body)
                (let ((ackfock.game.main-page:*current-user* (profile (ackfock.game.theme:init-site body))))
                  (create-web-page body
                                   :index
                                   `(:content ,#'ackfock.game.main-page:content))))))
