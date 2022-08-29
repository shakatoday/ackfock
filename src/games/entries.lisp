(in-package :cl-user)
(defpackage ackfock.game.entries
  (:use :cl #:clog #:clog-web)
  (:import-from :ackfock.game
                #:gamify)
  (:export #:all-entries))
(in-package :ackfock.game.entries)

(defun define-entry (name &key path clog-new-window-handler)
  (set-on-new-window clog-new-window-handler
                     :path (or path
                               (str:concat "/" name))))

(defmethod gamify ((object (eql 'ackfock.game.entries:all-entries)) (context (eql (asdf:find-system :clog))))
  (define-entry "channel-invitation"
    :path "/i"
    :clog-new-window-handler
    (lambda (body)
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
                   (let ((ackfock.feature.auth:*current-user* (profile web-site)))
                     (ackfock.feature.channel-invitation:consume-invitation-code code))
                   (url-replace (location body) "/")))
          (ackfock.feature.channel-invitation:no-such-code ()
            (create-web-page body
                             :index
                             `(:content ,(lambda (body)
                                           (clog-web-alert body
                                                           "Not Exists"
                                                           "No such invitation link"
                                                           :color-class "w3-red")))))
          (ackfock.feature.channel-invitation:invalid-code (invalid-code-condition)
            (create-web-page body
                             :index
                             `(:content ,(lambda (body)
                                           (clog-web-alert body
                                                           "Invalid"
                                                           (ackfock.feature.channel-invitation:condition-message invalid-code-condition)
                                                           :color-class "w3-red")))))))))

  (define-entry "account-activation"
    :path "/activate"
    :clog-new-window-handler
    (lambda (body)
      (let* ((path-name (path-name (location body)))
             (user (when (> (length path-name) (length "/activate/"))
                     (ackfock.feature.email-activation:activate (subseq path-name (length "/activate/")))))) ; TODO: 1. return 404 when failed 2. check type/length or other ways to avoid from db access and improve performance.
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

  (define-entry "search"
    :clog-new-window-handler
    (lambda (body)
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
                                 (let ((ackfock.feature.auth:*current-user* (profile web-site)))
                                   (loop for memo in (ackfock.feature.search:search-memo search-input)
                                         do (ackfock.game:gamify memo
                                                                 body))))))))
            (url-replace (location body) "/")))))

  (define-entry "login"
    :clog-new-window-handler
    (lambda (body)
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

  (define-entry "logout"
    :clog-new-window-handler
    (lambda (body)
      (ackfock.feature.auth:logout body)
      (url-replace (location body) "/")))

  (define-entry "signup"
    :clog-new-window-handler
    (lambda (body)
      (if (profile (ackfock.game.theme:init-site body))
          (url-replace (location body) "/")
          (create-web-page body
		           :signup `(:menu    ,'(())
			             :content ,(lambda (body)
					         (ackfock.feature.auth:sign-up body (ackfock.db:db))))))))

  (define-entry "change-password"
    :path "/pass"
    :clog-new-window-handler
    (lambda (body)
      (if (profile (ackfock.game.theme:init-site body))
          (create-web-page body
		           :change-password `(:menu    ,'(())
				              :content ,(lambda (body)
						          (ackfock.feature.auth:change-password body (ackfock.db:db)))))
          (url-replace (location body) "/"))))

  (define-entry "main"
    :path "/"
    :clog-new-window-handler
    (lambda (body)
      (let ((ackfock.feature.auth:*current-user* (profile (ackfock.game.theme:init-site body))))
        (create-web-page body
                         :index
                         `(:content ,#'ackfock.game.main-page:content))))))
