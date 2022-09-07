(in-package :cl-user)
(defpackage ackfock.game.entries
  (:use :cl :clog :clog-web)
  (:import-from :ackfock.game
                #:gamify)
  (:shadow #:name)
  (:export #:all-entries))
(in-package :ackfock.game.entries)

(defvar *mapper* (myway:make-mapper))

(defun define-entry (name &key (base-path (str:concat "/" name)) path-extention path-mapper clog-new-window-handler)
  (when (and path-extention
             path-mapper)
    (myway:connect *mapper*
                   (str:concat base-path path-extention)
                   path-mapper))
  (set-on-new-window clog-new-window-handler
                     :path base-path))

(defmethod gamify ((object (eql 'ackfock.game.entries:all-entries)) (context (eql 'ackfock.game:built-on-clog)))
  (define-entry "channel-invitation"
    :base-path "/i"
    :path-extention "/?:code?"
    :path-mapper (lambda (params) (getf params :code))
    :clog-new-window-handler
    (lambda (body)
      (let ((web-site (ackfock.game.theme:init-site body))
            (code (myway:dispatch *mapper*
                                  (path-name (location body))))) ; TODO: 1. return 404 when failed 2. check type/length or other ways to avoid from db access and improve performance.
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
    :base-path "/activate"
    :path-extention "/?:code?"
    :path-mapper (lambda (params) (getf params :code))
    :clog-new-window-handler
    (lambda (body)
      (let ((user (rutils:when-it (myway:dispatch *mapper*
                                                  (path-name (location body)))
                    (ackfock.feature.email-activation:activate rutils:it)))) ; TODO: 1. return 404 when failed 2. check type/length or other ways to avoid from db access and improve performance.
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
          (create-web-page body
                           :login
                           `(:content ,#'ackfock.game.auth:login)))))

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
    :base-path "/pass"
    :clog-new-window-handler
    (lambda (body)
      (let ((current-user (profile (ackfock.game.theme:init-site body))))
        (if current-user
            (create-web-page body
		             :change-password `(:content ,(lambda (body)
                                                            (let ((ackfock.feature.auth:*current-user* current-user))
						              (ackfock.feature.auth:change-password body (ackfock.db:db))))))
            (url-replace (location body) "/")))))

  (define-entry "main"
    :base-path "/"
    :clog-new-window-handler
    (lambda (body)
      (let ((ackfock.feature.auth:*current-user* (profile (ackfock.game.theme:init-site body))))
        (create-web-page body
                         :index
                         `(:content ,#'ackfock.game.main-entry:content))))))
