(in-package :cl-user)
(defpackage #:ackfock
  (:use #:cl #:clog #:clog-web #:clog-auth #:clog-web-dbi #:ackfock.theme)
  (:import-from :ackfock.auth
                #:current-user)
  (:export start-app))
(in-package :ackfock)

(defparameter *routes* `(("Account" (("Login"           "/login"   on-login    :login)
				     ("Signup"          "/signup"  on-signup   :signup)
				     ("Change Password" "/pass"    on-new-pass :change-password)
				     ("Logout"          "/logout"  on-logout   :logout)))
                         ("Content" (("Content"         "/content" on-main     :content))))
  "Setup website routes")

(defun start-app (&key (port 8080) (open-browser-p nil))
  ;; Here we add authorizations for content and editting content, not just
  ;; access to pages.
  (add-authorization '(:guest :member) '(:content-show-comments))
  (add-authorization '(:guest)         '(:login :signup))
  (add-authorization '(:member)        '(:logout
                                         :search
					 :change-password
				         :content-comment))
  (add-authorization '(:editor)        '(:content-edit))
  (add-authorization '(:admin)         '(:users :content-admin))
  ;; Setup clog
  (initialize 'on-main
              :port port
              :lack-middleware-list `(,lack.middleware.session:*lack-middleware-session*
                                      ,(lambda (app)
                                         (lambda (env)
                                           (ackfock.auth:sync-current-session(env))
                                           (funcall app env))))
	      :extended-routing t
              :static-root (merge-pathnames "./www/"
	                                    (asdf:system-source-directory :ackfock))
	      :boot-function (clog-web-meta
			      "Ackfock is a platform of mini agreements and mini memos of understanding."))
  (clog-web-routes-from-menu *routes*)
  (set-on-new-window 'on-activate :path "/activate")
  (set-on-new-window 'on-search :path "/search")
  (set-on-new-window 'on-invitation :path "/i")

  (when open-browser-p
    (open-browser)))

;;
;; Look and Feel
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-site (body)
  "Setup the website, called on each url switch"
  ;; Initialize the clog-web environment
  (clog-web-initialize body)
  ;; Instantly reload other windows open on authentication change
  (set-on-authentication-change body (lambda (body)
				       (url-replace (location body) "/")))
  (load-css (html-document body)
            "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ;; Initialzie the clog-web-site environment
  (let ((profile (current-user body)))
    (create-web-site body
		     :settings `(:color-class  ,*color-class*
				 :border-class ""
				 :signup-link  "/signup"
				 :login-link   "/login")
		     :profile profile
		     :roles (if profile
				'(:member)
				'(:guest))
                     :theme 'ackfock-theme
		     :title "Ackfock"
		     :footer "(c) 2022 Shaka Chen"
		     :logo nil)))

;;
;; URL Path Handlers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-invitation (body)
  (let* ((web-site (init-site body))
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
                                                       :color-class "w3-red"))))))))

(defun on-activate (body)
  (let* ((path-name (path-name (location body)))
         (user-token (when (> (length path-name) (length "/activate/"))
                       (ackfock.authenticate-user-email:authenticate-user-email (subseq path-name (length "/activate/")))))) ; TODO: 1. return 404 when failed 2. check type/length or other ways to avoid from db access and improve performance.
    (cond (user-token
           (clog-web-initialize body)
           (clog-web-alert body
                           "Success"
                           "Email verification success"
                           :color-class "w3-green")
           (store-authentication-token body user-token)
           (sb-ext:schedule-timer (sb-ext:make-timer (lambda ()
                                                       (url-replace (location body) "/")))
                                  3))
          (t
           (url-replace (location body) "/")))))

(defun on-search (body)
  (let ((web-site (init-site body)))
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
                               do (ackfock.view:render memo
                                                       (profile web-site)
                                                       body))))))
     :authorize t)))

(defun on-login (body)
  (init-site body)
  (create-web-page
   body
   :login `(:menu      ,'(())
	    :on-submit ,(lambda (obj)
			  (if (ackfock.auth:login body
                                                  (ackfock.db:db)
				                  (name-value obj "email")
				                  (name-value obj "password"))
			      (url-replace (location body) "/")
			      (clog-web-alert obj "Invalid" "The email and password are invalid."
					      :time-out 3
					      :place-top t))))
   :authorize t))

(defun on-logout (body)
  (logout body)
  (url-replace (location body) "/"))

(defun on-signup (body)
  (init-site body)
  (create-web-page body
		   :signup `(:menu    ,'(())
			     :content ,(lambda (body)
					 (ackfock.auth:sign-up body (ackfock.db:db))))
		   :authorize t))

(defun on-main (body)
  (let ((ackfock.main-page:*current-user* (profile (init-site body))))
    (create-web-page body
                     :index
                     `(:content ,(if ackfock.main-page:*current-user*
                                     #'ackfock.main-page:main
                                     #'ackfock.main-page:landing)))))

(defun on-new-pass (body)
  (init-site body)
  (create-web-page body
		   :change-password `(:menu    ,'(())
				      :content ,(lambda (body)
						  (ackfock.auth:change-password body (ackfock.db:db))))
		   :authorize t))
