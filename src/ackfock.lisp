(defpackage #:ackfock
  (:use #:cl #:clog #:clog-web #:clog-auth #:clog-web-dbi)
  (:export start-app))

(in-package :ackfock)

;;
;; Setup website structure, database and CLOG
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default user/pass is username: admin and password: admin

;; /content is our root content URL, if you are authorized as an
;; editor or admin you are able to add additional pages by going to
;; the url /content/whatever and then click to add page. If you want
;; it in the menu you would just need to add the url to the
;; menu. There is no need to add handlers for pages under /content as
;; when we initalized CLOG we used the option :extended-routing so
;; that a URL start with /content/ will be sent to the same handler as
;; /content in this case on-main. So our about page has no handler set
;; but functions as we added to out database.

(defparameter *menu* `(("Features" (("Home"            "/")
				    ("Login"           "/login"   on-login    :login)
				    ("Signup"          "/signup"  on-signup   :signup)
				    ("Change Password" "/pass"    on-new-pass :change-password)
				    ("Content"         "/content" on-main     :content)
				    ("Logout"          "/logout"  on-logout   :logout)))
		       ("Help"     (("About"           "/content/about"))))
  "Setup website menu")

(defun start-app (&key (port 8080))
  ;; Here we add authorizations for content and editting content, not just
  ;; access to pages.
  (add-authorization '(:guest :member) '(:content-show-comments))
  (add-authorization '(:guest)         '(:login :signup))
  (add-authorization '(:member)        '(:logout
					 :change-password
				         :content-comment))
  (add-authorization '(:editor)        '(:content-edit))
  (add-authorization '(:admin)         '(:users :content-admin))
  ;; Setup clog
  (initialize 'on-main
              :port port
	      :long-poll-first t
	      :extended-routing t
              :static-root (merge-pathnames "./www/"
	                                    (asdf:system-source-directory :ackfock))
	      :boot-function (clog-web-meta
			      "Some meta data about site"))
  (clog-web-routes-from-menu *menu*)
  ;; see clog/WEBSITE.md for directions on installing this as a webserver
  (open-browser))

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
  ;; Initialzie the clog-web-site environment
  (let ((profile (get-profile body (ackfock.db:db))))
    (create-web-site body
		     :settings '(:color-class  "w3-khaki"
				 :border-class ""
				 :signup-link  "/signup"
				 :login-link   "/login")
		     :profile profile
		     :roles (if profile
				'(:member)
				'(:guest))
                     :theme 'ackfock.theme:ackfock-theme
		     :title "Ackfock"
		     :footer "(c) 2022 Shaka Chen"
		     :logo nil)))

;;
;; URL Path Handlers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-login (body)
  (init-site body)
  (create-web-page
   body
   :login `(:menu      ,*menu*
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
		   :signup `(:menu    ,*menu*
			     :content ,(lambda (body)
					 (ackfock.auth:sign-up body (ackfock.db:db))))
		   :authorize t))

(defun on-main (body)
  (init-site body)
  (create-web-page body :index `(:menu    ,*menu*
				 :content ,(clog-web-content (ackfock.db:db)
							     :comment-table "content"
                                                             :sql-timestamp-func "now()"))))

(defun on-new-pass (body)
  (init-site body)
  (create-web-page body
		   :change-password `(:menu    ,*menu*
				      :content ,(lambda (body)
						  (change-password body (ackfock.db:db))))
		   :authorize t))
