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

                      ; Menu         Menu Item         URL        Handler     Actions Auth
(defparameter *menu* `(("Features" (("Home"            "/")
				    ("Login"           "/login"   on-login    :login)
				    ("Signup"          "/signup"  on-signup   :signup)
				    ("Change Password" "/pass"    on-new-pass :change-password)
				    ("Content"         "/content" on-main     :content)
				    ("Logout"          "/logout"  on-logout   :logout)))
		       ("Admin"    (("User List"       "/users"   on-users    :users)))
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
		     :settings '(:color-class  "w3-blue-gray"
				 :border-class ""
				 :signup-link  "/signup"
				 :login-link   "/login")
		     :profile profile
		     ;; We define the roles simply if logged out a :guest
		     ;; if logged in a :member and if username is admin
		     ;; a :member, :editor and :admin.
		     :roles (if profile
				(if (equalp "admin"
					    (getf profile :|username|))
				    '(:member :editor :admin)			    
				    '(:member))
				'(:guest))
		     :title "This site"
		     :footer "(c) 2022 Someone"
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
			  (if (login body (ackfock.db:db)
				     (name-value obj "username")
				     (name-value obj "password"))
			      (url-replace (location body) "/")
			      (clog-web-alert obj "Invalid" "The username and password are invalid."
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
					 (sign-up body (ackfock.db:db))))
		   :authorize t))

(defun on-main (body)
  (init-site body)
  (create-web-page body :index `(:menu    ,*menu*
				 :content ,(clog-web-content (ackfock.db:db)
							     :comment-table "content"
                                                             :sql-timestamp-func "now()"))))

(defun on-users (body)
  (init-site body)
  (create-web-page body :users
		   `(:menu    ,*menu*
		     :content ,(lambda (body)
				 (let ((users (dbi:fetch-all
					       (dbi:execute
						(dbi:prepare
						 (ackfock.db:db)
						 "select * from users")))))
				   (dolist (user users)
				     (let* ((box   (create-div body))
					    (suser (create-span box :content (getf user :|username|)))
					    (rbut  (create-button box :content "Reset Password"
								      :class "w3-margin-left")))
				       (declare (ignore suser))
				       (set-on-click rbut (lambda (obj)
							    (declare (ignore obj))
							    (reset-password (ackfock.db:db)
									    (getf user :|username|))
							    (setf (disabledp rbut) t)
							    (setf (text rbut) "Done"))))))))
			:authorize t))

(defun on-new-pass (body)
  (init-site body)
  (create-web-page body
		   :change-password `(:menu    ,*menu*
				      :content ,(lambda (body)
						  (change-password body (ackfock.db:db))))
		   :authorize t))
