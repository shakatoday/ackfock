(in-package :cl-user)
(defpackage #:ackfock
  (:use #:cl #:clog #:clog-web #:clog-auth #:clog-web-dbi #:ackfock.model-definition #:ackfock.theme)
  (:import-from :ackfock.auth
                #:current-user)
  (:export start-app))
(in-package :ackfock)

;;
;; Setup website structure, database and CLOG
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; /content is our root content URL, if you are authorized as an
;; editor or admin you are able to add additional pages by going to
;; the url /content/whatever and then click to add page. If you want
;; it in the menu you would just need to add the url to the
;; menu. There is no need to add handlers for pages under /content as
;; when we initalized CLOG we used the option :extended-routing so
;; that a URL start with /content/ will be sent to the same handler as
;; /content in this case on-main. So our about page has no handler set
;; but functions as we added to out database.

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
			      "Ackfock is a platform of mini agreements and mini memos of understanding."))
  (clog-web-routes-from-menu *routes*)
  (set-on-new-window 'on-activate :path "/activate")

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
           (print "success")
           (sb-ext:schedule-timer (sb-ext:make-timer (lambda ()
                                                       (url-replace (location body) "/")))
                                  3))
          (t
           (url-replace (location body) "/")))))

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
  (let ((web-site (init-site body)))
    (create-web-page body
                     :index
                     `(:content ,(when (profile web-site)
                                   (lambda (body)
                                     (with-clog-create body
                                         (web-sidebar (:bind sidebar)
                                                      (div (:content "<b>Channels</b>" :class "w3-margin-top")))
                                       (add-card-look sidebar)
                                       (let* ((main-div (create-div body))
                                              (channel-content (create-web-content main-div))
                                              (channels (cons (make-private-channel) ; for user-private-memos
                                                              (user-channels (profile web-site))))
                                              (channel-selects
                                                (make-array (length channels)
                                                            :initial-contents (mapcar
                                                                               (lambda (channel)
                                                                                 `(:channel
                                                                                   ,channel
                                                                                   :sidebar-item
                                                                                   ,(create-web-sidebar-item sidebar
                                                                                                             :content (channel-name channel))))
                                                                               channels)))
                                              (current-sidebar-item (getf (aref channel-selects 0) :sidebar-item)))
                                         (loop for channel-select across channel-selects
                                               do (let ((channel (getf channel-select :channel)))
                                                    (set-on-click (getf channel-select :sidebar-item)
                                                                  (lambda (sidebar-item)
                                                                    (remove-class current-sidebar-item "w3-blue-gray")
                                                                    (setf current-sidebar-item sidebar-item)
                                                                    (add-class sidebar-item "w3-blue-gray")
                                                                    (ackfock.view:render channel
                                                                                         (profile web-site)
                                                                                         (ackfock.view:make-web-content-and-sidebar-item-pair :sidebar-item sidebar-item
                                                                                                                                              :web-content channel-content))))))
                                         (set-margin-side main-div
                                                          :left (format nil "~apx" (width sidebar)))
                                         (ackfock.view:render (getf (aref channel-selects 0) :channel)
                                                              (profile web-site)
                                                              (ackfock.view:make-web-content-and-sidebar-item-pair :sidebar-item (getf (aref channel-selects 0) :sidebar-item)
                                                                                                                   :web-content channel-content))
                                         (add-class current-sidebar-item "w3-blue-gray")
                                         (with-clog-create sidebar
                                             (div (:class "w3-border")
                                                  (form (:bind new-channel-form :class "w3-section")
                                                        (form-element (:bind new-channel-form-input
                                                                        :text
                                                                        :name "name"))
                                                        (button (:class "fa fa-plus-circle w3-button"))))
                                           (setf (width new-channel-form-input) (format nil
                                                                                        "~apx"
                                                                                        (floor (* 0.75 (width sidebar)))))
                                           (center-children new-channel-form))))))))))

(defun on-new-pass (body)
  (init-site body)
  (create-web-page body
		   :change-password `(:menu    ,'(())
				      :content ,(lambda (body)
						  (change-password body (ackfock.db:db))))
		   :authorize t))
