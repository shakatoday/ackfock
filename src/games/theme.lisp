(in-package :cl-user)
(defpackage ackfock.game.theme
  (:use :cl :clog :clog-web)
  (:export #:*color-class*
           #:init-site))
(in-package :ackfock.game.theme)

(defparameter *color-class* "w3-khaki")

(defun init-site (body)
  "Setup the website, called on each url switch"
  ;; Initialize the clog-web environment
  (clog-web-initialize body)
  (load-css (html-document body)
            "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ;; Initialzie the clog-web-site environment
  (let ((profile (ackfock.feature.auth:current-user body)))
    (create-web-site body
		     :settings `(:color-class  ,*color-class*
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

(defun ackfock-theme (body page properties)
  "The Ackfock theme for clog-web-site.
Settings available:
  :color-class   - w3 color class for menu bars and buttons (def: w3-black)
  :border-class  - w3 border (def: \"\")
  :text-class    - w3 text color class (def: \"\")
  :signup-link   - link to signup (def: /signup)
  :login-link    - link to login (def: /login)
  :username-link - link when clicking on username (def: /logout)
Page properties:
  :menu - ((\"Menu Name\" ((\"Menu Item\" \"link\")))) (def: nil)
  :content - (def: \"\")"
  ;; Settings and Properties with default values
  (let* ((website        (get-web-site body))
	 (color-class    (get-setting website :color-class "w3-black"))
	 (login-link     (get-setting website :login-link "/login"))
	 ;; use this in the future (username-link  (get-setting website :username-link "/"))
	 (content        (get-property properties :content "")))
    ;;
    ;; Page layout
    ;; SECTION: top bar
    (let* ((menu (create-div body
                             :class "w3-top w3-bar"))
           (logo-a (create-a menu
                             :link (url website)
                             :content (title website)
                             :class "w3-bar-item w3-xlarge w3-sans-serif ackfock-theme")))
      (setf (z-index menu) 3)
      (add-class menu color-class)
      (if (profile website)
          (progn
            (when (eq page :index)
              (setf (connection-data-item body
                                          :sidebar-menu-button-for-mobile)
                    (create-div menu
                                :class "w3-bar-item fa fa-bars w3-hide-medium w3-hide-large"))
              (add-class logo-a "w3-hide-small"))
            (let* ((search-form (create-form menu
                                             :action "/search"
                                             :method :GET
                                             :class "w3-bar-item w3-hide-small w3-mobile"))
                   (login-menu-right-menu (create-div menu
                                                      :class "w3-bar-item fa fa-user w3-hide-medium w3-hide-large w3-right"))
                   (search-icon-for-mobile (create-div menu
                                                       :class "w3-bar-item fa fa-search w3-hide-medium w3-hide-large w3-right"))
                   (login-menu-right-class "w3-bar-item ackfock-theme w3-right w3-mobile w3-hide-small")
                   (login-menu-right-items nil))
              (create-form-element search-form
                                   :search
                                   :name "q"
                                   :value (when (eq page :search)
                                            (form-data-item (form-get-data body)
                                                            "q"))
                                   :hidden (not (profile website)))
              (create-button search-form
                             :content "search"
                             :hidden (not (profile website)))
              (push (create-a menu
                              :class login-menu-right-class
                              :content "logout"
                              :link "/logout")
                    login-menu-right-items)
              (push (create-a menu
                              :class login-menu-right-class
                              :content "change password"
                              :link "/pass")
                    login-menu-right-items)
              (push (create-div menu
                                :class login-menu-right-class
                                :content (ackfock.model:user-username (profile website)))
                    login-menu-right-items)
              (set-on-click login-menu-right-menu
                            (lambda (obj)
                              (declare (ignore obj))
                              (toggle-class login-menu-right-menu
                                            "w3-black fa-angle-double-up")
                              (toggle-class search-icon-for-mobile
                                            "w3-hide-small")
                              (dolist (login-menu-right-item login-menu-right-items)
                                (toggle-class login-menu-right-item
                                              "w3-hide-small"))))
              (set-on-click search-icon-for-mobile
                            (lambda (obj)
                              (declare (ignore obj))
                              (toggle-class search-icon-for-mobile
                                            "w3-black w3-right fa-angle-double-up w3-mobile")
                              (toggle-class login-menu-right-menu
                                            "w3-hide-small")
                              (toggle-class search-form
                                            "w3-hide-small")))))
	  (when login-link
            (create-a menu
                      :class "w3-bar-item ackfock-theme w3-right"
                      :content "login"
		      :link login-link)))
      (let ((menu-margin-div (create-div body)))
        (setf (height menu-margin-div) (height menu)))
      ;; SECTION: Content area
      (when content
        (typecase content
	  (string
	   (create-div body :content content))
	  (function
	   (funcall content body))
	  (t
	   (create-div body :content (format nil "~A" content)))))
      ;; SECTION: Footer
      (create-br body)
      (create-br body)
      (create-div body :content (format nil "~A" (footer website))))))
