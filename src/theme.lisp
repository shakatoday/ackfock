(in-package :cl-user)
(defpackage ackfock.theme
  (:use :cl :clog :clog-web :ackfock.model-definition)
  (:export #:ackfock-theme
           #:*color-class*))
(in-package :ackfock.theme)

(defparameter *color-class* "w3-khaki")

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
	 (border-class   (get-setting website :border-class ""))
	 (button-class   (get-setting website :button-class
				      "w3-button w3-round-xlarge
                                       w3-tiny w3-border w3-padding-small"))
	 (text-class     (get-setting website :text-class ""))
	 (login-link     (get-setting website :login-link "/login"))
	 (signup-link    (get-setting website :signup-link "/signup"))
	 (username-link  (get-setting website :username-link "/logout"))
	 (menu-property  (get-property properties :menu "w3-black"))
	 (content        (get-property properties :content "")))
    ;;
    ;; Setup CSS style changes
    (let ((sb (create-style-block body)))
      (add-style sb :element "a.clog-theme" '(("text-decoration" "none"))))
    ;;
    ;; Page layout
    ;;
    ;; SECTION: Above of menu bar
    (let* ((row   (create-web-auto-row body))
	   (left  (create-web-auto-column row))
	   (right (create-web-auto-column row :vertical-align :middle)))
      (when (logo website)
	(set-geometry (create-img (create-a left
					    :link (url website))
				  :url-src (logo website))
		      :height 75))
      (create-span (create-a right
			     :class "clog-theme"
			     :link (url website))
		   :content (title website)
		   :class "w3-xlarge w3-sans-serif"))
    ;; SECTION: Menu bar
    (let ((menu (create-web-menu-bar body :class "w3-card-4 w3-margin-top")))
      (add-class menu color-class)
      (with-clog-create menu
          (div (:bind menu-inner-div)
               (div ()
	            (form-element (:search :class "w3-margin-left"))
                    (button (:content "search")))
               (div (:bind menu-right-div)))
        (center-children menu-inner-div
                         :vertical t
                         :horizontal nil)
        (setf (justify-content menu-inner-div) :space-between)
	(if (profile website)
	    (progn
              (create-web-menu-item menu-right-div :content (user-username (profile website)))
              (create-web-menu-item menu-right-div :content "change password"
                                                   :link "/pass")
              (create-web-menu-item menu-right-div :content "logout"
                                                   :link "/logout"))
	    (when login-link
	      (create-web-menu-item menu-right-div :content "login"
					           :link login-link)))))
    ;; SECTION: Content area
    (create-br body)
    (when content
      (typecase content
	(string
	 (create-div body :content content))
	(function
	 (funcall content body))
	(t
	 (create-div body :content (format nil "~A" content)))))
    ;; SECTION: Special pages - Login
    (when (eq page :login)
      (let* ((outter    (create-web-container body))
	     (form      (create-form outter))
	     (p1        (create-p form))
	     (l1        (create-label p1 :content "Email"
					 :class text-class))
	     (user      (create-form-element p1 :email
					     :name "email"
					     :class (format nil "w3-input ~A" border-class)))
	     (p2        (create-p form))
	     (l2        (create-label p2 :content "Password"
					 :class text-class))
	     (pass      (create-form-element p2 :password
					     :name "password"
					     :class (format nil "w3-input ~A" border-class)))
	     (p3        (create-p form)))
	(declare (ignore l1 l2 p3))
	(setf (maximum-width outter) (unit :px 500))
	(setf (requiredp user) t)
	(setf (requiredp pass) t)
	(create-form-element form :submit :value "Submit"
					  :class (format nil "~A ~A" "w3-button" color-class))
	(set-on-submit form (getf properties :on-submit))
	(when signup-link
	  (create-a form :class "w3-right" :content "sign up" :link signup-link))))
    ;; SECTION: Footer
    (create-br body)
    (create-br body)
    (create-div body :content (format nil "~A" (footer website)))))
