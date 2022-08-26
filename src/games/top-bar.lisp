(in-package :cl-user)
(defpackage ackfock.game.top-bar
  (:use :cl :clog :clog-web)
  (:import-from :ackfock.game
                #:gamify)
  (:export #:top-bar))
(in-package :ackfock.game.top-bar)

(defmethod gamify ((object (eql 'top-bar)) (context ackfock.game.page:clog-web-page))
  (let* ((body (ackfock.game.page:clog-body context))
         (website        (get-web-site body))
	 (color-class    (get-setting website :color-class "w3-black"))
	 ;; use this in the future (username-link  (get-setting website :username-link "/"))
	 (login-link     (get-setting website :login-link "/login"))
         (top-bar (create-div body
                           :class (str:concat "w3-top w3-bar " color-class)))
         (logo-a (create-a top-bar
                           :link (url website)
                           :content (title website)
                           :class "w3-bar-item w3-xlarge w3-sans-serif ackfock-theme")))
    (setf (z-index top-bar) 3)
    (if (profile website)
        (progn
          (when (string= (ackfock.game.page:name context) "main")
            (setf (connection-data-item body
                                        :sidebar-top-bar-button-for-mobile)
                  (create-div top-bar
                              :class "w3-bar-item fa fa-bars w3-hide-medium w3-hide-large"))
            (add-class logo-a "w3-hide-small"))
          (let* ((search-form (create-form top-bar
                                           :action "/search"
                                           :method :GET
                                           :class "w3-bar-item w3-hide-small w3-mobile"))
                 (login-menu-right-menu (create-div top-bar
                                                    :class "w3-bar-item fa fa-user w3-hide-medium w3-hide-large w3-right"))
                 (search-icon-for-mobile (create-div top-bar
                                                     :class "w3-bar-item fa fa-search w3-hide-medium w3-hide-large w3-right"))
                 (login-menu-right-class "w3-bar-item ackfock-theme w3-right w3-mobile w3-hide-small")
                 (login-menu-right-items nil))
            (create-form-element search-form
                                 :search
                                 :name "q"
                                 :value (when (string= (ackfock.game.page:name context) "search")
                                          (form-data-item (form-get-data body)
                                                          "q"))
                                 :hidden (not (profile website)))
            (create-button search-form
                           :content "search"
                           :hidden (not (profile website)))
            (push (create-a top-bar
                            :class login-menu-right-class
                            :content "logout"
                            :link "/logout")
                  login-menu-right-items)
            (push (create-a top-bar
                            :class login-menu-right-class
                            :content "change password"
                            :link "/pass")
                  login-menu-right-items)
            (push (create-div top-bar
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
          (create-a top-bar
                    :class "w3-bar-item ackfock-theme w3-right"
                    :content "login"
		    :link login-link)))
    (let ((top-bar-margin-div (create-div body)))
      (setf (height top-bar-margin-div) (height top-bar)))
    top-bar))
