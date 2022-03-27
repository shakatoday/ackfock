(in-package :cl-user)
(defpackage ackfock.page
  (:use :cl :ackfock.view)
  (:import-from :ackfock.model-definition
                #:user-email
                #:user-username)
  (:import-from :ackfock.model
                #:user-memos)
  (:export #:login-page
           #:home-page))
(in-package :ackfock.page)

(defmacro with-page ((&key title) &body body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/foundation-sites@6.7.4/dist/css/foundation.min.css" :crossorigin "anonymous")
       (:link :rel "stylesheet" :href "/css/main.css")
       (:script :src "https://cdn.jsdelivr.net/npm/foundation-sites@6.7.4/dist/js/foundation.min.js" :crossorigin "anonymous")
       (:title ,title))
      (:body
       (:nav :class "top-bar topbar-responsive" 
             (:div :class "top-bar-title" 
                   ;; (:span :data-responsive-toggle "topbar-responsive" :data-hide-for "medium" 
                   ;;        (:button :class "menu-icon" :type "button" "data-toggle" ))
                   (:a :class "topbar-responsive-logo" :href "#" (:strong "Ackfock")))
             (:div :id "topbar-responsive" :class "topbar-responsive-links" 
                   (:div :class "top-bar-right" 
                         (:ul :class "menu simple vertical medium-horizontal" 
                              (:li
                               (when (ackfock.utils:current-user)
                                 (cl-who:htm (:form :action "/logout" :method "post"
                                                    (:button :type "submit"
                                                             :class "button hollow topbar-responsive-button" "Logout")))))))))
       ,@body))))

(defun login-page (&key message sign-up)
  (with-page (:title (if sign-up
                         "Sign Up"
                         "Login"))
    (:h1 (cl-who:str (if sign-up
                         "Sign Up"
                         "Login")))
    (when message
      (cl-who:htm
       (:p (cl-who:str message))))
    (:form :action (if sign-up
                       "/sign-up"
                       "/login")
           :method "post"
           (:p "Email" (:br)
               (:input :type "email" :name "email"))
           (when sign-up
             (cl-who:htm
              (:p "Username" (:br)
                  (:input :type "text" :name "username"))))
           (:p "Password" (:br)
               (:input :type "password" :name "password"))
           (when sign-up
             (cl-who:htm
              (:p "Confirm password" (:br)
                  (:input :type "password" :name "confirm_password"))))
           (:p (:input :type "submit"
                       :value (if sign-up
                                  "register"
                                  "login")))
           (if sign-up
               (cl-who:htm (:a :href "/login" "Login page "))
               (cl-who:htm (:a :href "/sign-up" "Sign-up page"))))))

(defun home-page (&optional message)
  (with-page (:title "My Memos")
    (when message
      (cl-who:htm
       (:p (cl-who:str message))))
    (:h1 "My Account")
    (:p (cl-who:str (user-email (ackfock.utils:current-user))))
    (:p (cl-who:str (user-username (ackfock.utils:current-user))))
    (:h2 "My Memos")
    (:table (:tr (:th "Memo")
                 (:th "with")
                 (:th "I ack?")
                 (:th "he/she ack?"))
            (dolist (memo (user-memos (ackfock.utils:current-user) :as-target-user t))
              (cl-who:str (render memo))))
    (:form :action "/add-memo" :method "post"
           (:h3 "Add Memo")
           (:p "Content" (:br)
               (:input :type "text" :name "content"))
           (:p (:input :type "submit" :value "Add Memo")))))
