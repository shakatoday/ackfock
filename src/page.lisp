(in-package :cl-user)
(defpackage ackfock.page
  (:use :cl :ackfock.view)
  (:export #:login-page
           #:home-page))
(in-package :ackfock.page)

(defmacro with-page ((&key title with-logout-button) &body body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:title ,title))
      (:body
       ,@(when with-logout-button
           '((:form :action "/logout" :method "post"
              (:input :type "submit" :value "Logout"))))
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

(defun home-page (current-user &optional message)
  (with-page (:title "My Memos" :with-logout-button t)
    (:h1 "Me")
    (:p (cl-who:str (ackfock.model:user-email current-user)))
    (:p (cl-who:str (ackfock.model:user-username current-user)))
    (:h2 "My Memos")
    (:table (:tr (:th "|_____Memo______|")
                 (:th "____with_________|")
                 (:th "___I__ack?_______|")
                 (:th "___he/she__ack?__|"))
            (dolist (memo (ackfock.model:user-memos current-user))
              (cl-who:htm (:tr (cl-who:str (render memo))))))
    (when message
      (cl-who:htm
       (:p (cl-who:str message))))
    (:form :action "/add-memo" :method "post"
           (:h3 "Add Memo")
           (:p "Content" (:br)
               (:input :type "text" :name "content"))
           (:p (:input :type "submit" :value "Add Memo")))))
