(in-package :cl-user)
(defpackage ackfock.web
  (:use :cl
        :caveman2
        :ackfock.config
        :ackfock.view
        :ackfock.model)
  (:export :*web*))
(in-package :ackfock.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defvar *email-validator* (make-instance 'clavier:email-validator))
;;
;; Routing rules

(defroute "/" ()
  (let ((current-user (gethash :user *session*)))
    (cond ((null current-user) (redirect "/login"))
          (t (home-page current-user)))))

(defroute "/login" ()
  (login-page))

(defroute ("/login" :method :POST) (&key _parsed)
  (let ((email (cdr (assoc "email" _parsed :test #'string=)))
        (password (cdr (assoc "password" _parsed :test #'string=))))
    (if (setf (gethash :user *session*) (authenticate email password))
        (redirect "/")
        (login-page :message "Email or password incorrect"))))

(defroute ("/logout" :method :POST) ()
  (setf (gethash :user *session*) nil)
  (redirect "/"))

(defroute "/sign-up" ()
  (login-page :sign-up t))

(defroute ("/sign-up" :method :POST) (&key _parsed)
  (let ((email (cdr (assoc "email" _parsed :test #'string=)))
        (username (cdr (assoc "username" _parsed :test #'string=)))
        (password (cdr (assoc "password" _parsed :test #'string=))))
    (cond ((or (string= email "")
               (string= username "")
               (string= password "")) (login-page :message "Email, username, or password empty"
                                                  :sign-up t))
          ((null (clavier:validate *email-validator*
                                   email)) (login-page :message "Not a valid email address"
                                                       :sign-up t))
          (t (setf (gethash :user *session*)
                   (new-user email username password))
             (redirect "/")))))

(defroute ("/add-memo" :method :POST) (&key _parsed)
  (let ((current-user (gethash :user *session*))
        (content (cdr (assoc "content" _parsed :test #'string=))))
    (when (and current-user
               content)
      (new-memo current-user content))
    (redirect "/")))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
