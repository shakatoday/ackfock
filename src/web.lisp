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

;;
;; Routing rules

(defroute "/" ()
  (let ((current-user (print (gethash :user *session*))))
    (cond ((null current-user) (redirect "/login"))
          (t (home-page current-user)))))

(defroute "/login" ()
  (login-page))

(defroute ("/login" :method :POST) (&key _parsed)
  (let ((email (cdr (assoc "email" _parsed :test #'string=)))
        (password (cdr (assoc "password" _parsed :test #'string=))))
    (if (setf (gethash :user *session*) (authenticate email password))
        (redirect "/")
        (login-page "Email or password incorrect"))))

(defroute ("/logout" :method :POST) ()
  (setf (gethash :user *session*) nil)
  (redirect "/"))

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
