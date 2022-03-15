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
  (print (gethash :user *session*))
  (render #P"index.html"))

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

        

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
