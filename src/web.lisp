(in-package :cl-user)
(defpackage ackfock.web
  (:use #:cl
        #:caveman2
        #:ackfock.config
        #:ackfock.page)
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
;; Utilities
(defvar *email-validator* (make-instance 'clavier:email-validator))

(defmacro with-login-user (&body body)
  "Continue only if the user is logined, redirect to login page otherwise."
  `(cond ((null (ackfock.utils:current-user)) (redirect "/login"))
           (t ,@body)))
;;
;; Routing rules

(defroute "/" ()
  (with-login-user
    (apply #'home-page
           (when (gethash :home-page-message *session*)
             (print (list (pop (gethash :home-page-message *session*))))))))

(defroute "/login" ()
  (login-page))

(defroute ("/login" :method :POST) (&key _parsed)
  (let ((email (cdr (assoc "email" _parsed :test #'string=)))
        (password (cdr (assoc "password" _parsed :test #'string=))))
    (if (setf (ackfock.utils:current-user) (ackfock.model:authenticate email password))
        (redirect "/")
        (login-page :message "Email or password incorrect"))))

(defroute ("/logout" :method :POST) ()
  (setf (ackfock.utils:current-user) nil)
  (redirect "/"))

(defroute "/sign-up" ()
  (login-page :sign-up t))

(defroute ("/sign-up" :method :POST) (&key _parsed)
  (let ((email (cdr (assoc "email" _parsed :test #'string=)))
        (username (cdr (assoc "username" _parsed :test #'string=)))
        (password (cdr (assoc "password" _parsed :test #'string=))))
    (cond ((find t
                 (list email username password)
                 :key #'str:emptyp) (login-page :message "Email, username, or password empty"
                                                :sign-up t))
          ((null (clavier:validate *email-validator*
                                   email)) (login-page :message "Not a valid email address"
                                                       :sign-up t))
          (t (setf (ackfock.utils:current-user)
                   (ackfock.model:new-user email username password))
             (redirect "/")))))

(defroute ("/add-memo" :method :POST) (&key _parsed)
  (with-login-user
    (let ((content (cdr (assoc "content" _parsed :test #'string=))))
      (unless (str:emptyp content)
        (ackfock.model:new-memo (ackfock.utils:current-user) content))))
  (redirect "/"))

(defroute ("/ackfock-memo" :method :POST) (&key _parsed)
  (with-login-user
    (let ((memo-uuid (cdr (assoc "uuid" _parsed :test #'string=)))
          (ackfock (cdr (assoc "ackfock" _parsed :test #'string=)))
          (as-target-user-ackfock (and (assoc "as_target_user_ackfock" _parsed :test #'string=)
                                       t)))
      (unless (or (str:emptyp memo-uuid)
                  (str:emptyp ackfock))
        (ackfock.model:ackfock-memo memo-uuid ackfock
                                    :as-target-user-ackfock as-target-user-ackfock))))
  (redirect "/"))

;; TODO: solve race condition
(defroute ("/send-memo" :method :POST) (&key _parsed)
  (with-login-user
    (let ((memo-uuid (cdr (assoc "uuid" _parsed :test #'string=)))
          (recipient (ackfock.model:get-user-by-email (cdr (assoc "recipient" _parsed :test #'string=)))))
      (if recipient
          (ackfock.model:send-memo memo-uuid recipient)
          (push "NO SUCH USER" (gethash :home-page-message *session*)))))
  (redirect "/"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
