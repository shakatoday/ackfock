(in-package :cl-user)
(defpackage ackfock.auth
  (:use :cl :clog :clog-web :clog-auth :ackfock.authenticate-user-email)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:login
           #:sign-up
           #:current-user))
(in-package :ackfock.auth)

;; think about how to redefine below functions in a meta way on clog-web-dbi

(defun make-token ()
  "Create a unique token used to associate a browser with a user"
  (str:downcase (uuid:print-bytes nil (uuid:make-v4-uuid))))

(defun-with-db-connection current-user (clog-body)
  (rutils:when-it (get-authentication-token clog-body)
    ;; servere symbol conflicts between datafly, sxql, and clog
    (datafly:retrieve-one
     (sxql:select :*
       (sxql:from :users)
       (sxql:where (:= :token rutils:it)))
     :as 'ackfock.model-definition:user)))

(defun login (body sql-connection email password)
  "Login and set current authentication token, it does not remove token
if one is present and login fails."
  (check-type body clog-body)
  (let ((contents (dbi:fetch-all
		   (dbi:execute
		    (dbi:prepare
		     sql-connection
		     "select * from users where email=?")
		    (list email)))))
    (when (and contents
               (cl-pass:check-password password (getf (car contents) :|password|)))
      (store-authentication-token body (getf (car contents) :|token|)))))

(defun sign-up (body sql-connection &key (title "Sign Up")
				      (next-step "/login"))
  "Setup a sign-up form and process a new sign-up"
  (check-type body clog-body)
  (clog-web-form
   body title
   `(("Email" "email" :email)
     ("Username" "username")
     ("Password" "password" :password)
     ("Retype Password" "repass" :password))
   (lambda (result)
     (let #.(mapcar (lambda (variable)
                      `(,variable (form-result result ,(str:downcase (symbol-name variable)))))
                    '(email username password))
       (cond ((not
	       (equal password
		      (form-result result "repass")))
	      (clog-web-alert body "Mismatch"
			      "The passwords do match."
			      :time-out 3
			      :place-top t))
	     ((< (length password) 4)
	      (clog-web-alert body "Missize"
			      "The passwords must at least 4 characters."
			      :time-out 3
			      :place-top t))
	     ((< (length username) 4)
	      (clog-web-alert body "Missize"
			      "The username must be at least 4 characters."
			      :time-out 3
			      :place-top t))
             ((null (clavier:validate ackfock.utils:*email-validator*
                                      email))
              (clog-web-alert body "Email invalid"
			      "Not a valid email address"
			      :time-out 3
			      :place-top t))
	     (t
	      (let ((contents (dbi:fetch-all
			       (dbi:execute
			        (dbi:prepare
			         sql-connection
			         "select email from users where email=?")
			        (list email)))))
                ;; Race condition between check email availability and sql-insert*
	        (cond (contents
		       (clog-web-alert body "Exists"
				       "The email is not available."
				       :time-out 3
				       :place-top t))
		      (t
		       (dbi:do-sql
		         sql-connection
		         (sql-insert*
			  "users"
			  `(:email ,email
                            :username ,username
			    :password ,(cl-pass:hash password)
			    :token    ,(make-token))))
                       (ackfock.utils:send-authentication-email email
                                                                username
                                                                (format nil
                                                                        "~a/activate/~a"
                                                                        ackfock.config:*application-url*
                                                                        (ackfock.model-definition:authentication-code-code (create-authentication-code email))))
		       (url-replace (location body) next-step))))))))))
