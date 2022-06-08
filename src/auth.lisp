(in-package :cl-user)
(defpackage ackfock.auth
  (:use :cl :clog :clog-web :clog-auth)
  (:export #:login
           #:sign-up
           #:reset-password))
(in-package :ackfock.auth)

;; think about how to redefine below functions in a meta way on clog-web-dbi

(defun make-token ()
  "Create a unique token used to associate a browser with a user"
  (cl-pass:hash (get-universal-time)
                :type :pbkdf2-sha1
                :iterations 512))

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
     (cond ((not
	     (equal (form-result result "password")
		    (form-result result "repass")))
	    (clog-web-alert body "Mismatch"
			    "The passwords do match."
			    :time-out 3
			    :place-top t))
	   ((< (length (form-result result "password")) 4)
	    (clog-web-alert body "Missize"
			    "The passwords must at least 4 characters."
			    :time-out 3
			    :place-top t))
	   ((< (length (form-result result "username")) 4)
	    (clog-web-alert body "Missize"
			    "The username must be at least 4 characters."
			    :time-out 3
			    :place-top t))
	   (t
	    (let ((contents (dbi:fetch-all
			     (dbi:execute
			      (dbi:prepare
			       sql-connection
			       "select username from users where username=?")
			      (list (form-result result "username"))))))
	      (cond (contents
		     (clog-web-alert body "Exists"
				     "The username is not available."
				     :time-out 3
				     :place-top t))
		    (t
		     (dbi:do-sql
		       sql-connection
		       (sql-insert*
			"users"
			`(:username ,(form-result result "username")
			  :password ,(cl-pass:hash (form-result result "password"))
			  :token    ,(make-token))))
		     (url-replace (location body) next-step)))))))))

(defun reset-password (sql-connection username &key (new-password "password"))
  "Reset USERNAME's password to :NEW-PASSWORD"
  (print username)
  (dbi:do-sql
    sql-connection
    (sql-update
     "users"
     `(:password ,(cl-pass:hash new-password))
     "username=?")
    (list username)))
