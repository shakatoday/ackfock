(in-package :cl-user)
(defpackage ackfock.auth
  (:use :cl :clog :clog-web :clog-auth :ackfock.authenticate-user-email)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:login
           #:sign-up
           #:current-user
           #:change-password
           #:*lack-sessions*
           #:sync-current-session))
(in-package :ackfock.auth)

(defvar *lack-sessions* (make-hash-table :test 'equal :synchronized t))
;; think about how to redefine below functions in a meta way on clog-web-dbi

(defun sync-current-session (env)
  (let ((current-lack-session-id (getf (getf env
                                             :lack.session.options)
                                       :id)))
    (setf (gethash current-lack-session-id *lack-sessions*) (getf env :lack-session))))

(defun lack-session-id-from-browser-cookie (clog-obj)
  (let* ((cookie (js-query clog-obj "document.cookie"))
         (key-value-pair-strings (ppcre:split ";" cookie))
         (key-value-pair-alist (mapcar (lambda (string)
                                         (let ((pairs (ppcre:split "="
                                                                   string
                                                                   :limit 2)))
                                           (cons (first pairs)
                                                 (second pairs))))
                                       key-value-pair-strings)))
    (cdr (assoc "lack.session"
                key-value-pair-alist
                :test #'string=))))

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

(defun change-password (body sql-connection &key (title "Change Password")
                                              (next-step "/"))
  "Setup a change password form and handle change of password"
  (check-type body clog-body)
  (clog-web-form
   body title
   `(("Old Password" "oldpass" :password)
     ("New Password" "password" :password)
     ("Retype Password" "repass" :password))
   (lambda (result)
     (cond ((not
             (equal (form-result result "password")
                    (form-result result "repass")))
            (clog-web-alert body "Password Mismatch"
                            "The new passwords do match."
                            :time-out 3
                            :place-top t))
           ((< (length (form-result result "password")) 4)
            (clog-web-alert body "Password Missize"
                            "The new passwords must at least 4 characters."
                            :time-out 3
                            :place-top t))
           (t
            (let ((contents (dbi:fetch-all
                             (dbi:execute
                              (dbi:prepare
                               sql-connection
                               "select uuid, password from users where uuid=?")
                              (list (ackfock.model-definition:user-uuid (profile (get-web-site body))))))))
              (cond ((and contents
                          (cl-pass:check-password (form-result result "oldpass")
                                                  (getf (car contents) :|password|)))
                     (dbi:do-sql
                       sql-connection
                       (sql-update
                        "users"
                        `(:password ,(cl-pass:hash (form-result result "password")))
                        "uuid=?")
                       (list (ackfock.model-definition:user-uuid (profile (get-web-site body)))))
                     (url-replace (location body) next-step))
                    (t
                     (clog-web-alert body "Old Password"
                                     "Old password is incorrect."
                                     :time-out 3
                                     :place-top t)))))))))

(defun sign-up (body sql-connection &key (title "Sign Up")
				      (next-step "/"))
  "Setup a sign-up form and process a new sign-up"
  (check-type body clog-body)
  (let ((form-top-div (create-div body)))
    (clog-web-form
     form-top-div
     title
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
	        (clog-web-alert form-top-div "Mismatch"
			        "The passwords do match."
			        :time-out 3
			        :place-top t))
	       ((< (length password) 4)
	        (clog-web-alert form-top-div "Missize"
			        "The passwords must at least 4 characters."
			        :time-out 3
			        :place-top t))
	       ((< (length username) 4)
	        (clog-web-alert form-top-div "Missize"
			        "The username must be at least 4 characters."
			        :time-out 3
			        :place-top t))
               ((null (clavier:validate ackfock.utils:*email-validator*
                                        email))
                (clog-web-alert form-top-div "Email invalid"
			        "Not a valid email address"
			        :time-out 3
			        :place-top t))
	       (t
	        (let ((contents (dbi:fetch-all
			         (dbi:execute
			          (dbi:prepare
			           sql-connection
			           "select email from users where email=? or username=?")
			          (list email username)))))
                  ;; Race condition between check email availability and sql-insert*
	          (cond (contents
                         (if (find email contents
                                   :key (lambda (row) (getf row :|email|))
                                   :test #'string=)
		             (clog-web-alert form-top-div "Exists"
				             "The email is not available."
				             :time-out 3
				             :place-top t)
		             (clog-web-alert form-top-div "Exists"
				             "The username is not available."
				             :time-out 3
				             :place-top t)))
		        (t
                         (let ((token (make-token)))
		           (dbi:do-sql
		             sql-connection
		             (sql-insert*
			      "users"
			      `(:email ,email
                                :username ,username
			        :password ,(cl-pass:hash password)
			        :token    ,token)))
                           (ackfock.utils:send-authentication-email email
                                                                    username
                                                                    (format nil
                                                                            "~a/activate/~a"
                                                                            ackfock.config:*application-url*
                                                                            (ackfock.model-definition:authentication-code-code (create-authentication-code email))))
                           (store-authentication-token body token)
		           (url-replace (location body) next-step))))))))))))
