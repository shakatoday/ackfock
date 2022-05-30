(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :datafly :sxql :ackfock.model-definition)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:new-user
           #:user-memos
           #:authenticate
           #:new-memo
           #:ackfock-memo
           #:get-user-by-email
           #:send-memo
           #:create-authentication-code
           #:authenticate-user-email))
(in-package :ackfock.model)

(eval-when (:compile-toplevel :load-toplevel)
  (labels ((recur-gen-args (args parse-keyword-p) ;  If any arg in args doesn't have a keyword nor a list before it, append a keyword with the same symbol name as the arg. ; <- not a correct comment!
             (when args
               (let ((first-arg (car args)))
                 (if (and parse-keyword-p
                          (symbolp first-arg)
                          (not (keywordp first-arg)))
                     (append (list (intern (str:upcase (str:snake-case (symbol-name first-arg)))
                                           :keyword)
                                   first-arg)
                             (recur-gen-args (cdr args) parse-keyword-p))
                     (cons first-arg
                           (recur-gen-args (cdr args) (not parse-keyword-p))))))))
    (set-macro-character #\$ nil) ; see the comment right after the next form.
    (set-macro-character #\$
                         (lambda (stream char)
                           (declare (ignore char))
                           (let ((input (read stream)))
                             (if (trivial-types:proper-list-p input)
                                 (cons (first input)
                                       (recur-gen-args (cdr input)
                                                       t))
                                 (alexandria:symbolicate '$ input))))))) ; there is a self reference, so we call (set-macro-character #\$ nil) before to avoid '$ here trigger another reader macro call.

(defconstant +dummy-uuid+ :A2543078-7D5B-4F40-B6FD-DBC58E863752)

(defun dummy-uuid ()
  (string +dummy-uuid+))

(defun string-to-ackfock (string)
  "If the STRING is \"ACK\" or \"FOCK\", this function returns :ACK or :FOCK. All the other cases it returns nil"
  (and (stringp string)
       (member string
               '("ACK" "FOCK")
               :test #'string=)
       (alexandria:make-keyword string)))

(defun-with-db-connection new-user (email username password)
  "Insert a new user into database and return an ACKFOCK.MODEL::USER instance"
  (retrieve-one 
   (insert-into :users
     $(set= email
            username
            :password_salted (cl-pass:hash password))
     (returning :*))
   :as 'user))

(defun-with-db-connection user-memos (user &key (as-source-user t) as-target-user (limit 20) (offset 0))
  (retrieve-all
   (select :*
     (from :memo)
     (where (:or (:= :source_user_id (cond ((null as-source-user) (dummy-uuid)) ; for uuid type consistency in postgresql
                                           (t (user-uuid user))))
                 (:= :target_user_id (cond ((null as-target-user) (dummy-uuid)) ; for uuid type consistency in postgresql
                                           (t (user-uuid user))))))
     (order-by (:desc :updated_at))
     (limit limit)
     (offset offset))
   :as 'memo))

(defun-with-db-connection authenticate (email password)
  (let ((user-data (retrieve-one
                    (select :*
                      (from :users)
                      (where (:= :email email))))))
    (and user-data
         (cl-pass:check-password password
                                 (getf user-data :password-salted))
         (apply #'make-user
                user-data))))

(defun-with-db-connection new-memo (user content)
  (when (user-p user)
    (execute
     (insert-into :memo
       $(set= :source_user_id (user-uuid user)
              content)))))

(defun-with-db-connection get-user-by-email (email)
  (retrieve-one
   (select :*
     (from :users)
     (where $(:= email)))
   :as 'user))

(defun-with-db-connection create-authentication-code (email &key (ttl-in-sec (* 60 60))) ; by default code will expire in 1 hour
  "Create an authentication code for EMAIL with TTL-IN-SEC, insert it into database, and return an AUTHENTICATION-CODE object if success."
  (let ((valid-until (local-time:timestamp+ (local-time:now)
                                            ttl-in-sec
                                            :sec))
        (code (str:downcase (uuid:print-bytes nil (uuid:make-v4-uuid)))))
    (retrieve-one
     (insert-into :authentication_code
       $(set= email
              code
              valid-until)
       (returning :*))
     :as 'authentication-code)))

(defun-with-db-connection get-authentication-code-by-code (code)
  (retrieve-one
   (select :*
     (from :authentication_code)
     (where $(:= code)))
   :as 'authentication-code))

(defun-with-db-connection update-user-email-authenticated-at (email email-authenticated-at)
  (retrieve-one
   (update :users
     $(set= email-authenticated-at)
     (where $(:= email))
     (returning :*))
   :as 'user))

(defun authenticate-user-email (code)
  "Return corresponding ACKFOCK.MODEL-DEFINITION:USERS when success, return nil otherwise"
  (let ((authentication-code (get-authentication-code-by-code code)) ; Race condition?
        (now (local-time:now)))
    (when (and authentication-code
               (local-time:timestamp<= now ; otherwise the code is timeout
                                       (authentication-code-valid-until authentication-code)))
      (update-user-email-authenticated-at (authentication-code-email authentication-code)
                                          now))))

(eval-when (:compile-toplevel :load-toplevel)
  (set-macro-character #\$ nil))
