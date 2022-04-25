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
           #:create-authentication-code))
(in-package :ackfock.model)

(defconstant +dummy-uuid+ :A2543078-7D5B-4F40-B6FD-DBC58E863752)

(defun dummy-uuid ()
  (string +dummy-uuid+))

(defun recursive-append-keywords-to-args (args)
  "If any arg in args doesn't have a keyword nor a list before it, append a keyword with the same symbol name as the arg." ; not a correct doc!
  (labels ((recur-gen-args (args parse-keyword-p)
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
    (recur-gen-args args t)))

(set-dispatch-macro-character #\# #\?
                              (lambda (stream subchar n)
                                (declare (ignore subchar n))
                                (let ((input (read stream)))
                                  (if (trivial-types:proper-list-p input)
                                      (cons (first input)
                                            (recursive-append-keywords-to-args (cdr input)))
                                      input))))

(defmacro $set= (&rest args)
  "Expand to SXQL:SET= with args reproduced by RECURSIVE-APPEND-KEYWORDS-TO-ARGS"
  `(set= ,@(recursive-append-keywords-to-args args)))

(defun-with-db-connection new-user (email username password)
  "Insert a new user into database and return an ACKFOCK.MODEL::USER instance"
  (retrieve-one 
   (insert-into :users
     ($set= email
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
       ($set= :source_user_id (user-uuid user)
              content)))))

(defun-with-db-connection ackfock-memo (memo-uuid ackfock &key as-target-user-ackfock)
  "Ackfock the memo with the given MEMO-UUID. This memo has to be either created by current user or shared by the creator."
  (unless (or (str:emptyp ackfock) (null (ackfock.utils:current-user)))
    (execute
     (update :memo
       (set= (if as-target-user-ackfock
                 :target_user_ackfock
                 :source_user_ackfock)
             ackfock)
       (where (:and (:= :uuid memo-uuid)
                    (:or (:= :source_user_id (user-uuid (ackfock.utils:current-user)))
                         (:= :target_user_id (user-uuid (ackfock.utils:current-user))))))))))

(defun-with-db-connection send-memo (memo-uuid recipient)
  (execute
   (update :memo
     (set= :target_user_id (user-uuid recipient))
     (where (:and (:= :uuid memo-uuid)
                  (:= :source_user_id (user-uuid (ackfock.utils:current-user))))))))

(defun-with-db-connection get-user-by-email (email)
  (retrieve-one
   (select :*
     (from :users)
     (where #?(:= email)))
   :as 'user))

(defun-with-db-connection create-authentication-code (email &key (ttl-in-sec (* 60 60))) ; by default code will expire in 1 hour
  "Create an authentication code for EMAIL with TTL-IN-SEC, insert it into database, and return an AUTHENTICATION-CODE object if success."
  (let ((valid-until (local-time:timestamp+ (local-time:now)
                                            ttl-in-sec
                                            :sec))
        (code (str:downcase (uuid:print-bytes nil (uuid:make-v4-uuid)))))
    (retrieve-one
     (insert-into :authentication_code
       ($set= email
              code
              valid-until)
       (returning :*))
     :as 'authentication-code)))

(defun-with-db-connection get-authentication-code-by-code (code)
  (retrieve-one
   (select :*
     (from :authentication_code)
     (where #?(:= code)))
   :as 'authentication-code))

(set-dispatch-macro-character #\# #\? nil)
