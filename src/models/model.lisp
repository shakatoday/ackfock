(in-package :cl-user)
(defpackage ackfock.model
  (:use :cl :ackfock.db :datafly :sxql)
  (:export #:ackfock
           #:string-to-ackfock
           #:user-ackfock
           #:user-ackfock-user
           #:user-ackfock-ackfock
           #:user-ackfock-created-at
           #:plist-to-user-ackfock
           #:user
           #:user-uuid
           #:user-email
           #:user-username
           #:user-p
           #:make-user
           #:user-from-plist
           #:user-by-email
           #:channel
           #:channel-p
           #:channel-uuid
           #:channel-name
           #:make-private-channel
           #:private-channel-p
           #:memo
           #:memo-p
           #:make-memo
           #:memo-uuid
           #:memo-content
           #:memo-channel-id
           #:memo-creator-id
           #:memo-parent-memo-id
           #:activation-code-code
           #:activation-code-email
           #:activation-code-valid-until
           #:activation-code
           #:invitation-code
           #:invitation-code-code
           #:invitation-code-valid-until
           #:invitation-code-used-by-user-id
           #:invitation-code-channel-id
           #:user-ackfock-list-to-alist-by-ackfock
           #:valid-email-address-p))
(in-package :ackfock.model)

(defun valid-email-address-p (string)
  (serapeum:true
   (ppcre:scan "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}$" string)))

(defmodel (user (:inflate created-at #'datetime-to-timestamp))
  uuid
  email
  username
  created-at)

(defun-with-db-connection user-by-email (email)
  (when (and (str:non-blank-string-p email)
             (valid-email-address-p email))
    (retrieve-one
     (select :*
       (from :users)
       (where (:= :email email)))
     :as 'user)))

(defmacro user-from-plist (plist)
  (cons 'make-user
        (reduce #'append
                (mapcar (lambda (keyword-arg)
                          `(,keyword-arg (getf ,plist ,keyword-arg))) ; use the later created-at so we need a copy and remf
                                        ; TODO: created-at needs an inflation-function
                        '(:uuid :email :username :created-at)))))

(deftype ackfock () '(member :ACK :FOCK)) ; the enum type in DB uses uppercase. we capitalize :ACK :FOCK as a reminder even if symbols in CL are uppercase by default.

(defun string-to-ackfock (string)
  "If the STRING is \"ACK\" or \"FOCK\", this function returns :ACK or :FOCK. All the other cases it returns nil"
  (and (stringp string)
       (member string
               '("ACK" "FOCK")
               :test #'string=)
       (alexandria:make-keyword string)))

(serapeum:defconstructor user-ackfock
  (user user)
  (ackfock ackfock)
  (created-at local-time:timestamp))

(defun plist-to-user-ackfock (plist)
  (when plist
    (let ((copy-plist (copy-list plist)))
      (remf copy-plist :created-at)
      (user-ackfock (user-from-plist copy-plist)
                    (string-to-ackfock (getf plist :ackfock))
                    (datetime-to-timestamp (getf plist :created-at))))))

(defun user-ackfock-list-to-alist-by-ackfock (user-ackfock-list)
  (list (cons "ACK"
              (remove-if-not (lambda (ackfock) (eq ackfock :ack))
                             user-ackfock-list
                             :key #'user-ackfock-ackfock))
        (cons "FOCK"
              (remove-if-not (lambda (ackfock) (eq ackfock :fock))
                             user-ackfock-list
                             :key #'user-ackfock-ackfock))))

(defmodel (channel)
  uuid
  name)

(defun make-private-channel ()
  (make-channel :uuid nil
                :name "My private memos"))

(defun private-channel-p (channel)
  (null (channel-uuid channel)))

(defmodel (memo (:inflate created-at #'datetime-to-timestamp))
  uuid
  content
  (as-an-update nil :type boolean)
  creator-id
  parent-memo-id
  channel-id
  created-at)

(defmodel (activation-code (:inflate created-at #'datetime-to-timestamp)
                           (:inflate valid-until #'datetime-to-timestamp))
  email
  code
  created-at
  valid-until)

(defmodel (invitation-code (:inflate created-at #'datetime-to-timestamp)
                           (:inflate valid-until #'datetime-to-timestamp)
                           (:has-a channel (where (:= :uuid channel-id))))
  code
  source-user-id
  used-by-user-id
  channel-id
  created-at
  valid-until)
