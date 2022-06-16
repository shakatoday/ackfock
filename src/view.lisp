(in-package :cl-user)
(defpackage ackfock.view
  (:use :cl :ackfock.model-definition)
  (:export #:render))
(in-package :ackfock.view)

(defmethod render ((model-obj memo) (current-user user) &optional env)
  (let* ((ackfock-alist (ackfock.model:memo-latest-ackfocks-per-user-by-ackfock current-user
                                                                                model-obj))
         (latest-ack-users (cdr (assoc "ACK" ackfock-alist :test #'string=)))
         (latest-fock-users (cdr (assoc "FOCK" ackfock-alist :test #'string=))))
    (spinneret:with-html-string
      (:div :class "w3-border-bottom w3-padding"
            (:div :class "w3-panel w3-light-gray"
                  (:span :class "w3-large"
                         ("~a:" (user-username (memo-creator model-obj))))
                  (:br)
                  (memo-content model-obj))
            (:div (:button :class "w3-btn w3-ripple w3-round-xlarge w3-green" "Ack")
                  (:p (reduce (lambda (usernames next-username)
                                (format nil "~a, ~a" usernames next-username))
                              (mapcar #'user-username
                                      (mapcar #'user-ackfock-user
                                              latest-ack-users))
                              :initial-value "")))
            (:div (:button :class "w3-btn w3-ripple w3-round-xlarge w3-purple" "Fock")
                  (:p (reduce (lambda (usernames next-username)
                                (format nil "~a, ~a" usernames next-username))
                              (mapcar #'user-username
                                      (mapcar #'user-ackfock-user
                                              latest-fock-users))
                              :initial-value "")))))))
