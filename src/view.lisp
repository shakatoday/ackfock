(in-package :cl-user)
(defpackage ackfock.view
  (:use :cl :ackfock.model-definition)
  (:export #:render))
(in-package :ackfock.view)

(setf (cl-who:html-mode) :html5)

(defmethod render ((memo memo) &optional env)
  (declare (ignore env))
  (let* ((memo-target-user (memo-target-user memo))
         (current-user-is-target-user (and memo-target-user
                                           (string= (user-uuid memo-target-user)
                                                    (user-uuid (ackfock.utils:current-user))))))
    (cl-who:with-html-output-to-string
        (*standard-output* nil :indent t)
      (:tr
       (:td (cl-who:str (memo-content memo)))
       (:td (cond ((null memo-target-user) (cl-who:htm
                                            (:form :action "/send-memo" :method "post"
                                                   (:input :type "hidden" :name "uuid" :value (memo-uuid memo))
                                                   (:input :type "email" :name "recipient")
                                                   (:br)
                                                   (:input :type "submit" :value "Send"))))
                  (current-user-is-target-user (cl-who:str (user-email (memo-source-user memo))))
                  (t (cl-who:str (user-email (memo-target-user memo))))))
       (:td (cl-who:str (string (or (if current-user-is-target-user 
                                        (memo-target-user-ackfock memo) ; it's :ACK or :FOCK keyword, so we have to build a string from it
                                        (memo-source-user-ackfock memo)) 
                                    "")))
            (:form :action "/ackfock-memo" :method "post"
                   (:input :type "hidden" :name "uuid" :value (memo-uuid memo))
                   (when current-user-is-target-user
                     (cl-who:htm (:input :type "hidden" :name "as_target_user_ackfock" :value "")))
                   (:input :type "submit" :name "ackfock" :value "ACK")
                   (:input :type "submit" :name "ackfock" :value "FOCK")))
       (:td (cl-who:str (string (or (if current-user-is-target-user 
                                        (memo-source-user-ackfock memo) ; it's :ACK or :FOCK keyword, so we have to build a string from it
                                        (memo-target-user-ackfock memo))
                                    ""))))))))
