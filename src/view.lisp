(in-package :cl-user)
(defpackage ackfock.view
  (:use :cl :ackfock.model-definition :clog :clog-web)
  (:export #:render))
(in-package :ackfock.view)

(defun latest-ackfock-users (current-user memo ack-or-fock)
  (format nil " ~{~a~^, ~}" (mapcar #'user-username
                                    (mapcar #'user-ackfock-user
                                            (cdr (assoc ack-or-fock
                                                        (ackfock.model:memo-latest-ackfocks-per-user-by-ackfock current-user
                                                                                                                memo)
                                                        :test #'string=))))))

(defmethod render ((model-obj memo) (current-user user) &optional env)
  (cond ((typep env 'clog-obj) (with-clog-create env
                                   (div (:class "w3-border-bottom w3-padding")
                                        (div (:class "w3-panel w3-light-gray")
                                             (span (:class "w3-large"
                                                    :content (format nil
                                                                     "~a:"
                                                                     (user-username (memo-creator model-obj)))))
                                             (br ())
                                             (span (:content (memo-content model-obj))))
                                        (div (:class "w3-margin-bottom")
                                             (button (:bind ack-btn :class "w3-btn w3-ripple w3-round-xlarge w3-green" :content "Ack"))
                                             (span (:bind ack-usernames-span :content (latest-ackfock-users current-user model-obj "ACK"))))
                                        (div ()
                                             (button (:bind fock-btn :class "w3-btn w3-ripple w3-round-xlarge w3-purple" :content "Fock"))
                                             (span (:bind fock-usernames-span :content (latest-ackfock-users current-user model-obj "FOCK")))))
                                 (flet ((ackfock-memo-and-rerender-handler (ackfock)
                                          (lambda (obj)
                                            (declare (ignore obj))
                                            (when (ackfock.model:ackfock-memo current-user
                                                                              model-obj
                                                                              ackfock)
                                              ;; rerender. memory leak?
                                              (setf (inner-html ack-usernames-span) (latest-ackfock-users current-user model-obj "ACK"))
                                              (setf (inner-html fock-usernames-span) (latest-ackfock-users current-user model-obj "FOCK"))))))
                                   (set-on-click ack-btn (ackfock-memo-and-rerender-handler "ACK"))
                                   (set-on-click fock-btn (ackfock-memo-and-rerender-handler "FOCK")))))))
