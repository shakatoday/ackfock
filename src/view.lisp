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
  (cond ((typep env 'clog-obj)
         (with-clog-create env
             (div (:class "w3-border-bottom w3-padding")
                  (div (:class "w3-panel w3-light-gray")
                       (span (:class "w3-large"
                              :content (str:concat (user-username (memo-creator model-obj))
                                                   ":")))
                       (br ())
                       ;; TODO: display multi line content but also handle xss risk
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

(defmethod render ((model-obj channel) (current-user user) &optional env)
  (cond ((typep env 'clog-obj)
         (setf (inner-html env) "")     ; memory leak?
         (center-children (create-div env :class "w3-xlarge"
                                          :content (if (private-channel-p model-obj)
                                                       "My private memos"
                                                       "Channel members")))
         (unless (private-channel-p model-obj)
           (with-clog-create env
               (div (:bind channel-members-div)
                    (span (:class "w3-large"
                           :content (format nil
                                            "~{~a~^, ~}"
                                            (mapcar #'user-username
                                                    (channel-users model-obj)))))
                    (span (:bind invite-to-channel-btn
                            :class (str:concat "w3-button fa fa-user-plus w3-margin-left " ackfock.theme:*color-class*))
                          (div (:content "Invite" :class "w3-small")))
                    (dialog (:bind invite-to-channel-dialog :content "clicked!")))
             (center-children channel-members-div)
             (set-on-click invite-to-channel-btn
                           (lambda (btn-obj)
                             (declare (ignore btn-obj))
                             (setf (dialog-openp invite-to-channel-dialog) t)))))
         (loop for memo in (if (private-channel-p model-obj)
                               (user-private-memos current-user)
                               (channel-memos model-obj))
               do (render memo current-user env))
         (with-clog-create env
             (web-container ()
                            (p ()
                               (label (:content "New Memo" :class "w3-large"))
                               (text-area (:bind memo-content-input
                                            :class "w3-input")))
                            (button (:bind new-memo-btn
                                      :content "Submit"
                                      :class (str:concat "w3-button " ackfock.theme:*color-class*))))
           (setf (requiredp memo-content-input) t)
           (set-on-click new-memo-btn
                         (lambda (btn-obj)
                           (declare (ignore btn-obj))
                           ;; xss risk!
                           (ackfock.model:new-memo current-user
                                                   model-obj ; will check the null case inside the function
                                                   (text-value memo-content-input))
                           (render model-obj current-user env))
                         :one-time t)))))
