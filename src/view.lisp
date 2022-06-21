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
                    (span (:bind channel-members-span
                           :class "w3-large"
                           :content (format nil
                                            "~{~a~^, ~}"
                                            (mapcar #'user-username
                                                    (channel-users model-obj)))))
                    (span (:bind invite-to-channel-btn
                            :class (str:concat "w3-button fa fa-user-plus w3-margin-left " ackfock.theme:*color-class*))
                          (div (:content "Invite" :class "w3-small")))
                    (dialog (:bind invite-to-channel-dialog)
                            (form (:bind invite-to-channel-form :method "dialog")
                                  (div (:content "Invite to this channel" :class "w3-xlarge"))
                                  (p ()
                                     (label (:content "Email" :class "w3-large"))
                                     (form-element (:text
                                                    :name "email"
                                                    :class "w3-input")))
                                  (span (:bind invite-to-channel-submit-span)
                                        (form-element (:submit
                                                       :value "Invite"
                                                       :class (str:concat "w3-button " ackfock.theme:*color-class*)))
                                        (form-element (:submit
                                                       :value "Cancel"
                                                       :class (str:concat "w3-button w3-black")))))))
             (center-children channel-members-div)
             (setf (display invite-to-channel-submit-span) "flex")
             (setf (justify-content invite-to-channel-submit-span) :space-between)
             (set-on-click invite-to-channel-btn
                           (lambda (btn-obj)
                             (declare (ignore btn-obj))
                             (setf (dialog-openp invite-to-channel-dialog) t)))
             (set-on-event invite-to-channel-dialog
                           "close"
                           (lambda (dialog-obj)
                             (when (string= (return-value dialog-obj) "Invite")
                               (let* ((email (name-value invite-to-channel-form "email"))
                                      (target-user (ackfock.model:user-by-email email)))
                                 ;; race condition gap notice
                                 (cond ((str:blankp email) (clog-web-alert env "Empty"
                                                                           "The email field can't be blank."
                                                                           :time-out 3
                                                                           :place-top t))
                                       ((null (clavier:validate ackfock.utils:*email-validator* email)) (clog-web-alert env "Email invalid"
                                                                                                                        "Not a valid email address"
                                                                                                                        :time-out 3
                                                                                                                        :place-top t))
                                       ((null target-user) (clog-web-alert env "Not Exists"
                                                                           "There is no user associated with the given email."
                                                                           :time-out 3
                                                                           :place-top t))
                                       ;; XSS DANGER!
                                       (t (ackfock.model:invite-to-channel current-user
                                                                           (user-email target-user)
                                                                           (channel-uuid model-obj))
                                          (setf (text channel-members-span) (format nil
                                                                                    "~{~a~^, ~}"
                                                                                    (mapcar #'user-username
                                                                                            (channel-users model-obj))))))))))))
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
                           ;; XSS DANGER!
                           (ackfock.model:new-memo current-user
                                                   model-obj ; will check the null case inside the function
                                                   (text-value memo-content-input))
                           (render model-obj current-user env))
                         :one-time t)))))
