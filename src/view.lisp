(in-package :cl-user)
(defpackage ackfock.view
  (:use :cl :ackfock.model-definition :clog :clog-web)
  (:export #:render
           #:web-content-and-sidebar-item-pair
           #:make-web-content-and-sidebar-item-pair))
(in-package :ackfock.view)

(defstruct (web-content-and-sidebar-item-pair (:conc-name nil))
  sidebar-item web-content)

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
                  (div (:bind memo-content-panel :class "w3-light-gray w3-padding")
                       (div ()
                            (span (:class "w3-large"
                                   :content (format nil
                                                    "<b>~a:</b>"
                                                    (user-username (memo-creator model-obj)))))
                            (br ())
                            ;; TODO: handle xss risk
                            (div (:content (lf-to-br (memo-content model-obj)))))
                       (div ()
                            (button (:class "fa fa-pencil-square w3-button" :content " Update" :hidden (not (string= (memo-creator-id model-obj)
                                                                                                                     (user-uuid current-user)))))
                            (button (:class "fa fa-reply w3-button" :content " Reply"))
                            (button (:class "fa fa-history w3-button" :content " History"))))
                  (div (:class "w3-section")
                       (button (:bind ack-btn :class "w3-btn w3-ripple w3-round-xlarge w3-green" :content "Ack"))
                       (span (:bind ack-usernames-span :content (latest-ackfock-users current-user model-obj "ACK"))))
                  (div (:class "w3-margin-bottom")
                       (button (:bind fock-btn :class "w3-btn w3-ripple w3-round-xlarge w3-purple" :content "Fock"))
                       (span (:bind fock-usernames-span :content (latest-ackfock-users current-user model-obj "FOCK"))))
                  (div (:class "w3-margin-bottom")
                       (button (:class "fa fa-history w3-button w3-small" :content " History of Ack/Fock"))
                       (span ()))
                  (div ()
                       (button (:class "fa fa-trash w3-ripple w3-round-xlarge w3-button w3-text-red w3-small" :content " Vote to delete"))
                       (span ())))
           (setf (display memo-content-panel) "flex")
           (setf (justify-content memo-content-panel) :space-between)
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
  (cond ((typep env 'web-content-and-sidebar-item-pair)
         (let ((web-content (web-content env)))
           (setf (inner-html web-content) "") ; memory leak?
           (with-clog-create web-content
               (div (:bind channel-head-div)
                    (form (:bind channel-name-form)
                          (form-element (:bind channel-name-input
                                          :text
                                          :name "channel-name"
                                          :class "w3-xlarge"
                                          :value (channel-name model-obj)))
                          (span ()
                                (button (:bind channel-name-edit-button
                                          :hidden (private-channel-p model-obj)
                                          :class "fa fa-edit w3-button w3-large")))))
             (setf (disabledp channel-name-input) t)
             (setf (requiredp channel-name-input) t)
             (set-on-click channel-name-edit-button
                           (lambda (btn-obj)
                             ;; TODO: solve race condition gap
                             (unless (disabledp channel-name-input)
                               (let ((new-channel-name (name-value channel-name-form
                                                                   "channel-name")))
                                 (cond ((str:blankp new-channel-name)
                                        (clog-web-alert web-content
                                                        "Blank"
                                                        "The new channel name can't be blank."
                                                        :time-out 3
                                                        :place-top t)
                                        (setf (value channel-name-input) (channel-name model-obj)))
                                       (t
                                        (ackfock.model:rename-channel current-user
                                                                      model-obj
                                                                      new-channel-name)
                                        (setf (text (sidebar-item env)) new-channel-name)))))
                             (setf (disabledp channel-name-input)
                                   (not (disabledp channel-name-input)))
                             (toggle-class btn-obj "fa-edit")
                             (focus channel-name-input)))
             (set-on-blur channel-name-input
                          (lambda (input-obj)
                            (unless (disabledp input-obj) ; submit a blank new channel name doesn't blur the input text field. However, it triggers the click handler on channel-name-edit-button. So we have to avoid duplicate toggles
                              (setf (value input-obj) (channel-name model-obj))
                              (setf (disabledp input-obj) t)
                              (toggle-class channel-name-edit-button "fa-edit"))))
             (center-children channel-head-div))
           (unless (private-channel-p model-obj)
             (with-clog-create web-content
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
                                   (cond ((str:blankp email) (clog-web-alert web-content "Blank"
                                                                             "The email field can't be blank."
                                                                             :time-out 3
                                                                             :place-top t))
                                         ((null (clavier:validate ackfock.utils:*email-validator* email)) (clog-web-alert web-content "Email invalid"
                                                                                                                          "Not a valid email address"
                                                                                                                          :time-out 3
                                                                                                                          :place-top t))
                                         ((null target-user) (clog-web-alert web-content "Not Exists"
                                                                             "There is no user associated with the given email."
                                                                             :time-out 3
                                                                             :place-top t))
                                         ;; XSS DANGER!
                                         (t (ackfock.model:invite-to-channel current-user
                                                                             (user-email target-user)
                                                                             model-obj)
                                            (setf (text channel-members-span) (format nil
                                                                                      "~{~a~^, ~}"
                                                                                      (mapcar #'user-username
                                                                                              (channel-users model-obj))))))))))))
           (loop for memo in (if (private-channel-p model-obj)
                                 (user-private-memos current-user)
                                 (channel-memos model-obj))
                 do (render memo current-user web-content))
           (with-clog-create web-content
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
                           :one-time t))))))
