(in-package :cl-user)
(defpackage ackfock.game.channel
  (:use :cl :ackfock.game :clog :clog-web)
  (:export #:channel-content-web-content
           #:channel-content-re-gamifier
           #:channel-content))
(in-package :ackfock.game.channel)

(defstruct channel-content
  re-gamifier web-content)

(defmethod gamify ((object ackfock.model:channel) (context main-page-env))
  (let ((web-content (web-content context))
        (current-user ackfock.feature.auth:*current-user*))
    (setf (inner-html web-content) "") ; memory leak? clog has destroy [generic-function] DESTROY CLOG-ELEMENT
    (with-clog-create web-content
        (div (:bind channel-head-div :class "w3-card w3-white w3-block")
             (div (:bind channel-name-div :class "w3-margin-left")
                  (form (:bind channel-name-form)
                        (form-element (:bind channel-name-input
                                        :text
                                        :name "channel-name"
                                        :class "w3-xlarge"
                                        :value (ackfock.model:channel-name object)))
                        (span ()
                              (button (:bind channel-name-edit-button
                                        :hidden (ackfock.model:private-channel-p object)
                                        :class "fa fa-edit w3-button w3-large"))))))
      (setf (disabledp channel-name-input) t)
      (setf (requiredp channel-name-input) t)
      (set-on-click channel-name-edit-button
                    (lambda (btn-obj)
                      ;; TODO: solve race condition gap
                      (unless (disabledp channel-name-input)
                        (let ((new-channel-name (name-value channel-name-form
                                                            "channel-name"))
                              (ackfock.feature.auth:*current-user* current-user))
                          (cond ((str:blankp new-channel-name)
                                 (clog-web-alert web-content
                                                 "Blank"
                                                 "The new channel name can't be blank."
                                                 :time-out 3
                                                 :place-top t)
                                 (setf (value channel-name-input) (ackfock.model:channel-name object)))
                                (t
                                 (ackfock.features:rename-channel object
                                                                  new-channel-name)
                                 (setf (text (sidebar-item context)) new-channel-name)))))
                      (setf (disabledp channel-name-input)
                            (not (disabledp channel-name-input)))
                      (toggle-class btn-obj "fa-edit")
                      (focus channel-name-input)))
      (set-on-blur channel-name-input
                   (lambda (input-obj)
                     (unless (disabledp input-obj) ; submit a blank new channel name doesn't blur the input text field. However, it triggers the click handler on channel-name-edit-button. So we have to avoid duplicate toggles
                       (setf (value input-obj) (ackfock.model:channel-name object))
                       (setf (disabledp input-obj) t)
                       (toggle-class channel-name-edit-button "fa-edit"))))
      (unless (ackfock.model:private-channel-p object)
        (with-clog-create channel-head-div
            (div (:bind channel-members-div :class "w3-margin-left")
                 (span (:bind channel-members-span
                         :class "w3-large"
                         :content (format nil
                                          "~{~a~^, ~}"
                                          (mapcar #'ackfock.model:user-username
                                                  (ackfock.model.relationships:channel-users object)))))
                 (span (:bind invite-to-channel-btn
                         :class (str:concat "w3-button fa fa-user-plus w3-margin-left " ackfock.game.theme:*color-class*))
                       (div (:class "w3-small")))
                 (span (:bind link-invitation-btn
                         :class (str:concat "w3-button fa fa-link w3-margin-left " ackfock.game.theme:*color-class*))
                       (div (:class "w3-small")))
                 (dialog (:bind invite-to-channel-dialog)
                         (form (:bind invite-to-channel-form :method "dialog")
                               (div (:content "Invite to this channel" :class "w3-xlarge"))
                               (p ()
                                  (label (:content "Email (won't send email)" :class "w3-large"))
                                  (form-element (:text
                                                 :name "email"
                                                 :class "w3-input")))
                               (span (:bind invite-to-channel-submit-span)
                                     (form-element (:submit
                                                    :value "Invite"
                                                    :class (str:concat "w3-button " ackfock.game.theme:*color-class*)))
                                     (form-element (:submit
                                                    :value "Cancel"
                                                    :class "w3-button w3-black")))))
                 (dialog (:bind link-invitation-dialog)
                         (p (:content "One-time invitation link. Expired in 3 days."))
                         (form-element (:bind invitation-link-text-input
                                         :text
                                         :class "w3-input"))
                         (button (:bind invitation-link-to-clipboard-btn
                                   :content "Copy"
                                   :class "fa fa-copy"))
                         (p (:content "Copy button currently doesn't support Safari" :class "w3-tiny"))
                         (p (:content "Generate more to invite more people."))
                         (span (:bind link-invitation-submit-span)
                               (button (:bind invitation-link-generate-btn
                                         :content "Generate"
                                         :class (str:concat "w3-button " ackfock.game.theme:*color-class*)))
                               (form (:method "dialog")
                                     (form-element (:submit
                                                    :value "Close"
                                                    :class "w3-button w3-black"))))))
          (setf (display invite-to-channel-submit-span) "flex"
                (display link-invitation-submit-span) "flex"
                (justify-content invite-to-channel-submit-span) :space-between
                (justify-content link-invitation-submit-span) :space-between
                (disabledp invitation-link-text-input) t)
          (set-on-click invitation-link-to-clipboard-btn
                        (lambda (btn-obj)
                          (declare (ignore btn-obj))
                          (system-clipboard-write invitation-link-text-input
                                                  (text-value invitation-link-text-input))))
          (set-on-click invite-to-channel-btn
                        (lambda (btn-obj)
                          (declare (ignore btn-obj))
                          (setf (dialog-openp invite-to-channel-dialog) t)))
          (flet ((generate-invitation-link ()
                   (setf (text-value invitation-link-text-input)
                         (str:concat ackfock.config:*application-url* "/i/"
                                     (ackfock.model:invitation-code-code
                                      (let ((ackfock.feature.auth:*current-user* current-user))
                                        (ackfock.feature.channel-invitation:create-invitation-code object)))))))
            (set-on-click link-invitation-btn
                          (lambda (btn-obj)
                            (declare (ignore btn-obj))
                            (setf (dialog-openp link-invitation-dialog) t)
                            (generate-invitation-link)))
            (set-on-click invitation-link-generate-btn
                          (lambda (btn-obj)
                            (declare (ignore btn-obj))
                            (generate-invitation-link))))
          (set-on-event invite-to-channel-dialog
                        "close"
                        (lambda (dialog-obj)
                          (when (string= (return-value dialog-obj) "Invite")
                            (let* ((email (name-value invite-to-channel-form "email"))
                                   (target-user (ackfock.model:user-by-email email)))
                              ;; race condition gap notice
                              (cond ((str:blankp email) (clog-web-alert channel-head-div "Blank"
                                                                        "The email field can't be blank."
                                                                        :time-out 3
                                                                        :place-top t))
                                    ((not (ackfock.model:valid-email-address-p email)) (clog-web-alert channel-head-div "Email invalid"
                                                                                                       "Not a valid email address"
                                                                                                       :time-out 3
                                                                                                       :place-top t))
                                    ((null target-user) (clog-web-alert channel-head-div "Not Exists"
                                                                        "There is no user associated with the given email."
                                                                        :time-out 3
                                                                        :place-top t))
                                    ;; XSS DANGER!
                                    (t (let ((ackfock.feature.auth:*current-user* current-user))
                                         (ackfock.features:invite-to-channel (ackfock.model:user-email target-user)
                                                                             object))
                                       (setf (text channel-members-span) (format nil
                                                                                 "~{~a~^, ~}"
                                                                                 (mapcar #'ackfock.model:user-username
                                                                                         (ackfock.model.relationships:channel-users object))))))))))))
      (with-clog-create channel-head-div
          (dialog (:bind go-to-memo-div-dialog :content "Scroll to searched memo?")
                  (form (:method "dialog")
                        (form-element (:submit :value "Yes" :class (str:concat "w3-button " ackfock.game.theme:*color-class*)))
                        (form-element (:submit :value "No" :class (str:concat "w3-button w3-black")))))
        (unless (string= (post-gamify-hash context) *bottom-new-memo-container-html-id*)
          (setf (dialog-openp go-to-memo-div-dialog) t)
          (let ((body-location *body-location*)
                (browser-window *window*))
            (set-on-event go-to-memo-div-dialog
                          "close"
                          (lambda (dialog-obj)
                            (declare (ignore dialog-obj))
                            (when (string= (return-value go-to-memo-div-dialog) "Yes")
                              (setf (hash body-location) "")
                              (setf (hash body-location) (post-gamify-hash context))
                              (scroll-by browser-window
                                         0
                                         (- *hash-scroll-work-around-px*))))))))
      (setf (positioning channel-head-div) "fixed")
      (setf (z-index channel-head-div) 1)
      ;; then, create an empty div so the beginning of the following content won't be blocked by channel-head
      (setf (height (create-div web-content)) (height channel-head-div))
      (let* ((body-location *body-location*)
             (window *window*)
             (current-user ackfock.feature.auth:*current-user*)
             (re-gamifier (lambda ()
                            (let ((*body-location* body-location)
                                  (*window* window)
                                  (ackfock.feature.auth:*current-user* current-user))
                              (gamify object context))))
             (memo-env (make-channel-content :web-content web-content
                                             :re-gamifier re-gamifier)))
        (loop for memo in (if (ackfock.model:private-channel-p object)
                              (ackfock.model.relationships:user-private-memos current-user)
                              (ackfock.model.relationships:channel-memos object))
              do (gamify memo
                         memo-env))
        (with-clog-create web-content
            (web-container (:html-id *bottom-new-memo-container-html-id*)
                           (p ()
                              (label (:content "New Memo" :class "w3-large"))
                              (text-area (:bind memo-content-input
                                           :class "w3-input")))
                           (button (:bind new-memo-btn
                                     :content "Submit"
                                     :class (str:concat "w3-button " ackfock.game.theme:*color-class*))))
          (setf (requiredp memo-content-input) t)
          (set-on-click new-memo-btn
                        (lambda (btn-obj)
                          (declare (ignore btn-obj))
                          ;; XSS DANGER!
                          (cond ((str:blankp (text-value memo-content-input)) (clog-web-alert channel-head-div
                                                                                              "Blank"
                                                                                              "New memo can't be blank"
                                                                                              :time-out 3
                                                                                              :place-top t))
                                (t (let ((ackfock.feature.auth:*current-user* current-user))
                                     (ackfock.features:new-memo object ; will check the null case inside the function
                                                                (text-value memo-content-input)))
                                   (funcall re-gamifier)))))
          (setf (hash *body-location*) "")
          (setf (hash *body-location*) *bottom-new-memo-container-html-id*))))))
