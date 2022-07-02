(in-package :cl-user)
(defpackage ackfock.view
  (:use :cl :ackfock.model-definition :clog :clog-web)
  (:export #:render
           #:make-main-page-env
           #:*bottom-new-memo-container-html-id*
           #:*body-location*
           #:*window*
           #:*hash-scroll-work-around-px*))
(in-package :ackfock.view)

(defvar *body-location*)

(defvar *window*)

(defparameter *bottom-new-memo-container-html-id* "bottom-new-memo-container")

(defparameter *memo-reply-link-class* "w3-leftbar w3-light-gray w3-text-gray w3-margin-left w3-padding-small")

(defparameter *hash-scroll-work-around-px* 124)

(defstruct (main-page-env (:conc-name nil))
  sidebar-item web-content post-render-hash)

(defstruct channel-content
  re-renderer web-content)

(defun make-memo-div-html-id (memo)
  (str:concat "memo-div-" (memo-uuid memo)))

(defun latest-ackfock-users (current-user memo ack-or-fock)
  (format nil " ~{~a~^, ~}" (mapcar #'user-username
                                    (mapcar #'user-ackfock-user
                                            (cdr (assoc ack-or-fock
                                                        (ackfock.model:memo-latest-ackfocks-per-user-by-ackfock current-user
                                                                                                                memo)
                                                        :test #'string=))))))

(defmethod render ((model-obj memo) (current-user user) &optional env)
  (cond ((channel-content-p env)
         (let ((web-content (channel-content-web-content env)))
           (with-clog-create web-content
               (div (:bind memo-div :class "w3-border-bottom w3-padding" :html-id (make-memo-div-html-id model-obj))
                    (div (:bind memo-reply-snippet-div))
                    ;; TODO: handle xss risk
                    (div (:bind memo-content-div :class "w3-light-gray w3-padding w3-card")
                         (div ()
                              (span (:class "w3-large"
                                     :content (format nil
                                                      "<b>~a:</b>"
                                                      (user-username (memo-creator model-obj)))))
                              (br ())
                              ;; TODO: handle xss risk
                              (div (:content (lf-to-br (memo-content model-obj)))))
                         (div ()
                              (button (:bind memo-reply-btn :class "fa fa-reply w3-button" :content " Reply"))))
                    (div (:bind memo-update-reply-div))
                    (div (:bind ackfock-div)
                         (div (:class "w3-section")
                              (button (:bind ack-btn :class "w3-btn w3-ripple w3-round-xlarge w3-green" :content "Ack"))
                              (span (:bind ack-usernames-span :content (latest-ackfock-users current-user model-obj "ACK"))))
                         (div (:class "w3-margin-bottom")
                              (button (:bind fock-btn :class "w3-btn w3-ripple w3-round-xlarge w3-purple" :content "Fock"))
                              (span (:bind fock-usernames-span :content (latest-ackfock-users current-user model-obj "FOCK")))))
                    (div (:bind ackfock-history-div))
                    (div ()
                         (button (:bind ackfock-history-btn :class "fa fa-history w3-button w3-small" :content " History"))))
             (setf (display memo-content-div) "flex")
             (setf (justify-content memo-content-div) :space-between)
             (set-on-click ackfock-history-btn
                           (lambda (btn-obj)
                             (declare (ignore btn-obj))
                             (cond ((str:containsp "Hide" (text ackfock-history-btn))
                                    (setf (hiddenp ackfock-div) nil)
                                    (add-class ackfock-div "w3-animate-right")
                                    (add-class ackfock-history-btn "fa-history")
                                    (remove-class ackfock-history-btn "fa-long-arrow-left")
                                    (remove-class ackfock-history-btn "w3-black")
                                    (setf (text ackfock-history-btn) " History")
                                    (setf (inner-html ackfock-history-div) "")
                                    (remove-class ackfock-history-div "w3-animate-right"))
                                   (t
                                    (setf (hiddenp ackfock-div) t)
                                    (remove-class ackfock-div "w3-animate-right")
                                    (remove-class ackfock-history-btn "fa-history")
                                    (add-class ackfock-history-btn "fa-long-arrow-left")
                                    (add-class ackfock-history-btn "w3-black")
                                    (setf (text ackfock-history-btn) " Hide History")
                                    (loop for user-ackfock in (ackfock.model:memo-user-ackfocks current-user model-obj)
                                          do (with-clog-create ackfock-history-div
                                                 (web-auto-row (:class "w3-margin w3-border-bottom")
                                                               (web-auto-column (:content (user-username (user-ackfock-user user-ackfock))))
                                                               (web-auto-column (:bind history-ack-column :content "Ack" :class "w3-text-green"))
                                                               (web-auto-column (:bind history-fock-column :content "Fock" :class "w3-text-purple"))
                                                               (web-auto-column (:content (local-time:format-timestring nil
                                                                                                                        (user-ackfock-created-at user-ackfock)
                                                                                                                        :format local-time:+rfc-1123-format+))))
                                               (setf (visiblep history-ack-column) (string= (user-ackfock-ackfock user-ackfock) "ACK"))
                                               (setf (visiblep history-fock-column) (string= (user-ackfock-ackfock user-ackfock) "FOCK"))))
                                    (add-class ackfock-history-div "w3-animate-right")))))
             (rutils:when-it (memo-parent-memo model-obj)
               (add-class memo-reply-snippet-div *memo-reply-link-class*)
               (let ((body-location *body-location*)
                     (browser-window *window*))
                 (set-on-click memo-reply-snippet-div
                               (lambda (snippet-div)
                                 (declare (ignore snippet-div))
                                 (setf (hash body-location) "")
                                 (setf (hash body-location) (make-memo-div-html-id rutils:it))
                                 (scroll-by browser-window
                                            0
                                            (- *hash-scroll-work-around-px*)))))
               (setf (inner-html memo-reply-snippet-div) (format nil
                                                                 "<b>~a</b>: ~a..."
                                                                 (user-username (memo-creator rutils:it))
                                                                 (str:substring 0 100 (memo-content rutils:it)))))
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
               (set-on-click fock-btn (ackfock-memo-and-rerender-handler "FOCK")))
             (flet ((memo-update-reply-handler (memo-update-reply-btn)
                      (setf (inner-html memo-update-reply-div) "") ; memory leak?
                      (with-clog-create memo-update-reply-div
                          (web-container ()
                                         (p ()
                                            (label (:content (text memo-update-reply-btn) :class "w3-large"))
                                            (text-area (:bind memo-content-input
                                                         :value (if (str:containsp "Update"
                                                                                   (text memo-update-reply-btn))
                                                                    (memo-content model-obj)
                                                                    "")
                                                         :class "w3-input")))
                                         (button (:bind memo-update-reply-submit-btn
                                                   :content "Submit"
                                                   :class (str:concat "w3-button " ackfock.theme:*color-class*)))
                                         (button (:bind memo-update-reply-cancel-btn
                                                   :content "Cancel"
                                                   :class (str:concat "w3-button w3-black w3-margin-left"))))
                        (set-on-click memo-update-reply-cancel-btn
                                      (lambda (btn)
                                        (declare (ignore btn))
                                        (setf (inner-html memo-update-reply-div) ""))) ; memory leak?
                        (set-on-click memo-update-reply-submit-btn
                                      (lambda (btn)
                                        (declare (ignore btn))
                                        (cond ((str:blankp (text-value memo-content-input)) (clog-web-alert memo-update-reply-div
                                                                                                            "Blank"
                                                                                                            "New memo can't be blank"
                                                                                                            :time-out 3
                                                                                                            :place-top t))
                                              ;; XSS danger!
                                              (t (ackfock.model:reply-memo current-user
                                                                           model-obj
                                                                           (text-value memo-content-input)
                                                                           :as-an-update-p (str:containsp "Update"
                                                                                                          (text memo-update-reply-btn)))
                                                 (funcall (channel-content-re-renderer env)))))))))
               (set-on-click memo-reply-btn #'memo-update-reply-handler))
             memo-div)))
        ((typep env 'clog-obj)
         (with-clog-create env
             (div (:bind memo-div :class "w3-border-bottom w3-padding" :html-id (make-memo-div-html-id model-obj))
                  ;; TODO: handle xss risk
                  (p ()
                     (span (:content "<b>In Channel</b>: "))
                     (span (:bind channel-name)))
                  (form (:method :POST :action "/")
                        (button (:class "w3-padding w3-light-grey w3-card w3-btn w3-ripple")
                                (span (:class "w3-large"
                                       :content (format nil
                                                        "<b>~a:</b>"
                                                        (user-username (memo-creator model-obj)))))
                                (br ())
                                ;; TODO: handle xss risk
                                (div (:content (lf-to-br (memo-content model-obj)))))
                        (form-element (:bind memo-div-html-id-input :text :hidden t :name "memo-div-html-id"))
                        (form-element (:bind channel-id-input :text :hidden t :name "channel-id"))))
           (let ((memo-channel (memo-channel model-obj)))
             (setf (text channel-name) (or (rutils:when-it memo-channel
                                             (channel-name rutils:it))
                                           "My private memos"))
             (setf (text-value memo-div-html-id-input) (make-memo-div-html-id model-obj))
             (setf (text-value channel-id-input) (or (memo-channel-id model-obj)
                                                     "")))
           memo-div))))

(defmethod render ((model-obj channel) (current-user user) &optional env)
  (cond ((main-page-env-p env)
         (let ((web-content (web-content env)))
           (setf (inner-html web-content) "") ; memory leak? clog has destroy [generic-function] DESTROY CLOG-ELEMENT
           (with-clog-create web-content
               (div (:bind channel-head-div :class "w3-card w3-white w3-block")
                    (div (:bind channel-name-div :class "w3-margin-left")
                         (form (:bind channel-name-form)
                               (form-element (:bind channel-name-input
                                               :text
                                               :name "channel-name"
                                               :class "w3-xlarge"
                                               :value (channel-name model-obj)))
                               (span ()
                                     (button (:bind channel-name-edit-button
                                               :hidden (private-channel-p model-obj)
                                               :class "fa fa-edit w3-button w3-large"))))))
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
             (unless (private-channel-p model-obj)
               (with-clog-create channel-head-div
                   (div (:bind channel-members-div :class "w3-margin-left")
                        (span (:bind channel-members-span
                                :class "w3-large"
                                :content (format nil
                                                 "~{~a~^, ~}"
                                                 (mapcar #'user-username
                                                         (channel-users model-obj)))))
                        (span (:bind invite-to-channel-btn
                                :class (str:concat "w3-button fa fa-user-plus w3-margin-left " ackfock.theme:*color-class*))
                              (div (:content "Invite account" :class "w3-small")))
                        (span (:bind link-invitation-btn
                                :class (str:concat "w3-button fa fa-link w3-margin-left " ackfock.theme:*color-class*))
                              (div (:content "Link invitation" :class "w3-small")))
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
                                                           :class (str:concat "w3-button " ackfock.theme:*color-class*)))
                                            (form-element (:submit
                                                           :value "Cancel"
                                                           :class "w3-button w3-black")))))
                        (dialog (:bind link-invitation-dialog)
                                (p (:content "One-time invitation link. Expired in 3 days."))
                                (web-auto-row ()
                                              (web-auto-column ()
                                                               (form-element (:bind invitation-link-text-input
                                                                               :text
                                                                               :value "test link"
                                                                               :class "w3-input")))
                                              (web-auto-column ()
                                                               (button (:bind invitation-link-to-clipboard-btn
                                                                         :content "Copy"
                                                                         :class "fa fa-copy"))))
                                (p (:content "Generate more to invite more people."))
                                (span (:bind link-invitation-submit-span)
                                      (form-element (:submit
                                                     :value "Generate"
                                                     :class (str:concat "w3-button " ackfock.theme:*color-class*)))
                                      (form (:method "dialog")
                                            (form-element (:submit
                                                           :value "Close"
                                                           :class "w3-button w3-black"))))))
                 (setf (display invite-to-channel-submit-span) "flex"
                       (display link-invitation-submit-span) "flex"
                       (justify-content invite-to-channel-submit-span) :space-between
                       (justify-content link-invitation-submit-span) :space-between
                       (disabledp invitation-link-text-input) t)
                 ;; (set-on-click invitation-link-to-clipboard-btn
                 ;;               (lambda (btn-obj)
                 ;;                 (declare (ignore btn-obj))))
                 (set-on-click invite-to-channel-btn
                               (lambda (btn-obj)
                                 (declare (ignore btn-obj))
                                 (setf (dialog-openp invite-to-channel-dialog) t)))
                 (set-on-click link-invitation-btn
                               (lambda (btn-obj)
                                 (declare (ignore btn-obj))
                                 (setf (dialog-openp link-invitation-dialog) t)))
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
                                           ((null (clavier:validate ackfock.utils:*email-validator* email)) (clog-web-alert channel-head-div "Email invalid"
                                                                                                                            "Not a valid email address"
                                                                                                                            :time-out 3
                                                                                                                            :place-top t))
                                           ((null target-user) (clog-web-alert channel-head-div "Not Exists"
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
             (with-clog-create channel-head-div
                 (dialog (:bind go-to-memo-div-dialog :content "Scroll to searched memo?")
                         (form (:method "dialog")
                               (form-element (:submit :value "Yes" :class (str:concat "w3-button " ackfock.theme:*color-class*)))
                               (form-element (:submit :value "No" :class (str:concat "w3-button w3-black")))))
               (unless (string= (post-render-hash env) *bottom-new-memo-container-html-id*)
                 (setf (dialog-openp go-to-memo-div-dialog) t)
                 (let ((body-location *body-location*)
                       (browser-window *window*))
                   (set-on-event go-to-memo-div-dialog
                                 "close"
                                 (lambda (dialog-obj)
                                   (declare (ignore dialog-obj))
                                   (when (string= (return-value go-to-memo-div-dialog) "Yes")
                                     (setf (hash body-location) "")
                                     (setf (hash body-location) (post-render-hash env))
                                     (scroll-by browser-window
                                                0
                                                (- *hash-scroll-work-around-px*))))))))
             (setf (positioning channel-head-div) "fixed")
             (setf (z-index channel-head-div) 1)
             ;; then, create an empty div so the beginning of the following content won't be blocked by channel-head
             (setf (height (create-div web-content)) (height channel-head-div))
           (let* ((body-location *body-location*)
                  (window *window*)
                  (re-renderer (lambda ()
                                 (let ((*body-location* body-location)
                                       (*window* window))
                                   (render model-obj current-user env))))
                  (memo-env (make-channel-content :web-content web-content
                                                  :re-renderer re-renderer)))
             (loop for memo in (if (private-channel-p model-obj)
                                   (user-private-memos current-user)
                                   (channel-memos model-obj))
                   do (render memo
                              current-user
                              memo-env))
             (with-clog-create web-content
                 (web-container (:html-id *bottom-new-memo-container-html-id*)
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
                               (cond ((str:blankp (text-value memo-content-input)) (clog-web-alert channel-head-div
                                                                                                   "Blank"
                                                                                                   "New memo can't be blank"
                                                                                                   :time-out 3
                                                                                                   :place-top t))
                                     (t (ackfock.model:new-memo current-user
                                                                model-obj ; will check the null case inside the function
                                                                (text-value memo-content-input))
                               
                                        (funcall re-renderer)))))
               (setf (hash *body-location*) "")
               (setf (hash *body-location*) *bottom-new-memo-container-html-id*))))))))
