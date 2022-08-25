(in-package :cl-user)
(defpackage ackfock.game.memo
  (:use :cl :clog :clog-web)
  (:import-from :ackfock.game.channel
                #:channel-content-web-content
                #:channel-content-re-gamifier)
  (:import-from :ackfock.game
                #:gamify
                #:*body-location*
                #:*window*
                #:*hash-scroll-work-around-px*))
(in-package :ackfock.game.memo)

(defparameter *memo-reply-link-class* "w3-leftbar w3-light-gray w3-text-gray w3-margin-left w3-padding-small")

(defun make-memo-div-html-id (memo)
  (str:concat "memo-div-" (ackfock.model:memo-uuid memo)))

(defun latest-ackfock-users (memo ack-or-fock)
  (format nil " ~{~a~^, ~}" (mapcar #'ackfock.model:user-username
                                    (mapcar #'ackfock.model:user-ackfock-user
                                            (cdr (assoc ack-or-fock
                                                        (ackfock.features:memo-latest-ackfocks-per-user-by-ackfock ackfock.game:*current-player*
                                                                                                                   memo)
                                                        :test #'string=))))))

(defmethod gamify ((object ackfock.model:memo) (context ackfock.game.channel:channel-content))
  (let ((web-content (channel-content-web-content context))
        (current-player ackfock.game:*current-player*))
    (with-clog-create web-content
        (div (:bind memo-div :class "w3-border-bottom w3-padding" :html-id (make-memo-div-html-id object))
             (div (:bind memo-reply-snippet-div))
             ;; TODO: handle xss risk
             (div (:bind memo-content-div :class "w3-light-gray w3-padding w3-card")
                  (div ()
                       (span (:class "w3-large"
                              :content (format nil
                                               "<b>~a:</b>"
                                               (ackfock.model:user-username (ackfock.model.relationships:memo-creator object)))))
                       (br ())
                       ;; TODO: handle xss risk
                       (div (:content (lf-to-br (ackfock.model:memo-content object)))))
                  (div ()
                       (button (:bind memo-reply-btn :class "fa fa-reply w3-button" :content " Reply"))))
             (div (:bind memo-update-reply-div))
             (div (:bind ackfock-div)
                  (div (:class "w3-section")
                       (button (:bind ack-btn :class "w3-btn w3-ripple w3-round-xlarge w3-green" :content "Ack"))
                       (span (:bind ack-usernames-span :content (latest-ackfock-users object "ACK"))))
                  (div (:class "w3-margin-bottom")
                       (button (:bind fock-btn :class "w3-btn w3-ripple w3-round-xlarge w3-purple" :content "Fock"))
                       (span (:bind fock-usernames-span :content (latest-ackfock-users object "FOCK")))))
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
                             (loop for user-ackfock in (ackfock.features:memo-user-ackfocks current-player object)
                                   do (with-clog-create ackfock-history-div
                                          (web-auto-row (:class "w3-margin w3-border-bottom")
                                                        (web-auto-column (:content (ackfock.model:user-username (ackfock.model:user-ackfock-user user-ackfock))))
                                                        (web-auto-column (:bind history-ack-column :content "Ack" :class "w3-text-green"))
                                                        (web-auto-column (:bind history-fock-column :content "Fock" :class "w3-text-purple"))
                                                        (web-auto-column (:content (local-time:format-timestring nil
                                                                                                                 (ackfock.model:user-ackfock-created-at user-ackfock)
                                                                                                                 :format local-time:+rfc-1123-format+))))
                                        (setf (visiblep history-ack-column) (eq (ackfock.model:user-ackfock-ackfock user-ackfock) :ack))
                                        (setf (visiblep history-fock-column) (eq (ackfock.model:user-ackfock-ackfock user-ackfock) :fock))))
                             (add-class ackfock-history-div "w3-animate-right")))))
      (rutils:when-it (ackfock.model.relationships:memo-parent-memo object)
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
                                                          (ackfock.model:user-username (ackfock.model.relationships:memo-creator rutils:it))
                                                          (str:substring 0 100 (ackfock.model:memo-content rutils:it)))))
      (flet ((ackfock-memo-and-regamify-handler (ackfock)
               (lambda (obj)
                 (declare (ignore obj))
                 (when (ackfock.features:ackfock-memo current-player
                                                      object
                                                      ackfock)
                   ;; regamify. memory leak?
                   (let ((ackfock.game:*current-player* current-player)) ; latest-ackfock-users references this special variable
                     (setf (inner-html ack-usernames-span) (latest-ackfock-users object "ACK"))
                     (setf (inner-html fock-usernames-span) (latest-ackfock-users object "FOCK")))))))
        (set-on-click ack-btn (ackfock-memo-and-regamify-handler "ACK"))
        (set-on-click fock-btn (ackfock-memo-and-regamify-handler "FOCK")))
      (flet ((memo-update-reply-handler (memo-update-reply-btn)
               (setf (inner-html memo-update-reply-div) "") ; memory leak?
               (with-clog-create memo-update-reply-div
                   (web-container ()
                                  (p ()
                                     (label (:content (text memo-update-reply-btn) :class "w3-large"))
                                     (text-area (:bind memo-content-input
                                                  :value (if (str:containsp "Update"
                                                                            (text memo-update-reply-btn))
                                                             (ackfock.model:memo-content object)
                                                             "")
                                                  :class "w3-input")))
                                  (button (:bind memo-update-reply-submit-btn
                                            :content "Submit"
                                            :class (str:concat "w3-button " ackfock.game.theme:*color-class*)))
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
                                       (t (ackfock.features:reply-memo ackfock.game:*current-player*
                                                                       object
                                                                       (text-value memo-content-input)
                                                                       :as-an-update-p (str:containsp "Update"
                                                                                                      (text memo-update-reply-btn)))
                                          (funcall (channel-content-re-gamifier context)))))))))
        (set-on-click memo-reply-btn #'memo-update-reply-handler))
      memo-div)))

(defmethod gamify ((object ackfock.model:memo) (context clog-obj))
  (with-clog-create context
             (div (:bind memo-div :class "w3-border-bottom w3-padding" :html-id (make-memo-div-html-id object))
                  ;; TODO: handle xss risk
                  (p ()
                     (span (:content "<b>In Channel</b>: "))
                     (span (:bind channel-name)))
                  (form (:method :POST :action "/")
                        (button (:class "w3-padding w3-light-grey w3-card w3-btn w3-ripple")
                                (span (:class "w3-large"
                                       :content (format nil
                                                        "<b>~a:</b>"
                                                        (ackfock.model:user-username (ackfock.model.relationships:memo-creator object)))))
                                (br ())
                                ;; TODO: handle xss risk
                                (div (:content (lf-to-br (ackfock.model:memo-content object)))))
                        (form-element (:bind memo-div-html-id-input :text :hidden t :name "memo-div-html-id"))
                        (form-element (:bind channel-id-input :text :hidden t :name "channel-id"))))
           (let ((memo-channel (ackfock.model.relationships:memo-channel object)))
             (setf (text channel-name) (or (rutils:when-it memo-channel
                                             (ackfock.model:channel-name rutils:it))
                                           "My private memos"))
             (setf (text-value memo-div-html-id-input) (make-memo-div-html-id object))
             (setf (text-value channel-id-input) (or (ackfock.model:memo-channel-id object)
                                                     "")))
           memo-div))
