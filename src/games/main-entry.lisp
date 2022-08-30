(in-package :cl-user)
(defpackage ackfock.game.main-entry
  (:use :cl #:clog #:clog-web #:clog-auth #:clog-web-dbi)
  (:export #:content))
(in-package :ackfock.game.main-entry)

(defun content (body)
  (with-clog-create body
      (web-sidebar (:bind sidebar :class "w3-mobile w3-hide-small")
                   (div (:content "<b>Channels</b>" :class "w3-margin-top")))
    (add-card-look sidebar)
    (setf (z-index sidebar) 2)
    (let* ((ackfock.game:*body-location* (location body))
           (ackfock.game:*window* (window body))
           (sidebar-menu-button-for-mobile (connection-data-item body :sidebar-menu-button-for-mobile))
           (channel-content (create-div body))
           (channels (cons (ackfock.model:make-private-channel) ; for user-private-memos
                           (ackfock.model.relationships:user-channels ackfock.feature.auth:*current-user*)))
           (current-user ackfock.feature.auth:*current-user*)
           (channel-selects
             (make-array (length channels)
                         :initial-contents (mapcar
                                            (lambda (channel)
                                              `(:channel
                                                ,channel
                                                :sidebar-item
                                                ,(create-web-sidebar-item sidebar
                                                                          :content (ackfock.model:channel-name channel))))
                                            channels)))
           (current-sidebar-item))
      (set-on-click sidebar-menu-button-for-mobile
                    (lambda (obj)
                      (declare (ignore obj))
                      (add-class sidebar "w3-animate-left")
                      (toggle-class sidebar "w3-hide-small")))
      (loop for channel-select across channel-selects
            do (let ((channel (getf channel-select :channel)))
                 (set-on-click (getf channel-select :sidebar-item)
                               (lambda (sidebar-item)
                                 (add-class sidebar "w3-hide-small")
                                 (remove-class current-sidebar-item "w3-blue-gray")
                                 (setf current-sidebar-item sidebar-item)
                                 (add-class sidebar-item "w3-blue-gray")
                                 (let ((ackfock.game:*body-location* (location body))
                                       (ackfock.game:*window* (window body))
                                       (ackfock.feature.auth:*current-user* current-user))
                                   (ackfock.game:gamify channel
                                                        (ackfock.game:make-main-page-env :sidebar-item sidebar-item
                                                                                         :web-content channel-content
                                                                                         :post-gamify-hash ackfock.game:*bottom-new-memo-container-html-id*)))))))
      (with-clog-create sidebar
          (div (:class "w3-border")
               (form (:bind new-channel-form :class "w3-section w3-row")
                     (form-element (:text
                                    :class "w3-col s9"
                                    :name "name"))
                     (button (:class "fa fa-plus-circle w3-button w3-col s3"))))
        (center-children new-channel-form)
        (set-on-submit new-channel-form
                       (lambda (form-obj)
                         (declare (ignore form-obj))
                         (cond ((str:blankp (name-value new-channel-form "name")) (clog-web-alert sidebar
                                                                                                  "Blank"
                                                                                                  "New channel name can't be blank"
                                                                                                  :time-out 3
                                                                                                  :place-top t))
                               (t (let ((ackfock.feature.auth:*current-user* current-user))
                                    (ackfock.features:new-channel (name-value new-channel-form "name")))
                                  (url-replace (location body) "/"))))))
      (set-margin-side channel-content
                       :left (format nil "~apx" (width sidebar)))
      (let* ((channel-id (form-data-item (form-post-data body) "channel-id"))
             (channel-id (unless (str:blankp channel-id)
                           channel-id))
             (memo-div-html-id (form-data-item (form-post-data body) "memo-div-html-id"))
             (channel-select (find channel-id
                                   channel-selects
                                   :key (lambda (element) (ackfock.model:channel-uuid (getf element :channel)))
                                   :test #'string=)))
        (setf current-sidebar-item (getf channel-select :sidebar-item))
        (ackfock.game:gamify (getf channel-select :channel)
                             (ackfock.game:make-main-page-env :sidebar-item current-sidebar-item
                                                              :web-content channel-content
                                                              :post-gamify-hash (or memo-div-html-id
                                                                                    ackfock.game:*bottom-new-memo-container-html-id*))))
      (add-class current-sidebar-item "w3-blue-gray"))))
