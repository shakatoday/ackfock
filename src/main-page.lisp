(in-package :cl-user)
(defpackage ackfock.main-page
  (:use :cl #:clog #:clog-web #:clog-auth #:clog-web-dbi #:ackfock.model-definition)
  (:export #:*current-user*
           #:main
           #:landing))
(in-package :ackfock.main-page)

(defvar *current-user*)

(defun landing (body)
  (with-clog-create body
      (div (:bind hero-div :class "w3-display-container w3-dark-gray")
           (div (:class "w3-padding w3-display-middle")
                (div (:class "w3-xxxlarge" :content "Evolve mini agreements"))
                (div (:class "w3-large" :content "Accumulating memos to reach consensus with casual or business friends. "))
                (div (:class "w3-large w3-margin-top" :content "Ack a memo when agree."))
                (div (:class "w3-large w3-margin-bottom" :content "Fock a memo when disagree."))
                (div (:bind sign-up-login-in-buttons-div)
                     (span ()
                           (a (:link "/signup"
                               :content "Sign Up"
                               :class (str:concat "w3-button w3-margin-right w3-border w3-border-khaki " ackfock.theme:*color-class*))))
                     (span ()
                           (a (:link "/login"
                               :content "Login"
                               :class "w3-button w3-margin-left w3-border w3-border-white"))))))
    (setf (display sign-up-login-in-buttons-div) "flex"
          (justify-content sign-up-login-in-buttons-div) :center)
    (setf (minimum-height hero-div) "400px"))
  (with-clog-create body
      (div (:class "w3-black w3-padding")
           (web-row (:padding t)
                    (div (:class "w3-col l6 w3-center")
                         (br ())
                         (div (:class "w3-xlarge" :content "Memos from daily consensus to business agreements"))
                         (img (:url-src "/img/use_case_examples.jpg" :class " w3-padding")))
                    (div (:class "w3-col l6 w3-center")
                         (br ())
                         (div (:class "w3-xlarge" :content "All Ack (agree) and Fock (disagree) histories recorded"))
                         (img (:url-src "/img/ackfock_history_feature.jpg" :class " w3-padding"))))
           (web-row (:padding t)
                    (div (:class "w3-col l6 w3-center")
                         (br ())
                         (div (:class "w3-xlarge" :content "Channel-based access control"))
                         (img (:url-src "/img/channel_feature.jpg" :class " w3-padding")))
                    (div (:class "w3-col l6 w3-center")
                         (br ())
                         (div (:class "w3-xlarge" :content "Search memos and agreements"))
                         (img (:url-src "/img/search_feature.jpg" :class " w3-padding"))))
           (web-row (:padding t)
                    (div (:class "w3-col l6 w3-center")
                         (br ())
                         (div (:class "w3-xlarge" :content "Private memos for self commitments"))
                         (img (:url-src "/img/self_commitment_feature.jpg" :class " w3-padding")))
                    (div (:class "w3-col l6 w3-center")
                         (br ())
                         (div (:class "w3-padding")
                              (p (:content "Start to accumulate mini agreements" :class "w3-xlarge"))
                              (a (:link "/signup"
                                  :content "Sign Up"
                                  :class (str:concat "w3-button w3-margin-right w3-border " ackfock.theme:*color-class*)))))))))

(defun main (body)
  (with-clog-create body
      (web-sidebar (:bind sidebar :class "w3-mobile w3-hide-small")
                   (div (:content "<b>Channels</b>" :class "w3-margin-top")))
    (add-card-look sidebar)
    (setf (z-index sidebar) 2)
    (let* ((ackfock.view:*body-location* (location body))
           (ackfock.view:*window* (window body))
           (sidebar-menu-button-for-mobile (connection-data-item body :sidebar-menu-button-for-mobile))
           (channel-content (create-div body))
           (channels (cons (make-private-channel) ; for user-private-memos
                           (user-channels *current-user*)))
           (current-user *current-user*)
           (channel-selects
             (make-array (length channels)
                         :initial-contents (mapcar
                                            (lambda (channel)
                                              `(:channel
                                                ,channel
                                                :sidebar-item
                                                ,(create-web-sidebar-item sidebar
                                                                          :content (channel-name channel))))
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
                                 (let ((ackfock.view:*body-location* (location body))
                                       (ackfock.view:*window* (window body)))
                                   (ackfock.view:render channel
                                                        current-user
                                                        (ackfock.view:make-main-page-env :sidebar-item sidebar-item
                                                                                         :web-content channel-content
                                                                                         :post-render-hash ackfock.view:*bottom-new-memo-container-html-id*)))))))
      (with-clog-create sidebar
          (div (:class "w3-border")
               (form (:bind new-channel-form :class "w3-section w3-row")
                     (form-element (:bind new-channel-form-input
                                     :text
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
                               (t (ackfock.model:new-channel current-user
                                                             (name-value new-channel-form "name"))
                                  (url-replace (location body) "/"))))))
      (set-margin-side channel-content
                       :left (format nil "~apx" (width sidebar)))
      (let* ((channel-id (form-data-item (form-post-data body) "channel-id"))
             (channel-id (unless (str:blankp channel-id)
                           channel-id))
             (memo-div-html-id (form-data-item (form-post-data body) "memo-div-html-id"))
             (channel-select (find channel-id
                                   channel-selects
                                   :key (lambda (element) (channel-uuid (getf element :channel)))
                                   :test #'string=)))
        (setf current-sidebar-item (getf channel-select :sidebar-item))
        (ackfock.view:render (getf channel-select :channel)
                             *current-user*
                             (ackfock.view:make-main-page-env :sidebar-item current-sidebar-item
                                                              :web-content channel-content
                                                              :post-render-hash (or memo-div-html-id
                                                                                    ackfock.view:*bottom-new-memo-container-html-id*))))
      (add-class current-sidebar-item "w3-blue-gray"))))
