(in-package :cl-user)
(defpackage ackfock.game.auth
  (:use :cl :clog :clog-web)
  (:export #:login))
(in-package :ackfock.game.auth)

(defun login (body)
  (let* ((website (get-web-site body))
	 (signup-link (get-setting website :signup-link "/signup"))
         (outter (create-web-container body))
	 (form (create-form outter))
	 (p1 (create-p form))
	 (l1 (create-label p1 :content "Email"))
	 (user (create-form-element p1 :email
				    :name "email"
				    :class "w3-input"))
	 (p2 (create-p form))
	 (l2 (create-label p2 :content "Password"))
	 (pass (create-form-element p2 :password
				    :name "password"
				    :class "w3-input"))
	 (p3 (create-p form)))
    (declare (ignore l1 l2 p3))
    (setf (maximum-width outter) (unit :px 500))
    (setf (requiredp user) t)
    (setf (requiredp pass) t)
    (create-form-element form
                         :submit
                         :value "Submit"
			 :class (format nil "~A ~A" "w3-button" ackfock.game.theme:*color-class*))
    (set-on-submit form
                   (lambda (obj)
		     (if (ackfock.feature.auth:login body
                                                     (ackfock.db:db)
				                     (name-value obj "email")
				                     (name-value obj "password"))
			 (url-replace (location body) "/")
			 (clog-web-alert obj "Invalid" "The email and password are invalid."
					 :time-out 3
					 :place-top t))))
    (create-a form :class "w3-right" :content "sign up" :link signup-link)
    (with-clog-create outter
        (div (:bind password-recovery-div
               :class "w3-margin")
             (div (:bind forgot-password-btn
                    :content "<u>Forgot password?</u>"
                    :class "w3-small"))
             (dialog (:bind password-recovery-dialog)
                     (form (:bind password-recovery-form :method "dialog")
                           (div (:content "Password recovery by email" :class "w3-xlarge"))
                           (p ()
                              (label (:content "Email" :class "w3-large"))
                              (form-element (:text
                                             :name "email-to-recover"
                                             :class "w3-input")))
                           (span (:bind password-recovery-submit-span)
                                 (form-element (:submit
                                                :value "Send"
                                                :class (str:concat "w3-button " ackfock.game.theme:*color-class*)))
                                 (form-element (:submit
                                                :value "Cancel"
                                                :class "w3-button w3-black"))))))
      (set-on-click forgot-password-btn
                    (lambda (obj)
                      (declare (ignore obj))
                      (setf (dialog-openp password-recovery-dialog) t)))
      (set-on-event password-recovery-dialog
                    "close"
                    (lambda (dialog-obj)
                      (when (string= (return-value dialog-obj) "Send")
                        (let* ((email (name-value password-recovery-form "email-to-recover"))
                               (target-user (ackfock.model:user-by-email email)))
                          ;; race condition gap notice
                          (cond ((str:blankp email) (clog-web-alert password-recovery-div "Blank"
                                                                    "The email field can't be blank."
                                                                    :time-out 3
                                                                    :place-top t))
                                ((not (ackfock.model:valid-email-address-p email)) (clog-web-alert password-recovery-div "Email invalid"
                                                                                                   "Not a valid email address"
                                                                                                   :time-out 3
                                                                                                   :place-top t))
                                ((null target-user) (clog-web-alert password-recovery-div "Not Exists"
                                                                    "There is no user associated with the given email."
                                                                    :time-out 3
                                                                    :place-top t))
                                (t ;; TODO: make it atomic!!!
                                 (let ((activation-code (ackfock.feature.email-activation:create-code email)))
                                   (ackfock.feature.email-activation:send-reset-password email
                                                                                         (ackfock.model:user-username target-user)
                                                                                         (str:concat ackfock.config:*application-url*
                                                                                                     "/pass/"
                                                                                                     (ackfock.model:activation-code-code activation-code)))
                                   (clog-web-alert password-recovery-div "Sent"
                                                   "Please check your email inbox to reset your password"
                                                   :color-class "w3-green"
                                                   :time-out 3
                                                   :place-top t)))))))))))
