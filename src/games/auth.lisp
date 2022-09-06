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
    (when signup-link
      (create-a form :class "w3-right" :content "sign up" :link signup-link))))
