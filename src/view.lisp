(in-package :cl-user)
(defpackage ackfock.view
  (:use :cl)
  (:import-from :ackfock.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*)
  (:import-from :datafly
                :encode-json)
  (:export #:render
           #:render-json
           #:with-page
           #:login-page
           #:home-page))
(in-package :ackfock.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

(setf (cl-who:html-mode) :html5)

(defmacro with-page ((&key title with-logout-button) &body body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :indent t)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:title ,title))
      (:body
       ,@(when with-logout-button
           '((:form :action "/logout" :method "post"
              (:input :type "submit" :value "Logout"))))
       ,@body))))

(defun login-page (&key message sign-up)
  (with-page (:title (if sign-up
                         "Sign Up"
                         "Login"))
    (:h1 (cl-who:str (if sign-up
                         "Sign Up"
                         "Login")))
    (when message
      (cl-who:htm
       (:p (cl-who:str message))))
    (:form :action (if sign-up
                       "/sign-up"
                       "/login")
           :method "post"
           (:p "Email" (:br)
               (:input :type "text" :name "email"))
           (:p "Password" (:br)
               (:input :type "password" :name "password"))
           (when sign-up
             (cl-who:htm
              (:p "Confirm password" (:br)
                  (:input :type "password" :name "confirm_password"))))
           (:p (:input :type "submit"
                       :value (if sign-up
                                  "register"
                                  "login"))))))

(defmethod render ((template-path pathname) &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defmethod render ((memo ackfock.model:memo) &optional env)
  (declare (ignore env))
  (macrolet ((data-row (form)
               `(cl-who:with-html-output-to-string
                    (*standard-output* nil :indent t)
                  (:td (:p (cl-who:str ,form))))))
    (concatenate 'string
                 (data-row (ackfock.model:memo-content memo))
                 (data-row (cond ((null (ackfock.model:memo-target-user-id memo)) "")
                                 (t (ackfock.model:user-email (ackfock.model:memo-target-user memo)))))
                 (data-row (or (ackfock.model:memo-source-user-ackfock memo)
                               ""))
                 (data-row (or (ackfock.model:memo-target-user-ackfock memo)
                               "")))))

(defun home-page (current-user &optional message)
  (with-page (:title "My Memos" :with-logout-button t)
    (:h1 "My Memos")
    (when message
      (cl-who:htm
       (:p (cl-who:str message))))
    (:form :action "/add-memo" :method "post"
           (:h2 "Add Memo")
           (:p "Content" (:br)
               (:input :type "text" :name "content"))
           (:p (:input :type "submit" :value "Add Memo")))
    (:table (:tr (:th "|_____Memo______|")
                 (:th "____with_________|")
                 (:th "___I__ack?_______|")
                 (:th "___he/she__ack?__|"))
            (dolist (memo (ackfock.model:user-memos current-user))
              (cl-who:htm (:tr (cl-who:str (render memo))))))))

;;
;; Execute package definition

(defpackage ackfock.djula
  (:use :cl)
  (:import-from :ackfock.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*djula-execute-package* (find-package :ackfock.djula))
