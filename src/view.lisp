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
           #:login-page))
(in-package :ackfock.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

(setf (cl-who:html-mode) :html5)

(defmacro with-page ((&key title) &body body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :indent t)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:title ,title))
      (:body
       (:form :action "/logout" :method "post"
              (:input :type "submit" :value "Logout"))
       ,@body))))

(defun login-page (&optional message)
  (with-page (:title "Login")
    (:h1 "Login")
    (when message
      (cl-who:htm
       (:p (cl-who:fmt "~a" message))))
    (:form :action "/login" :method "post"
           (:p "Email" (:br)
               (:input :type "text" :name "email"))
           (:p "Password" (:br)
               (:input :type "password" :name "password"))
           (:p (:input :type "submit" :value "Login")))))

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
