(in-package :cl-user)
(defpackage ackfock.view
  (:use :cl :ackfock.model-definition)
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
                #:encode-json)
  (:import-from :ackfock.model
                #:*current-user*)
  (:export #:render
           #:render-json))
(in-package :ackfock.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(setf (cl-who:html-mode) :html5)

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

(defmethod render ((template-path pathname) &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defmethod render ((memo memo) &optional env)
  (declare (ignore env))
  (let* ((memo-target-user (memo-target-user memo))
         (current-user-is-target-user (and memo-target-user
                                           (string= (user-uuid memo-target-user)
                                                    (user-uuid *current-user*)))))
    (cl-who:with-html-output-to-string
        (*standard-output* nil :indent t)
      (:tr
       (:td (cl-who:str (memo-content memo)))
       (:td (cond ((null memo-target-user) (cl-who:htm
                                            (:form :action "/send-memo" :method "post"
                                                   (:input :type "hidden" :name "uuid" :value (memo-uuid memo))
                                                   (:input :type "email" :name "recipient")
                                                   (:br)
                                                   (:button :type "submit" "Send"))))
                  (current-user-is-target-user (cl-who:str (user-email (memo-source-user memo))))
                  (t (cl-who:str (user-email (memo-target-user memo))))))
       (:td (cl-who:str (string (or (if current-user-is-target-user 
                                        (memo-target-user-ackfock memo) ; it's :ACK or :FOCK keyword, so we have to build a string from it
                                        (memo-source-user-ackfock memo)) 
                                    "")))
            (:form :action "/ackfock-memo" :method "post"
                   (:input :type "hidden" :name "uuid" :value (memo-uuid memo))
                   (when current-user-is-target-user
                     (cl-who:htm (:input :type "hidden" :name "as_target_user_ackfock" :value "")))
                   (:button :type "submit" :name "ackfock" :value "ACK" "ACK")
                   (:button :type "submit" :name "ackfock" :value "FOCK" "FOCK")))
       (:td (cl-who:str (string (or (if current-user-is-target-user 
                                        (memo-source-user-ackfock memo) ; it's :ACK or :FOCK keyword, so we have to build a string from it
                                        (memo-target-user-ackfock memo))
                                    ""))))))))

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
