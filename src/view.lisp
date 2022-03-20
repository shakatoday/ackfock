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
                 (data-row (string (or (ackfock.model:memo-source-user-ackfock memo)
                                       "")))
                 (data-row (string (or (ackfock.model:memo-target-user-ackfock memo)
                                       ""))))))

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
