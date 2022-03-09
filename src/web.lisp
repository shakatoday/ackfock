(in-package :cl-user)
(defpackage ackfock.web
  (:use :cl
        :caveman2
        :ackfock.config
        :ackfock.view
        :ackfock.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :ackfock.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
