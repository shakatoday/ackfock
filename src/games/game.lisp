(in-package :cl-user)
(defpackage ackfock.game
  (:use :cl)
  (:export #:gamify
           #:make-main-page-env
           #:main-page-env
           #:web-content
           #:sidebar-item
           #:post-gamify-hash
           #:*bottom-new-memo-container-html-id*
           #:*body-location*
           #:*window*
           #:*hash-scroll-work-around-px*
           #:*environment*))
(in-package :ackfock.game)

(defvar *body-location*)

(defvar *window*)

(defparameter *landing-page*
  (rutils.string:read-file (merge-pathnames #p"www/index.html"
                                            ackfock.config:*application-root*)))

(defvar *environment*
  '(:port 8080))

(defparameter *hash-scroll-work-around-px* 124)

(defparameter *bottom-new-memo-container-html-id* "bottom-new-memo-container")

(defstruct (main-page-env (:conc-name nil))
  sidebar-item web-content post-gamify-hash)

(defgeneric gamify (object context))

(defgeneric init (options context))

(defgeneric operation-start (context))

(defgeneric operation-stop (context))

(defmethod init (options (context (eql (asdf:find-system :clog))))
  (clog:initialize nil
                   :port (getf *environment* :port)
                   :lack-middleware-list `(,lack.middleware.session:*lack-middleware-session*
                                           ,clog-lack-session:*middleware*
                                           ,(lambda (app)
                                              (lambda (env)
                                                (cond ((and (string= (getf env
                                                                           :path-info)
                                                                     "/")
                                                            (null (gethash :current-user
                                                                           (getf env
                                                                                 :lack.session))))
                                                       `(200 (:content-type "text/html")
                                                             (,*landing-page*)))
                                                      (t
                                                       (funcall app env))))))
	           :extended-routing t
                   :static-root (merge-pathnames "./www/"
	                                         ackfock.config:*application-root*)
	           :boot-function (clog-web:clog-web-meta
			           "Ackfock is a platform of mini agreements and mini memos of understanding.")))
