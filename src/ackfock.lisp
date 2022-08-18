(in-package :cl-user)
(defpackage #:ackfock
  (:use #:cl #:clog #:clog-web)
  (:export #:start-app))
(in-package :ackfock)

(defparameter *landing-page*
  (with-open-file (stream (merge-pathnames #P"www/index.html" (asdf:system-source-directory :ackfock)))
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun start-app (&key (port 8080) (open-browser-p nil))
  ;; Setup clog
  (initialize 'on-main
              :port port
              :lack-middleware-list `(,lack.middleware.session:*lack-middleware-session*
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
                                                  (ackfock.auth:current-session-from-lack-session env)
                                                  (funcall app env))))))
	      :extended-routing t
              :static-root (merge-pathnames "./www/"
	                                    (asdf:system-source-directory :ackfock))
	      :boot-function (clog-web-meta
			      "Ackfock is a platform of mini agreements and mini memos of understanding."))
  (ackfock.game.pages:define-all)
  (when open-browser-p
    (open-browser)))
