(in-package :cl-user)
(defpackage ackfock.config
  (:use :cl)
  (:import-from :envy
                #:config-env-var
                #:defconfig)
  (:export #:config
           #:*application-root*
           #:*static-directory*
           #:*template-directory*
           #:*application-url*
           #:appenv
           #:developmentp
           #:productionp))
(in-package :ackfock.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :ackfock))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))
(defparameter *application-url* "https://www.ackfock.com")

(defconfig :common
    '(:debug T
      :databases
      ((:maindb :postgres :database-name "ackfock" :username "ackfock"))))

(defconfig |development|
    '(:debug T
      :databases
      ((:maindb :postgres :database-name "ackfock" :username "ackfock"))))

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
