(in-package :cl-user)
(defpackage ackfock
  (:use :cl)
  (:import-from :ackfock.config
                :config)
  (:import-from :clack
                :clackup)
  (:export :start
           :stop))
(in-package :ackfock)

(defvar *appfile-path*
  (asdf:system-relative-pathname :ackfock #P"app.lisp"))

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (unless (ackfock.db:all-migrations-applied-p)
    (restart-case (error "There are pending database migrations.")
      (apply-pending ()
        :report "Apply all pending migrations."
        (migratum:apply-pending (ackfock.db::migration-driver)))))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* args)))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))
