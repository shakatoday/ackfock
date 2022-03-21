(in-package :cl-user)
(defpackage ackfock.db
  (:use :cl)
  (:import-from :ackfock.config
                :config)
  (:import-from :datafly
                :*connection*)
  (:import-from :cl-dbi
                :connect-cached)
  (:export #:connection-settings
           #:db
           #:with-connection
           #:defun-with-db-connection))
(in-package :ackfock.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defmacro defun-with-db-connection (name lambda-list &body body)
  "Define a function by DEFUN and put the BODY inside (WITH-CONNECTION (DB)). Docstring will be safely processed."
  (let* ((docstring-list (when (and (stringp (first body))
                                     (> (length body) 1))
                            (list (first body))))
         (body (if (null docstring-list)
                   body
                   (subseq body 1))))
    `(defun ,name ,lambda-list
       ,@docstring-list
       (with-connection (db)
         ,@body))))
