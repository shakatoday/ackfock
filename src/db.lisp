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
           #:defun-with-db-connection
           #:all-migrations-applied-p))
(in-package :ackfock.db)

(defparameter *migration-provider*
  (migratum.provider.local-path:make-provider (list (merge-pathnames #p"db/migrations/"
                                                                     ackfock.config:*application-root*))))

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
                                    (cdr body)) ; which means (> (length body) 1))
                            (list (first body))))
         (body (if (null docstring-list)
                   body
                   (subseq body 1))))
    `(defun ,name ,lambda-list
       ,@docstring-list
       (with-connection (db)
         ,@body))))

;; This has to be a function because *connection* can change.
;;
;; NOTICE: making two drivers as arguments of one function invocation can cause ERROR
;; for example
;; (migratum:driver-register-migration :up
;;                                     (migration-driver)
;;                                     (first (migratum:list-pending (migration-driver)))))

(defun-with-db-connection migration-driver ()
  (migratum.driver.dbi:make-driver *migration-provider* *connection*))

(defun all-migrations-applied-p ()
  (unless (migratum:list-pending (migration-driver))
    t))
