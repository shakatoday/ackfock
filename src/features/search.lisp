(in-package :cl-user)
(defpackage ackfock.feature.search
  (:use :cl)
  (:import-from :ackfock.db
                #:defun-with-db-connection)
  (:export #:search-memo))
(in-package :ackfock.feature.search)

(defparameter *search-query-sql-string*
  (rutils.string:read-file (merge-pathnames #P"db/search_query.sql"
                                            ackfock.config:*application-root*)))

(defun-with-db-connection search-memo (query)
  "Return a current-user accessible list of ACKFOCK.MODEL-DEFINITION:MEMO order by search rank."
  (when (stringp query)
    (let* ((query (ppcre:regex-replace-all
                   "\\s+"
                   (ppcre:regex-replace-all
                    "\\s+$"
                    (ppcre:regex-replace-all
                     "^\\s+"
                     (ppcre:regex-replace-all "[\||&]"
                                              query
                                              " "
                                              :preserve-case t)
                     ""
                     :preserve-case t)
                    ""
                    :preserve-case t)
                   " | " ; ts_query of postresql doesn't accept whitespace between tokens. What between them has to be a logical operator.
                   :preserve-case t))
           (data-plist-list (mapcar #'datafly.db::convert-row
                                    (dbi:fetch-all
                                     (dbi:execute
                                      (dbi:prepare datafly:*connection*
                                                   *search-query-sql-string*)
                                      (list query query))))))
      (remove-if-not #'ackfock.feature.auth:current-user-has-access-p
                     (loop for data-plist in data-plist-list
                           collect (progn
                                     (remf data-plist :search-rank)
                                     (apply #'ackfock.model:make-memo data-plist)))))))
