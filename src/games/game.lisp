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

(defvar *environment*)

(defparameter *hash-scroll-work-around-px* 124)

(defparameter *bottom-new-memo-container-html-id* "bottom-new-memo-container")

(defstruct (main-page-env (:conc-name nil))
  sidebar-item web-content post-gamify-hash)

(defgeneric gamify (object context))
