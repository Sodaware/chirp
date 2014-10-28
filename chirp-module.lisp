(restas:define-module
 #:chirp
 (:use :cl :restas))

(in-package #:chirp)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" chirp-config:*base-directory*))

(defparameter *asset-directory*
  (merge-pathnames #P"assets/" chirp-config:*base-directory*))

(setq *show-lisp-errors-p* t)
