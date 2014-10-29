;; Chirp

(ql:quickload "restas")

(restas:define-module
 #:chirp
 (:use :cl :restas :cl-template))

(in-package #:chirp)

(defparameter *chirps* nil)

;; Set up some routes

(restas:define-route
 homepage ("")
 (chirp-render-view "index" (list :text "Hello, world!" :chirps *chirps*)))

(restas:define-route
 add-chirp ("/chirps/" :method :post)
 (let ((content (hunchentoot:post-parameter "content")))
   (push content *chirps*)
   (redirect 'homepage)))
