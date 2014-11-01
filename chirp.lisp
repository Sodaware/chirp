;; Chirp

(ql:quickload "restas")

(restas:define-module #:chirp
    (:use :cl :restas :cl-template))

(in-package #:chirp)

(defparameter *chirps* nil)

;; Set up some routes

(restas:define-route homepage ("")
  (chirp-render-view "index" (list :text "Hello, world!" :chirps *chirps*)))

(restas:define-route add-chirp ("/chirps/" :method :post)
  (push (hunchentoot:post-parameter "content") *chirps*)
  (redirect 'homepage))

(restas:define-route chirps/list ("/chirps/list.json")
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string *chirps*))
