;; Chirp

(ql:quickload "restas")

(restas:define-module
 #:chirp
 (:use :cl :restas :cl-template))

(in-package #:chirp)

;; Set up some routes
(restas:define-route
 homepage ("")
 (chirp-render-view "index" (list :text "Hello, world!")))

;; Start chirp on 8080 (so nginx can proxy it)
;;(start '#:chirp-app :port 8080)

