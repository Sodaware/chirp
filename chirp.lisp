;; Chirp

(ql:quickload "restas")

(restas:define-module
 #:chirp
 (:use :cl :restas :cl-template))

(in-package #:chirp)

;; Set up some routes
(restas:define-route
 homepage ("")
 (with-open-file (template-file "templates/index.html.clt")
                 (let ((template (make-string (file-length template-file)))
                       (text "Hello, World!"))
                   (read-sequence template template-file)
                   (funcall (cl-template:compile-template template) (list :text text)))))

;; Start chirp on 8080 (so nginx can proxy it)
;;(start '#:chirp-app :port 8080)

