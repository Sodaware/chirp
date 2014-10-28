(restas:define-module
 #:chirp
 (:use :cl :restas)
 (:export #:chirp-render-view))

(in-package #:chirp)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" chirp-config:*base-directory*))

(defparameter *asset-directory*
  (merge-pathnames #P"assets/" chirp-config:*base-directory*))

(defun chirp-render-template (name params)
  (with-open-file (template-file name)
                  (let ((template (make-string (file-length template-file))))
                    (read-sequence template template-file)
                    (funcall (cl-template:compile-template template) params))))

(defun chirp-render-view (name params)
  (let ((view (concatenate 'string "templates/" name ".html.clt")))
    (chirp-render-template "templates/layout.html.clt"
                           (list :body (chirp-render-template view params)))))

(setq *show-lisp-errors-p* t)
