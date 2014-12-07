(restas:define-module
 #:chirp
 (:use :cl :restas)
 (:export #:chirp-render-view))

(in-package #:chirp)

;; Configuration

(defparameter *template-directory*
  (merge-pathnames #P"templates/" chirp-config:*base-directory*))

(defparameter *asset-directory*
  (merge-pathnames #P"assets/" chirp-config:*base-directory*))


;; Models

(defclass user ()
  ((id :initform 0)
   (username :initarg :username
             :accessor username)
   (password :initarg :password
             :reader user-password)
   (email :initform "")
   (description :initform "")
   (photo :initform "")
   (homepage :initform "")
   (created-at :initform "")
   (last-sign-in :initform "")))

(defclass chirp ()
  ((id :initform 0)
   (text :initarg :text)
   (created-at :initform "")
   (user-id :initarg :user-id)))

(defun register-user (username password)
  "Create a new user with USERNAME and hashed PASSWORD"
  (let* ((hashed-password (hash-password password))
         (user (make-instance 'user :username username :password hashed-password)))
    (push user *users*)))

(defun authorize-user (username password)
  (let ((user (username-exists? username)))
    (if (and user (string= (hash-password password) (user-password user)))
        user
        nil)))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha256 
    (ironclad:ascii-string-to-byte-array password))))

(defun username-exists? (username)
  "Check if USERNAME exists in the database."
  (find username *users* :test #'username-equal
        :key #'username))

(defun username-equal (username value)
  (string= (string-downcase username) (string-downcase value)))

;; Helper functions

(defun chirp-render-template (name params)
  (with-open-file (template-file name)
    (let ((template (make-string (file-length template-file))))
      (read-sequence template template-file)
      (funcall (cl-template:compile-template template) params))))

(defun chirp-render-view (name params)
  (let ((view (concatenate 'string "templates/" name ".html.clt")))
    (chirp-render-template "templates/layout.html.clt"
                           (list :title "*chirp*"
                                 :body (chirp-render-template view params)))))

;; Allow restas to publish static assets under /assets/
(restas:mount-module -assets- (#:restas.directory-publisher)
                     (:url "/assets/")
                     (restas.directory-publisher:*directory* *asset-directory*))

(setq *show-lisp-errors-p* t)
