(restas:define-module
    #:chirp
    (:use :cl :restas)
  (:export #:chirp-render-view
           #:user-logged-in?
           #:current-user
           #:user-username
           #:user-homepage))

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
             :accessor user-username)
   (password :initarg :password
             :reader user-password)
   (email :initform "")
   (description :initform "")
   (photo :initform "")
   (homepage :initform ""
             :accessor user-homepage)
   (created-at :initform "")
   (last-sign-in :initform "")))

(defclass chirp ()
  ((id :initform 0)
   (text :initarg :text)
   (created-at :initform "")
   (user-id :initarg :user-id)))


;; User functions

(defun user-logged-in? ()
  "Check if a user is logged in."
  (not (null (hunchentoot:session-value :username))))

(defun current-user ()
  "Get the currently logged in user object."
  (username-exists? (hunchentoot:session-value :username)))

(defun register-user (username password)
  "Create a new user with USERNAME and hashed PASSWORD"
  (let* ((hashed-password (hash-password password))
         (user (make-instance 'user :username username :password hashed-password)))
    (push user *users*)))

(defun authorize-user (username password)
  "Check that USERNAME can login with PASSWORD."
  (let ((user (username-exists? username)))
    (if (and user (string= (hash-password password) (user-password user)))
        user
        nil)))

(defun username-exists? (username)
  "Check if USERNAME exists in the database."
  (find username *users* :test #'username-equal
        :key #'user-username))

(defun username-equal (username value)
  "Ignore case and check if USERNAME equals VALUE."
  (string= (string-downcase username) (string-downcase value)))


;; Path helpers

(defun user-profile-page-path (user)
  (concatenate "/profiles/" (string-downcase (user-username user)) "/"))


;; Helper functions

(defun hash-password (password)
  "Generates a hash for PASSWORD."
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha256 
    (ironclad:ascii-string-to-byte-array password))))

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
