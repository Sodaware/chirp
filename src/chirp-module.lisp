(restas:define-module
    #:chirp
  (:use :cl :restas)
  (:export #:user-logged-in?
           #:current-user
           #:user-username
           #:user-homepage
           #:user-description
           #:user-photo
           #:chirp-text
           #:chirp-id
           #:chirp-user-id
           #:chirp-created-at
           #:chirp-author
           #:user-profile-page-path
           #:chirp-page-path
           #:render-partial))

(in-package #:chirp)


;; ----------------------------------------------------------------------
;; -- Configuration
;; ----------------------------------------------------------------------

(defparameter *template-directory*
  (merge-pathnames #P"templates/" chirp-config:*base-directory*))

(defparameter *asset-directory*
  (merge-pathnames #P"assets/" chirp-config:*base-directory*))


;; ----------------------------------------------------------------------
;; -- Models
;; ----------------------------------------------------------------------

(defclass user ()
  ((id           :initarg :id         :reader user-id)
   (username     :initarg :username   :accessor user-username)
   (password     :initarg :password   :reader user-password)
   (email        :initform ""         :accessor user-email)
   (description  :initform ""         :accessor user-description)
   (photo        :initform ""         :accessor user-photo)
   (homepage     :initform ""         :accessor user-homepage)
   (created-at   :initarg :created-at :accessor user-created-at)
   (last-sign-in :initform 0          :accessor user-last-sign-in)))

(defclass chirp ()
  ((id           :initarg :id         :reader chirp-id)
   (text         :initarg :text       :reader chirp-text)
   (created-at   :initarg :created-at :reader chirp-created-at)
   (user-id      :initarg :user-id    :reader chirp-user-id)))


;; ----------------------------------------------------------------------
;; -- "Chirp" object functions
;; ----------------------------------------------------------------------

(defun create-chirp (author text)
  "Create a new Chirp instance for AUTHOR containing TEXT."
  (make-instance 'chirp
                 :id (+ 1 (length *chirps*))
                 :text text
                 :user-id (user-id author)
                 :created-at (get-universal-time)))

(defun chirp-author (chirp)
  "Get the user object that created CHIRP."
  (get-user-by-id (chirp-user-id chirp)))

(defun get-user-chirps (user)
  "Get all chirps that belong to USER."
  (remove-if-not #'(lambda (chirp)
                     (eq (user-id user) (chirp-user-id chirp)))
                 *chirps*))


;; ----------------------------------------------------------------------
;; -- User functions
;; ----------------------------------------------------------------------

(defun user-logged-in? ()
  "Check if a user is logged in."
  (not (null (hunchentoot:session-value :username))))

(defun current-user ()
  "Get the currently logged in user object."
  (username-exists? (hunchentoot:session-value :username)))

(defun register-user (username password)
  "Create a new user with USERNAME and hashed PASSWORD"
  (push (make-instance 'user
                       :id (+ 1 (length *users*))
                       :username username
                       :password (hash-password password))
        *users*))

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

(defun get-user-by-id (user-id)
  "Get a user by their USER-ID."
  (find user-id *users* :test #'eq
        :key #'user-id))

(defun username-equal (username value)
  "Ignore case and check if USERNAME equals VALUE."
  (string= (string-downcase username) (string-downcase value)))


(defun hash-password (password)
  "Generates a hash for PASSWORD."
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha256 
    (ironclad:ascii-string-to-byte-array password))))


;; ----------------------------------------------------------------------
;; -- Path Helpers
;; ----------------------------------------------------------------------

(defun user-profile-page-path (user)
  "Get the full path to view USER."
  (format nil "/profiles/~(~a~)/" (string-downcase (user-username user))))

(defun chirp-page-path (chirp)
  "Get the full path to view CHIRP."
  (format nil "/chirps/~a/" (chirp-id chirp)))


;; ----------------------------------------------------------------------
;; -- Rendering Helpers
;; ----------------------------------------------------------------------

(defun render-partial (name &optional params)
  (render-template
   (format nil "templates/~A.html.clt" name)
   params))

(defun render-view (name params &optional (layout "application"))
  "Render the view NAME with PARAMS, and optionally override LAYOUT"
  (let* ((view (concatenate 'string "templates/" name ".html.clt"))
         (body (render-template view params))
         (vars (append params (list :title "*chirp*" :body body))))
    (render-template
     (format nil "templates/layouts/~A.html.clt" layout)
     vars)))

(defun render-template (name params)
  (with-open-file (template-file name)
    (let ((template (make-string (file-length template-file))))
      (read-sequence template template-file)
      (funcall (cl-template:compile-template template) params))))


;; Allow restas to publish static assets under /assets/
(restas:mount-module -assets- (#:restas.directory-publisher)
                     (:url "/assets/")
                     (restas.directory-publisher:*directory* *asset-directory*))

(setq *show-lisp-errors-p* t)
