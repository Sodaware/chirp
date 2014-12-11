;; Chirp

(ql:quickload "restas")

(restas:define-module #:chirp
    (:use :cl :restas :cl-template))

(in-package #:chirp)

(defparameter *chirps* nil)
(defparameter *users* nil)

;; Set up some routes

;; Display the homepage. If not signed in, the visitor will be shown the login
;; screen, otherwise they will see their timeline.
(restas:define-route homepage ("")
  (if (user-logged-in?)
      (progn
        (render-view "timeline" (list :user (current-user)
                                      :chirps *chirps*)))
      (render-view "homepage" nil "no-sidebar")))

;; Display a list of ALL chirps, not just from people the current user is
;; following
(restas:define-route chirps/timeline ("/chirps/" :method :get)
  (render-view "timeline" (list :chirps *chirps*)))

(restas:define-route chirps/show ("/chirps/:id/")
  (render-view "chirps/show"
               (list :chirp (car *chirps*)
                     :id id)))

(restas:define-route chirps/add ("/chirps/" :method :post)
  (push (create-chirp (current-user) (hunchentoot:post-parameter "content")) *chirps*)
  (redirect 'homepage))

(restas:define-route chirps/list ("/chirps/list.json")
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string *chirps*))


;; User stuff

(restas:define-route users/show ("/profiles/:username/")
  (let ((user (username-exists? username)))
    (if user
        (render-view "users/show" (list :user user :chirps (get-user-chirps user)) "profile")
        (render-template "templates/404.html.clt" nil))))

(restas:define-route users/self ("/me/")
  (if (user-logged-in?)
      (redirect "/profiles/sodaware/")
      (redirect 'users/login)))

(restas:define-route users/login ("/login/" :method :get)
  (render-view "login" (list :errors nil :username nil) "no-sidebar"))

(restas:define-route users/login-check ("/login/" :method :post)
  (let ((errors nil)
        (user nil))
    (setq user (authorize-user (hunchentoot:post-parameter "username")
                               (hunchentoot:post-parameter "password")))

    (if user
        (progn
          (hunchentoot:start-session)
          (setf (hunchentoot:session-value :username) (user-username user))
          (redirect 'homepage))
        (progn
          (push "Username/password incorrect" errors)
          (render-view "login"
                       (list :errors errors :username (hunchentoot:post-parameter "username"))
                       "no-sidebar")))))

(restas:define-route users/logout ("/logout/")
  (setf (hunchentoot:session-value :username) nil)
  (redirect 'homepage))

(restas:define-route users/register ("/register/" :method :get)
  (render-view "register" (list :errors nil :username nil) "no-sidebar"))

(restas:define-route users/create ("/register/" :method :post)
  (let ((errors nil))
    (when (username-exists? (hunchentoot:parameter "username"))
      (push "Username already exists" errors))
    (when (string= "" (hunchentoot:post-parameter "username"))
      (push "A username is required" errors))
    (when (string= "" (hunchentoot:post-parameter "password"))
      (push "A password is required" errors))
    (unless (string= (hunchentoot:post-parameter "password") (hunchentoot:post-parameter "confirm"))
      (push "Passwords do not match" errors))

    (if (= 0 (length errors))
        (progn
          (register-user (hunchentoot:post-parameter "username")
                         (hunchentoot:post-parameter "password"))
          (redirect 'homepage))
        (render-view "register"
                     (list :errors errors :username (hunchentoot:post-parameter "username"))
                     "no-sidebar"))))
