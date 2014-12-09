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
        (chirp-render-view "timeline" (list :user (current-user)
                                            :chirps *chirps*)))
      (chirp-render-template "templates/homepage.html.clt" nil)))

;; Display a list of ALL chirps, not just from people the current user is
;; following
(restas:define-route chirps/timeline ("/chirps/" :method :get)
  (chirp-render-view "timeline" (list :chirps *chirps*)))

(restas:define-route chirps/show ("/chirps/:id/")
  (chirp-render-view "chirps/show"
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
        (chirp-render-view "users/show" (list :user user :chirps (get-user-chirps user)) "profile")
        (chirp-render-template "templates/404.html.clt" nil))))

(restas:define-route users/self ("/me/")
  (if (user-logged-in?)
      (redirect "/profiles/sodaware/")
      (redirect 'users/login)))

(restas:define-route users/register ("/register/" :method :get)
  (chirp-render-template "templates/register.html.clt" (list :errors nil :username nil)))

(restas:define-route users/login ("/login/" :method :get)
  (chirp-render-template "templates/login.html.clt" (list :errors nil :username nil)))

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
          (chirp-render-template "templates/login.html.clt"
                                 (list :errors errors :username (hunchentoot:post-parameter "username")))))))


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
        (chirp-render-template "templates/register.html.clt"
                               (list :errors errors :username (hunchentoot:post-parameter "username"))))))
