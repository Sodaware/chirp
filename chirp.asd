(defpackage #:chirp-config (:export #:*base-directory*))
(defparameter chirp-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:chirp
    :serial t
    :description "A common lisp Twitter clone"
    :author "Phil Newton"
    :license "GPL 3.0"
    :depends-on (:restas :cl-template :restas-directory-publisher :cl-json :ironclad)
    :pathname "src"
    :components ((:file "chirp-module")
                 (:file "chirp")))
