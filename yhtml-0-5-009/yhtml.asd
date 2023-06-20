;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2010 Dr. Dmitriy Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASDF system definition

(in-package :cl-user)

;; The :phtml feature controls whether the HTML parser is included into the system
(pushnew :phtml *features*)

(asdf:defsystem yhtml
  :depends-on (ystok-library
               :html-template
               #+uri ystok-uri
               #+local-time local-time
               acl-compat)
  :serial t
  :components
  ((:file "html-package")
   (:module "lang" :depends-on ("html-package")
	:components
	(#+Russian             (:file "ru-html")
	 #+(and Russian phtml) (:file "ru-phtml")))
   (:file "charsets")
   (:file "elements")
   (:file "entities")
   (:file "html-basics")
   (:file "htmlgen")
   (:file "bbcode")

   #+uri        (:file "html-utils")
   #+(and uri local-time)
                (:file "html-local-time")

   #+phtml      (:file "phtml-package")
   #+phtml      (:file "phtml")
   #+phtml      (:file "phtml-utils")
   #+phtml      (:file "lhtt")
   #+ys-product (:file "ys-product") ))

;(asdf:operate 'asdf:load-op 'yhtml)
