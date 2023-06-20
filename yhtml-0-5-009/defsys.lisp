;;; -*- Mode: LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitry Ivanov
;;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;; copyright (c) 2003 Kevin Rosenberg
;;; $Id: defsys.lisp 7 2015-12-13 14:03:59Z digo $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LispWorks system definition

(in-package :cl-user)

;(pushnew :ys-product *features*)
;(pushnew :deliver *features*)
;(pushnew :debug *features*)
;(pushnew :html *features*)
;(pushnew :phtml *features*)
;(pushnew :Russian *features*)		; only ys-product depends on Russian/English
#-Russian (pushnew :English *features*)

(load #P"PROJECTS:ylib;src;defsys")
(compile-system 'YSTOK-LIBRARY :load t)

(load #P"PROJECTS:net;html-template;defsys.lisp")
(compile-system 'YHTML-TEMPLATE :load t)

(defsystem YHTML (:object-pathname (features-folder "FASL:net;html;"
                                                    :debug '(:Russian . "ru")))
 :members
 ((YSTOK-LIBRARY  :type :system)
  (YHTML-TEMPLATE :type :system)
  #+uri        (YSTOK-URI  :type :system)
  #+local-time (LOCAL-TIME :type :system)
  "html-package"
  ("lang/ru-html"	:features :Russian)
  "charsets"
  "elements"
  "entities"
  "html-basics"
  "htmlgen"
  "bbcode"
  ("html-utils"		:features :uri)
  ("html-local-time"	:features (and :uri :local-time))
  ("phtml-package"	:features :phtml)
  ("lang/ru-phtml"	:features (and :Russian :phtml))
  ("phtml"		:features :phtml)
  ("phtml-utils"	:features :phtml)
  ("lhtt"		:features :phtml)
  ("ys-product"		:features :ys-product))
 :rules
 ((:in-order-to :compile "html-package"
   (:requires (:load YSTOK-LIBRARY YHTML-TEMPLATE)))
  (:in-order-to :load "html-package"
   (:requires (:load YSTOK-LIBRARY YHTML-TEMPLATE)))

  (:in-order-to :compile "ru-html"
   (:requires (:load "html-package")))
  (:in-order-to :load "ru-html"
   (:requires (:load "html-package")))

  (:in-order-to :compile ("charsets" "elements" "entities")
   (:requires (:load "html-package" "ru-html")))
  (:in-order-to :load ("charsets" "elements" "entities")
   (:requires (:load "html-package" "ru-html")))
  
  (:in-order-to :compile "html-basics"
   (:requires (:load "elements" "entities")))
  (:in-order-to :load "html-basics" 
   (:requires (:load "elements" "entities")))
  
  (:in-order-to :compile "htmlgen"
   (:caused-by (:load "elements"))
   (:requires (:load "elements" "html-basics")))
  (:in-order-to :load "htmlgen"
   (:caused-by (:load "elements"))
   (:requires (:load "elements" "html-basics")))

  (:in-order-to :compile "bbcode"
   (:requires (:load YSTOK-LIBRARY YHTML-TEMPLATE "html-basics" "htmlgen")))
  (:in-order-to :load "bbcode"
   (:caused-by (:load YHTML-TEMPLATE))
   (:requires (:load YSTOK-LIBRARY YHTML-TEMPLATE "html-basics" "htmlgen")))
  
  (:in-order-to :compile "html-utils" 
   (:requires (:load #+uri YSTOK-URI "htmlgen" "bbcode")))
  (:in-order-to :load "html-utils"
   (:caused-by (:load YHTML-TEMPLATE "html-basics" "htmlgen" "bbcode"))
   (:requires (:load #+uri YSTOK-URI "htmlgen" "bbcode")))
  
  (:in-order-to :compile "html-local-time"
   (:requires (:load "html-utils" #+local-time LOCAL-TIME)))
  (:in-order-to :load "html-local-time"
   (:requires (:load "html-utils" #+local-time LOCAL-TIME)))
  
  (:in-order-to :compile "phtml-package"
   (:requires (:load "html-basics")))
  (:in-order-to :load "phtml-package"
   (:requires (:load "html-basics")))

  (:in-order-to :compile "ru-phtml"
   (:requires (:load "phtml-package")))
  (:in-order-to :load "ru-phtml"
   (:requires (:load "phtml-package")))

  (:in-order-to :compile "phtml"
   (:requires (:load "phtml-package" "ru-phtml" "entities" "htmlgen")))
  (:in-order-to :load "phtml"
   (:caused-by (:load "elements" "entities"))
   (:requires (:load "phtml-package" "ru-phtml" "entities" "htmlgen")))

  (:in-order-to :compile "phtml-utils"
   (:requires (:load "charsets" "phtml")))
  (:in-order-to :load "phtml-utils"
   (:requires (:load "charsets" "phtml")))

  (:in-order-to :compile "lhtt"
   (:requires (:load "elements" "htmlgen" "phtml")))
  (:in-order-to :load "lhtt"
   (:caused-by (:load "elements" "htmlgen" "ru-phtml"))
   (:requires (:load "elements" "htmlgen" "phtml")))

  (:in-order-to :compile "ys-product"
   (:requires (:load "html-utils")))
  (:in-order-to :load "ys-product"
   (:requires (:load "html-utils")))
))

#+ys-product
(ys:defmodule :html (0 4) :abbrev "htm"
	      :depends-on (#+uri :uri :html-template))

;(compile-system 'YHTML :load t :force nil)
;(load-system 'YHTML)
