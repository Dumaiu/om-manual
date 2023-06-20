;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; YHTML-Template
;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2014, Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LispWorks system definition based on Edi's html-template.system

(in-package :cl-user)

;(load #P"PROJECTS:ylib;defsys")
;(compile-system 'YSTOK-LIBRARY :load t)

(defsystem YHTML-TEMPLATE (:object-pathname (lw:pathname-location (lw:current-pathname
				  #+(and debug Russian)		"bin/debug/ru/"
				  #+(and debug (not Russian))	"bin/debug/"
				  #+(and (not debug) Russian)	"bin/ru/"
				  #-(or debug Russian)		"bin/")))
 :members ((YSTOK-LIBRARY :type :system)
           "packages"
           ("lang/ru" :features (and :ylib :Russian))
           "errors"
           "specials"
           "util"
           "template"
           "api")
 :rules ((:in-order-to :compile :all
          (:requires (:load :previous)))
         (:in-order-to :load :all
          (:caused-by (:compile :previous))
          (:requires (:load :previous))) ))

;(compile-system 'YHTML-TEMPLATE :load t :force nil)
