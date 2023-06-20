;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;; Part of HTMLGen and PHTML, copyright (c) 1986-2000 Franz Inc, Berkeley, CA
;;; $Id: phtml-package.lisp 1 2015-10-29 17:37:49Z digo $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser package definition

(defpackage :ystok.html.parser
 (:use :common-lisp :ystok.library)
 (:nicknames :phtml)
 (:export
  #:*callback-parent-tag*
  #:*end-tag-mismatch-action*
  #:*html-entity-to-code*
  #:*retain-whitespace*
  #:*xhtml-mode*
  #:collapse-whitespace
  #:ensure-entity-table					; called from YstokHelp
  #:phtml-internal
  #:parse-content-type-charset
  #:parse-declarations
  #:parse-entity
  #:parse-html
  #:syntax-cerror
  #:syntax-error
  #:syntax-log-warn
  #:tag-retain-whitespace
  #:whitespace-string-p
  #:with-syntax-error-location))

