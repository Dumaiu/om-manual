;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; Ystok-URI (RFC3986) - Package definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2002-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;; copyright (c) 1999-2001 Franz Inc, Berkeley, CA  - All rights reserved.

(in-package :cl-user)

(defpackage :ystok.uri
  (:use :common-lisp :ystok.library)
  (:nicknames :uri)
  (:export 
   #:*default-external-format*
   #:*fill-scheme-authority*
   #:*fragment-char-p*
   #:*query-char-p*
   #:change-uri-base
   #:copy-uri
   #:enough-uri		; non-standard: (... &optional fill)
   #:clear-cache
   #:concat-trailing
   #:default-port
   #:make-uri
   #:merge-uris
   #:open-uri
   #:parse-uri		; non-standard: (... &key errorp decode query-test fragment-test)
   #:render-uri
   #:uri
   #:uri-authority
   #:uri-fragment
   #:uri-host
   #:uri-query
   #:uri-port
   #:uri-password
   #:uri-path
   #:uri-plist
   #:uri-p
   #:uri-parsed-path
   #:uri-parsed-query
   #:uri-scheme
   #:uri-type
   #:uri-user
   #:uri=

   #:percent-decode
   #:percent-encode

   #:uri-error
   #:uri-parse-error		; Non-standard

   ;#:make-uri-space		; Interning
   ;#:uri-space
   ;#:intern-uri
   ;#:unintern-uri
   ;#:do-all-uris
) )
