;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (X)HTML generator package definition

(in-package :cl-user)

(defpackage :ystok.html.generator
 (:use :common-lisp #+ylib :ystok.library)
 (:nicknames :html)
 ;#+html-template			; seems to be always included
 (:import-from :html-template
  #:*escape-char-p*
  #:*template-environment*
  #:escape-string)
 (:export
  #:*html-external-format*		; mainly output format but also mentioned in PHTML
  #:*html-stream*			; HTMLGen and BBCode

  ;; HTMLGen
  #:*unknown-tag-action*
  #:htm					; was html 
  #:html-print
  #:html-print-list
  ;#:html-print-subst			; do we need these internals?
  ;#:html-print-list-subst
  #:html-stream 
  #:princ-http #:prin1-http #:princ-safe-http #:prin1-safe-http 
  #:print-attribute

  ;; CL-WHO
  ;#:*html-mode*
  #:*prologue*
  #:html-mode
  #:with-html-mode

  ;; Charset
  #:canonicalize-charset
  #:charset-ef				; was #:get-external-format
  #:charset-ef-match-p
  #:charsets

  ;; Other Basics
  #:+nbsp+
  #:+nbsp-char+
  #:*attribute-case*
  #:*default-include-pathname*
  #:*desc-hash-table*
  #:*html-code-to-entity*
  ;#:*include-function*
  #:*lhtml-safe*
  #:desc-uri-attrs
  #:elem-attrs
  #:elem-cont
  #:elem-desc
  #:elem-key
  #:escape-string
  #:make-elem
  #:output-html
  #:tag-name-string
  #:tag-standard-p
  #:with-html-file
  #:with-html-stream

  ;; Utils
  #:*bbcode-inline-tags*
  #:*bbcode-span-classes*
  #:bbcode-html-string
  #:bbcode-lhtml
  #:bbcode-output-html
  #:ensure-attribute
  #:find-element
  #:princ-safe-or-nbsp
  #:text-to-html-paragraph
  #:set-attribute

  ;; Templates and public symbols for uri output; some imported by ystok.help
  #:*default-template-file-type*
  #:*template-environment*
  #:html-princ-to-string
  #:icon
  #:icon-html
  #:lhtt-print
  #:lhtt-print-list
  #:parse-htt
  #:slots-environment
  #:template-environment
  #:template-read-from-string
  #:templates-folder
  #:text
  #:uri					; symbol different form uri:uri!
  #:uri-text))
