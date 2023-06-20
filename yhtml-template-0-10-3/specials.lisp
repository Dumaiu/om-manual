;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HTML-TEMPLATE; Base: 10 -*-
;;; YHTML-Template
;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2014, Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global parameters and special variables

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :html-template)

(defparameter *template-start-marker* "<!--"
  "The string template tags must start with")
(defparameter *template-end-marker* "-->"
  "The string template tags must end with")

(defparameter *accept-braces-markers* t
  "Additionally accept pattern {form} in curly braces as a replacement of <!--TMPL_EVAL form -->")

(defparameter *external-format* :default
  "The external format used when opening template files for input.")

(defparameter *ignore-tag-rest* t
  "When true, ingore any text after tag name, forms, or attribute
up to the closing marker.
When false, a syntax error is signaled.")

(defparameter *ignore-empty-lines* nil
  "Controls whether template tags on their own lines produce empty
lines or not.")

;; Until we generate HTML within an interface process or progress-dialog,
;; the top-level-hook redirects errors and warnings to log.
;; Additionally when the value of this parameter is the symbol yl:logg,
;; the yl:logg is called instead of warning.
(defparameter *warn-on-creation* t
  "Controls whether a warning should be signaled or logged if
a new template printer is created from a pathname argument.")

(defparameter *format-null-forms* t
  "Controls what to do when TMPL_EVAL or TMPL_VAR results in NIL. One of:
T       Resolve to the empty string.
string  Format control string, which means applying the format function to
        this string and the form given to the evaluator;
        then what the format returns is written to the HTML stream.
NIL     Raise an error.")

(defparameter *format-non-strings* t
  "Controls whether TMPL_EVAL and TMPL_VAR will accept values that
are not strings. One of:
T - convert them using PRINC-TO-STRING, or
funcion of one parameter, which must return a string.")

(defparameter *string-modifier* 'escape-string-iso-8859-1
  "The function which is applied to strings resulted from TMPL_EVAL
and TMPL_VAR tags.
Use #'CL:IDENTITY if you don't want to change the strings.")

(defparameter *escape-char-p*
  (lambda (char)
    (or (find char "<>&'\"")
        (> (char-code char) 127)))
  "Used by ESCAPE-STRING to test whether a character should be escaped.")

;;; Default argument values for API functions

(defparameter *force-default* nil
  "The default value for the FORCE keyword argument to
CREATE-TEMPLATE-PRINTER. One of:
 :NO-CACHE Recompile template unconditionally but do not cache.
 True      Recompile unconditionally and cache.
 NIL       Recompile printer based on FILE-WRITE-DATE.
 :NO-CHECK Cached printer if any is always considered up-to-date.")

(defparameter *default-template-pathname* (make-pathname)
  "Each pathname is merged with this value before it is used by
CREATE-TEMPLATE-PRINTER.")

(defparameter *default-template-output* *standard-output*
  "The output stream used by FILL-AND-PRINT-TEMPLATE when no STREAM
keyword was provided.")

;;; Control reading and evaluating symbols (attributes)

(defparameter *template-package* (find-package :cl-user)
  "The package symbols are interned into when reading forms
from template or HTML source files.")

(defvar *template-readtable* nil
  "Readtable used when reading Lisp forms within tags and inialized on first call.")

(defvar *template-environment* ()
  "Stores global template variable bindings and is used by
template-eval as a default parameter value.")

(defparameter *value-access-function* yl:*tiny-symbol-value-function*
  "The function which associates \(attribute) symbols with their values.")

(defparameter *value-setq-function* yl:*tiny-setq-function*
  "Function to assign VALUE to SYMBOL; do nothing if it is missing from ENVIRONMENT.")

;; Compatibility parameter used by create-template-printer-aux.
;; Its value does not matter for the below tags:
;; - TMPL_EVAL always treats tag rest as a form,
;; - TMPL_VAR  always treats tag rest as a plain attribute.
(defparameter *attributes-are-lisp-forms* t
  "When true, attributes are read and interpreted as Lisp forms for most
of the following tags:
  TMPL_IF, TMPL_ELIF, TMPL_UNLESS, TMPL_LOOP, TMPL_REPEAT, and TMPL_CALL.
When false, the reader works on plane attribute syntax only:
accepts symbols and interns them into *TEMPLATE-SYMBOL-PACKAGE*;
filling templates also proceeds in old fashion.")

;;; The following two settings affect TMPL_VAR unconditionally
;;; and other tags only when *attributes-are-lisp-forms* is false.

(defparameter *template-symbol-package* (find-package '#:keyword)
  "The package symbols are interned into.")

(defparameter *upcase-attribute-strings* t
  "Controls whether attribute strings associated with TMPL_VAR
and other template tags (in compatibility mode only) are upcased
before they are interned.")

;;; TMPL_CALL settings

(defparameter *call-template-access-function* #'car
  "Accessor function for extracting the called template from a
TMPL_CALL form.")

(defparameter *call-value-access-function* #'cdr
  "Accessor function for extracting the values from a TMPL_CALL
form.")

;;; Internal structures

(defvar *find-string-hash* (make-hash-table :test #'equal)
  "Hash tables used internally by READ-UNTIL to cache offset arrays.")

(defvar *printer-hash* (make-hash-table :test #'equal)
  "The cache for template printers.
Each entry is a dotted pair (PRINTER . CREATION-UNIVERSAL-TIME).")

(defvar *template-output* nil
  "The output stream that's used internally.")
