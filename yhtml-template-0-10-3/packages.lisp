;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; YHTML-Template
;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2014, Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package definition

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

(in-package :cl-user)

(defpackage :html-template
  (:nicknames :template :htt)
  (:use :common-lisp)
  (:export #:*accept-braces-markers*
           #:*attributes-are-lisp-forms*
           #:*call-template-access-function*
	   #:*call-values-access-function*
	   #:*convert-nil-to-empty-string*	; ignored
           #:*current-column*			; used in PHTML as well
           #:*current-line*
           #:*default-template-output*
           #:*default-template-pathname*
           #:*external-format*
           #:*escape-char-p*
           #:*force-default*
           #:*format-non-strings*
	   #:*format-null-forms*
           #:*ignore-empty-lines*
           #:*ignore-tag-rest*
           #:*no-cache-check*			; ignored
           #:*sequences-are-lists*		; ignored
           #:*string-modifier*
           #:*template-end-marker*
           #:*template-environment*
           #:*template-package*
           #:*template-readtable*
           #:*template-start-marker*
           #:*template-symbol-package*
           #:*upcase-attribute-strings*
           #:*value-access-function*
           #:*value-setq-function*
           #:*warn-on-creation*
           #:clear-template-cache
           #:create-template-printer
           #:delete-from-template-cache
           #:escape-string
           #:escape-string-all
           #:escape-string-iso-8859-1
           #:escape-string-minimal
           #:escape-string-minimal-plus-quotes
           #:fill-and-print-template
           #:template-error
           #:template-eval
           #:template-invocation-error
           #:template-missing-value-error
           #:template-not-a-string-error
           #:template-not-a-string-error-value
           #:template-syntax-condition		; used in PHTML
           #:template-syntax-error
           #:template-syntax-error-col
           #:template-syntax-error-line
           #:template-syntax-error-stream))

(pushnew :html-template *features*)
