;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HTML-TEMPLATE; Base: 10 -*-
;;; YHTML-Template
;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2014, Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Condition class definitions, signaling functions and macros

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

(define-condition template-error (simple-error)
  ()
  (:documentation "All errors signaled by HTML-TEMPLATE are of
this type."))

(defvar %source-stack% ()
  "A list of the template source objects (pathnames and strings) collected
during recursive calls. Its head indicates what is being filled currently.
Used to avoid infinite TMPL_INCLUDE loops.")

(define-condition template-invocation-error (template-error)
  ((source :initarg :source
           :reader template-invocation-error-source))
  (:default-initargs
           :source (first %source-stack%))
  (:report (lambda (condition stream)
             (format stream "~?~@[~%[Source ~S]~]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (template-invocation-error-source condition))))
  (:documentation "Signaled in fill time when wrong arguments are encountered
by some printer function."))
  
(defmacro signal-template-invocation-error (format-control &rest format-arguments)
  `(error 'template-invocation-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(define-condition template-missing-value-error (template-error)
  ()
  (:documentation "Signaled when a TMPL_EVAL or TMPL_VAR printer
is provided with a NIL value and *CONVERT-NIL-FORMAT* is false."))
  
(defmacro signal-template-missing-value-error (format-control &rest format-arguments)
  `(error 'template-missing-value-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))


(define-condition template-not-a-string-error (template-error)
  ((value :initarg :value
          :reader template-not-a-string-error-value))
  (:documentation "Signaled when a TMPL_VAR printer is provided
with a non-string value."))

(defmacro with-use-value-restart ((form) body)
  "Provide a USE-VALUE restart for BODY in case the value of FORM is null."
  `(restart-case ,body
    (use-value (other-value)
      :report (lambda (stream)
                (format stream #L"Use another value for ~S: " ,form))
      :interactive (lambda ()
                     (format t #L"Enter another value for ~S: " ,form)
                     (multiple-value-list (template-eval (read))))
      other-value)))

;; indentation for LispWorks editor
#+:lispworks
(editor:setup-indent "with-use-value-restart" 1 2 4)


(defstruct syntax-error-location
  "Structure to store parser locations consisting of a stream, a line
number, and a column number."
  line col stream)
 
(defvar *syntax-error-location* (make-syntax-error-location)
  "Used internally to remember the last position which made sense to
the parser.")

;;; The below two specials and two condition classes are also used in PHTML

(defvar *current-line* 1
  "Internal line counter of the parser.")
(defvar *current-column* 0
  "Internal column counter of the parser.")

(define-condition template-syntax-condition (condition)
  ((line :initarg :line
         :reader template-syntax-error-line)
   (col :initarg :col
        :reader template-syntax-error-col)
   (stream :initarg :stream
           :reader template-syntax-error-stream))
  (:default-initargs
      :line (syntax-error-location-line *syntax-error-location*)
      :col (syntax-error-location-col *syntax-error-location*)
      :stream (syntax-error-location-stream *syntax-error-location*))
  (:report (lambda (condition stream)
             (format stream #L"~?~%[line ~A, column ~A~@[, ~A~]]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (template-syntax-error-line condition)
                     (template-syntax-error-col condition)
                     (let ((stream (template-syntax-error-stream condition)))
                       (if #-lispworks (typep stream 'file-stream)
                           #+lispworks (sys:file-stream-p stream)
                           (pathname stream)
                           stream)))))
  (:documentation "Signaled when a syntax error or warning occurs
while parsing a template or HTML file."))

(define-condition template-syntax-error (template-syntax-condition template-error)
  ())

(defmacro signal-template-syntax-error (format-control &rest format-arguments)
  `(error 'template-syntax-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defun signal-unexpected (token &optional comment)
  (signal-template-syntax-error #L"Unexpected ~A~@[~A~]"
                                (if (eq token :eof) #L"end of file" token)
                                comment))

(defmacro with-syntax-error-location ((&rest rest) &body body)
  "This is wrapped around forms in order to remember a meaningful
position within the stream in case an error has to be signaled."
  (declare (ignore rest))
  `(let ((*syntax-error-location* (make-syntax-error-location
                                   :line *current-line*
                                   :col *current-column*
                                   :stream *standard-input*)))
    ,@body))
