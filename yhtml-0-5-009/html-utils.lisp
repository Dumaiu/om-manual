;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General utils coupled with HTMLGen, HTML-TEMPLATE, and YstokURI

(in-package :html)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LHTML Utils  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-element (key cont &optional attr (value nil supplied-p)
                              &key (test #'equal))	; (max-depth 0)
 "Search the content CONT for an element with the tag KEY and
 optionally with the ATTR.
 Value: Element or NIL"
  ;; cont is usually the content tree of a top-level element.
  ;; TODO: Allow key and attr to be lists of possible matches?
  (if attr						; attrubute should be specified
      (find-if (lambda (element) (and (consp element)
                                      (consp (setq element (car element)))
                                      (eq (first element) key)
                                      (let ((v (getf (rest element) attr *unbound-value*)))
                                        (if supplied-p
                                            (funcall test v value)
                                            (neq v *unbound-value*)))))
               cont)
      (find key cont :key #'elem-key)))

#|(defun find-top-level-element (key lhtml)
 ;;; Args: lhtml - a list, usually a result of phtml.
  (find key (elem-cont (find :html lhtml :key #'elem-key))
        :key #'elem-key))|#

(defun set-attribute (elem attr value)
 "Set the value of attribute ATTR to the VALUE destructively.
 Arguments:
   value  If null, delete the ATTR from the attributes of ELEM if exists.
 Value: True if the ELEM is a cons; false otherwise."
  (when (consp elem)
    (if value
        (setf (getf (elem-attrs elem) attr) value)
        (progn (remf (elem-attrs elem) attr) t))))

(defun ensure-attribute (elem attr value &optional (test #'equal))
 "Set the value of attribute ATTR to the VALUE non-destructively.
 Arguments:
   value  If null, remove the ATTR from the attributes if exists.
 Value: The ELEM itself or its copy if the ELEM is an atom."
  (let ((key (elem-key elem))
        (attrs (elem-attrs elem))
        (cont (elem-cont elem)))
    (if attrs
        (let ((old (getf attrs attr)))
          (cond (old
                 (cond ((not value)
                        (make-elem key (remove-property attrs attr) cont))
                       ((funcall test old value)
                        elem)
                       (t
                        (setf attrs (copy-list attrs)
                              (getf attrs attr) value)
                        (make-elem key attrs cont))))
                (value
                 (make-elem key (list* attr value attrs) cont))
                (t
                 elem)))
        (if value
            (make-elem key (list attr value) cont)
            elem))))

(defmethod print-attribute (key name (value uri:uri) &optional uri-p (stream *html-stream*))
  (declare (ignore key))
  (unless uri-p
    (warn "URI value for non-URI attribute: ~a ~s."
          (attribute-name-string name) value))
  (write-string (attribute-name-string name) stream)
  (write-string "=\"" stream)
  (uri:render-uri value stream)
  (write-char #\" stream))

(defvar *uri-attribute-rewriter* nil)

(defmethod print-attribute (key (name symbol) value &optional uri-p (stream *html-stream*))
  (if (and uri-p *uri-attribute-rewriter*)
      (print-attribute key name (funcall *uri-attribute-rewriter* value)
                            nil stream)				; prevent recursion
      (call-next-method)))

#|(defmethod html:print-attribute ((key (eql :form)) (name (eql :action)) (value null))
                                 &optional uri-p (stream html:*html-stream*))
 ;;; <form> tag without action attribute
  (if (and uri-p *uri-attribute-rewriter*)
      (html:print-attribute key name (funcall *uri-attribute-rewriter* value)
                            nil stream)				; prevent recursion
      (call-next-method)))|#


(defun princ-safe-or-nbsp (cont &optional break)
  ;; Args: break
  (cond ((or (null cont) (equal cont +null-string+))
         (html:princ-http +nbsp+))
        (break
         (html-print-list (interleave-seq 'list :br
                           (mapcar (lambda (string) `(:princ-safe ,string))
                                   (split-seq #\Newline cont :keep-empty-subseqs t))
                           t)
                          *html-stream*))
        ((princ-safe-http cont))))

(defun text-to-html-paragraph (arg &optional (bbcode t))
 ;;; Convert multi-line text to the corresponding string <P>par1<P>par2...<P>parN</P>.
  ;; Value: String if arg is not null or NIL if arg is null.
  (let ((length (length arg)))
    (when (plusp length)
      (string-append "<p>"
                     (if bbcode
                         (html:bbcode-html-string arg)
                         (interleave-seq 'string "<br>"
                           (split-seq #\Newline (funcall htt:*string-modifier* arg)
                                      :keep-empty-subseqs t)
                           t))
                     "</p>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;  YHTML-TEMPLATE CUSTOMIZATION  ;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some of these are redefined as preferences in ys-product.lisp

(setq htt:*external-format*	#+(and lispworks win32) win32:*multibyte-code-page-ef*
                                #+sbcl                  SB-IMPL::*DEFAULT-EXTERNAL-FORMAT*
				#-(or (and lispworks win32) sbcl) :default
      htt:*ignore-empty-lines*	t
      htt:*format-non-strings*	'html-princ-to-string
      htt:*warn-on-creation*	t
      *escape-char-p*		#'escape-char-not-ef
      htt:*string-modifier*	'escape-string)
      ;htt:*string-modifier*	'htt:escape-string-minimal-plus-quotes)

;; Keep the names and functions for the symbols oftenly used in templates
(yl:keep-symbol-names '(IF WHEN UNLESS AND OR NOT QUOTE PROGN SETQ CASE
                        number sequence time))
(yl:keep-symbol-names '(eq eql equal
                        evenp integerp oddp  = < <= > >=  + - 1+ 1-
                        elt length subseq
                        first last listp member rest second
                        uri:uri-fragment uri:uri-host uri:uri-query
                        uri:uri-port uri:uri-path uri:uri-scheme uri:uri-user)
                      :function)

(defparameter *default-template-file-type* "htt"
  "String used as a pathname-type default along with templates-folder.")

;(defparameter *wild-template-file*  #P"*.htt")
;(defparameter *wild-shtt-file* #P"*.shtt")
;(defparameter *wild-phtt-file* #P"*.phtt")

(defgeneric templates-folder (object)
 (:documentation "An (absolute) directory pathname containing template files related to the object.")
 (:method (object) nil))

(defgeneric html-princ-to-string (object)
 (:documentation "Write an object into the string that is output to HTML.
 Used as a value of html-template:*format-non-strings*.
 The output can be placed into attribute value position or content.
Values:
 (1) Resulting string,
 (2) Safep - true means that the first value should be output verbatim,
     false means it will be modified further according to template:*string-modifier*.")
 (:method (object)
  (values (princ-to-string object) nil)))

(defmethod html-princ-to-string ((object cons))
 ;;; Q: Maybe interpret as LHTML? E.g. :princ instead of :verbatim etc.
  (if (eq (car object) :verbatim)			; treat HTML string specially
      (values (if-bind (string (cdr object))		; namely
                string
                +nbsp+)
              t)					; do not escape
      (values (interleave-seq 'string #\Space (mapcar #'princ-to-string object))
              nil)))


(yl:keep-symbol-names
 '(icon icon-html text) ;title
 :function)

(defun uri-text (uri)
  (getf (uri:uri-plist uri) :text))
(defun (setf uri-text) (value uri)
  (setf (getf (uri:uri-plist uri) :text) value))

(defgeneric icon (object)
 ;;; Q: Is the result an absolute or relative URI? If the latter is the case,
 ;;;    should not it be merged against a base URI (opitional parameter)?
 (:documentation "URI of the icon file or reference associated with the object.
Values: 1) instance of uri:uri or NIL, (2) width, (3) height.")
 (:method (self) nil))

(defun icon-html (object)
 "HTML code <img src=...> for an icon or reference associated with the object."
  ;; Used by: html-princ-to-string
  (multiple-value-bind (uri width height) (icon object)
    (when uri
      (cons :verbatim (format nil "<img src=\"~A\" width=\"~D\" height=\"~D\"~:[~; /~]>"
                              uri (or width 16) (or height 16)
                              (eq *html-mode* :xml))))))

  
(defgeneric template-environment (object &optional uri)
 (:documentation "Compute an alist of (template-variable . value) pairs corresponding to
essential properties of the object.
This alist is usually merged with *template-environment*.
Arguments:
 uri  The final location of the file in target directory or in the server.
      Computed with (http-uri object) by default.
Value: Freshly consed alist.")
 (:method (object &optional uri)
   (declare (ignore object uri))
   ())
 (:method ((object uri:uri) &optional uri)
  ;; CAUTION: Do not output the entire uri-plist as some credentials can be there!
   (declare (ignore uri))
   `((uri   . ,object)
     (text  . ,(uri-text object))
     ;(title . ,(title object))
     (icon       . ,(icon object))
     (icon-html  . ,(icon-html object)))) )

(defun slots-environment (instance &optional exclude subst)
 ;;; Helper to provide standard template environment generation
  ;; Args: exclude List of the names of the slots that must be exluded from
  ;;	           the template environment.
  ;;       subst   Alist of slot name substitutions that will be bound within
  ;;		   the environment instead the original slot names defined in defclass.
  ;; Value: A fresly consed alist.
  ;; HINT: You can override a value of some slot by pushing corresponding pair
  ;;       at the head of environment.
  (loop for slot in (clos:class-effective-slots (class-of instance))
        for slot-name = (clos:slot-definition-name slot)
        when (and (slot-boundp instance slot-name)
                  (not (memq slot-name exclude)))
        collect `(,(cdr-assoq3 slot-name subst slot-name) .
                  ,(slot-value instance slot-name))))
