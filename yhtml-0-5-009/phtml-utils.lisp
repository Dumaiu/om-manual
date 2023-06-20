;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser add-ons

(in-package :ystok.html.parser)

;;; Parse both CSS and Content-Type declarations
;;; HTTP properties are case-insensitive
;;; CSS class names are case-insensitive

(defun parse-declaration (declaration &optional (separator #\:))
 ;;; Return (1) property (2) value as strings
  (when-let (position (char-position separator declaration))
    (values (string-trim #1='(#\Space #\Tab) (subseq declaration 0 position))
            (string-trim #1# (subseq declaration (1+ position))))))

(defun parse-declarations (string &optional (separator #\:) default-prop)
 ;;; Collect declarations in a plist while
  ;; 1) intern property into the keyword package,
  ;; 2) replace empty sting "" and strings "none" or "nil" by NIL.
  ;; Args: separator    Character separating indicator from value within one declaration;
  ;;                    declarations are always assumed to be divided by #\;
  ;;       default-prop Property indicator for the very first property,
  ;;			e.g. :mime-type
  (when string
    (let ((plist ()))
      (dolist (declaration (split-seq #\; string))
        (multiple-value-bind (prop value) (parse-declaration declaration separator)
          (cond (prop
                 (setq plist (list* (cond ((zerop (length value)) nil)
                                          ((or (string-equal value "none")	; :none ?
                                               (string-equal value "nil")) nil)
                                          (t value))
                                    (intern-upcase prop)
                                    plist)))
                ((and (null plist) default-prop)
                 (setq plist (list* (string-trim '(#\Space #\Tab) declaration)
                                    default-prop
                                    plist))))))
      (nreverse plist))))

(defun parse-content-type-charset (string)
 ;;; Find canonicalized charset keyword (Lisp system-independent)
  ;; Args: string ::=  mime-type/subtype (";" parameter)*
  ;;	   parameter := property "=" value
  ;;       All properties and values are case-insensitive
  ;; Value: NIL if charset is not specified within the declarations string.
  ;;        An error is signaled if specified but wrong.
  (if-bind (declaration (getf (parse-declarations string #\=) :charset))
    (html:canonicalize-charset declaration t)
    nil))
