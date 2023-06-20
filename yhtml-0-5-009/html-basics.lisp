;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;; Part of HTMLGen and PHTML, copyright (c) 1986-2000 Franz Inc, Berkeley, CA
;;; Part of LML2, copyright (c) 2000-2003 by Kevin Rosenberg.
;;; Part of CL-WHO, Copyright (c) 2003-2005, Dr. Edmund Weitz. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specials and helpers

(in-package :html)

(defconstant* +nbsp+ "&nbsp;")				; HTML char entity (string)
(defconstant +nbsp-char+ #+lispworks #\No-Break-Space
			 #-lispworks (code-char 160))

;; Implementation-specific external format designator
;; Used by: Generator's write-html-string (mainly) and PHTML as a default.
;; The following specials can supply the default value:
;;   YHTML-Template template::*external-format*
;;   PAserve	    *default-aserve-external-format*
;; On the contrast, Hunchentoot's *hunchentoot-default-external-format* is an object
;; of a class defined in of flexi-streams, which is an implementation independent!
(defvar *html-external-format*
  #+(and lispworks win32)	win32:*multibyte-code-page-ef*
  #+(and lispworks (not win32))	:latin-1
  #+sbcl			SB-IMPL::*DEFAULT-EXTERNAL-FORMAT*
  #+allegro			:latin1
  #-(or lispworks sbcl allegro) :iso-8859-1)

;; Stream where all output is sent to
(defvar *html-stream*)

;;; LispWorks 4.3 implements bivalent streams.
;;; OT1H, write-string can safely write to a binary stream both base strings and
;;;       text-strings which only contain base characters.
;;; OTOH, write-string cannot be used to output non-base Unicode characters 
;;;       even when we explicitly specify either or both
;;;	  :external-format :unicode  and  :element-type (unsigned-byte 16)

(defun write-html-string (string &optional (stream *html-stream*) &key (start 0) end)
 ;;; JSC - Handling binary output hack
  ;; Aserve:
  ;;   The same socket is used for transfering binary data (e.g. images)
  ;;   and text data (e.g. HTML).
  ;;   This kludge solves this problem at the cost of I/O performance.
  ;; 
  ;; Hunchentoot:
  ;;   Stream should be a flexi-stream
  ;;   - of element-type character or lw:simple-char,
  ;;   - with an external format embedded inside.
  ;;   It "knows" itself how to output strings. Therefore, do we need this?
  (declare (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0))
           (fixnum start))
  (if (and (equal (stream-element-type stream) '(unsigned-byte 8))
           #+lispworks (lw:text-string-p string))	; Unicode chars may be encountered
      #+(or allegro lispworks sbcl)
      (write-sequence (string-to-octets string :start start :end end :null-terminate nil
                                        :external-format *html-external-format*)
                      stream)
      #|#+(and lispworks (not acl-compat))
      (if (eq *html-external-format* :utf-8)
          (write-sequence (ef:encode-lisp-string (if (and (eql start 0) (null end))
                                                     string
                                                     (subseq string start end))
                                                 :utf-8)

                          stream)
          (loop with end fixnum = (or end (length string))
                and charset = (if (consp *html-external-format*)
                                  (EF::EF-CODED-CHARACTER-SET *html-external-format*)
                                  *html-external-format*)
                for i fixnum from start below end
                do (write-byte (ef:char-external-code (schar string i) charset)
                               stream)))|#
      #-(or allegro lispworks sbcl)
      (loop with end fixnum = (or end (length string))
            for i fixnum from start below end
            do (write-byte (char-code (schar string i)) stream))

      ;; Otherwise stream is text or string is base -  write simply
      (write-string string stream :start start :end end)))


;; True means to following:
;; 1) LHTML is already safe for printing, i.e. all atoms are prepared
;;    for direct call of write-string.
;; 2) Character code to entity/(hexa)decimal conversion is not needed.
;; You can always override by means of explicit pseudo-tags :escape, :<some>-safe, etc.
(defvar *lhtml-safe* nil)

;; Converting attribute symbols, one of :upcase, :downcase, or :preserve
;; Does not matter in XML mode.
(defparameter *attribute-case* :downcase)

;<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
;<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
(defconstant* +html-dtd-string+ "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">")

;(defconstant +html-frameset-dtd-string+
;  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">")

;; This must precede the !DOCTYPE
(defconstant* +xml-declaration-format+ "<?xml version=\"1.0\"~@[ encoding=\"~a\"~]?>")

(defconstant* +xhtml-dtd-string+
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")

(defconstant* +xhtml-frameset-dtd-string+
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"xhtml1-frameset.dtd\">")

;; To be placed as an attribute into the html element
(defconstant* +xmlns+ "http://www.w3.org/1999/xhtml")

(defvar *prologue* +html-dtd-string+
  "This is the first line that'll be printed if the :PROLOGUE keyword
argument is T")

(defvar *html-mode* :sgml
  ":SGML for (SGML-)HTML, :XML for XHTML.")

(defun html-mode ()
  "Returns the current HTML mode. :SGML for (SGML-)HTML and :XML for XHTML."
  *html-mode*)

(defun (setf html-mode) (mode)
  "Sets the output mode to XHTML or (SGML-)HTML.
 MODE can be one of:
 :SGML for HTML,
 :XML for XHTML, or
 a canonicalized charset keyword for XHTML with XML declaration at the beginning."
  (case mode
    (:sgml
     (setf *html-mode* :sgml
           *prologue* +html-dtd-string+))
    (:xml
     (setf *html-mode* :xml
           *prologue* +xhtml-dtd-string+))
    (otherwise
     (if (keywordp mode)
         (setf *html-mode* :xml
               *prologue* (string-append (format nil +xml-declaration-format+ mode)
                                         #\Newline
                                         +xhtml-dtd-string+))
         (error "Wrong HTML mode ~s." mode)))))

(defmacro with-html-mode (mode &body body)
  `(let (*html-mode* *prologue*)
     (setf (html-mode) ,mode)
     ,@body))

(defgeneric output-html (object &key &allow-other-keys)
 (:documentation "Writes the object into html:*html-stream*.
  Is not obliged to return the resulting string."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ESCAPE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates used by escape-string to test whether a character should be escaped

(defun escape-char-not-ascii (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (or (char-position char "<>&'\"")
      (> (char-code char) 127)))

(defun escape-char-not-windows-1251 (char)
  ;; Should be accompanied by
  ;;  ((:meta :http-equiv "Content-Type" :content "text/html; charset=windows-1251"))
  ;;  ((:meta :http-equiv "Content-Language" :content "ru"))
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (or (char-position char "<>&'\"")
      (and (> (char-code char) 127)
           #+lispworks (null (ef:char-external-code char 1251)))))

(defun escape-char-not-ef (char &optional (ef *html-external-format*))
 ;;; Value: True if the CHAR does not match the external-format EF.
  ;; TODO: Add SBCL and others.
  (declare (type character char)
           (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (or (char-position char "<>&'\"")
      #+lispworks 
      (and (> (char-code char) 127)
           (null (ef:char-external-code char (if (consp ef) (getf (rest ef) :id) ef))))
      #-lispworks					; = htt:escape-string-iso-8859-1
      (> (char-code char) 255)))

;; Redefine HTML-TEMPLATE's but both are actually the same
(let (#+lispworks (dspec:*redefinition-action* :quiet))

(defparameter *escape-char-p* #'escape-char-not-ascii)

(defun escape-string (string &key (test *escape-char-p*))
 ;;: Replace unsafe characters by numeric or entity character references.
  ;; Args: string String (maybe not simple) or NIL,
  ;;       test   Predicate of one argument.
  ;; Value: Either a fresh string
  ;;        or the string itself if it does not require modification.
  ;; NB: This version redefines HTML-TEMPLATE's and is close to CL-WHO's
  #-debug (declare (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (if-bind (first-pos (position-if test string))
    (locally (declare (type string string)
                      (fixnum first-pos))
     (with-output-to-string (stream)
      (loop with format-string = (if (eq *html-mode* :xml) "&#x~x;" "&#~d;")
            and end of-type fixnum = (length string)
            for old-pos of-type fixnum = 0 then (1+ pos)
            for pos = first-pos
                    then (position-if test string :start old-pos)
            ;; Now the characters from OLD-POS to (excluding) POS
            ;; don't have to be escaped while the next character has to
            while pos
            do (write-sequence string stream :start old-pos :end pos)
               (let* ((code (char-code (char string pos)))
                      (entity (gethash code *html-code-to-entity*)))
                 (cond (entity					; put entity reference
                        (write-char #\& stream)
                        (write-sequence entity stream)
                        (write-char #\; stream))
                       (t					; put numeric reference
                        (format stream format-string code))))
            while (< (1+ pos) end)
            finally (unless pos
                      (write-sequence string stream :start old-pos)))))
    string)))							; else no unsafe chars

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INCLUDE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This parameter can differ from html-template:*default-template-pathname*
(defparameter *default-include-pathname* nil)

(defconstant* +bad-variable-format-string+
  "In pseudo-tag ~a, bad variable: ~s")

;; Checked by include-file-using-format methods to avoid infinite recursion
(defvar %included-files% nil)

(defgeneric include-file-using-format (format file &key &allow-other-keys)
 ;;; Core inclusion function
  ;; Args: format Keyword or another specifier
  ;;       file   Pathname, namestring, or uri instance.
  ;; TODO: Provide methods on uri with FTP and HTTP schemes to include from everywhere.
 (:method :before (type file &key (verbose #+debug *load-verbose* #-debug nil))
   (when (and verbose type)
     (logg :html "Including file ~a" file)))

 (:method ((type null) file &rest args)
  ;; BAD: File type cannot come from *default-include-pathname* as we need it here
  (let ((pathname (pathname file)))
    (apply #'include-file-using-format
           (file-type-format pathname (pathname-type pathname) t) pathname
           args))) )

 ;(:method ((type string) file &rest args)
 ; (apply #'include-file-using-format (file-type-format string) file args))

(define-file-format :lhtml (:ascii) "LHTML" "LHTM")	; FTP in ASCII mode
(define-file-format :html (:ascii) "HTML" "HTM")

(defmethod include-file-using-format ((format (eql :lhtml)) (pathname pathname)
                                 &key (if-does-not-exist :error)
                                      (defaults *default-include-pathname*)
                                      (external-format *html-external-format*)
                                      (element-type (ef-type external-format)))
                                      ;(charset *html-charset*)
                                      ;(external-format (get-external-format charset)))
  (declare (ignore format))
  (let ((merged-pathname (if defaults (merge-pathnames pathname defaults) pathname)))
    (when (member merged-pathname %included-files% :test #'equal)
      (error "Infinite recursion - file ~A includes itself" merged-pathname))
    (with-open-file (stream merged-pathname
                            :direction :input
                            :if-does-not-exist if-does-not-exist
                            :element-type element-type
                            :external-format external-format)
      (do* ((%included-files% (cons merged-pathname %included-files%))
            (form #1=(read stream nil :eof) #1#))
           ((eq form :eof))
        (html-print form *html-stream*)))))

(defmethod include-file-using-format ((format (eql :html)) (pathname pathname)
                                 &key (if-does-not-exist :error)
                                      (defaults *default-include-pathname*)
                                      (external-format *html-external-format*)
                                      (element-type (ef-type external-format)))
                                      ;(charset *html-charset*)
                                      ;(external-format (get-external-format charset)))
  (declare (ignore format))
  (let ((merged-pathname (if defaults (merge-pathnames pathname defaults) pathname)))
    (when (member merged-pathname %included-files% :test #'equal)
      (error "Infinite recursion - file ~A includes itself" merged-pathname))
    (with-open-file (stream merged-pathname
                            :direction :input
                            :if-does-not-exist if-does-not-exist
                            :element-type element-type
                            :external-format external-format)
      (let ((%included-files% (cons merged-pathname %included-files%)))
        (copy-stream stream *html-stream*)))))

(defun include (file &optional initargs
                &rest args &key format ((:stream *html-stream*) *html-stream*)
                           &allow-other-keys)
 ;;; Args: file     Logical pathname is also possible.
  ;;       initargs Plist (variable value ...),  where
  ;;	 	variable - symbol, if is a keyword, its printname is interned
  ;;			   into *template-package*;
  ;;		value	 - arbitrary Lisp object placed into *template-environment*.
  (let ((html-template:*template-environment*
         (do ((new html-template:*template-environment*) ; convert pairs from initargs 
              var)					 ; into dotted pairs and
             ((null initargs)				 ; collect into new environment
              new)
           (setq var (pop initargs)
                 new (acons (cond ((keywordp var)
                                   (intern (symbol-name var) html-template:*template-package*))
                                  ((and (symbolp var) var)
                                   var)
                                  (t
                                   (error +bad-variable-format-string+ :include var)))
                            (pop initargs)		; value - do not touch
                            new)))))
    (apply #'include-file-using-format format (pathname file)
           (remove-property args :format))))

