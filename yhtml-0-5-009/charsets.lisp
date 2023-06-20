;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Characters sets (encoding) and external format matching
;;;
;;; Canonical charset is a keyword to be printed in the meta tag:
;;;  <meta http-equiv="Content-Type" content="text/html; charset=CanonicalCharset>

(in-package :html)

;; Map: implementation-specific charset name -> canonical counterpart
;; The most of it was stolen form Edi's flexi-streams +name-map+.
(defvar +charset-canonical-map+
  '((:utf8	. :utf-8)
    (:utf16	. :utf-16)
    (:ucs2	. :utf-16)
    (:ucs-2	. :utf-16)
    (:unicode	. :utf-16)		; LispWorks
    (:utf32	. :utf-32)
    (:ucs4	. :utf-32)
    (:ucs-4	. :utf-32)
    (:ascii	. :us-ascii)
    (:latin-1	. :iso-8859-1)
    (:latin1	. :iso-8859-1)
    (:latin-2	. :iso-8859-2)
    (:latin2	. :iso-8859-2)
    (:latin-3	. :iso-8859-3)
    (:latin3	. :iso-8859-3)
    (:latin-4	. :iso-8859-4)
    (:latin4	. :iso-8859-4)
    (:cyrillic	. :iso-8859-5)
    (:arabic	. :iso-8859-6)
    (:greek	. :iso-8859-7)
    (:hebrew	. :iso-8859-8)
    (:latin-5	. :iso-8859-9)
    (:latin5	. :iso-8859-9)
    (:latin-6	. :iso-8859-10)
    (:latin6	. :iso-8859-10)
    (:thai	. :iso-8859-11)
    (:latin-7	. :iso-8859-13)
    (:latin7	. :iso-8859-13)
    (:latin-8	. :iso-8859-14)
    (:latin8	. :iso-8859-14)
    (:latin-9	. :iso-8859-15)
    (:latin9	. :iso-8859-15)
    (:latin-0	. :iso-8859-15)
    (:latin0	. :iso-8859-15)
    (:latin-10	. :iso-8859-16)
    (:latin10	. :iso-8859-16)

    (:koi8r	. :koi8-r)		; Cyrillic
    #+sbcl (:koi8-u . :koi8-u)

    (:cp1250	. :windows-1250)	; sbcl
    (:cp1251	. :windows-1251)
    (:cp1251	. :windows-1252)
    (:cp1251	. :windows-1253)
    (:cp1251	. :windows-1255)
    (:cp1251	. :windows-1256)
    (:cp1251	. :windows-1257)

    #+lispworks		  (:jis-x-208 . :jis)
    #+lispworks		  (:jis-x-212 . :jis)
    #+allegro		  (:shift-jis . :sjis)
    #+sbcl		  (:shift_jis . :sjis) 
    #+(or lispworks sbcl) (:euc-jp    . :euc-jp)
)  )

(put :windows-1250 :code-page 1250)
(put :windows-1251 :code-page 1251)
(put :windows-1252 :code-page 1252)
(put :windows-1253 :code-page 1253)
(put :windows-1255 :code-page 1255)
(put :windows-1256 :code-page 1256)
(put :windows-1257 :code-page 1257)

(defun windows-charset-page (charset)
  (get charset :code-page))

(defun charsets (&optional test)
 ;;; List of all or property-related canonical charset keywords
  (let ((result (delete-duplicates (mapcar #'cdr +charset-canonical-map+))))
    (if test
        (remove-if-not test result)
        result)))

;(defparameter *windows-charsets* (charsets #'windows-charset-page))

(defun canonicalize-charset (arg &optional (errorp t))
 ;;; Convert the arg to a canonical charset keyword
  ;; Args: arg    Implementation-specific extertnal-format designator, one of
  ;;		  - string or symbol
  ;;		  - list (win32:code-page :id code) on LW or (keyword . plist) on SBCL,
  ;;              - integer (Windows code page),
  ;;              - object of type excl:external-format on Allegro.
  ;;       errorp When true, signals is cannot be canonicalized.
  ;; Values: 1) Charset keyword with a name suitable to output to HTML <meta ...>
  ;;         2) Args than can be used
  ;; NB: This function can also be user as ef-charset.
  ;; Examples: :LATIN1 -> :ISO-8859-1.
  (let* (keyword
         (result
          (cond
           ((null arg)
            nil)
           ((cond ((stringp arg) (setq keyword (intern-upcase arg)))
                  ((keywordp arg) (setq keyword arg
                                           arg (string keyword)))
                  ((symbolp arg) (setq arg (string keyword)
                                          keyword (intern-upcase arg))))
            (let ((charset (assoq keyword +charset-canonical-map+)))
              (cond (charset
                     (cdr charset))
                    ((rassoc keyword +charset-canonical-map+ :test #'eq)
                     keyword)
                    (t
                     (let ((length (length arg)))
                       (declare (fixnum length))
                       (cond ((and (< 8 length)
                                   (string-equal arg #1="WINDOWS-" :end1 8))
                              (if (setq charset (parse-integer arg :start 8 :radix 10
                                                               :junk-allowed t))
                                  keyword
                                  nil)
                            #+allegro
                            ((and (= 4 length)				; :1251
                                  (setq charset (parse-integer arg :radix 10
                                                            :junk-allowed t)))
                             (if (< 1250 charset 1258)
                                 (intern (string-append #1# arg) *kwd-package*)
                                 nil))
                            #+sbcl
                            ((and (< 2 length)
                                  (string-equal arg "CP" :end1 2))	; :CP1251
                             (if (and (setq charset (parse-integer arg :start 2
                                                                   :radix 10
                                                                   :junk-allowed t))
                                      (<= 1250 charset 1258))
                                 (intern (string-append #1# (subseq arg 2))
                                         *kwd-package*)
                                 nil))
                            #+allegro
                            ((and (< 8 length)
                                  (string-equal arg "ISO8859-" :end1 8))
                             (if (and (setq charset (parse-integer arg :start 8
                                                                   :radix 10
                                                                   :junk-allowed t))
                                      (<= 1 charset 15))
                                 (intern (string-append "ISO-8859-" (subseq arg 8))
                                         *kwd-package*)
                                 nil)))))))))
           ((integerp arg)
            (if (< 1250 arg 1258)
                (intern (string-append #1# (princ-to-string arg)) *kwd-package*)
                nil))
           #+allegro
           ((typep arg 'excl:external-format)
            (canonicalize-charset (excl:ef-name arg) errorp))
           #+lispworks
           ((and (consp arg)
                 (eq (first arg) 'win32:code-page)
                 (let ((charset (getf (rest arg) :id)))
                   (and (integerp charset)
                        (< 1250 charset 1258)
                        (intern (string-append #1# (princ-to-string charset))
                                *kwd-package*)))))
           #+sbcl
           ((consp arg)
            (canonicalize-charset (first arg) errorp))
    ) )   )
    (cond (result)
          (errorp
           (error #L"The external format or charset ~s is illegal or not supported."
                  arg)))))

(defun charset-ef (charset &rest args &key errorp &allow-other-keys)
 ;;; Convert the charset to an implementation-specific extenal format designator
  ;; Args: charset Canonical or implementation-specific
  ;;       errorp  If true, signals an error if charset is not apropriate for
  ;;               the current Lisp implementation.
  ;;       args    Args that are passed to external format constructor.
  ;; Value: External-format designator or :default if errorp is false.
  ;; LW NB: :default -> stream::*default-external-format* or guess
  (cond ((cond 
          ((or (keywordp charset) (stringp charset))
           (let* ((keyword (canonicalize-charset charset errorp))
                  (args (remove-property args :errorp))
                  (result
                   (case keyword
                     (:iso-8859-1
                      #+allegro :iso8859-1
                      #+lispworks :latin-1
                      #+sbcl keyword
                      #-(or allegro lispworks sbcl) :default)
                     (:us-ascii
                      #+(or allegro lispworks sbcl) :ascii
                 #-(or allegro lispworks sbcl) keyword)
                     (:utf-16
                      #+lispworks :unicode		; = (:unicode :little-endian t)
                      #+sbcl :utf-16le			; or :utf-16be
                      #-(or lispworks sbcl) :default)
                     #+allegro
                     (:sjis :shift-jis)
                     #+lispworks
                     (:jis :jis-x-208)			; :jis-x-212?
                     (#+allegro   (:utf-8 :jis)
                      #+lispworks (:utf-8 :macos-roman :sjis :euc-jp)
                      #+sbcl      (:utf-8 :euc-jp)
                      keyword)
                     (otherwise
                      (let ((code (get keyword :code-page)))
                        (cond (code
                               #+allegro
                               (intern (princ-to-string code) *kwd-package*) ; e.g. :1251
                               #+lispworks
                               `(win32:code-page :id ,code :eol-style :crlf ,@args)
                               #+sbcl
                               keyword)
                              #+allegro
                              ((string-equal keyword "ISO" :end1 3)
                               (intern (string-append "ISO8859-"
                                                      (subseq (string keyword) 9))
                                       *kwd-package*))
                              #+allegro
                              (t
                               (apply #'excl:find-external-format keyword :errorp errorp
                                      args))))))))
             (if (and (keywordp result) args (neq result :default))
                 #+(or lispworks sbcl) (cons result args)
                 #-(or lispworks sbcl) result
                 result)))
          ((consp charset)				; lispworks and sbcl
           charset)
          ((numberp charset)				; Windows code-page
           #+allegro
           (intern (princ-to-string charset) *kwd-package*)
           #+(and lispworks win32)
           `(win32:code-page :id ,charset :eol-style :crlf ,@args)
           #+sbcl					; not only Windows code pages
           (intern (string-append "CP" (princ-to-string charset)) *kwd-package*))))
        (errorp
         (error #L"Charset ~s is not supported as a file external format designator."
                charset))
        (t :default)))

(defun charset-ef-match-p (charset external-format)
 ;;; Return true, if charset (parsed or unparsed) "corresponds" to the external-format
  ;; A la (equal (excl:find-external-format charset) external-format)
  (cond ((null (setq charset (canonicalize-charset charset nil)))
         t)					; unknown charset matches any format
        ((and external-format
              (setq external-format (canonicalize-charset external-format nil)))
         (eq charset external-format))))

