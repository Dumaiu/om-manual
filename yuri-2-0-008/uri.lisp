;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; Ystok-URI (RFC3986) - The core code and API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2002-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;; Based on:	uri.lisp 1.0.1	(c) 2001 by Jochen Schmidt
;;;      	uri.cl 1.1	(c) 1999-2001 Franz Inc, Berkeley, CA
;;; RFC: http://www.ietf.org/rfc/rfc3986 (and obsolete rfc2986, rfc2396)
;;;
;;; Implementation notes
;;;  1.	Percent encoding and decoding are supported for all components.
;;;  2. By default, all component except string are stored percent-decoded.
;;;     Use cautiously if you overwrite this default behavior!
;;;  3. Decoded userinfo is stored in plist with indicators :user and :password.
;;;     The print-object never outputs the password.
;;;  4. On changing any "public" component slot via setf, the following internal
;;;	cache slots are not cleared automatically:
;;;      %parsed-path% - should be affected by (setf uri-path),
;;;      string        - should be affected by (setf uri-any_slot) except password.
;;;     You must invoke uri:clear-cache explicitly.
;;;  5. Query
;;;	- when decoded, query is always stored parsed as an alist of the form:
;;;	    ((parameter . value) ...),
;;;	- the *query-char-p* special and query-char-p key parameter allow more
;;;	  (national) characters to be accepted not percent-encoded inside a query part.
;;;  6. Fragment
;;;	- the empty string is accepted as a fragment if # is present;
;;;	- the *fragment-char-p* special and fragment-char-p key parameter allow more
;;;	  (national) characters to be accepted not percent-encoded inside a fragment part.
;;;
;;; CAUTION:
;;;  - Do not invoke (setf uri-...) with a non-simple string value for any of
;;;    the components!
;;;
;;; Compatibility notes
;;;  1. A completely empty URI is represented as "#".
;;;     In contrast, Franz and PURI implementations replace an empty URI by "/".
;;;  2. As a URI object is represented as a structure (not a CLOS object),
;;;     its slot accessors are ordinary (not generic!) functions.
;;;
;;;  Unsupported features
;;;  1. URN.
;;;  2. Interning URI.

(in-package :ystok.uri)

;; Use a designator accepted by both ylib/octets.lisp and flexi-streams.
(defparameter *default-external-format* :utf-8
  "The external format used by percent-encode/decode by default.")

;;; Globals that designate predicates and are used only for parsing

(defvar *query-char-p* nil
  "Predicate to extend character set allowed in query.")
(defvar *fragment-char-p* nil
  "Predicate to extend character set allowed in fragment.")

(defstruct (uri (:copier nil))
   scheme		; keyword or NIL
   host 		; simple string or NIL
   port			; integer or NIL
   path			; simple string or NIL
   query		; simple string, alist or NIL
   fragment		; simple string or NIL
   plist		; :user, :password and/or arbitrary properties
                        ; as can be mutated, we apply copy-list in copy-uri
			; ADDED 2005-02-01: can fail loading older FASL-files!
   string		; internal cached representation, percent-encoded without userinfo
                        ; may differ from original
   %parsed-path%)	; internal cached parsed representation of the URI path
   ;%parsed-query%	; can invalidate loading older fasl-files!

(define-condition uri-error (simple-error) ())

(define-condition uri-parse-error (uri-error
                                   #+lispworks conditions::simple-parse-error
                                   #-lispworks parse-error)
  ())

(defgeneric uri (object)
 (:method (object)
  (error 'uri-error
    :format-control #-Russian "Object cannot be converted to URI: ~s."
					#+Russian (error)
    :format-arguments (list object)))
 (:method ((object uri))
  object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  AUTHORITY  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.2.1.  User Information
;;;  Use of the format "user:password" in the userinfo field is deprecated.
;;;  Applications should not render as clear text any data after the first colon (":")
;;;  found within a userinfo subcomponent unless the data after the colon is
;;;  the empty string (indicating no password).
;;;  Applications may choose to ignore or reject such data when it is received as part 
;;;  of a reference and should reject the storage of such data in unencrypted form.
;;;  The passing of authentication information in clear text has proven to 
;;;  a security risk in almost every case where it has been used.

(defun uri-user (uri)
  (getf (uri-plist uri) :user))

(defun (setf uri-user) (value uri)
  (setf (uri-string uri) nil
        (getf (uri-plist uri) :user) value))

(defun uri-password (uri)
  (getf (uri-plist uri) :password))

(defun (setf uri-password) (value uri)
  (setf (uri-string uri) nil
        (getf (uri-plist uri) :password) value))

(defun uri-authority (uri &optional (userinfo :user))
 "Construct the authority component out of the host and port slots
as well as a user information."
  (let ((host (uri-host uri))
        (port (uri-port uri))
        (user (and userinfo (uri-user uri)))
        (password (and (eq userinfo :password) (uri-password uri))))
    (if (or host port user password)
        (format nil "~@[~A~]~@[:~A~]~:[~;@~]~A~@[:~A~]"
                user password (or user password) host port)
        nil)))

(defun %set-uri-authority (uri authority &optional decode)
 ;;; Writer for parsing and storing non-null authority components.
  ;; Args: authority If only host and/or port is specified, can be
  ;;                 either encoded or decoded.
  ;; NB: As host is case-insensitive, should not it be normalized to lowercase?
  #+debug (assert (simple-string-p authority))
  #-debug (declare (type simple-string authority)
                   (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (let* ((at (char-position #\@ authority))
         (port-colon (char-position #\: authority :start (if at (1+ at) 0)))
         (user-colon (if at (char-position #\: authority :end at) nil))
         (end (or user-colon at))
         (host (subseq authority (if at (1+ at) 0) port-colon))
         (user (if (and end (< 0 end))
                   (subseq authority 0 end)
                   nil))
         (password (if (and user-colon (< (1+ user-colon) at))
                       (subseq authority (1+ user-colon) at)
                       nil))
         (plist (uri-plist uri)))
    (if user
        (setf (getf plist :user) (if decode
                                     (percent-decode user
                                      :external-format #1=(if (eq decode t)
                                                              *default-external-format*
                                                              decode))
                                     user))
        (remf plist :user))
    (if password
        (setf (getf plist :password) (if decode
                                         (percent-decode password :external-format #1#)
                                         password))
        (remf plist :password))
    (setf (uri-host uri) (if (and decode (char-position #\% host))
                             (percent-decode host :external-format #1#)
                             host)
          (uri-port uri) (if (and port-colon (< (1+ port-colon) (length authority)))
                             (parse-integer (subseq authority (1+ port-colon)))
                             nil)
          (uri-plist uri) plist)))

(defun (setf uri-authority) (value uri)
  (if value 
      (%set-uri-authority uri
                          (if (simple-string-p value) value (coerce value 'simple-string)))
      (setf (uri-host uri) nil				; null authority
            (uri-port uri) nil
            (uri-plist uri) (yl:remove-properties (uri-plist uri) '(:user :password))))
  (setf (uri-string uri) nil)
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PARSING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %chars-bitmask (chars &optional (length 127))
 ;;; Args: chars List or string of chars or list of codes
  (do ((bitmask (make-array length :element-type 'bit :initial-element 0))
       (i (length chars))
       elt)
      ((< (decf i) 0)
       bitmask)
    (setf elt (elt chars i)
          (sbit bitmask (if (integerp elt) elt (char-code elt))) 1)))

(declaim (ftype (function (character simple-bit-vector) boolean) char-bitp)
         (inline char-bitp))
(defun charbitp (char bitmask)
  (declare (type character char) (type simple-bit-vector bitmask)
           (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (let ((code (char-code char)))
    (declare (fixnum code))
    (and (< code 127) (/= (sbit bitmask code) 0))))


;; 2.2. Reserved Characters
;; If a data for a URI component would conflict with a reserved character's 
;; purpose as a delimiter, then the conflicting data must be percent-encoded 
;; before the URI is formed.
;; reserved   ::= gen-delims / sub-delims
;; gen-delims ::= ":" / "/" / "?" / "#" / "[" / "]" / "@"
;; sub-delims ::= "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="

(defparameter *gen-delims-bitmask* (%chars-bitmask ":/?#[]@"))
(defparameter *sub-delims-bitmask* (%chars-bitmask "!$&'()*+,;="))
(defparameter *reserved-bitmask* (bit-ior *gen-delims-bitmask*
                                          *sub-delims-bitmask*))
				;(%chars-bitmask ";/?:@&=+$,%")

;; 2.3. Unreserved Characters
;; For consistency, percent-encoded octets in the ranges of
;;   ALPHA (%41-%5A and %61-%7A),
;;   DIGIT (%30-%39),
;;   hyphen (%2D), period (%2E), underscore (%5F), or tilde (%7E)
;; should not be created by URI producers and, when found in a URI,
;; should be decoded to their corresponding unreserved characters by URI normalizers.
;;
;; unreserved ::= ALPHA / DIGIT / "-" / "." / "_" / "~"

(defparameter *alphanumericp-bitmask* (%chars-bitmask
                                       (loop for code upfrom 0 below 127
                                             when (alphanumericp (code-char code))
                                             collect code)))

(defparameter *unreserved-bitmask* (bit-ior *alphanumericp-bitmask*
                                            (%chars-bitmask "-._~")))	;"!*'()"

;; scheme ::= ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
(defparameter *scheme-bitmask* (bit-ior *alphanumericp-bitmask*
                                        (%chars-bitmask "+-.")))


;; authority ::= [ userinfo "@" ] host [ ":" port ]
;; userinfo  ::= *( unreserved / pct-encoded / sub-delims / ":" )
;; host      ::= IP-literal / IPv4address / reg-name
;; reg-name  ::= *( unreserved / pct-encoded / sub-delims )
;;
;; As the below bitmasks are passed to percent-encode direclty,
;; we exclude some of gen-delims from there.
(defparameter *authority-bitmask* (bit-ior *unreserved-bitmask*
                                           *sub-delims-bitmask*))	;";:@&=+$,%"

;; 3.3. Path
;; If a URI contains an authority component,
;; then the path component must either be empty or begin with a slash ("/") character.
;; If a URI does not contain an authority component,
;; then the path cannot begin with two slash characters ("//").
;; In addition, a URI reference (Section 4.1) may be a relative-path reference,
;; in which case the first path segment cannot contain a colon (":") character.
;;
;; path ::= path-abempty		; begins with "/" or is empty
;;	/ path-absolute			; begins with "/" but not "//"
;;	/ path-noscheme			; begins with a non-colon segment
;;	/ path-rootless			; begins with a segment
;;	/ path-empty			; zero characters
;; path-abempty  ::= *( "/" segment )
;; path-absolute ::= "/" [ segment-nz *( "/" segment ) ]
;; path-noscheme ::= segment-nz-nc *( "/" segment )
;; path-rootless ::= segment-nz *( "/" segment )
;; path-empty    ::= 0<pchar>
;; segment       ::= *pchar
;; segment-nz    ::= 1*pchar
;;					; non-zero-length segment without any colon ":". 
;; segment-nz-nc ::= 1*( unreserved / pct-encoded / sub-delims / "@" )
;; pchar ::= unreserved / pct-encoded / sub-delims / ":" / "@"
;;
;; NB: We put '/' and ':' there as it its very unlikely to be encountered in path data.
(defparameter *pchar-bitmask* (bit-ior (bit-ior *unreserved-bitmask*
                                                *sub-delims-bitmask*)
                                       (%chars-bitmask "/:")))

;; 3.4. Query
;; The characters slash ("/") and question mark ("?") may represent data
;; within the query component.  Beware that some older, erroneous
;; implementations may not handle such data correctly when it is used as
;; the base URI for relative references (Section 5.1), apparently
;; because they fail to distinguish query data from path data when
;; looking for hierarchical separators.
;; However, as query components are often used to carry identifying information
;; in the form of "key=value" pairs and one frequently used value is a reference
;; to another URI, it is sometimes better for usability to avoid percent-encoding
;; those characters.
;; query ::= *( pchar / "/" / "?" )

;; 3.5. Fragment
;; The fragment's format and resolution is therefore dependent on the media type 
;; [RFC2046] of a potentially retrieved representation, even though such a retrieval
;; is only performed if the URI is dereferenced. If no such representation exists,
;; then the semantics of the fragment are considered unknown and are effectively
;; unconstrained.
;; Fragment identifier semantics are independent of the URI scheme and thus cannot be
;; redefined by scheme specifications
;; fragment ::= *( pchar / "/" / "?" )
;;
;; HTML 4.0, section 6.2: SGML basic types
;;  ID and NAME tokens must begin with a letter ([A-Za-z]) and may be followed by
;;  any number of letters, digits ([0-9]), hyphens ("-"), underscores ("_"),
;;  colons (":"), and periods (".").
;;
;; XHTML 1.0
;;  id/name atrubute is of Name extending syntax defined by XML:
;;
;; [4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z]
;;	| [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF]
;;	| [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F]
;;	| [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
;;	| [#x10000-#xEFFFF]
;; [4a] NameChar ::= NameStartChar | "-" | "." | [0-9]
;;	| #xB7 | [#x0300-#x036F] | [#x203F-#x2040] 
;; [5]  Name ::= NameStartChar (NameChar)* 
;;
;;  Do not use space (#x32), non-break-space, and other whitespace characters.
;;  National characters are allowed in anchors but should be escaped in URI fragment.

(defparameter *qfchar-bitmask* (bit-ior *pchar-bitmask*
                                        (%chars-bitmask "/?")))

(eval-when (:compile-toplevel :load-toplevel :execute) (meta:enable-meta-syntax))

(defun %meta-parse-uri (string query-char-p fragment-char-p)
 (let ((qfchar-bitmask (load-time-value (bit-ior *qfchar-bitmask* (%chars-bitmask "?@%"))))
       (buf (make-array 0 :element-type (if (or query-char-p fragment-char-p)
                                            #+lispworks 'lw:simple-char
                                            #-lispworks 'character
                                            'base-char)
                        :fill-pointer 0 :adjustable t)))
  ;; URI ::= scheme ":" hier-part [ "?" query ] [ "#" fragment ]
  ;; hier-part ::= "//" authority path-abempty / path-absolute / path-rootless / path-empty
  (flet ((scheme-char (char) (charbitp char *scheme-bitmask*))        
         (authority-char (char) (charbitp char (load-time-value		; % is unlikely
                                                (bit-ior *authority-bitmask*
                                                         (%chars-bitmask ":@%")))))
         (path-char (char) (charbitp char (load-time-value
                                           (bit-ior *pchar-bitmask*
                                                     (%chars-bitmask "/@%")))))
         (query-char (char) (or (charbitp char qfchar-bitmask)
                                (and query-char-p (funcall query-char-p char))))
         (fragment-char (char) (or (charbitp char qfchar-bitmask)
                                   (and fragment-char-p (funcall fragment-char-p char))))
        )
   (meta:with-string-meta (string)
    (macrolet ((backtrack (&body body)
                 `(progn ,@body (setq meta:%index% old-index) nil)))
     (labels
       ((scheme (&aux (old-index meta:%index%) c)
          (setf (fill-pointer buf) 0)
          (or (meta:match [@(alpha-char-p c) !(vector-push-extend c buf)
                           $[@(scheme-char c) !(vector-push-extend c buf)]
                           #\:])
              (backtrack)))
        (authority (&aux (old-index meta:%index%) c)
          (setf (fill-pointer buf) 0)
          (or (meta:match [#\/ #\/ $[@(authority-char c) !(vector-push-extend c buf)]])
              (backtrack)))
        (path (&aux (old-index meta:%index%) c)
          (setf (fill-pointer buf) 0)
          (or (meta:match $[@(path-char c) !(vector-push-extend c buf)])
              (backtrack)))
        (query (&aux (old-index meta:%index%) c)
          (setf (fill-pointer buf) 0)
          (or (meta:match $[@(query-char c) !(vector-push-extend c buf)])
              (backtrack)))
        (fragment (&aux (old-index meta:%index%) c)
          (setf (fill-pointer buf) 0)
          (or (meta:match $[@(fragment-char c) !(vector-push-extend c buf)])
              (backtrack))) )
       (let (scheme authority path query fragment)
         (when (and
                (meta:match
                 [{[!(scheme)
                    !(setq scheme (copy-seq buf))]
                   []}
                  {[!(authority)
                    !(setq authority (copy-seq buf))]
                   []}
                  {[!(path) !(plusp (fill-pointer buf))
                    !(setq path (coerce buf 'simple-string))]
                   []}
                  {[#\? !(query) !(plusp (fill-pointer buf))
                    !(setq query (coerce buf 'simple-string))]
                   []}
                  {[#\# !(fragment)		; 2009-Apr-05: Fragment string can be empty
                    !(setq fragment (coerce buf 'simple-string))]
                   []}
                 ])
                (= meta:%index% meta:%end%))
           (values scheme authority path query fragment))) ))))))

(eval-when (:compile-toplevel :load-toplevel :execute) (meta:disable-meta-syntax))

;; 2.4. When to Encode or Decode
;; ... When a URI is dereferenced, the components and subcomponents
;; significant to the scheme-specific dereferencing process (if any)
;; must be parsed and separated before the percent-encoded octets within
;; those components can be safely decoded, as otherwise the data may be
;; mistaken for component delimiters.
;; The only exception is for percent-encoded octets corresponding to characters
;; in the unreserved set, which can be decoded at any time.

(defun parse-uri (string &key (decode t)
                              (query-char-p *query-char-p*)
                              (fragment-char-p *fragment-char-p*)) 	;(errorp t) 
 "Parse an URI string and return a new URI object."
 ;;; Args: decode NIL - do not call percent-decode at all,
  ;;		  T   - call percent-decode with *default-external-format*,
  ;;		  otherwise must be an external format passed to percent-decode.
  ;; CAUTION: There is no much point in percent decoding path and query
  ;;          until their components are separated.
  (if (zerop (length string))
      (make-uri)	; empty URI should be accepted according to abnormal examples
      (multiple-value-bind (scheme authority path query fragment)
          (%meta-parse-uri string query-char-p fragment-char-p)
        (if (or scheme authority path query fragment)
            (let* (#|(at (if authority
                              (char-position #\@ authority)
                              nil))
                          (port-colon (if authority
                                     (char-position #\: authority :start (if at (1+ at) 0))
                                      nil))
                          (port (if (and port-colon (< (1+ port-colon) (length authority)))
                                    (subseq authority (1+ port-colon))
                                    nil))|#
                   (%parsed-path% (if (and decode path (char-position #\% path))
                                      (parse-path path decode)
                                      nil))
                   (uri
                    (make-uri
                     :scheme (and scheme (intern (string-upcase scheme) *kwd-package*))
                     :path (if %parsed-path%
                               (%render-parsed-path %parsed-path% nil)
                               path)
                     :%parsed-path% %parsed-path%
                     :query (if (and query decode)
                                (parse-query query
                                             (if (char-position #\% query) decode nil))
                                query)
                     :fragment (if (and decode fragment
                                        (char-position #\% fragment))
                                   (percent-decode fragment
                                    :external-format (if (eq decode t)
                                                         *default-external-format*
                                                         decode))
                                   fragment))))
              (when authority
                (%set-uri-authority uri authority decode))
              uri)
            (error 'uri-parse-error
                   :format-control #-Russian "Bad-formed URI string ~s"
                                   #+Russian (error)
                   :format-arguments (list string)) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PRINTING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j;;;;;

;; Characters / and ? may may be unsafe in query and fragment passed to
;; older erroneous implementations.
;; Additional parameter controlling these parts.
(defparameter *unsafe-qchar-bitmask* (%chars-bitmask "&="))
(defparameter *unsafe-fchar-bitmask* nil)			;(%chars-bitmask "/?")

(defun render-uri (uri &optional stream (encode t) (userinfo nil))
 "Print an URI object in normal text notation."
 ;;; Args: encode If is null, no percent encoding is called at all.
  ;;		  Otherwise denotes an external format passed to percent-encode:
  ;;		  - either an external format designator
  ;;		  - or T meaning the value of *default-external-format*.
  (let ((string (uri-string uri)))
    (unless (and string encode (not userinfo))
      (let ((scheme (uri-scheme uri))
            (user (and userinfo (uri-user uri)))
            (password (and (eq userinfo :password) (uri-password uri)))
            (host (uri-host uri))
            (port (uri-port uri))
            (path (uri-path uri))
            (query (uri-query uri))
            (fragment (uri-fragment uri))
            (external-format (if (eq encode t) *default-external-format* encode)))
        (setq string
              (concatenate 'simple-string
                           (when scheme (string-downcase (symbol-name scheme)))
                           (when scheme #1=":")
                           (when host "//")
                           (if (and user encode)
                               (percent-encode user
                                               :safe *authority-bitmask*
                                               :external-format external-format)
                               user)
                           (when password #1#) password
                           (when user "@")
                           (if (and host encode)
                               (percent-encode host
                                               :safe *authority-bitmask*
                                               :external-format external-format)
                               host)
                           (when port ":") (when port (princ-to-string port))
                           (if (and path encode)
                               (percent-encode path
                                               :safe *pchar-bitmask*
                                               :external-format external-format)
                               path)
                           (when query "?")
                           (render-query query)
                           ;(if (and query encode)
                           ;    (percent-encode query :safe *qfchar-bitmask*
                           ;                    :unsafe *unsafe-qchar-bitmask*)
                           ;    query)
                           (when fragment "#")
                           (if (and fragment encode)
                               (percent-encode fragment
                                               :safe *qfchar-bitmask*
                                               :unsafe *unsafe-fchar-bitmask*
                                               :external-format external-format)
                               fragment))))
      (when (and encode (not userinfo))			; cache only if pct-encoded
        (setf (uri-string uri) string)))		; without userinfo
    (if stream
        (write-string string stream)
        string)))

(defmethod print-object ((uri uri) stream)
  (if *print-escape*
      (if *print-readably*
          (progn
            (write-string "#u\"" stream)
            (render-uri uri stream t :user)
            (write-char #\" stream))
          (print-unreadable-object (uri stream :type t :identity nil)
            (render-uri uri stream nil :user)))
      (render-uri uri stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  MERGING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.2. Relative Resolution
;;; CAUTION: When scheme is present in uri, uri is considered final and no merging occurs!

(defgeneric merge-uris (uri base &optional place)
 (:method (uri base &optional place)
  (merge-uris (uri uri) (uri base) place))

 (:method ((uri uri) (base-uri uri) &optional place)
  (let ((scheme (uri-scheme uri))
        (path (uri-path uri)))
    (cond (scheme					; if defined(R.scheme)
           (copy-uri uri :place place))
          ((or (uri-host uri) (uri-port uri) (uri-user uri))
           (copy-uri uri :place place			; if defined(R.authority) 
                     :scheme (uri-scheme base-uri)))
                     ;:path)				; remove_dot_segments(R.path) !
          ((not path)					; if (R.path == "")
           (copy-uri base-uri :place place
                     :query (or (uri-query uri) (or (uri-query base-uri)))
                     :fragment (uri-fragment uri)))
          (t
           (setq scheme (uri-scheme base-uri))		; inherit scheme
           (let ((host (uri-host base-uri))
                 (port (uri-port base-uri)))		; inherit authority
             (if (char= (char path 0) #\/)		; if (R.path starts-with "/")
                 (copy-uri uri :place place		; remove_dot_segments(R.path) !
                           :scheme scheme :host host :port port)
                 (%merge-relative-path-uris uri base-uri place
                                                scheme host port))))))) )


(defun %canonicalize-path (ppath)
 ;;; Some normalization to make the merging easier
  ;; A leading "" is only ok if it is the only element
  (let* ((path (if (and (cdr ppath) (equal (car ppath) ""))
                   (cdr ppath)
                   ppath))
         (last-segment (first (last path))))
    ;; Remove trailing "." or ".."
    (cond ((equal last-segment ".")
           (nconc (nbutlast path) (list "")))
          ((equal last-segment "..")
           (nconc path (list "")))
          (t path))))

(defun %merge-relative-path-uris (uri base-uri place scheme host port)
  ;; Step 6 a+b(+d)
  (let* ((bpath (uri-path base-uri))
         (buffer (%canonicalize-path 
                  (append (rest (parse-path
                                 (subseq bpath 0 (or (position #\/ bpath :from-end t)
                                                     0))))
                          (or (rest (uri-parsed-path uri))
                              '(""))))))
    ;; Step c) Remove all occurrences of "./" where . is a complete path segment
    (setf buffer (delete-if #'(lambda (segment) (and (stringp segment)
                                                     (string= "." segment)))
                            buffer))
    ;; Step e) Remove all occurrences of <segment>/../
    (loop for start = (position ".." buffer :test-not #'equal)
          for pos = (and start (position ".." buffer :test #'equal :start (1+ start)))
          while pos 
          do (unless (equal (elt buffer (1- pos)) "..")
               (setf buffer (delete-if (constantly t) buffer :count 2 :start (1- pos)))))
    (let ((ppath (cons :absolute buffer)))
      (copy-uri uri
                :place place
                :scheme scheme 
                :host host 
                :port port
                :path (%render-parsed-path ppath nil)
                :%parsed-path% ppath)) ))

(defvar *fill-scheme-authority* t)

(defgeneric enough-uri (uri base &optional fill)
 ;;; Args: uri	Should not be relative
  ;;	   fill	If true, set scheme, host, and port missing in uri according to base's.
  ;; NB: Code borrowed from Franz works fine only if URI is not relative!
  ;; DI 2008-Jun-24:
  ;;	Ports in both uri and base are being defaulted before comparison.
 (:method (uri base &optional (fill *fill-scheme-authority*))
  (enough-uri (uri uri) (uri base) fill))
 (:method ((uri uri) (base uri) &optional (fill *fill-scheme-authority*))
  (let ((scheme (uri-scheme uri))
        (host (uri-host uri))
        (port (uri-port uri))
        (fragment (uri-fragment uri)))
    (if (and (imp scheme (eq scheme (uri-scheme base)))
             (imp host (string-equal host (uri-host base)))
             (eql (or port (default-port uri))
                  (or (uri-port base) (default-port base))))
        (let ((new-parsed-path 
               (do* ((path (uri-parsed-path uri))
                     (base-path (uri-parsed-path base))
                     (base-rest base-path (rest base-rest))
                     (rest path (rest rest))
                     (first (first rest) (first rest)))
                    ((not (and base-rest rest (equal (car base-rest) first)))
                     ;; If rest is nil, that means we have something like
                     ;;   (enough-uri "/foo/bar" "/foo/bar/baz.htm"), 
                     ;; so new-parsed-path will be nil.
                     (nconc (when (and rest (not (symbolp first)))
                              ;; Either base-rest exhausted or first elements
                              ;; differ and seconds are neither :absolute nor :relative.
                              ;; Calculate minimum relative path
                              (list* :relative 
                                     (if (rest base-rest)		; length >= 2
                                         (make-list (1- (length base-rest))
                                                    :initial-element "..") )))
                                         ;(list "."))))			; do we need this?
                            (copy-list rest))))))
          (setq scheme (cond (scheme nil) (fill (uri-scheme base)))	; do we need this?
                host   (cond (host nil)   (fill (uri-host base)))
                port   (cond (port nil)   (fill (uri-port base))))
          (let ((some (or new-parsed-path scheme host port (uri-query uri) fragment)))
            ;; DI 2009-Apr-05: We cannot have a completely empty URI.
            ;; Thus replace an empty URI with "#".
            ;; In contrast, both Franz and PURI replace an empty URI by "/".
            (make-uri :scheme scheme
                      :host   host
                      :port   port
                      :path   (if new-parsed-path
                                  (%render-parsed-path new-parsed-path nil)
                                  nil)
                              ;(if some (%render-parsed-path new-parsed-path) "/")
                      :%parsed-path% new-parsed-path
                      :query    (uri-query uri)
                      :fragment (if some fragment "")
                      :plist    (copy-list (uri-plist uri)))))
        uri))))

(defun %render-parsed-path (list &optional (encode nil))
 ;;; Convert a Lispy list-notation path to it's normal string representation
  ;; Args: ecnode If is null, no percent encoding is called at all.
  ;;		  Otherwise denotes an external format passed to percent-encode:
  ;;		  - either an external format designator
  ;;		  - or T meaning the value of *default-external-format*.
  (when (eq encode t)
    (setq encode *default-external-format*))
  (flet ((%encode (string) (if encode
                               (percent-encode string
                                               :safe *pchar-bitmask*
                                               :external-format encode)
                               string)))
    (with-output-to-string (stream)
      (when (eq (first list) :absolute) (write-char #\/ stream))
      (do* ((tail (rest list) rest)
            (first (first tail) (first tail))
            (rest (rest tail) (rest tail)))
           ((endp tail))
        (if (consp first)
            (do* ((subtail first rest)
                  (rest (rest subtail) (rest subtail)))
                 ((endp subtail))
              (princ (%encode (first subtail)) stream)
            (when rest (write-char #\; stream)))
          (princ (%encode first) stream))
        (when rest (write-char #\/ stream))))))
                  
(defun parse-path (string &optional (decode t))
 ;;; Convert the normal string representation of a path to a Lispy list-notation
  ;; Args: decode If is null, no percent decoding is called at all.
  ;;		  Otherwise denotes an external format passed to percent-decode:
  ;;		  - either an external format designator
  ;;		  - or T meaning the value of *default-external-format*.
  (declare (type simple-string string)
           (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (let ((length (length string)))
    (declare (fixnum length))
    (if (zerop length)
        (list :absolute "")
        (flet ((%decode (string)
                 (if decode
                     (percent-decode string :external-format decode)
                     string)))
          (when (eq decode t)
            (setq decode *default-external-format*))
          (do* ((token ())
                (start 0)
                (end 0 (1+ end))
                (result (if (char= (schar string 0) #\/)
                            (progn (incf start) (incf end)
                              (list :absolute))
                            (list :relative))))
               ((>= end length)
                (when (> end start)
                  (if token 
                      (push (nreverse (cons #1=(%decode (subseq string start end)) token))
                            result)
                      (push #1# result)))
                (nreverse (if (char= (schar string (1- length)) #\/)
                              (cons "" result)
                              result)))
            (declare (fixnum start end))
            (case (schar string end)
              (#\; (push #1# token)
                   (setq start (1+ end)))
              (#\/ (if token
                       (progn (push (nreverse (cons #1# token)) result)
                         (setq token nil))
                       (push #1# result))
                   (setq start (1+ end)))) )))))

(defun uri-parsed-path (uri)
 ;;; Collect a lispy representation and cache it
  (when-bind (path (uri-path uri))		; must be decoded and simple already
    (orf (uri-%parsed-path% uri) (parse-path path nil))))

(defun (setf uri-parsed-path) (list uri)
 ;;; Args: list  Must have all components decoded already!
  ;(assert (and (consp list) (member (first list) '(:absolute :relative) :test #'eq)))
  (setf (uri-path uri) (%render-parsed-path list nil)
        (uri-string uri) nil
        (uri-%parsed-path% uri) list))

(defun uri-parsed-query (uri)
 ;;; Collect a lispy as an alist of (param . value) pairs
  (let ((query (uri-query uri)))
    (if (listp query)
        query		; must be decoded already
        (setf (uri-string uri) nil
              (uri-query uri) (parse-query query)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  EQUIVALENCE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.2.3. Port
;;; URI producers and normalizers should omit the port component and its ":"
;;; delimiter if port is empty or if its value would be the same as that of the
;;; scheme's default.
;;; So comparison must be scheme dependent.
;;;
;;; 6.1.  Equivalence
;;; URIs presumably should be considered equivalent when they identify the
;;; same resource.  However, there is no way for an implementation to compare
;;; two resources unless it has full knowledge or control of them.
;;; For this reason, determination of equivalence or difference of URIs is based
;;; on string comparison, perhaps augmented by reference to additional rules
;;; provided by URI scheme definitions.
;;;
;;; 6.2.3.  Scheme-Based Normalization
;;; Normalization should not remove delimiters when their associated
;;; component is empty unless licensed to do so by the scheme specification. 
;;; For example, the URI "http://example.com/?" cannot be
;;; assumed to be equivalent to any of the examples above ("http://example.com/").
;;; Likewise, the presence or absence of delimiters within a userinfo subcomponent is
;;; usually significant to its interpretation.
;;; The fragment component is not subject to any scheme-based normalization;
;;; thus, two URIs that differ only by the suffix "#" are considered different
;;; regardless of the scheme.

(defun default-port (uri)
  (cdr-assoq (uri-scheme uri) '((:http  .  80)
                                (:https . 443)
                                (:ftp   .  21)
                                (:telnet . 23))))

(defun uri= (uri1 uri2)
  (when (eq (uri-scheme uri1) (uri-scheme uri2))
    (let ((default-port (default-port uri1)))
      (and (equalp (uri-host uri1) (uri-host uri2))
           (eql (or (uri-port uri1) default-port)
                (or (uri-port uri2) default-port))
           (string= (uri-path uri1) (uri-path uri2))
           (let ((x (uri-query uri1))
                 (y (uri-query uri2)))
             (if (listp x)
                 (if (listp y)
                     (equal x y)
                     nil)
                 (if (listp y)
                     nil
                     (string= x y))))
           ;(string= (uri-query uri1) (uri-query uri2))
           (string= (uri-fragment uri1) (uri-fragment uri2))))))

(defun copy-uri (uri &key place
                          (scheme   (when uri (uri-scheme uri)))
                          (host     (when uri (uri-host uri)))
                          (port     (when uri (uri-port uri)))
                          (path     (when uri (uri-path uri)) path-specified-p)
                          (query    (when uri (uri-query uri)))
                          (fragment (when uri (uri-fragment uri)))
                          (plist    (when uri (copy-list (uri-plist uri))))
                          (%parsed-path% (when (and uri (not path-specified-p))
                                           (uri-%parsed-path% uri))))
 ;;; Args: uri   Instance of the uri class or NIL.
  ;;       place Existing instance of the uri class, can coincide with uri.
  ;; Value: A newly created URI instance or the place if given.
  ;; NB: The acceptability of a null argument is not mentioned in Allegro documentation
  ;;     explicitly but is handled correcly in other URI/PURI implementations.
  ;;     (NIL can be passed by yportal)
  (if place
      (progn (setf (uri-scheme place)	scheme
                   (uri-host place)	host
                   (uri-port place)	port
                   (uri-path place)	path
                   (uri-query place)	query
                   (uri-fragment place) fragment
                   (uri-plist place)	plist
                   (uri-string place)	nil			; drop cache string
                   (uri-%parsed-path% place) %parsed-path%)
        place)
      (make-uri :scheme	  scheme
                :host	  host
                :port	  port
                :path	  path
                :query	  query
                :fragment fragment
                :plist	  plist
                :%parsed-path% %parsed-path%)))

(defun clear-cache (uri)
 ;;; This must be called explicitly on invoking any external slot setter.
  (nilf (uri-%parsed-path% uri) (uri-string uri)))

(pushnew :uri *features*)
