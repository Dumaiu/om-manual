;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; Ystok-URI (RFC3986) - Escaping and other utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2002-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;; Based on:	uri.lisp 1.0.1	(c) 2001 by Jochen Schmidt
;;;      	uri.cl 1.1	(c) 1999-2001 Franz Inc, Berkeley, CA
;;;             puri            (c) 2003-2006 Kevin Rosenberg

(in-package :ystok.uri)

(defun change-uri-base (uri old new)
  (merge-uris (enough-uri uri old) new))

(defun concat-trailing (string &optional (char #\/))
 ;;; Concatenate slash to the string end if there is none yet.
  (let ((length (length string)))
    (if (and (plusp length) (char= (char string (1- length)) char))
        string
        (string-append string char))))

;;; ASSUMPTION (or coding hygiene):
;;;  - Strings are properly percent-encoded already.
;;;  - Pathnames are not encoded.

(defmethod uri ((object string))
  (parse-uri object :decode t))

(defmethod uri ((object pathname))
 ;;; Convert without decoding
  (let* ((host (pathname-host object))
         (device (pathname-device object))
         (directory (pathname-directory object))
         (first (first directory)))
    ;; BAD: LispWorks on Windows puts device "C:" into the host slot instead of device.
    ;; Workaround: Move a drive specification from the host to path in URI.
    (when (eq host :unspecific) (setq host nil))
    (when (eq device :unspecific) (setq device nil))
    #+(and lispworks win32)
    (when (and (not device)
               (stringp host) (= (length host) 1))
      (shiftf device host nil))
    ;; We we can hardly have relative path when host or device given
    (let ((%parsed-path% (cons (if (or host device)
                                   (setq first :absolute)
                                   first)
                               (append (if device
                                           (list (string-append device #\:))
                                           ())
                                       (sublis '((:up .  #1="..") (:back . #1#))
                                               (rest directory))
                                       (list (if (directory-pathname-p object)
                                                 ""
                                                 (file-namestring object)))))))
      (make-uri :scheme (if (or host device (eq first :absolute))
                            :file
                            nil)
                :host   host
                :path   (%render-parsed-path %parsed-path% nil)
                :%parsed-path% %parsed-path%))))

(defun uri-type (uri)
 "For URI referring to a file, extract the extension part from the path"
  (when-let* ((last (first (last (uri-parsed-path (uri uri)))))
              (pos (char-position #\. last :from-end t)))
    (subseq last (1+ pos))))

(set-dispatch-macro-character #\# #\u (lambda (stream subchar arg)
                                        (declare (ignore subchar arg))
                                        (parse-uri (read stream t nil t)))) ;:decode arg?
;(set-dispatch-macro-character #\# #\u nil)	; to disable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PERCENT-ENCODING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Every non-ASCII (including UCS-2 or UCS-4) character
;;; - first, is transformed into a sequence of octets using the UTF-8 
;;;   or another external format transformation.
;;; - then the resulting sequence of octets is transformed into a string with each
;;;   octet represented by an escape sequence of the form “%HH”.
;;; MS JScript:
;;;   %uHHHH escape sequences (not in RFC3986) are also recognized by percent-decode.

;;; The bigest length of an escape sequence representing one character.
;;; Depends on the language. The worst case of the original size is:
;;;  3 - if only reserved and 8-bit charset characters allowed,
;;;  6 - if only "normal" languages based on UCS-2 allowed
;;;	  (UTF-8 sequence of no more than 2 octets),
;;; 12 - needed for all possible languages.
(defconstant +max-char-escape-length+ 6)

(defun percent-encode (string &key (external-format *default-external-format*)
                                   (safe *unreserved-bitmask*) unsafe)
 ;;; Replace every "unsafe" character with a percent-encoding sequence %HH
  ;; Args: string  String representation of an URI component.
  ;;	   external-format
  ;;               :UTF-8 or an external format specifier (single- or multiple-byte)
  ;;		   in accordance to
  ;;               - either your Lisp implementation,
  ;;               - or flexi-streams.
  ;;	   safe    Bitmask of characters with code in the range 0..126 that
  ;;		   may not be escaped.
  ;;               Usually, *pchar-bitmask* or *qfchar-bitmask* are passed.
  ;;               If NIL, all characters in this range are considered safe.
  ;;	   unsafe  Additional bitmask of characters with code in the range 0..126
  ;;		   that must be escaped.
  ;;               If NIL, none of the characters in this range is considered unsafe.
  ;; Value: Simple string.
  ;; NB1: If both safe and unsafe are null, only characters with code in the range
  ;;      127..oo  will be encoded.
  ;; NB2: This function is similar to ECMAScript encodeURIComponent.
  ;; Used by: render-uri and HTML generator/utils code.
  (declare (type simple-string string)
           #-debug (optimize (speed 3) (space 0)
                             #+lispworks (hcl:fixnum-safety 0)))
  (let* ((length (length string))				; allocate a string
         (new-length (* +max-char-escape-length+ length))	; big enough
         (new (make-string new-length))
         (j -1)
         char code)
   (declare (fixnum length new-length j)
            (type simple-string new))
   (flet ((%escape (code)					; push octet as %HH
            (declare (type octet code))
            (when (< new-length (the fixnum (+ j 3)))		; safety check
              (setq new (string-append new new)))		; double the buffer
            (multiple-value-bind (q r) (truncate code 16)
              (setf (schar new (incf j)) #\%
                    (schar new (incf j)) (schar #2="0123456789ABCDEF" q)
                    (schar new (incf j)) (schar #2# r)))))
    (dotimes (i length)
      (declare (fixnum i))
      (setq char (schar string i))
      (cond ((< (setq code (char-code char)) 127)
             (if (and (or (null safe) (charbitp char safe))
                      (not (and unsafe (charbitp char unsafe))))
                 (setf (schar new (incf j)) char)		; no need to escape
                 (%escape code)))				; must escape

            ((eq external-format :utf-8)			; special UTF-8 treatment
             (locally (declare (type octet code)))
             (tagbody
              (cond ((< code #x80)
                     (%escape code)
                     (go zero))
                    ((< code #x800)
                     (%escape (the octet (logior #b11000000 (the fixnum (ash code -6)))))
                     (go one))
                    ((< code #x10000)
                     (%escape (the octet (logior #b11100000 (the fixnum (ash code -12)))))
                     (go two))
                    (t
                     (%escape (the octet (logior #b11110000 (the fixnum (ash code -18)))))))
              (%escape (the octet
                            (logior #b10000000
                                    (the fixnum (logand #b00111111
                                                        (the fixnum (ash code -12)))))))
              two
              (%escape (the octet
                            (logior #b10000000
                                    (the fixnum (logand #b00111111
                                                        (the fixnum (ash code -6)))))))
              one
              (%escape (the octet
                            (logior #b10000000 (the fixnum (logand #b00111111 code)))))
              zero))

            (external-format
             (do* ((octets 
                    #+(or allegro lispworks sbcl)
                    (string-to-octets string :start i :end (1+ i)
                                      :null-terminate nil
                                      :external-format external-format)
                    #+(and (not (or allegro lispworks sbcl)) flexi-streams)
                    (flex:string-to-octets string :start i :end (1+ i)
                                           :external-format external-format)
                    #-(or allegro lispworks sbcl flexi-streams)
                    (error "Cannot convert UCS-2 or UCS-4 character ~S to ~S for URI"
                           char external-format))
                  (length (length octets))
                  (i 0 (incf i)))
                 ((>= i length))
               (declare (type octet-vector octets)
                        (fixnum length i))
               (%escape (the octet (aref octets i)))))
            (t
             (setf (schar new (incf j)) char)))))		; no need to escape
   (shrink-vector new (1+ j))))					; truncate new string

(defun percent-decode (string &key (external-format *default-external-format*))
 ;;; Return a string with the real characters used as parsed result.
  ;; Args: string  Lisp string denoting an URI component and possibly
  ;;               embedding %HH or %uHHHH sequences, maybe not simple.
  ;;	   external-format
  ;;		   :UTF-8 or a single-byte external format specifier in accordance to
  ;;               - either your Lisp implementation,
  ;;               - or flexi-streams.
  #-debug (declare (optimize (speed 3) (space 0) #+lispworks (hcl:fixnum-safety 0)))
  (let* ((length (length string))
         (new (make-string length :element-type #+lispworks 'lw:simple-char
                                                #-lispworks 'character))
         (j -1)						; accumulated new lenght
         (unicode nil)
         octets						; "static" buffer
         char)
   (declare (fixnum length j)
            (type simple-string new))
   (do ((i 0 (1+ i)))					; is also increased within loop
       ((<= length i))
    (declare (fixnum i))
    (if (char= #\% (setq char (char string i)))
        (flet ((%unescape ()				; pull %XX and returns its code
                 (when (<= length (+ i 2))
                   #3=(error 'uri-parse-error
                             :format-control #1="Incomplete escape sequence in ~S."
                             :format-arguments (list (subseq string i))))
                 (let ((ch1 (char string (incf i)))
                       (ch2 (char string (incf i)))
                       hex1 hex2)
                   (cond ((char= ch1 #\u)		; MS JScript - not octet!
                          (when (<= length (+ i 2))
                            (decf i 2)
                            #3#)
                          (let* ((hex3 (digit-char-p (char string (incf i)) 16))
                                 (hex4 (digit-char-p (char string (incf i)) 16)))
                            (if (and (setq hex1 (digit-char-p ch1 16))
                                     (setq hex2 (digit-char-p ch2 16))
                                     hex3 hex4)
                                (progn (setq unicode t)
                                  (+ (ash hex1 12) (ash hex2 8) (ash hex3 4) hex4))
                                (error 'uri-parse-error
                                 :format-control "Non-hexidecimal digits after %u: ~A."
                                 :format-arguments (list (subseq string (- i 4) i))))))
                         ((and (setq hex1 (digit-char-p ch1 16))
                               (setq hex2 (digit-char-p ch2 16)))
                          (+ (ash hex1 4) hex2))
                         (t
                          (error 'uri-parse-error
                                 :format-control "Non-hexidecimal digits after %: ~A."
                                 :format-arguments (subseq string (- i 2) i)))))))
          (let ((start i)
                (code (%unescape)))
            (declare (fixnum start code))
            (cond (unicode
                   (setq char (code-char code)
                         unicode nil))
                  ((< 127 code)
                   (if (eq external-format :utf-8)
                       ;; A la ylib/octets.lisp: octets-to-string
                       (let ((chars-remaining 0)
                             (accumulator 0))
                         (declare (fixnum chars-remaining accumulator))
                         (loop
                          (cond ((= chars-remaining 0)
                                 (cond ((= (logand code #b10000000) 0)
                                        (setq char (code-char code))
                                        (return))
                                       ((= (logand code #b11100000) #b11000000)
                                        (setq accumulator (logand code #b00011111)
                                              chars-remaining 1))
                                       ((= (logand code #b11110000) #b11100000)
                                        (setq accumulator (logand code #b00001111)
                                              chars-remaining 2))
                                       ((= (logand code #b11111000) #b11110000)
                                        (setq accumulator (logand code #b00000111)
                                              chars-remaining 3))
                                       ((= (logand code #b11111100) #b11111000)
                                        (setq accumulator (logand code #b00000011)
                                              chars-remaining 4))
                                       ((= (logand code #b11100000) #b11111110)
                                        (setq accumulator (logand code #b00000001)
                                              chars-remaining 5))
                                       (t
                                        #2=(error 'uri-parse-error
                                            :format-control "Invalid octet %~2,'0X at position ~D in UTF-8 sequence ~S."
                                            :format-arguments (list code i
                                                                 (subseq string start))))))
                                ((= (logand code #b11000000) #b10000000)
                                 (setq accumulator (logior (ash accumulator 6)
                                                           (logand code #b00111111)))
                                 (when (= (decf chars-remaining) 0)
                                   (setq char (code-char (setq code accumulator)))
                                   (return)))
                                (t #2#))
                          (unless (char= #\% (char string (incf i)))
                            #3#)
                          (setq code (%unescape))))
                       ;; Not UTF-8
                       ;; TODO: Optimize by collecting several bytes into octets vetctor
                       (progn (unless octets
                                (setq octets (make-array 1 :element-type 'octet)))
                         (setf (aref octets 0) code
                               char (char
                                     #+(or allegro lispworks sbcl)
                                     (octets-to-string octets
                                                       :external-format external-format)
                                     #+(and (not (or allegro lispworks sbcl))
                                            flexi-streams)
                                     (flex:octets-to-string octets
                                                          :external-format external-format)
                                     #-(or allegro lispworks sbcl flexi-streams)
                                     (error 'uri-parse-error
                                      :format-control
                                       "Cannot convert ~A escape sequence to character."
                                       :format-arguments (list external-format))
                                     0)))))
                               ;code (char-code char)))))
                  (t ;; (<= code 127)
                   (setq char (code-char code)))))	; simply collect decoded ASCII char
          (setf (schar new (incf j)) char))
	;; Not %
        (setf (schar new (incf j)) char)))		;(if (char= char #\+) #\Space char)

   (shrink-vector new (1+ j))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;  QUERY SPLITTING/PRINTING  ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Universal Resource Identifiers in WWW: A Unifying Syntax for the Expression of
;;; Names and Addresses of Objects on the Network as used in the World-Wide Web
;;; T. Berners-Lee, June 1994.
;;; http://www.w3.org/Addressing/URL/4_URI_Recommentations.html
;;;
;;; Query strings
;;;
;;; Within the query string, the plus sign is reserved as shorthand notation for
;;; a space. Therefore, real plus signs must be encoded. This method was used
;;; to make query URIs easier to pass in systems which did not allow spaces.
;;;
;;; ODD: However this section is missing from the similar but a bit more recent:
;;;      Uniform Resource Locators (URL) T. Berners-Lee, L. Masinter, M. McCahill,
;;;      December 1994.
;;;      http://www.w3.org/Addressing/URL/4_Recommentations.html
;;; RFC: None of RFCs have mentioned such an information about plus-encoded spaces.
;;;
;;; NB: For HTTP, attribute names are actually HTML element names specified by
;;;     the NAME attribute. Thus they must satisfy the fragment rules.

(defun parse-query (query &optional (decode t))
 ;;; Separate uri-query string into subcomponents end percent-decode them.
  ;; Value: Alist of (name . value) pairs.
  (if (stringp query)
      (loop with decode = (if (eq decode t) *default-external-format* decode)
            and value
            for subseq in (split-seq #\& query)
            for pair = (split-seq #\= subseq :count 2)		; = misses, e.g. for ldap
            when pair
            do (when decode					; decode names as well
                 (rplaca pair (percent-decode (car pair) :external-format decode)))
               (rplacd pair (if (and (setq value (second pair)) decode)
                                (percent-decode value :external-format decode)
                                value))
            and collect pair)
      query))

(defun render-query (query &optional (encode t))
 ;;; Convert a alist query to it's normal string representation and percent-encode it.
  (if (consp query)
      (flet ((%encode (string)
               (if encode
                   (percent-encode string :safe *qfchar-bitmask*
                                   :unsafe *unsafe-qchar-bitmask*
                                   :external-format encode)
                   string)))
        (when (eq encode t)
          (setq encode *default-external-format*))
        (with-output-to-string (stream)
          (let ((rest nil))
            (dolist (pair query)
              (when rest
                (write-char #\& stream))
              (let ((name (car pair))
                    (value (cdr pair)))
                (when name				; do we need
                  (princ name stream))			; (%encode (string name)) ?
                (when (and name value)
                  (write-char #\= stream))
                (when value
                  (princ (%encode (if (stringp value) value (princ-to-string value)))
                         stream)))
              (setq rest t)))))
      query))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INVOKE BROWSER  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+lispworks
(defun open-uri (uri)
 ;;; All versions seem to reuse the browser window most recently opened.
  #+win32
  (win32:shell-execute 0 "open" (uri:render-uri uri nil) nil nil win32:sw_show)
  #+(and mac lispworks4)
  (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace")
               "openURL:" (objc:invoke "NSURL" "URLWithString:" (uri:render-uri uri nil)))
  #+(and (not win32) (not mac) (not lispworks4))
  (sys:open-url (uri:render-uri uri nil))
  #+(and (not win32) (not mac) lispworks4.3)
  (hqn-web::browse (uri:render-uri uri nil)))


