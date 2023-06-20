;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2016 Dr. Dmitry Ivanov
;;; Part of HTMLGen and PHTML, copyright (c) 1986-2000 Franz Inc, Berkeley, CA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser core
;;;
;;; NOTES AND CHANGES/ADDITIONS TO FRANZ CODE
;;;  + Added the element-type and external-format parameters to the default
;;;    parse-html method (i.e. on a pathname).
;;;
;;;  * Changed signature of phtml-internal:
;;;    ** read-sequence-func is the last optional parameter now.
;;;
;;;  * HTML character references
;;;    ++ Numeric and entity references are supported in (delimited) attribute
;;;       values as well as in PCDATA.
;;;    ++ Added pseudo-entities - non-standard character names that can
;;;       be replaces by an arbitrary LHTML element.
;;;    ** The parsing code was abstracted and moved into the new %parse-entity
;;;       local function. It converts an HTML escaped character references, i.e. 
;;;       numeric or named entity strings, to a
;;;       - character,
;;;       - string, or
;;;       - LHTML element.
;;;    ** 2016-Feb-11: Entities are no longer parsed within URI attributes
;;;	  as they follow their own escape convention.
;;;
;;;  * Function next-token
;;;    ** For state names, symbols are used instead of integers.
;;;    ** Replaced macrolet with flet.
;;;    ** Attribvaluedelim: escape char #\\ is actually skipped and next char is read.
;;;
;;;  * Callbacks
;;;    ** Callbacks are invoked in bottom-top order, i.e. applied
;;;       - first to leaves,
;;;       - then to the root.
;;;    ** The callback of an element is invoked
;;;       --- for an element without an end tag - just after parsing the start tag;
;;;       --- for an element with an end tag - just after parsing the end tag
;;;            (this can also be triggered during auto-closing when
;;;	       the start of a sibling or the end of a parent is encountered);
;;;       --- for comment - just after the whole comment is read
;;;	      (useful for specifying control info like TMPL_...).
;;;    ++ Rewriting elements via callbacks is possible.
;;;       A callback can return the two values:
;;;       (1) A new element that should replace the argument received,
;;;	      does not matter if the second value is null.
;;;       (2) A generalized true as an indication that the element has changed.
;;;	      :append   - the new element should be revappend'ed to pending content,
;;;	      otherwise - the new element should be consed to pending content.
;;;    ++ Added *callback-parent-tag* special variable.
;;;	  It is bound around any call of a callback.
;;;    ++ Added *xhtml-mode* special variable indicating that an XHTML prologue
;;;       has been encountered.
;;;
;;;  + Pseudo-elements
;;;    ++ Added the following pseudo-elements similar to the !-- comment.
;;;       !:doctype
;;;       <?xml ...?>
;;;       <?php ...?> - in attributes, PHP code is left unparsed.
;;;    ** Marked #\? as tag-char.
;;;    ** Renamed xml-bailout to sgml-bailout, and the lexer returns the :sgml kind
;;;       instead of :xml now.
;;;
;;;  * Whitespace
;;;    ++ Added function collapse-whitespace.
;;;    ++ Added property tag-retain-whitespace for preformatted element symbols,
;;;       e.g. <pre>.
;;;       Spaces within the content of these elements are never collapsed.
;;;    ++ Added :retain-whitespace parameter to parse-html
;;;       t        - whitespace is preserved everywhere in full,
;;;       :newline - replace #\Newline with :newline keyword and
;;;                  collapse or eliminate some space around it,
;;;       nil      - neither linebreaks nor other whitespace survive, except for
;;;                  --- raw-mode elements: script, !DOCTYPE, etc.
;;;                  --- inside the scope of tag-retain-whitespace.
;;;
;;;  * More error reporting
;;;    ++ Tracking the input line number and column similar to YHTML-Template.
;;;    ++ Signaling template-syntax-error when an end tag is encountered but
;;;       no matching start tag pending within the document tree.
;;;    ++ Warning about absence of an explicit end tag,
;;;    ++ Added template-syntax-warning conditon to issue a warning about
;;;       intersection of element scopes or missing end tags.
;;;       That could help to write HTML in a more strict manner.
;;;
;;;  * Considerable code rewrite and lots of improvements
;;;    -- Removed dependency on Allegro's excl package or ACL-Compat lite:
;;;       the if* macro is no longer needed.
;;;    ++ More integration with HTMLGen.
;;;    ** Macro tag-no-end replaced by the inline function and implemented
;;;       by means of html:*desc-hash-table*.
;;;    ** Fixed :callback-only mode, which malfunctioned in the original Franz code.

(in-package :ystok.html.parser)

;; Special bound to the parent tag (i.e. keyword or (keyword attr value...))
;; around the call of any callback.
(defvar *callback-parent-tag*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ERRORS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What to do when the ending tag is encountered that does not match the current
;; start tag. That help to control embeding tags in strict manner.
;; Possible values:
;;  :warn   - issue a warning,
;;  yl:logg - print to the log stream provided the logging option :html-template is on,
;;  NIL     - silently do nothing.
(defparameter *end-tag-mismatch-action* :warn)

(defparameter *retain-whitespace* :newline)

(defvar *phtml-stream*)

(defmacro with-syntax-error-location ((&optional stream) &body body)
 ;;; Wrapper around calls to parse-enitity and phtml-internal body.
  `(let ((*phtml-stream* ,stream)
         (htt:*current-column* 0)
         (htt:*current-line* 1))
     ,@body))

(define-condition template-syntax-warning (htt:template-syntax-condition simple-warning)
  ())

(defun syntax-error (format-control &rest format-arguments)
  (error 'htt:template-syntax-error
         :format-control format-control
         :format-arguments format-arguments
         :stream *phtml-stream*
         :line htt:*current-line*
         :col  htt:*current-column*))

(defun eof-error (&key start (line htt:*current-line*) (col htt:*current-column*))
 ;;; Args: start NIL or string Starting 
  (error 'htt:template-syntax-error
         :format-control #L"Unexpected end of input stream encountered~@[ started by ~s~]."
         :format-arguments (list start)
         :stream *phtml-stream*
         :line line :col col))

(defun syntax-cerror (continue format-control &rest format-arguments)
  (cerror continue
          'htt:template-syntax-error
          :format-control format-control
          :format-arguments format-arguments
          :stream *phtml-stream*
          :line htt:*current-line*
          :col  htt:*current-column*))

(defun syntax-log-warn (action format-control &rest format-arguments)
 ;;; Args: action Generalized true
  (case action
    (yl:logg					; log instead of warn but prints only if
     (yl:logg '(:html-template . :warn)		; logging for :html-template module is on
               (make-condition 'template-syntax-warning
                               :format-control format-control
                               :format-arguments format-arguments
                               :line   htt:*current-line*
                               :col    htt:*current-column*
                               :stream *phtml-stream*)))
    (:warn
     (warn 'template-syntax-warning
           :format-control format-control
           :format-arguments format-arguments
           :stream *phtml-stream*
           :line htt:*current-line*
           :col  htt:*current-column*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  CHARACTER-ISTICS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +tag-bit+          1)		; valid char for a tag
(defconstant +attrib-name-bit+  2)		; valid char for an attribute name
(defconstant +attrib-value-bit+ 4)		; valid in undelimited attribute value
(defconstant +whitespace-bit+   8)

;; Array of bits describing character characteristics
(defparameter *character-istics* 
  (let ((arr (make-octet-vector 128 :initial-element 0)))
    (declare (optimize (space 3) (safety 0)))
    (macrolet ((%logiorf (chars bits)		 		; chars is not evaluated
                 (with-gensyms (index char val)
                   `(let ((,val ,bits))
                      ,(cond ((atom chars)			; char
                              `(let ((,index (char-code ,chars)))
                                 #1=(setf (aref arr ,index)
                                          (logior (aref arr ,index) ,val))))
                             ((listp (cdr chars))		; (char ...)
                              `(dolist (,char ',chars)
                                 (let ((,index (char-code ,char)))
                                   #1#)))
                             (t					; (from . to)
                              `(loop for ,index upfrom (char-code ,(car chars))
                                                upto (char-code ,(cdr chars))
                                     do #1#)) )))) )
      (%logiorf (#\A . #\Z) (logior +tag-bit+ +attrib-name-bit+ +attrib-value-bit+))
      (%logiorf (#\a . #\z) (logior +tag-bit+ +attrib-name-bit+ +attrib-value-bit+))
      (%logiorf (#\0 . #\9) (logior +tag-bit+ +attrib-name-bit+ +attrib-value-bit+))

      (%logiorf #\:       (logior +tag-bit+ +attrib-name-bit+)) ; XML namespace?
      (%logiorf (#\- #\.) (logior +attrib-name-bit+ +attrib-value-bit+))
	
      ;; All typeable chars except for whitespace and >
      (%logiorf (#\: #\@ #\/ #\! #\# #\$ #\% #\^ #\& #\( #\) #\_ #\= #\+ #\\ #\|
                 #\{ #\} #\[ #\] #\; #\' #\" #\, #\< #\?)
                +attrib-value-bit+)
	
      (%logiorf (#\Space #\Tab #\Return #\Linefeed) +whitespace-bit+)

      ;; Not sure what can be in a tag name but these used in comments:
      ;;   '!' '-' in SGML,
      ;;   '?'     in XML and PHP. 
      (%logiorf (#\- #\! #\?) +tag-bit+))
    arr))
	
(defun character-istic (char bit)
 ;;; Value: True if the given char has the given characteristic bit set
  (declare (type character char)
           (type fixnum bit)
           #-debug (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (let ((code (char-code char)))
    (declare (fixnum code))
    (and (<= 0 code 127)					; in range
         (logtest (the octet (aref *character-istics* code)) bit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ENTITIES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pseudo-entities usage:
;;; 1) Define alist ::= ((name elem-or-string) ...)
;;;    for expanding non-standard names into LHTML elements.
;;;    For example
;;;      (("nequiv" . "<img class='glyph' src='help:/gfx/nequiv.png'>")
;;;       ("sqrt"   . "&radic;&oline;"))		; <- this should be parsed
;;;    Some of the standard HTML entities that are not supported by all browsers
;;;    can be redefined this way.
;;; 
;;; 2) Add the corresponding entries to *html-entity-to-code* hash-table
;;;    by calling ensure-entity-table
;;;    - either on starting the application up
;;;    - or on loading some document.
;;;
;;; 3) Optionally, provide a callback rewriting any pseudo-element
;;;    (:entity elem-or-string) to another resulting element.

(html::def-special-html :entity
  t								; non-standard tag
  (lambda (desc cmd args form subst stream)
    (declare (ignore cmd args desc subst))
    ;; The LHTML form is unlikely safe as it has been produced while parsing entities.
    ;; We rely on html:*lhtml-safe* (should print using emit-safe).
    (html::html-print-list-subst (rest form) nil stream)))	; no subst in entity

;; Total acceptable length of entity or numeric character reference.
;; Starting & and terminator ; are not counted.
(defparameter *max-entity-length* 10)

(defun ensure-entity-table (&optional alist table)
 ;;; Args: alist Addional mapping: string -> string/list
  (if table
      (clrhash table)
      (setq table (make-hash-table :test #'equal)))
  (loop for code being each hash-key in html:*html-code-to-entity*
        using (hash-value name)
        do (setf (gethash name table) code))
  (dolist (pair alist)
    (setf (gethash (car pair) table) (cdr pair)))
  table)

(defparameter *html-entity-to-code* (ensure-entity-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TAGS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elements
;; - without content (no end tag),
;; - or allowing only inline content (no blocks).
(defparameter *inline-tags*
 '(:br :i :b :em :span :strong :var :dfn :code :kbd :tt :sub :sup :a :cite :img
   :font :samp :big :small :abbr :acronym :object :map :q :bdo	  ; I'm not using these
   :label :button :input :select :textarea))

;; Elements of character formatting
;; - their scopes can intersect,
;; - they subset of *inline-tags*.
(defparameter *character-tags*
  '(:i :b :tt :big :small :strike :s :u :em :strong :font))

(declaim (inline tag-no-end))
(defun tag-no-end (key)					; does not have ending </ > tag
  (when-bind (desc (gethash key html:*desc-hash-table*))
    (not (html::desc-has-end desc))))

(defmacro tag-siblings (key)				; pending tags to be closed
 `(get ,key 'tag-siblings))				; when the start tag key is seen

(defmacro tag-parents (key)				; parent tags that limit the scope
 `(get ,key 'tag-parents))				; of auto closing when key is seen

;;; Elements whose start tag can terminate a previous sibling or child element
;;; as the end tag of the latter is optional.
;;; NB: Some inline elements have siblings to auto-close.
;;;     But character tags cannot have siblings!
(progn
 (put :address	  'tag-siblings	#1='(:head :p))		; P cannot contain block elements
 (put :blockquote 'tag-siblings	#1#)
 (put :body	  'tag-siblings	'(:body :frameset :head))

 (put :div 'tag-siblings #1#)

 (put :dl 'tag-siblings	#1#)
 (put :dt 'tag-siblings	'(:dt :dd))
 (put :dt 'tag-parents	'(:dl))
 (put :dd 'tag-siblings	'(:dd :dt))
 (put :dd 'tag-parents	'(:dl))

 (put :fieldset 'tag-siblings	#1#)
 (put :form	'tag-siblings	#1#)
 (put :frameset 'tag-siblings	'(:body :frameset :head))

 (put :h1 'tag-siblings #1#)
 (put :h2 'tag-siblings #1#)
 (put :h3 'tag-siblings #1#)
 (put :h4 'tag-siblings #1#)
 (put :h5 'tag-siblings #1#)
 (put :h6 'tag-siblings #1#)

 (put :hr 'tag-siblings #1#)

 (put :li 'tag-siblings '(:li))
 (put :li 'tag-parents  '(:ul :ol))
 (put :ol 'tag-siblings	#1#)
 (put :ul 'tag-siblings	#1#)

 (put :noscript 'tag-siblings #1#)

 (put :optgroup 'tag-siblings	'(:option))	; may be nested, end tag required
 (put :optgroup 'tag-parents	'(:select))	; CAUTION: SELECT is inline itself.
 (put :option   'tag-siblings	'(:option))
 (put :option   'tag-parents	'(:optgroup :select))

 (put :p     'tag-siblings	#1#)
 (put :pre   'tag-siblings	#1#)

 (put :table 'tag-siblings	#1#)
 (put :tbody 'tag-siblings	'(:colgroup :tfoot :tbody :thead))
 (put :tbody 'tag-parents	'(:table))

 (put :tfoot 'tag-siblings	'(:colgroup :tfoot :tbody :thead))
 (put :tfoot 'tag-parents	'(:table))

 (put :thead 'tag-siblings	'(:colgroup :tfoot :tbody :thead))
 (put :thead 'tag-parents	'(:table))

 (put :tr 'tag-siblings	'(:tr :td :th :colgroup))
 (put :tr 'tag-parents	'#65=(:tbody :thead :tfoot :table))

 (put :td 'tag-siblings	'(:td :th))
 (put :td 'tag-parents	#66='(:tr . #65#))

 (put :th 'tag-siblings	'(:td :th))
 (put :th 'tag-parents	#66#)
)

(defmacro tag-no-pcdata (key)				; only subelements allowed
  `(get ,key 'tag-no-pcdata))				; in this element, no strings

(setf (tag-no-pcdata :select) t
      (tag-no-pcdata :table)  t
      (tag-no-pcdata :tr)     t)

(defmacro tag-retain-whitespace (key)			; preformatted elements
  `(get ,key 'tag-retain-whitespace))

(setf (tag-retain-whitespace :pre) t
      (tag-retain-whitespace :ins) t
      (tag-retain-whitespace :del) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  COLLECTOR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Our pool implementation allows not to bother about unwind-protect
;;; the free-collector call: on non-local exit, GC collect it later.

(defvar *collectors* #4(nil))				; global pool of collectors

(defun alloc-collector ()
  #-debug (declare (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  ;mp::without-scheduling
  (dotimes (i (length *collectors*) ;(make-collector))
              (make-array 100
                          :element-type 'character
                          :fill-pointer 0 :adjustable t))
    (declare (fixnum i))
    (when-let (elt (svref *collectors* i))		; found in pool
      (setf (svref *collectors* i) nil			; exclude from the pool
            (fill-pointer elt) 0)			; reinitialize and reuse
      (return elt))))

(defun free-collector (coll)
  (dotimes (i (length *collectors*))
    (declare (fixnum i))
    (unless (svref *collectors* i)			; if there is free cell in pool,
      (return (setf (svref *collectors* i) coll)))))	; place there, otherwise toss away

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TOKENBUF  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tokenbufs* #4(nil))			; global pool of token buffers

(defstruct tokenbuf
  (cur 0)					; next index to use to grab from tokenbuf
  (max 0)					; index one beyond last character
  (data (make-string 1024))			; buffer string read from input
  scan)						; plist (token kind ...) of scanned earlier

(defun alloc-tokenbuf ()
  #-debug (declare (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  ;mp::without-scheduling
  (dotimes (i (length *tokenbufs*) (make-tokenbuf))	; create new if no free buffers
    (declare (fixnum i))
    (when-let (elt (svref *tokenbufs* i))		; found in pool
      (setf (svref *tokenbufs* i) nil			; exclude from the pool
            (tokenbuf-cur elt) 0			; reinitialize and reuse
            (tokenbuf-max elt) 0)
      (return elt))))

(defun free-tokenbuf (buf)
  #-debug (declare (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (dotimes (i (length *tokenbufs*))
    (declare (fixnum i))
    (unless (svref *tokenbufs* i)			; if there is free cell in pool,
      (return (setf (svref *tokenbufs* i) buf)))))	; place there, otherwise toss away

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  NEXT-TOKEN  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bound to nil inside phtml-internal but changed to T after we have encoutnered
;;  <!DOCTYPE ...XHTML...>
(defvar *xhtml-mode*)

(defvar %collect-comments%)

(defun next-token (stream collect-data tokenbuf raw-mode-delimiter parse-entities)
                   ;read-sequence-func
 ;;; Helper
  ;; Args: collect-data
  ;;            If null, neither pcdata nor rawdata strings are collected.
  ;;       tokenbuf
  ;;		Current buffer used
  ;;       raw-mode-delimiter ::= NIL		- we are not in raw mode
  ;;		| ">" | "?>"			- we are in SGML/XML directive
  ;;		| "</STYLE>" | "</style>"	- we are within a stylesheet element
  ;;		| "</SCRIPT>" | "</script>"	- we are within a script element
  ;; UNUSED: read-sequence-func
  ;;		Used to fill the buffer, read-sequence by default.
  ;; Values: 1) Next token from the stream,
  ;; 	     2) Kind of token ::= :pcdata | :start-tag | :end-tag | :eof |
  ;;
  (declare (optimize (speed 3) (safety 1)))
  (let ((coll (alloc-collector))
        (line htt:*current-line*)				; beginning of token
        (col htt:*current-column*))				; for eof-error
   (labels
       ((%read-char ()
          ;; Buffered input a la (read-char stream nil nil))
          ;; Value: Character or NIL in case of end of file
          (let ((cur (tokenbuf-cur tokenbuf))
                (data (tokenbuf-data tokenbuf)))
            (when (>= cur (tokenbuf-max tokenbuf))		; fill buffer
              (setq cur (if (zerop (setf (tokenbuf-max tokenbuf)
                                         (read-sequence data stream)))
                            nil				; eof
                            0)))
            (when cur
              (let ((char (schar data cur)))
                (setf (tokenbuf-cur tokenbuf) (1+ cur))
                  (cond ((char= char #\Newline)
                         (setq htt:*current-column* 0
                               htt:*current-line* (1+ htt:*current-line*)))
                        (t (incf htt:*current-column*)))
                  char))))
			  
        (%unread-char (char)
          (declare (ignore char))				; place char back
          (when (< (decf htt:*current-column*) 0)		; last char was Newline
            (decf htt:*current-line*))
          (decf (tokenbuf-cur tokenbuf)))			; to the buffer

        (%char-to-case (char)
          (if *xhtml-mode*	;(eq excl:*current-case-mode* :CASE-INSENSITIVE-UPPER)
              char		;(char-downcase ch)
              (char-upcase char)))
	     
        (%coll-clear ()
          (setf (fill-pointer coll) 0))
        
        (%coll-push (char)
          (vector-push-extend char coll))

        (%coll-key ()			; create a keyword from the collector
          (intern-upcase (subseq coll 0 (fill-pointer coll))))

        (%coll-string ()			; create a string that's in the collector
          (subseq coll 0 (fill-pointer coll)))

        (%eof-error ()
          (eof-error :line line :col col
                     :start (let ((length (fill-pointer coll)))
                              (when (< 0 length)
                                (subseq coll 0 (min 30 length))))))

        (%parse-entity (&optional (max *max-entity-length*))
          ;; Read an entity ending at semicolon: &...; or &#...;
          ;; Value: (1) Character, string or list - success,
          ;;            NIL - failure.
          ;;        (2 - failure only)
          ;;            String with a fill pointer conaining all the accumulated chars,
          ;;	 	the string can end with #\; terminator if it has also been read.
          ;;
          ;; Q: Shall we parse within attibute value string representing an URI?
          ;; Q: Do we need this global?
          ;;  (defun (reader &key (max *max-entity-length*) (errorp t))
          ;; Args: reader Function of zero parameters.
          ;;              The starting #\& char is assumed to be read already.
          ;; Usage:
          ;;  (let ((string "&alpha;")
          ;;        (i 1))
          ;;    (with-syntax-error-location ()
          ;;      (parse-entity (lambda () (when (< i (length string))
          ;;                                 (prog1 (schar string i) (incf i)))))))
          #-debug (declare (optimize (speed 3) (safety 0)
                                     #+lispworks (hcl:fixnum-safety 0)))
          (let ((acc (make-array (1+ max)
                                 :element-type *default-char-type*	; own accumulator
                                 :fill-pointer 0)))
            (loop (let ((ch (%read-char))) ;(funcall reader)))
                    (cond ((null ch)
                           ;(when errorp
                           (syntax-error
                     #L"End of input stream after the HTML character reference marker  &~a"
                            acc))
                           ;(return-from %parse-entity (values nil acc)))
                          ((char= ch #\;)
                           (return))
                          ((zerop (decf max))
                           ;(when errorp
                           (syntax-error
                     #L"No semicolon found after HTML character reference starting  &~a..."
                            acc))
                           ;(return-from parse-entity (values nil acc)))
                          (t (vector-push ch acc)))))
            (if (and (< 0 (fill-pointer acc)) (char= (char acc 0) #\#))
                ;; Decimal or hexadecimal character reference
                (loop with code of-type fixnum = 0
                      and radix of-type fixnum = (if (and (< 1 (fill-pointer acc))
                                                          (char-equal (char acc 1) #\x))
                                                     16 10)
                      for i of-type fixnum upfrom (if (= radix 10) 1 2)
                      below (fill-pointer acc)
                      for ch = (char acc i)
                      for weight = (digit-char-p ch radix)
                      do (cond (weight
                                (setq code (+ (* radix code) weight)))
                               (t				; errorp
                                (syntax-cerror
                                 #1=#L"Replace the entire character reference by space."
                                 #L"Non ~:[decimal~;hexadecimal~] digit '~a'~
				in HTML numeric character reference  &~a;"
                                 (= radix 16) ch acc)
                                (return #\Space)))
                                ;(t #2=(vector-push #\; acc)
                                ;   (return #3=(values nil acc))))
                      finally (return (code-char code)))
                ;; Entity reference, may be empty
                (multiple-value-bind (code found) (gethash acc *html-entity-to-code*)
                  (cond ((integerp code)
                         (code-char code))
                        ((and found (or (listp code) (stringp code)))
                         code)					; element, possibly null
                        (t					; errorp
                         (syntax-cerror #1#
                                        #L"Unknown HTML character entity name  &~a;"
                                        acc)
                         #\Space))) )))
                        ;(t #2# #3#)
        )
    (let ((state (if raw-mode-delimiter 'rawdata 'pcdata))
	  (value-delim)
          (tag-to-return)			; tag keyword of elem being parsed
	  (attribs-to-return)
	  (end-tag)
          (attrib-name)				; attrib keyword of elem being parsed
	  (attrib-value)
	  (name-length 0)			; count only when it could be a comment
	  (raw-index 0)				; current index in raw-mode-delimiter
          (sgml-bailout nil)			; true after open <! or <? has encoutered
	  ch)
      (loop
      	(unless (setq ch (%read-char))
          (return))				; eof -- exit loop
      	(debug-format :phtml-token "ch ~s, state ~s" ch state)
	(case state
	  (pcdata
           ;; Collect everything until we see <
	   (cond ((char= ch #\<)
		  (if (< 0 (fill-pointer coll))			; have somestring collected
                      (progn (%unread-char ch)			; push '<' back and
                        (return))				; return the string
                      ;; We have collected nothing - this is really a tag
                      (setq state 'readtagfirst)))
                 ((and parse-entities (char= ch #\&))   	; read character reference 
                  (let ((entity (%parse-entity))) ;#'%read-char	; ending at semicolon
                    (cond ((characterp entity)
                           (%coll-push entity))
                          ((or (consp entity)			; non-null elem or text
                               (stringp entity))		; in *html-entity-to-code*
                           (push :entity (tokenbuf-scan tokenbuf))
                           (push entity (tokenbuf-scan tokenbuf)) ; to be lexed later
                           (setq state 'entity)			; pcdata is going on
                           (return)) )))
                 ((not (char= ch #\Return))	; we will check for & here eventually
                  (%coll-push ch)) ))
	
	  (readtagfirst
	   ;; Starting to read a tag name
	   (cond ((char= ch #\/)				; end tag
                  (setq end-tag t))
                 (t (when (or (char= ch #\!) (char= ch #\?))	; possible comment
		      (setq sgml-bailout t
                            name-length 0))
                    (%unread-char ch)))
	   (setq state 'readtag))
	
	  (readtag
	   ;; Reading the whole tag name
	   (cond ((character-istic ch +tag-bit+)
                  (%coll-push (%char-to-case ch))
                  (incf name-length)
                  (when (and (eql name-length 3) (string= coll "!--"))
                    (%coll-clear)
                    (setq state 'readcomment)))
                 (t
                  (setq tag-to-return (%coll-key))
                  (%coll-clear)
                  (cond ((char= ch #\>)
                         (return))			; we're done
                        (sgml-bailout
                         (%unread-char ch)
                         (return))
                        ((eq tag-to-return :!--)	; comment - DI: very unlikely
                         (setq state 'readcomment))
                        (t (%unread-char ch)
                           (setq state 'findattribname)) )) ))
	
	  (findattribname
	   ;; Search until we find the start of an attribute name or the end of the tag
	   (cond ((char= ch #\>)			; end of the line
                  (return))
                 ((char= ch #\=)			; value for previous attribute name
		  ;; NB: Syntax  "foo = bar" is bogus I think but it's
		  ;;     used some places, here is where we handle this
                  (pop attribs-to-return)
                  (setq attrib-name (pop attribs-to-return)
                        state 'findvalue))
                 ((character-istic ch +attrib-name-bit+)
                  (%unread-char ch)
                  (setq state 'attribname))))
                 ; ignore other things
	  
	  (findvalue
	   ;; Find the start of the value
	   (cond ((character-istic ch +whitespace-bit+))	; keep looking
                 ((char= ch #\>)				; no value
                  ;; Assign the value to be the name as a string
                  (setq attrib-value (string-downcase (string attrib-name)))
                  (push attrib-name attribs-to-return)
                  (push attrib-value attribs-to-return)
                  (%unread-char ch)
                  (setq state 'findattribname))
                 (t (%unread-char ch)
		    (setq state 'attribstartvalue))))
	   
	  (attribname
	   ;; Collect attribute name
	   (cond ((character-istic ch +attrib-name-bit+)
                  (%coll-push (%char-to-case ch)))
                 ((eq #\= ch)					; end of attribute name,
		   (setq attrib-name (%coll-key))		; value is next
		   (%coll-clear)
		   (setq state 'attribstartvalue))
                 (t						; end of attribute name
		   (setq attrib-name (%coll-key))		; with no value
		   (%coll-clear)
		   (setq attrib-value (string-downcase (string attrib-name)))
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (%unread-char ch)
		   (setq state 'findattribname))))
	
	  (attribstartvalue
	   ;; Begin to collect value while skipping spaces:
           ;; assume since we've seen a '=' there really is a value
	   (cond ((or (char= ch #\") (char= ch #\'))
                  (setq value-delim ch
                        state 'attribvaluedelim))
                 ((char/= ch #\Space)				; attribute value
                  (%unread-char ch)				; is not in quotes
                  (setq state 'attribvaluenodelim))))
	
	  (attribvaluedelim
	   ;; An attribute value is delimited by the value-delim
	   (cond ((char= ch value-delim)
                  (setq attrib-value (%coll-string))
                  (%coll-clear)
                  (push attrib-name attribs-to-return)
                  (push attrib-value attribs-to-return)
                  (setq state 'findattribname))
                 ((and parse-entities (char= ch #\&)
                       (not (html::uri-attribute-p tag-to-return attrib-name)))
                  (let ((entity (%parse-entity)))   		; read character reference 
                    (cond ((characterp entity)
                           (%coll-push entity))
                          ((stringp entity)
                           (loop for char across entity
                                 do (%coll-push char)))
                          (entity				; NIL is completely ignored
                           (syntax-cerror #L"Completely ignore the (pseudo-)entity."
                            #L"In attrubute value, wrong pseudo-entity substituion ~s."
                            entity)) )))
                 ((and (char= ch #\\)				; borrowed from
                       *xhtml-mode*				; pxmlutils (bese.it)
                       (null (setq ch (%read-char))))
                  (return))
                 (t (%coll-push ch))))
	
	  (attribvaluenodelim
	   ;; An attribute value not delimited by ' or " and thus restricted
	   ;; in the possible characters
	   (cond ((character-istic ch +attrib-value-bit+)
                  (%coll-push ch))
                 (t (%unread-char ch)
                    (setq attrib-value (%coll-string))
                    (%coll-clear)
                    (push attrib-name attribs-to-return)
                    (push attrib-value attribs-to-return)
                    (setq state 'findattribname))))
	  
	  (readcomment				; a comment ends on the first --, but we'll
	   (if (eq ch #\-)			; look for --> since that's what expected
	       (setq state 'readcomment1)
	       (%coll-push ch)))
	  
	  (readcomment1				; seen one -, looking for ->
	   (cond ((eq ch #\-) (setq state 'readcomment2))
                 (t (%coll-push #\-)	; not a comment end, put back the -'s
                    (%coll-push ch)
                    (setq state 'readcomment))))
	  
	  (readcomment2
	   ;; Seen two -'s, looking for >
	   (cond ((eq ch #\>)			; end of the line
                  (return))
                 ((eq ch #\-)			; still at two -'s, have to put out first
                  (%coll-push #\-))
                 (t
                  (%coll-push #\-)	; put out two hypens 
                  (%coll-push #\-)
                  (setq state 'readcomment))))	; and go back to looking for a hypen
	  
	  (rawdata
	   ;; Collect everything until we match the entire delimiter
	   (cond ((and raw-mode-delimiter
                       (char= (%char-to-case ch) (schar raw-mode-delimiter raw-index)))
                  ;; Stream char matches the current char in the delimiter
                  (let ((length (length raw-mode-delimiter)))
                    (when (= (incf raw-index) length)		 ; have matched the entire
                      (when (< 2 length)			 ; not SGML stuff
                        (push :end-tag (tokenbuf-scan tokenbuf)) ; for :end-tag to be lexed
                        (cond ((or (string= raw-mode-delimiter "</STYLE>")
                                   (string= raw-mode-delimiter "</style>"))
                               (push :style (tokenbuf-scan tokenbuf)))
                              ((or (string= raw-mode-delimiter "</SCRIPT>")
                                   (string= raw-mode-delimiter "</script>"))
                               (push :script (tokenbuf-scan tokenbuf)))
                              (t (error 			; not a syntax error
                                  "Internal PHTML error: unexpected raw-mode-delimiter ~s"
                                  raw-mode-delimiter)) ))
                      (return))))				; return :pcdata to caller
                  (t						; matches only partially
		   (dotimes (i raw-index)			; - push substring
		     (%coll-push (schar raw-mode-delimiter i)))	; into collector
		   (setq raw-index 0)
		   (%coll-push ch)) ))
	  ))
            
      ;; Loop ends - return a value depending on state
      (case state
	((pcdata rawdata entity)				; convert collector string
	 (if (zerop (fill-pointer coll))
	     (values nil
                     (if (eq state 'pcdata) :eof :pcdata))
             (values (prog1 (when collect-data (%coll-string))
                       (free-collector coll))
                     :pcdata)))
	
	(readtag
	 (unless tag-to-return
           (%eof-error))
	 (free-collector coll)					; tag without attributes
	 (values tag-to-return
		 (cond (end-tag      :end-tag)
                       (sgml-bailout :sgml)		; tag ::= :!doctype | :xml | :php
                       (t            :start-tag))))
	
	(findattribname						; tag with possible attrs
	 (free-collector coll)
	 (cond (end-tag						; ignore any attributes
                (values tag-to-return
                        :end-tag))
               (attribs-to-return
                (values (cons tag-to-return (nreverse attribs-to-return))
                        :start-tag))
               (t (values tag-to-return
                          :start-tag))))
	
	(readcomment2						; comment
	 (values (prog1 (when %collect-comments% (%coll-string))
		   (free-collector coll))
		 :comment))
	
	(otherwise
	 (if ch
	     (error "Internal PHTML error: cannot be in state ~d." state)
             (%eof-error))) )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PHTML-INTERNAL  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun phtml-internal (stream
                       callbacks collect-data
		       parse-entities retain-whitespace
                       unknown-tag-action)
                       ;&optional (read-sequence-func #'read-sequence))
 ;;; Args: callbacks	Alist ((keyword . callback-function) ...)
  ;; 			The callback-function signature: elem -> no values.
  ;;       collect-data
  ;;		t          Generate a complete LHTML output.
  ;;		:callbacks Strings are only collected for the elements mentioned
  ;;			   in the callbacks alist.
  ;;            nil	   Only LHTML document tree without pcdata, rawdata or comments
  ;;			   is collected.
  ;;	   parse-entities
  ;;			If true, then entities are converted to the character they name.
  ;;			E.g. &lt; -> '<'.
  ;;                    CAUTION: True implies that the resulting LHTML is not safe!
  ;;	   retain-whitespace
  ;;		t        Whitespace is preserved everywhere in full,
  ;;		:newline Replace #\Newline with :newline keyword and
  ;;		         collapse or eliminate some space around it,
  ;;		nil      Neither linebreaks nor other whitespace survive, except for
  ;;			 - raw-mode elements: script, !DOCTYPE, etc.
  ;;			 - inside the scope of elements with tag-retain-whitespace.
  ;;       unknown-tag-action
  ;;		:collect Return the list of such tags as the second value (see below).
  ;;		:warn	 Issue a warning.
  ;;		yl:logg	 Print into the logging stream.
  ;;		nil      Silently do nothing.
  ;; Value: (1) LHTML output.
  ;;	    (2) List of non-standard tags closed by the end of the stream 
  ;;            i.e. for which no explicit end tag was encountered.
  ;;	        (Only returned when collect-rogue-tags is true).
  ;;            Q: As we consider all non-standatd tags to be no-end,
  ;;               is it possible to collect anything at all?
  (declare (optimize (speed 3) (safety 1)))
  (macrolet ((tag-callback (tag)
               `(cdr-assoq ,tag callbacks)))
   (let ((*xhtml-mode* nil)		; have encountered XHTML in <!DOCTYPE ...>
         (collect-callbacks (eq collect-data :callbacks))
 	 (callback-tags ())		; stack of current + pending tags with callbacks
         (%collect-comments% (or (eq collect-data t)
                                 (and (eq collect-data :callbacks)
                                      (tag-callback :comment))))
         (tokenbuf (alloc-tokenbuf))
         (raw-mode-delimiter nil)	; NIL or terminating string: ">" "?>" etc.
         (last-start-tag :start-parse)	; last start tag seen at input stream
         (current-tag    :start-parse)	; tag of the element above the stack
         (current-cont ())		; content of the element above the stack
         (pending ())			; stack of unfinished elements
         (pending-ch-tags ())		; additional stack of character format tags
         ;; List of char tags accumulated during the last call %close-off -> %close-current
         (pending-ch-tags-just-closed ())
         (unknown-tags ()))		; only for :unknown-tag-action :collect
    (with-syntax-error-location (stream)
     (labels ((%save-state (tag &optional key (callback nil supplied-p))
                ;; Push current tag state into stack since we're starting a new open tag
                ;; Args: key Either the keyword of the tag
                ;;           or NIL - means "do not push into the callback-tags"
                (setq pending (acons (shiftf current-tag tag) (shiftf current-cont ())
                                     pending))
                (when (and key collect-callbacks
                           (if supplied-p callback (tag-callback key)))
                  (push key callback-tags))
                (debug-format :phtml "state saved, pending ~s" pending))

              (%cont-push (elem &optional (callback (tag-callback (html:elem-key elem)))
                                          (op t)
                                          (parent-tag current-tag)
                                          (cont current-cont))
                ;; First, if a callback associated with the element elem, invoke it.
                ;; Then, push the elem as is or modified by the callback into the cont
                ;; and make the whole new content assigned to current-cont.
                ;; Args: op  T - the default means cons
                ;;           NIL - passed only to skip comments.
                ;; To skip the elem, the callback must return: (1) nil, (2) :append
                ;; This function invoked after parsing
                ;; - the start tag token for the elements having no end and comments,
                ;; - the end tag token for the others, i.e. on closing-off.
                (when callback
                  (let ((*callback-parent-tag* parent-tag))
                    (multiple-value-bind (elt changed) (funcall callback elem)
                      (when changed				; return (1)nil,(2):append
                        (setq elem elt				; to add nothing
                              op changed)))))
                (setq current-cont (cond ((eq op :append) (revappend elem cont))
                                         (op (cons elem cont))
                                         (t cont))))

              (%close-current ()
                ;; Close current-tag and current-cont, i.e. restore state:
                ;; - crate an element from them,
                ;; - move one level up along the document tree to a pending parent,
                ;; - cons the newly created element to the content of the parent.
                ;; Value: The tag of the parent assigned to current-tag.
                (let* ((key (first-or-self current-tag))
                       (character-tag (memq key *character-tags*))
                       (cont (if (tag-no-pcdata key)
                                 (let ((acc ()))		; omit all the strings
                                  (dolist (elt current-cont)
                                    (unless (stringp elt)
                                      (push elt acc)))
                                  acc)
                                 (nreverse current-cont)))
                       (elem (cons current-tag cont))
                       (callback (tag-callback key))
                       (parent (pop pending))			; ::= (tag . cont)
                       (parent-tag (car parent))
                       (parent-cont (cdr parent)))
                  (when character-tag				; just close the char tag
                    (push current-tag pending-ch-tags-just-closed))
                  (when (and callback (eq key (first callback-tags)))
                    (setq callback-tags (rest callback-tags)))	; for auto-closed too
                  ;; Exclude char tags elements with empty content (shall we?)
                  (if (or cont (not character-tag))
                      (%cont-push elem callback t parent-tag parent-cont)
                      (setq current-cont parent-cont))
                  (debug-format :phtml "closed elem ~s, new current ~s,~%~tcontent ~s"
                                elem parent-tag current-cont)
                  (setq current-tag parent-tag)))		; restore state
	   
              (%close-off (keys &optional parents end) ; collect-rogues)
                ;; Close off tags in stack that are in keys,
                ;; but no further than any of the parent tag in stop-at
                ;; Args: keys      Either a list of one explicit end tag keyword
                ;;		  or a list of sibling keywords of some start tag.
                ;;       parents   List of parents or NIL we are unwinding up to.
                ;;       end       True = one level, i.e. call %close-current only once
                ;;                 True is passed only on explicit-inverse-tag
                ;;		  or on terminating SGML-bailout.
                (debug-format :phtml "close off keys ~s, parents ~s, end ~s, current ~s"
                              keys parents end current-tag)
                (let ((key (first-or-self current-tag))		; keyword of current-tag
                      count)					; number of levels
                  (cond ((memq key keys)
			 ;; Close the current tag and possibly other pending
                         (loop (setq key (first-or-self (%close-current)))
                               (when (or end			; explicit end tag
                                         (memq key *character-tags*)
                                         (not (memq key keys)))
                                 (return))))
                        ((memq key parents))			; already unwound
                        ((setq count			        ; search upward for a 
                               (loop for parent in pending	; parent key within either
                                     and i upfrom 1		; the keys or parents
                                     for parent-key = (first-or-self (car parent))
                                     if (memq parent-key parents) return i
                                     else if (memq parent-key keys) return (1+ i)))
                         ;; Unwind until the nearest parent gets assigned to current-tag
                         (debug-format :phtml "restore ~s levels" count)
                         (loop repeat count
                               do (setq key (first-or-self (%close-current)))))
                        (end					; explicit end tag seen
                         (syntax-error #L"Start tag is not found for </~a>"
                                       (html:tag-name-string end))) )))

              (%scan-inline (key)
                ;; Scan forward for the end tag corresponding key.
                ;; We do not actually eat any tokens in here but
                ;; unwind all charater formatting tags made outside this scan.
                ;; Args: key Start tag of an inline element with content, but not character
                (let ((scanned-ch-tags ())
                      (old (tokenbuf-scan tokenbuf))
                      (new ())		; tokens and kinds collected in reverse order
                      token kind)
                  (loop (if old
                            (setq token (pop old)
                                  kind (pop old))
                            (progn (multiple-value-setq (token kind) (%next-token t))
                              (push token new)
                              (push kind new)))
                        (cond ((or (eq kind :eof)
                                   (and (eq token key) (eq kind :end-tag)))
                               (when new
                                 (nconcf (tokenbuf-scan tokenbuf) (nreverse new)))
                               (return))
                              ((memq token *character-tags*)
                               (cond ((eq kind :start-tag)
                                      (push token scanned-ch-tags))
                                     ((memq token scanned-ch-tags)
                                      #+debug (assert (eq kind :end-tag))
                                      ;; Start tag was seen within this scan
                                     (deletef scanned-ch-tags token :count 1))
                                     (t ;; Start tag was saved before this scan
                                      (%close-off (list token))) ))) )))
		 
               (%known-tag-p (key kind &optional (desc (gethash key
                                                                html:*desc-hash-table*)))
                 (if (and desc (not (html::desc-special desc))) ; = (tag-standard-p key)
                     t
                     (case unknown-tag-action
                       ((:warn yl:logg)
                        (syntax-log-warn unknown-tag-action
                                         #2=#L"Unknown HTML tag <~:[~;/~]~a>"
                                         #3=(eq kind :end-tag) key))
                       (:collect
                        (pushnew key unknown-tags)
                        nil)
                       (otherwise
                        (when unknown-tag-action
                          (syntax-cerror #L"Skip the tag." #2# #3# key))))))
       
              (%next-token (force)
                ;; Values: (1) token ::= key | (key ...) | string | NIL (for :pcdata only)|
                ;;         (2) kind
                (let (buf)
                  (if (or force (null (setq buf (tokenbuf-scan tokenbuf))))
                      (next-token stream      ; maybe (tag-callback current-tag) is enough?
                                  (or (eq collect-data t) callback-tags)
                                  tokenbuf raw-mode-delimiter parse-entities)
                      (progn (setf (tokenbuf-scan tokenbuf) (cddr buf))
                        (values (first buf) (second buf))))))
             )
      (loop
	(multiple-value-bind (token kind) (%next-token nil)
	  (debug-format :phtml "token ~s, kind ~s, last-start ~s, current ~s~%~tpending ~s"
                        token kind last-start-tag current-tag pending)
	  (case kind
	    (:pcdata
	     ;(when (or (not callback-only) callback-tags)
               ;; Suppress extra whitespace:
               ;; - even when the starting tag is inline but end is missing,
               ;; - even after not inline.
               ;; NB1: Cannot collapse the embedded whitespace if some (grand)parent
               ;;      is preformatted (has tag-retain-whitespace)
               ;;      e.g. within <pre>...<a>aaa</a>... </pre>
               ;; Q1:  Is it possible that pending is not the same as a parent list?
               ;;      we can collapse and following preformating by mistake,
               ;; NB2: Cannot remove at all if the last-start-tag is inline.
               ;; NB3: current-tag can be just a reopened char tag
               ;;      so can differ from the last-start-tag seen.
	       (let ((key (first-or-self last-start-tag)))
	         (cond ((null token))				; empty content allowed
                       ((or raw-mode-delimiter
                            (and retain-whitespace
                                 (not (eq retain-whitespace :newline)))
                            ;(memq key *inline-tags*)
                            (tag-retain-whitespace (first-or-self current-tag))
                            (dolist (parent pending nil)
                              (when (tag-retain-whitespace (first-or-self (car parent)))
                                (return t))))
                        (push token current-cont))		; push as is
                       ((eq retain-whitespace :newline)	; #\Newline -> :newline
                        (do* ((rest (split-seq #\Newline token :keep-empty-subseqs t)
                                    (rest rest))
                              (prev nil t)
                              (nullable (or (rest rest) (not (memq key *inline-tags*)))
                                        t))			; true after first newline
                             ((null rest))
                          (when prev
                            (push :newline current-cont))
                          (when (setq token (collapse-whitespace (first rest) nullable))
                            (push token current-cont))))
                       ((setq token (collapse-whitespace
                                     token (not (memq key *inline-tags*))))
                        (push token current-cont))))
	       ;; Special treatment of SGML bailout, i.e. ">" (SGML) or "?>"
               (when (<= 1 (length raw-mode-delimiter) 2)
                 (case last-start-tag
                   (:!doctype
                    (when (search "XHTML" token)
                      (debug-format :phtml "switched to XHTML mode")
                      (setq *xhtml-mode* t))))
                 (%close-off (list last-start-tag) () t)) ;)	; tag-parents is ()
	     (setq raw-mode-delimiter nil))
	    
	    (:sgml
	     ;; token ::= :!<something> or :?<something>
	     (let ((key (first-or-self (setq last-start-tag token))))
               (setq raw-mode-delimiter (case key
                                          ((:?xml :?php) "?>")
                                          (otherwise ">")))	; <!DOCTYPE ...>
	       (%save-state token key)))

	    (:start-tag
	     (let* ((key (first-or-self (setq last-start-tag token)))
                    (desc (gethash key html:*desc-hash-table*))
                    (no-end (if desc (not (html::desc-has-end desc)) t))
                    ;(no-end (or (tag-no-end key) (memq key no-body-tags)))
                    (callback (tag-callback key)))
               (when (case key
                       (:style
                        (setq raw-mode-delimiter (if *xhtml-mode* "</style>" "</STYLE>")))
                       (:script
                        (setq raw-mode-delimiter(if *xhtml-mode* "</script>" "</SCRIPT>")))
                       (otherwise
                        (%known-tag-p key kind desc)))
                 ;; Really walk through the tag
                 (when-bind (siblings (tag-siblings key))	; key is also an ending 
                   (%close-off siblings (tag-parents key)))	; tag for its sibling
		 (cond ((not pending-ch-tags))			; no char tags - no bother
                       (no-end)					; no content
                       ((memq key *character-tags*))
                       ((memq key *inline-tags*)		; inline with content
                        ;; Close off only the char tags that are within
                        ;; this inline element <key>...</key>
                        (%scan-inline key))
                       (t					; block with content
                        ;; Close ALL pending char tags and then reopen below
                        (dolist (tag (reverse pending-ch-tags))
                          (%close-off (list (first-or-self tag))))) )
                 (if no-end
                     ;; Element with no content - concatente it to the current-cont
                     (%cont-push (if (atom token) token (list token)) callback)
		     ;; An end tag expected - recurse one level down
                     (%save-state token key callback))
		 (if (memq key *character-tags*)
                     (push token pending-ch-tags)
		     (case key
                       ((:style :script))
                       (otherwise				; reopen just closed
                        (dolist (tag (reverse pending-ch-tags-just-closed)) ; char tags
                          (%save-state tag tag)))))
		 (case key
                   ((:style :script))
                   (otherwise
                    (setq pending-ch-tags-just-closed ()))) )))
	  
	    (:end-tag
	     (setq raw-mode-delimiter nil)
             (let ((key (first-or-self current-tag)))
               (when (%known-tag-p token kind)
                 (unless (eq token key) 
                   (syntax-log-warn *end-tag-mismatch-action*
                 #L"Unexpected </~a> mismathes the current tag <~a>~@[ (missing </~a>)~]."
                    token key (if (eq key :start-tag) nil key)))
	         (%close-off (list token) () token)
                 (when (memq token *character-tags*)
                   (removef pending-ch-tags token :count 1
                            :key #'first-or-self)
                   (removef pending-ch-tags-just-closed token :count 1
                            :key #'first-or-self))
                 (dolist (tag (reverse pending-ch-tags-just-closed)) ; reopen just closed
                   (%save-state tag tag))			     ; char format tags
                 (setq pending-ch-tags-just-closed ()))))

	    (:comment
	     (setq raw-mode-delimiter nil)			; = (make-elem :comment ()
             (%cont-push (cons :comment (listify token))	;     (list token))
                         (tag-callback :comment)
                         %collect-comments%)) ;(or (eq collect-data t) callback-tags)
	    (:entity						; pseudo-entity
             ;; BAD: Whitespace get collapsed when one pseudo-entity is followed by another
             (%cont-push (list :entity token)))

	    (:eof
	     (setq raw-mode-delimiter nil)
             (%close-off '(:start-parse) ())
	     (free-tokenbuf tokenbuf)
             (return (values (cdar current-cont) unknown-tags))) )))))))) ;rogue-tags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  WHITESPACE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NB: In order for HTMLGen to invoke emit-safe while printing LHTML,
;;;	we suggest expanding it by the html:*lhtml-safe* control variable.

(defun whitespace-string-p (val)
  (and (stringp val)
       (loop for ch across val always (character-istic ch +whitespace-bit+))))

(defun collapse-whitespace (val &optional (nullable t))
 ;;; Args: nullable  When true, return nil if val consists only of whitespace.
  ;; Value: "simple string" or nil.
  ;; NB: We must not trim white spaces from left and right as HTMLGen does not
  ;;	 separate items by spaces itself.
  ;; Q: Shall we replace #\Return and #\Tab by #\Space?
  (declare (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0))
           (type simple-string val))
  (do ((length (length val))
       (result ())
       (count 0)				; number of chars in result
       (changed nil)				; is result somehow different form val?
       (last nil)				; was previous char a whitespace?
       (ch #\Null)
       (i 0 (1+ i)))
      ((>= i length)
       (cond ((null result)			; empty string
              (if nullable nil val))
             ((and (= count 1) last nullable)	; collapsed to a single space
              nil)
             ((not changed)			; nothing was collapsed
              val)
             (t (make-array count :element-type 'character
                            :initial-contents (nreverse result)))))
    (declare (fixnum length count i) (type character char)
             (dynamic-extent result))
    (cond ((character-istic (setq ch (schar val i)) +whitespace-bit+)
           (if last
               (setq changed t)
               (progn
                 (push #\Space result)
                 (incf count)
                 (or changed (char= ch #\Space) (setq changed t))))
           (setq last t))
          (t
           (push ch result)
           (incf count)
           (setq last nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TOP-LEVEL API  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod parse-html ((stream stream) &key callbacks (collect-data t)
                                            (parse-entities (not html:*lhtml-safe*))
					    (retain-whitespace *retain-whitespace*)
                                            (unknown-tag-action html:*unknown-tag-action*))
  (phtml-internal stream callbacks collect-data
                  parse-entities retain-whitespace
                  unknown-tag-action))

(defmethod parse-html (file &key callbacks (collect-data t)
                                 (parse-entities (not html:*lhtml-safe*))
                                 (retain-whitespace *retain-whitespace*)
                                 (external-format html:*html-external-format*)
                                 (element-type (yl:ef-type external-format))
                                 (unknown-tag-action html:*unknown-tag-action*))
  (with-open-file (stream file :direction :input
                               :element-type element-type 
                               :external-format external-format)
    (parse-html stream :callbacks callbacks
                :collect-data collect-data
		:parse-entities parse-entities
                :retain-whitespace retain-whitespace	     
                :unknown-tag-action unknown-tag-action)))
	     
(defmethod parse-html ((str string) &key callbacks (collect-data t)
                                         (parse-entities (not html:*lhtml-safe*))
                                         (retain-whitespace *retain-whitespace*)
                                         (unknown-tag-action html:*unknown-tag-action*))
  (phtml-internal (make-string-input-stream str)
                  callbacks collect-data
                  parse-entities retain-whitespace
                  unknown-tag-action))

(provide :phtml)