;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;; Part of HTMLGen and PHTML, copyright (c) 1986-2000 Franz Inc, Berkeley, CA
;;; copyright (c) 2003 Kevin Rosenberg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generator core
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by 
;;; the Free Software Foundation, as clarified by the LLGPL
;;;
;;; KMR changes from Allegro version:
;;;   - Support XHTML end tags
;;;	DI: See parameter *html-mode*.
;;;   - Lowercase symbol names for attributes.
;;;     DI: See parameter *attribute-case*;
;;;         -- element tags are always lowercased,
;;;         -- the parameter also affects html-standard-print.
;;;   - Added custom tags such as :jscript, :insert-file, :nbsp.
;;;	DI: :nbsp and :jscript were preserved as was;
;;;	    :load-file and :insert-file replaced by a more general :include pseudo-tag.
;;;   - Removed if* macro
;;;	DI: Handling :if* in attributes is preserved but is deprecated if favor :when.
;;;   - Added attribute conditions
;;;	DI: Only :optional is of value, but we use attr (:optional form) everywhere.
;;;	    The others are equivalent to corresponding Lisp forms.
;;;   - Added conversion attribute values to strings (quoting).
;;;	DI: When *html-mode* is :sgml, only values that are symbols are quoted,
;;;	    but numbers are not.
;;;   - Converted some comments to function doc strings
;;;
;;; DI changes and additions
;;;   * Renamed html macro to htm (a la CL-WHO).
;;;   * Renamed html-process to descriptor:
;;;	-- it is of a normal structure type now, not of type list;
;;;	-- slot name-attrs renamed to name-attr;
;;;	-- added slot uri-attrs specifying a list of attributes of type URI.
;;;   * Function prin1-safe-http-string replaced by generic print-attribute
;;;	of signature: (key name value &optional uri-p (stream *html-stream*))
;;;   * Added special variable *unknown-tag-action*.
;;;     Parameter unknown removed from all the functions concerned.
;;;   * Changed html-print-list and html-print signature:
;;;	  (form &optional stream)
;;;   * def-std-html now defines a macro named as follows:
;;;	** a symbol explicitly passed as an optional macro-name argument, or
;;;	** (intern "key")  instead of  (intern "WITH-HTML-key") by default.
;;;   * Owing to calls of attribute-name-string and print-attribute, 
;;;	html-standard-print generates output close to what is produced by
;;;     our htm macro now.
;;;   * Changes in html-standard-print:
;;;	-- If the value of an attribute is NIL, the attribute is not printed at all.
;;;	-- The following list is supported as an attribute value:
;;;		(:optional form1 [form2]...)
;;;	   so that the overall attribute value string is computed by means of
;;;        concatenating all the strings resulted from the calls
;;;		(html-template:template-eval form).
;;;   + The tag :script is defined as custom but with "standard" and "has end" meaning.
;;;   + Added custom tags :format, :format-safe.
;;;   + Added custom tag :escape, which invokes full-fledged html-template:escape-string.
;;;	It escapes not only four characters, but all satisfying
;;;     html-template:*escape-char-p*.
;;;   +	Added XHTML termination "/>" based on *html-mode* to print-html-... functions.
;;;   +	Added custom :?php tag.
;;;     Parsed similarly to the comment element.
;;;	In attributes, PHP code is printed correctly only when
;;;	-- *html-mode* is :xml and
;;;	-- the code is a whole attribute value.

(in-package :html)

;;; Common helpers

(defun emit-safe (stream string)
  ;; send the string to the http response stream watching out for
  ;; special html characters and encoding them appropriately
  (do* ((i 0 (1+ i))
	(start i)
	(end (length string)))
       ((>= i end)
        (when (< start i)
	  (write-html-string string stream :start start :end i)))
    (let* ((ch (char string i))
           (name (gethash (char-code ch) *html-code-to-entity*)))
      (when (orf name (and (eq *html-mode* :xml) (char= ch #\') "#x27"))	;#039
	;; Entity name found, emit previous chars first
	(when (< start i)
	  (write-html-string string stream :start start :end i))
        (write-char #\& stream)
        (write-string name stream)			; no char external codes needed
        (write-char #\; stream)
        (setq start (1+ i))))))

(defun princ-http (val)
  ;; print the given value to the http stream using ~a
  (when val (write-html-string (princ-to-string val))))

(defun prin1-http (val)
  ;; print the given value to the http stream using ~s
  (when val (write-html-string (prin1-to-string val))))

(defun princ-safe-http (val)
  (when val (emit-safe *html-stream* (princ-to-string val))))

(defun prin1-safe-http (val)
  (when val (emit-safe *html-stream* (prin1-to-string val))))


(declaim (inline tag-name-string))
(defun tag-name-string (key)
 ;; Tag printnames are always lowercased in YstokHTML. We cash them to reduce consing.
  (orf (get key 'string-downcase) (string-downcase key)))

(defun attribute-name-string (name)
  (etypecase name
    (symbol
     (ecase (if (eq *html-mode* :xml) :downcase *attribute-case*)
       (:downcase (orf (get name 'string-downcase) (string-downcase name)))
       (:upcase   (orf (get name 'string-upcase) (string-upcase name)))
       (:preserve (symbol-name name))))
    (string name)))

(defgeneric print-attribute (key name value &optional uri-p stream)
 ;;; Output name-string=safe-value-string preceeded by one space into the stream.
  ;; Args: key   Element tab keyword
  ;;       name  Attribute name, a keyword preferred.
  ;;       value Any Lisp object
  ;;       uri-p If true, the attribute is of "URI" type.
 (:method :before (key name value &optional uri-p (stream *html-stream*))
  (declare (ignore key name uri-p))
  (when value
    (write-char #\Space stream)))

 (:method (key name value &optional uri-p (stream *html-stream*))
  ;; Default method: surround the value by double quotes, which are optional in SGML.
  #1=(declare (ignore key uri-p))
  (let ((x (eq *html-mode* :xml)))
    #2=(write-string (attribute-name-string name) stream)
    (write-char #\= stream)
    (when x #4=(write-char #\" stream))
    (emit-safe stream (prin1-to-string value))		; a la (prin1-safe-http val)
    (when x #4#)))

 (:method (key name (value string) &optional uri-p (stream *html-stream*))
  #1#
  #2#
  #3=(write-string "=\"" stream)
  (if (and (eq *html-mode* :xml)
           (string= value "<?php" :end1 (min (length value) #.(length "<?php"))))
      (write-html-string value stream)			; output verbatim
      (emit-safe stream value))
  #4#)

 (:method (key name (value symbol) &optional uri-p (stream *html-stream*))
  ;; Symbol is turned into its name except a boolean attribut case.
  ;; If the value is eq to T or its print name is empty, it represents boolean true
  ;; and we are printing only the attribute name is SGML mode.
  ;; Thus || differs from "".
  ;; NB: Check for null here as the class symbol preceeds cons.
  #1#
  (cond ((null value))					; false - ignore the attribute
        ((and (not (eq value t))			; usual (not boolean true)
              (plusp (length (symbol-name value))))
         #2# #3#
         (emit-safe stream (if (eq *html-mode* :xml)
                               (string-downcase (symbol-name value))
                               (symbol-name value)))
         #4#)
        ((eq *html-mode* :xml)				; boolean true
         #2# #3# #2# #4#)				; value coinsiding with name
        (t
         #2#)))						; attribute name alone
         
 (:method (key name (value character) &optional uri-p (stream *html-stream*))
  #1# #2# #3#
  (emit-safe stream (string value))
  #4#)

 (:method (key name (list cons) &optional uri-p (stream *html-stream*))
  #1# #2# #3#
  (dolist (val list)					; concatenate all elemenents
    ;(when val						; excluding NILs?
    (emit-safe stream (princ-to-string val)))
  #4#))

;; Action invoked during prining when a tag keyword has no element descriptor associated.
;; One of:
;;   T        - signal unknown-tag error,
;;   :warn    - issue a warning and ignore the element,
;;   yl:logg  - write to the logging stream and ignore the element,
;;   NIL      - ignore the element form.
;;   otherwise- function of signature (form stream) - call it.
(defvar *unknown-tag-action* t)

(defun signal-unknown-tag (key &optional form (action t))
  (case action
    (:warn
     (warn #1=#L"Unknown HTML tag keyword ~s~@[ in form ~s~]" key form))
    (yl:logg
     (logg '(:html-template . warn) #1# key form))
    (otherwise
     (error #1# key form))))

;;;;;;;;;;;;;;;;;;;;;;;;  MACROGENERATION of EMBEDDED HTML  ;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-attributes (desc args)
 ;;; For LHTML consistency, we accept
 ;;	... attr (:optional expr [expr2]...) ...
  ;; Though, the macro expansions of the two following forms are equivalent:
  ;;	... attr (:optional expr) ...
  ;;	... attr expr ...
  ;; CAUTION: Use a keyword as an attribute name, not an expression, in order
  ;;          for true value of uri-p to be passed to print-attribute correctly.
  ;;
  ;; Compatibitly NB:
  ;; The following KMR's special attribute expressions are not supported.
  ;; (1) ... {:fformat | :format | :if} (attr . args) ...
  ;;     == ... attr ({format nil | if} . args) ...
  ;; (2) ... :optional (attr expr) ...
  ;;     == ... :when expr attr expr ...		; though this evaluates expr twice
  ;; (3) ... :when (attr test value) ...
  ;;     == ... :when test attr expr ...
  (do* ((key (desc-key desc))
        (uri-attrs (desc-uri-attrs desc))
        (rest args (cddr rest))
        (res ())
        (name (first rest) (first rest))
        (value (second rest) (second rest)))
       ((null rest)
        (nreverse res))
      (cond ((memq name '(:when :if*))
             (push `(when ,value
                      (print-attribute ,key ,(third rest) ,(fourth rest)
                                       ,(first (memq (third rest) uri-attrs))))
                   res)
             (setq rest (cddr rest)))
            ((and (consp value) (eq (first value) :optional))
             (push `(print-attribute ,key
                                     ,name
                                     ,(if (cddr value)		; collect non-null
                                          (with-gensyms (list)
                                            `(let ((,list (remove nil
                                                                  (list ,@(rest value)))))
                                               (if (rest ,list)	; two or more elements
                                                   ,list
                                                   (first ,list))))
                                          (second value))
                                     ,(first (memq name uri-attrs)))
                   res))
            (t
             (push `(print-attribute ,key ,name ,value
                                     ,(first (memq name uri-attrs)))
                   res)))))
             ;(setq res (cons `(prin1-attribute-value ,value)
             ;                (nreconc (write-attribute-name-forms name) res))))))))

(defun process-html-forms (forms env)
 ;;; Value: Single Lisp form (progn ...)
  (let ((res ()))
    (flet ((do-desc (desc args argsp body)
	     ;; Push into res on form processed
	     ;; Args: desc  Descriptor object associated with the 
	     ;;	            html tag we're processing
	     ;;       args  List of values after the tag in the form
	     ;;             ((:tag . args) ....)
	     ;;       argsp True if this isn't a singleton tag  (i.e. it has a body)
	     ;;             (:tag ...) or ((:tag ...) ...)
	     ;;       body  Body if any of the form
             (push  
              (if-bind (desc-special (desc-special desc))
                (funcall desc-special desc args body)
                (let ((rest (cond ((null argsp)		  ; singleton tag, just do the set
                                   (setq args :set)
                                   nil)
                                  ((equal args '(:unset)) ; ((:tag :unset)) - close off
                                   (setq args :unset)	  ; singleton tags printed earlier
                                   nil)
                                  (t		 	  ; process recursively
                                   (process-html-forms body env)))))
                  (if-bind (desc-macro (desc-macro desc))
                    `(,desc-macro ,args ,rest)
                    (process-html-form desc args (list rest)))))
                #|(let ((rest (cond ((null argsp)	  ; singleton tag, just do the set
                                   
                                   ;'(:set))
                                  ((equal args '(:unset)) ; ((:tag :unset)) - close off
                                   '(:unset)) 		  ; singleton tags printed earlier
                                  (t		 	  ; process recursively
                                   `(,args ,(process-html-forms body env)))))))
                  (if-bind (desc-macro (desc-macro desc))
                    `(,desc-macro ,@rest)
                    (apply #'process-html-form desc rest)))|#
              res)))
      (dolist (form forms)
	(setq form (macroexpand form env))
        (if (atom form)
            (cond ((keywordp form)
                   (if-bind (desc (gethash form *desc-hash-table*))
                     (do-desc desc nil nil nil)
                     (signal-unknown-tag form)))
                  ((stringp form)				; turn into a print of it
                   (push `(write-html-string ,form *html-stream*) res))
                  (t
                   (push form res)))
            (let ((first (first form)))
              (cond ((keywordp first)				; (:xxx . body) form
                     (if-bind (desc (gethash first *desc-hash-table*))
                       (do-desc desc nil t (rest form))
                       (signal-unknown-tag first form)))
                    ((and (consp first) (keywordp (first first))) ; ((:xxx args ) . body)
                     (if-bind (desc (gethash (first first) *desc-hash-table*))
                       (do-desc desc (rest first) t (rest form))
                       (signal-unknown-tag (first first) form)))
                    (t
                     (push form res)))))))
    `(progn ,@(nreverse res))))

(defun process-html-form (desc args body)
 ;;; Main work horse doing the macroexpansion of an element with a "standard" tag.
  ;; Args: desc  Tag descriptor
  ;;       args  Plist of attributes or one of keywords :set or :unset
  ;;       body  Form body
  ;; Value: Single Lisp form.
  (let ((tag-name-string (tag-name-string (desc-key desc)))
        (has-end (desc-has-end desc )))
    (when (and args (atom args))				; single arg
      (return-from process-html-form
        (case args
          (:set (if has-end
                    `(write-html-string ,(format nil "<~a>" tag-name-string))
                    #1=`(write-html-string (if (eq *html-mode* :xml)
                                               ,(format nil "<~a />" tag-name-string)
                                               ,(format nil "<~a>" tag-name-string)))))
          (:unset (when has-end
                    `(write-html-string ,(format nil "</~a>" tag-name-string))))
          (t (error "Illegal arg ~s to ~s" args tag-name-string)))))
  
    (unless (evenp (length args))
      (warn "Attribute list is not even: ~s" args))
    
    (cond (args
           `(progn (write-html-string ,(format nil "<~a" tag-name-string))
              ,@(process-attributes desc args)
              ,(unless (or body has-end)
                 `(when (eq *html-mode* :xml)
                    (write-string " /" *html-stream*)))
              (write-char #\> *html-stream*)
              ,@body
              ,(when (or body has-end)
                 `(write-html-string ,(format nil "</~a>" tag-name-string)))))
          ((or body has-end)
           `(progn (write-html-string ,(format nil "<~a>" tag-name-string))
              ,@body
              (write-html-string ,(format nil "</~a>" tag-name-string))))
          (t
           #1#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PRINTING LHTML  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The subst parameter is an alist-like tree of an arbitrary depth.
;;;   subst ::= ( [dotted-pair | subst]* )
;;;   dotted-pair  ::= (old . new)
;;; It can be viewed as an alist but can embed another alist instead of a dotted pair.
;;;
;;; This invokes element rewriting provided
;;; - attribute name is either eq to desc-name-attr of the element tag (usually :id)
;;;   or :iter, and
;;; - attrubute value is any atom equal to some old.
;;;
;;; For each attribute  attr-name old, the associated new value can be one of:
;;;  - an LHTML form, which the printer is invoked on recursively,
;;;  - a function of signature (stream) producing print represenetaion of the element,
;;;  - a function of signature (form subst stream) only if the attr-name is :iter.

;;; Low-level helpers 

(defun html-find-value (key subst)
 ;;; Find the (key . value) object in the subst tree in a depth-first mode.
  ;; The subst list is an assoc list ((key . value) ....)
  ;; but instead of a (key . value) pair may embed another assoc list.
  (let ((to-process nil)
	(alist subst))
    (loop
      (do* ((rest alist (rest rest))
	    (pair (first rest) (first rest)))
           ((null rest) (setq alist nil))
        (cond ((consp (car pair))		; this is a lower-level subalist
               (when (rest rest)		; delay the rest
                 (push (rest rest) to-process))
               (setq alist pair)		; look at the subalist alist first
               (return))			; exit do*
              ((equal key (car pair))		; Q: can we do with eq?
               (return-from html-find-value pair))))
      (unless alist
        (if to-process
            (setq alist (pop to-process))	; return to a an upper-level alist
            (return))))))

(defun html-print-subst (form subst stream)
 ;;; Print the given LHTML form to the given stream
  ;; Args: stream  Usually *html-stream* is passed.
  ;; Value: What the print hander return and should be ignored as a rule.
  #+debug (assert (streamp stream))
  (let* ((possible-kwd (elem-key form))
         (attrs (elem-attrs form))
	 (desc (cond ((not (keywordp possible-kwd))
                      nil)
                     ((gethash possible-kwd *desc-hash-table*))
                     ((memq *unknown-tag-action* '(t :warn yl:logg))
                      (return-from html-print-subst
                        (signal-unknown-tag possible-kwd nil *unknown-tag-action*)))
                     (*unknown-tag-action*
                      (return-from html-print-subst
                        (funcall *unknown-tag-action* form stream)))
                     (t
                      (return-from html-print-subst nil))))
	 (desc-print (cond ((null desc)
                            nil)
                           ((and subst attrs           		; see if we should subst
                                 (when-let* ((name-attr (desc-name-attr desc))
                                             (var (getf attrs name-attr))
                                             (pair (html-find-value var subst)))
                                   (return-from html-print-subst ; pair ::= (var . value)
                                     (if (functionp (cdr pair))
                                         (funcall (cdr pair) stream)
                                         (html-print-subst (cdr pair) subst stream))))))
                           (t
                            (desc-print desc)))))
    (cond ((atom form)
           (cond (desc-print				; (assert (keywordp form))
                  (funcall desc-print desc :set nil nil nil stream))
                 (*lhtml-safe*
                  (write-html-string #1=(if (stringp form)
                                            form
                                            (princ-to-string form)) stream))
                 (t
                  (emit-safe stream #1#))))
          (desc-print
           (funcall desc-print desc :full attrs form subst stream))
          (t
           (error "Illegal LHTML form: ~s" form)))))

(defun html-print-list-subst (list-of-forms subst stream)
  (dolist (x list-of-forms)
    (html-print-subst x subst stream)))


(defun html-standard-print (desc cmd args form subst stream)
  ;; Print handler for normal tags, which are defined by HTML specification.
  ;; Args: desc - element descriptor
  ;;       form - the whole element
  ;;	   args - (:attr1 value1...)
  ;; NB: When we encounter :iter var in the args,
  ;;     1) search subst for a pair (var . fn),
  ;;        where fn has signature: (form subst stream);
  ;;	 2) if found, apply fn to the from given to print all HTML needed.
  (ecase cmd
    (:set					; just turn it on
     (write-char #\< stream)
     (write-string (tag-name-string (desc-key desc)) stream)
     (write-char #\> stream))
    (:full					; set, do body and then unset
     (let* ((desc-has-end (desc-has-end desc))
            (has-end (or desc-has-end (not (eq *html-mode* :xml))))
            (tag-name-string (tag-name-string (desc-key desc))))
       (if args
           (let ((iter (getf args :iter)))
             (cond ((and iter (setq iter (html-find-value iter subst)))
                    ;; Recreate the element with :iter removed from the attributs
                    ;; and apply the function on it
                    (funcall (cdr iter)
                             (cons (cons (caar form) (remove-property args :iter))
                                   (cdr form))
                             subst
                             stream)
                    (return-from html-standard-print))
                   (t
                    (write-char #\< stream)
                    (write-string tag-name-string stream)
                    (do ((key (desc-key desc))
                         (uri-attrs (desc-uri-attrs desc))
                         (xx args (cddr xx)))
                        ((null xx))
		      ; assume that the arg is already escaped
                      ; since we read it from the parser
                      ;; DI 2007-Feb-01: This is not true.
                      ;;  The attribute value comes not only from parser but also 
                      ;;  implicitly from calling html-print and html-print-list.
		      ;;  For consistencey, we use print-attribute.
                      ;;  and ignore attribute with null value at all
                      ;; DI 2012-Jan-25: We do not check for (null val)
                      ;;  - print-attribute is reposible for this from now on.
                      (let ((name (first xx))
                            (value (second xx)))
                        (print-attribute
                         key
                         name
                         (if (and (consp value) (eq (first value) :optional))
                             (if (cddr value)
                                 ;; Map over the list of forms:
                                 ;; - call template-eval on each and exclude NILs.
                                 ;; - replace a sigleton result list by the first element.
                                 (let ((list (mapcar #'html-template:template-eval
                                                     (rest value))))
                                   (if (rest list)		; two or more elemenets
                                       list
                                       (first list)))		; singleton list
                                 ;; Single form
                                 (html-template:template-eval (second value)))
                             value)
                         (memq name uri-attrs)
                         stream))))))
                          ;(write-char #\Space stream)
                          ;(write-string (attribute-name-string (car xx)) stream)
                          ;(prin1-attribute-value val stream))))
                    ;(unless has-end
                    ;  (write-string " /" stream))
                    ;(write-char #\> stream))))
           ;; No attributes
           (progn
             (write-char #\< stream)
             (write-string tag-name-string stream)))
       (unless has-end
         (write-string " /" stream))
       (write-char #\> stream)
       (dolist (child (rest form))		 	; output element content
	 (html-print-subst child subst stream))
       (when desc-has-end				; end the form
         (write-string "</" stream)
         (write-string tag-name-string stream)
         (write-char #\> stream))))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PSEUDO-TAGS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-special-html (key fn print-function &optional has-end uri-attrs)
  ;; key  The tag we're defining behavior for.
  ;; fn   Function to compute the macroexpansion of a use of this tag.
  ;;      Arguments to fn are: 
  ;;		desc  - descriptor object holding info on this tag
  ;;		args  - plist of attribute-value pairs following tag
  ;;		body  - list of body forms.
  ;;	   DI: The following has dropped as actually is never checked by any of the fcn:
  ;;	        argsp - true if there is a body in this use of the tag.
  ;; print-function
  ;;	  Function to print an LHTML form with this tag.
  ;;	  Arguments to print-function are:
  ;;		desc    - descriptor instance holding info on this tag
  ;;		cmd     - one of :set, :unset, :full
  ;;		args    - property list of :attribute value pairs
  ;;		form    - LHTML element
  ;;		subst   - subsitution alist
  ;;		stream  - stream to write to
  ;;	   DI: The following has dropped in favor of the *unknown-tag-action* special.
  ;;		unknown - function to call for unknown tags
  ;; uri-attrs
  ;;      See def-std-html
  ;;
  ;; Q: Where the fn is called from? Is just T/NIL enough (i.e. special or standard)?
  `(setf (gethash ,key *desc-hash-table*) 
         (make-desc ,key ,has-end nil ,fn ,print-function nil ',uri-attrs)))

#+lispworks
(editor:setup-indent "def-special-html" 1 2)	; indent args starting from the second by 2

;(defmacro named-function (name &body body)
;  (declare (ignore name))
;  `(function ,@body))

(defconstant* +spec-bad-args-format-string+
  "LHTML pseudo-tag ~s: bad number of arguments XXX"
  ;; "LHTML pseudo-tag ~s: bad number of arguments~@[ ~d~]"
  )
(defconstant* +bad-tag-within-body+
  "Illegal LHTML pseudo-tag ~s within body: ~s")
(defconstant* +bad-tag-place+
  "Illegal LHTML pseudo-tag ~s in this place: ~s")

(def-special-html :comment			; chars should not be converted
  (lambda (desc args body)			; to potential entity references
    ;; must use <!--   --> syntax
    (declare (ignore desc args))
    `(progn (write-string #1="<!--" *html-stream*)
       ,@(mapcar (lambda (arg) `(princ-http ,arg)) body)	;(htm ,@body)
       (write-string #2="-->" *html-stream*)))
  (lambda (desc cmd args form subst stream &aux (*lhtml-safe* t))
   ;; As LHTML can be obtained by parsing LHTT, the body of the comment element can
   ;; - consist of several subforms,
   ;; - contain subforms like (:eval expr), which results from the {expr} template.
    (declare (ignore desc cmd args subst))
    (write-html-string #1# stream)
    (html-print-list-subst (rest form) nil stream)		; no subst with comment
    (write-html-string #2# stream)))
    ;(write-html-string (format nil "<!--~a-->" (cadr form)) stream)))

;; Provide escaping basing on the value of html-template:*escape-char-p*
(def-special-html :escape
  (lambda (desc args body)
    (declare (ignore desc args))
    `(write-html-string (escape-string ,@body) *html-stream*))
  (lambda (desc cmd args form subst stream)
    (declare (ignore args desc subst))
    ;(assert (eql 1 (length form)))
    (if (eq cmd :full)
        (write-html-string (escape-string (second form)) stream)
        (error +spec-bad-args-format-string+ :escape))))

(def-special-html :format 
  (lambda (desc args body)
    (declare (ignore desc args))
    `(write-html-string (format nil ,@body) *html-stream*))
  (lambda (desc cmd args form subst stream)
    (declare (ignore args desc subst))
    (if (eq cmd :full)
        (write-html-string (apply #'format nil (rest form)) stream)
        (error +spec-bad-args-format-string+ :format))))

(def-special-html :format-safe
  (lambda (desc args body)
    (declare (ignore desc args))
    `(emit-safe *html-stream* (format nil ,@body)))
  (lambda (desc cmd args form subst stream)
    (declare (ignore args desc subst))
    (if (eq cmd :full)
        (emit-safe stream (apply #'format nil (rest form)))
        (error +spec-bad-args-format-string+ :format-safe))))

(def-special-html :include
  ;; Insert an HTML, LHTML, or even HTT file
  ;; args/attrs - plist of bindings
  (lambda (desc args body)
    (declare (ignore desc))
    (if body
        `(include ,(first body) (list ,@args) ,@(rest body))
        (error "LHTML pseudo-tag :INCLUDE must have a body.")))

  ;; For html-standard-print compatibility, we analyse every attribute value form.
  ;; It it is a list of forms, call template-eval on each and exclude NILs.
  ;; Replace a sigleton result list by the first element.
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore desc subst))
    (if (eq cmd :full)
        (apply #'include (second form)
               (loop for rest on attrs by #'cddr
                     for val = (second rest)
                     collect (first rest)
                     collect (if (and (consp val) (eq (first val) :optional))
                                 ;; :optional attribute compatibility
                                 (if (cddr val)
                                     (do ((acc ())
                                          x
                                          (rest (cdr val) (rest rest)))
                                         ((null rest)
                                          (if (rest acc)	; two or more elemenets
                                              (htt::list-to-string
                                               (mapcar #'princ-to-string acc))
                                              (first acc)))	; singleton list
                                       (when (setq x (htt:template-eval (first rest)))
                                         (push x acc)))
                                     ;; Single ":optional" form
                                     (htt:template-eval (second val)))
                                 (htt:template-eval val)))
               :stream stream
               (cddr form))
        (error +spec-bad-args-format-string+ :include))))

(def-special-html :jscript
  (lambda (desc args body)
    ;; must use <!--   --> syntax
    (declare (ignore desc args))
    `(progn
       (write-line #1="<script type=\"text/javascript\">" *html-stream*)
       (when (eq *html-mode* :xml) (write-line #2="<!--[CDATA[" *html-stream*))
       (htm ,@body)
       (terpri *html-stream*)
       (when (eq *html-mode* :xml) (write-line #3="// ]]-->" *html-stream*))
       (write-string #4="</script>" *html-stream*)))
  (lambda (desc cmd args form subst stream)
    (declare (ignore desc cmd args subst))
    (write-line #1# stream)
    (when (eq *html-mode* :xml) (write-line #2# stream))
    (write-html-string (princ-to-string (cadr form)) stream)
    (terpri stream)
    (when (eq *html-mode* :xml) (write-line #3# stream))
    (write-string #4# stream))
  t (:src))							; has-end uri-attrs

;; SCRIPT
;; Print attributes in a standard manner but output the content in raw mode, i.e.
;; - neither print an additional CDATA comment
;; - nor convert to character entities.
;; Define convenience macro as well

(def-special-html :script
  nil								; considered standard
  (lambda (desc cmd args form subst stream &aux (*lhtml-safe* t))
    (html-standard-print desc cmd args form subst stream))
  t (:src))							; has-end uri-attrs

(export '(script))
(defmacro script (args &body body)
  (process-html-form (gethash :script *desc-hash-table*) args body))

(def-special-html :nbsp
  (lambda (desc args body)
    (declare (ignore desc args))
    (when body
      (error +bad-tag-within-body+ :nbsp body))
    '(write-string +nbsp+ *html-stream*))
  (lambda (desc cmd args form subst stream)
    (declare (ignore args desc subst))
    (if (eq cmd :set)
        (write-string +nbsp+ stream)
        (error +bad-tag-place+ :nbsp form))))

(def-special-html :newline
  (lambda (desc args body)
    (declare (ignore desc args))
    (if body
        (error +bad-tag-within-body+ :newline body)
        `(terpri *html-stream*)))
  (lambda (desc cmd args form subst stream)
    (declare (ignore args desc subst))
    (if (eq cmd :set)
        (terpri stream)
        (error +bad-tag-place+ :newline form))))


(def-special-html :princ 
  (lambda (desc args body)
    (declare (ignore desc args))
    `(progn ,@(mapcar (lambda (bod) `(princ-http ,bod))
                      body)))
  (lambda (desc cmd args form subst stream)
    (declare (ignore args desc subst))
    (if (eq cmd :full)
        (dolist (val (rest form))
          (write-html-string (princ-to-string val) stream))
        (error +spec-bad-args-format-string+ :princ))))

(def-special-html :princ-safe 
  (lambda (desc args body)
    (declare (ignore desc args))
    `(progn ,@(mapcar (lambda (bod) `(princ-safe-http ,bod))
                      body)))
  (lambda (desc cmd args form subst stream)
    (declare (ignore args desc subst))
    (if (eq cmd :full)
        (dolist (val (rest form))
          (emit-safe stream (princ-to-string val)))
	  ;(emit-safe stream (princ-to-string (cadr form)))
        (error +spec-bad-args-format-string+ :princ-safe))))


(def-special-html :prin1 
  (lambda (desc args body)
    (declare (ignore desc args))
    `(progn ,@(mapcar (lambda (bod) `(prin1-http ,bod))
                      body)))
  (lambda (desc cmd args form subst stream)
    (declare (ignore desc args subst))
    (if (eq cmd :full)
        (dolist (val (rest form))
          (write-html-string (prin1-to-string val) stream))
        (error +spec-bad-args-format-string+ :prin1))))

(def-special-html :prin1-safe 
  (lambda (desc args body)
    (declare (ignore desc args))
    `(progn ,@(mapcar (lambda (bod) `(prin1-safe-http ,bod))
                      body)))
  (lambda (desc cmd args form subst stream)
    (declare (ignore args desc subst))
    (if (eq cmd :full)
        (dolist (val (rest form))
          (emit-safe stream (prin1-to-string val)))
        (error +spec-bad-args-format-string+ :prin1-safe))))

;;; The following are needed for parsing and for printing parsed LHTML

(def-special-html :!doctype
  nil
  (lambda (desc cmd args form subst stream)
    (declare (ignore desc cmd args subst))
    (format stream "<!DOCTYPE~a>" (cadr form)) stream))

(def-special-html :?xml
  nil							; rest of xml-prologue is parsed 
  (lambda (desc cmd args form subst stream)		; in raw mode for now
    (declare (ignore desc cmd args subst))
    (format stream "<?xml ~a?>" (cadr form)) stream))

(def-special-html :?php
  nil							; unlikely to come from Lisp
  ;; PHP code is printed in raw mode (see also print-attribute)
  (lambda (desc cmd args form subst stream)
    (declare (ignore desc cmd args subst))
    (format stream "<?php~a?>" (cadr form)) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  API  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro htm (&rest forms &environment env)
 ;;; Emit HTML to *html-stream*
  ;; CAUTION: In Franz's htmlgen, this marco is named html!
  (process-html-forms forms env))

(defmacro with-html-stream ((stream &key (mode *html-mode*) prologue) &body body)
  (with-gensyms (p)
    `(let ((*html-stream* ,stream)
           (,p ,prologue))
       (with-html-mode ,mode
         (when ,p				; output explicit or default prologue
           (write-line (if (stringp ,p) ,p *prologue*) *html-stream*))
         (htm ,@body)))))

(defmacro with-html-file ((filename &rest args
                                    &key (direction :output) (if-exists :supersede)
                                    &allow-other-keys)
                          &body body)
 ;;; Open a file-based output stream and emit HTML
  ;; Args: filename        Pathname or string.
  ;;       mode, prologue  See with-html-stream.
  ;;       external-format Shall we default it in here?
  `(with-open-file (*html-stream* ,filename :direction ,direction :if-exists ,if-exists
                    ,@(remove-properties args '(:direction :if-exists :mode :prologue)))
     (with-html-stream (*html-stream* ,@(remove-properties args
                                         '(:direction :external-format :if-exists)))
       ,@body)))

(defun html-print (form &optional (stream *html-stream*)) ;&key unknown
  (html-print-subst form nil stream))

(defun html-print-list (list-of-forms &optional (stream *html-stream*)) ;&key unknown
  ;; html print a list of forms
  (dolist (x list-of-forms)
    (html-print-subst x nil stream)))

;;; DEPRECATED in favor with-html-stream

(defmacro html-out-stream-check (stream)
  ;; ensure that a real stream is passed to this function
  (let ((s (gensym)))
  `(let ((,s ,stream))
     (if (streamp ,s)
         ,s
         (error "html-stream must be passed a stream object, not ~s" ,s)))))

(defmacro html-stream (stream &rest forms)
 ;;; set output stream and emit html
  `(let ((*html-stream* (html-out-stream-check ,stream)))
     (htm ,@forms)))
