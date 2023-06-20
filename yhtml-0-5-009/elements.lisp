;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;; Part of HTMLGen and PHTML, copyright (c) 1986-2000 Franz Inc, Berkeley, CA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element descriptors and standard definitions

;; Most tags are handled in a standard way and the def-std-html
;; macro is used to define such tags
;;
;; Some tags need special treatment and def-special-html defines how these are handled.
;; These tags are often referred as "pseudo-tags" and added for extra operations
;; in the generator.
;; 
;; Tags can be found in three ways:
;;  :br	    			- singleton, no attributes, no body
;;  (:b "foo")			- no attributes but with a body
;;  ((:a :href "bar") "balh")	- attributes and body

(in-package :html)

;;; HTML Element Descriptor

(defstruct (descriptor (:constructor make-desc (key has-end macro special print
						name-attr uri-attrs))
                       (:conc-name desc-))
  key		; keyword naming this tag
  has-end	; t if the there end tag </name>
  macro  	; the macro to define this
  special       ; if true then call this to process the keyword and return macroexpansion
  print         ; function used to handle this in html-print
  name-attr     ; attribute keyword which can name a standard element for subst purposes,
                ; to replace the whole element by another form (printing LHTML only).
                ; DI: Do we really need this? Can we assume ID for all elements?
                ;     Maybe it indicates an alternative attribute when ID does not work.
  uri-attrs)	; list of attribute keywords that have an URI as a value

;; We want to avoid rehashes though #'eq may be more accurate
(defvar *desc-hash-table* (make-hash-table :test #'equal))

;;; Standard element defintions
;;; Our names of macros are the same as element names.
;;; On the contrast in Franz: (macro-name (intern (format nil "~a-~a" :with-html key)))

(defmacro def-std-html (key has-end name-attr
                        &optional uri-attrs (macro-name (intern (symbol-name key))))
  `(let ((desc (make-desc ,key ,has-end ',macro-name nil 'html-standard-print
                          ',name-attr ',uri-attrs)))
     (setf (gethash ,key *desc-hash-table*) desc)
     ,@(when macro-name
         `((export '(,macro-name))
           (defmacro ,macro-name (args &body body)
             (process-html-form desc args body))))))
             ;(html-body-key-form ,key ,has-end args body)

(def-std-html :a	t   nil	(:href))
(def-std-html :abbr	t   nil)
(def-std-html :acronym	t   nil)
(def-std-html :address	t   nil)
(def-std-html :applet	t   nil)
(def-std-html :area	nil nil	(:href))

(def-std-html :b	t   nil)
(def-std-html :base	nil nil	(:href))
(def-std-html :basefont	nil nil)
(def-std-html :bdo	t   nil)
(def-std-html :big	t   nil)
(def-std-html :blockquote t nil	(:cite))
(def-std-html :body	t   nil)
(def-std-html :br	nil nil)
(def-std-html :button	nil nil)

(def-std-html :caption  t   nil)
(def-std-html :center   t   nil)
(def-std-html :cite     t   nil)
(def-std-html :code     t   nil)
(def-std-html :col      nil nil)
(def-std-html :colgroup t   nil)			; DI BUG fixed: nil

(def-std-html :dd	t   nil)
(def-std-html :del	t   nil	(:cite))
(def-std-html :dfn	t   nil)
(def-std-html :dir	t   nil)
(def-std-html :div	t   nil)
(def-std-html :dl	t   nil)
(def-std-html :dt	t   nil)

(def-std-html :em	t   nil)
(def-std-html :embed	t   nil)

(def-std-html :fieldset	t   nil)
(def-std-html :font	t   nil)
(def-std-html :form	t :name	(:action))	; XHTML: name deprecated if favor of id
(def-std-html :frame	nil nil (:src :longdesc))
(def-std-html :frameset	t   nil)

(def-std-html :h1	t   nil)
(def-std-html :h2	t   nil)
(def-std-html :h3	t   nil)
(def-std-html :h4	t   nil)
(def-std-html :h5	t   nil)
(def-std-html :h6	t   nil)
(def-std-html :head	t   nil	(:profile))
(def-std-html :hr	nil nil)
(def-std-html :html	t   nil)			; Franz's HTML was renamed to HTM

(def-std-html :i	t   nil)
(def-std-html :iframe	t   nil)
(def-std-html :ilayer	t   nil)
(def-std-html :img	nil :id	(:src :longdesc :usemap))
(def-std-html :input	nil nil	(:src :usemap))
(def-std-html :ins	t   nil	(:cite))
(def-std-html :isindex	nil nil)			; deprecated

(def-std-html :kbd	t   nil)

(def-std-html :label	t   nil)			; :for has 'name' syntax
(def-std-html :layer	t   nil)
(def-std-html :legend	t   nil)
(def-std-html :li  	t   nil)
(def-std-html :link	nil nil	(:href))

(def-std-html :map  	t   nil	()       nil)		; no macro name as cl:MAP exists
(def-std-html :marquee	t   nil)
(def-std-html :menu	t   nil)
(def-std-html :meta  	nil nil)
(def-std-html :multicol	t   nil)

(def-std-html :nobr  	t   nil)			; IE only
(def-std-html :noframes	t   nil)
(def-std-html :noindex 	t   nil)			; extension? 
(def-std-html :noscript	t   nil)

(def-std-html :object	t   nil	(:classid :codebase :data :usemap))
(def-std-html :ol  	t   nil)
(def-std-html :optgroup	t   nil)
(def-std-html :option	t   nil)

(def-std-html :p  	t   nil)
(def-std-html :param	t   nil)
(def-std-html :pre  	t   nil)

(def-std-html :q  	t   nil	(:cite))	; doesn't work in IE, no FrontPage support

(def-std-html :s  	t   nil)
(def-std-html :samp  	t   nil)
;(def-std-html :script	t   nil	(:src))			; :for has 'name' syntax
(def-std-html :select	t   nil)
(def-std-html :small	t   nil)
(def-std-html :spacer	nil nil)
(def-std-html :span  	t   :id)
(def-std-html :strike	t   nil)
(def-std-html :strong	t   nil)
(def-std-html :style	t   nil)  
(def-std-html :sub  	t   nil)
(def-std-html :sup  	t   nil)

(def-std-html :table	t   :id)			; name is wrong for table!
(def-std-html :tbody	t   nil)
(def-std-html :td	t   nil)
(def-std-html :textarea	t   nil)
(def-std-html :tfoot	t   nil)
(def-std-html :th  	t   nil)
(def-std-html :thead	t   nil)
(def-std-html :title	t   nil)
(def-std-html :tr  	t   nil)
(def-std-html :tt  	t   nil)

(def-std-html :u 	t   nil)
(def-std-html :ul 	t   nil)

(def-std-html :var 	t   nil)

;(def-std-html :bgsound	nil nil)			; deprecated or IE only
;(def-std-html :blink	t   nil)
;(def-std-html :keygen	nil nil)
;(def-std-html :listing  t  nil)			; deprecated in favor of PRE, SAMP
;(def-std-html :noembed t   nil)
;(def-std-html :plaintext nil nil)			; deprecated in favor PRE
;(def-std-html :server 	 t  nil)
;(def-std-html :wbr  	nil nil)			; IE only
;(def-std-html :xmp 	 t  nil)			; deprecated in favor of PRE, SAMP

(declaim (inline tag-standard-p))
(defun tag-standard-p (key)
  (when-bind (desc (gethash key *desc-hash-table*))
    (not (desc-special desc))))

(defun uri-attribute-p (key attr)
  (when-bind (desc (gethash key *desc-hash-table*))
    (memq attr (desc-uri-attrs desc))))

;;; Helpers

(declaim (inline elem-key elem-attrs elem-cont get-attr))
(defun elem-key   (tree) (if (consp tree) (first-or-self (first tree)) tree))
(defun elem-attrs (tree) (if (and (consp tree) (consp (setq tree (first tree))))
                             (rest tree)
                             nil))
(defun elem-cont  (tree) (if (consp tree) (rest tree) nil))
(defun elem-desc  (tree) (gethash (elem-key tree) *desc-hash-table*))

(defun make-elem (key attrs cont)
  (if (and key (symbolp key))
      (cond (attrs (cons (cons key attrs) cont))
            (cont  (cons key cont))
            (t     key))
      key))

(defun (setf elem-attrs) (attrs tree)
  (if (consp tree)
      (let ((first (first tree)))
        (if (consp first)
            (if attrs
                (rplacd first attrs)
                (rplaca tree (car first)))
            (rplaca tree (cons first attrs)))
        attrs)
      nil))
(defun (setf elem-cont) (cont tree)
  (when (consp tree)
    (rplacd tree cont)
    cont))
