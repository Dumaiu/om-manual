;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;; Based on code by Timofei Shatrov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bulletin Board markup (BBcode) to HTML conversion
;;;
;;; #-Russian
;;; Convert pseudo-tags and other patterns used in forums into HTML:
;;;	[b]text[/b]		Output in bold
;;;	[i]text[/i]		Output in italics
;;;	[u]text[/u]		Output underlined
;;;	[code]text[/code]	Output in monospace -> <code>text</code>
;;;	[tt]text[/tt]		Output in monospace -> <kbd>text</kbd>
;;;	[img]address[/img]	Insert picture from an arbitrary location
;;;	[url]address[/url]	Insert hyperlink to an arbitrary resource
;;;	[url=address]text[/url]	Like the above but display as the text specified
;;;     #\Newline -> <br>	Insert line break
;;; #+Russian
;;; Конвертировать в НTML-теги другие образцы, принятые в форумах:
;;;	[b]текст[/b]		выводит текст жирно 
;;;	[i]текст[/i]		выводит текст курсивом 
;;;	[u]text[/u]		выводит подчёркнутый текст
;;;	[code]текст[/code]	выводит текст машинописным шрифтом
;;;	[tt]текст[/tt]		выводит текст машинописным шрифтом
;;;	[img]адрес[/img]	вставляет картинку, хранющуюся в произвольном месте
;;;	[url]адрес[/url]	вставляет гиперссылку на произвольный ресурс
;;;	[url=адрес]текст[/url]	вставляет гиперссылку на произвольный ресурс,
;;;				отображаемую в виде текста
;;;     #\Newline -> <br>	вставляет перевод строки
;;;
;;; Along with HTML-TEMPLATE, this add-on depends on
;;; - either Ystok-Library on LispWorks
;;;   (relying on Emacs-like regular expressions internally),
;;; - or Edi Weitz's CL-PPCRE.

(in-package :html)

#-(or (and ylib lispworks) cl-ppcre)
(error "BBCode module either requires Ystok-Library on LispWorks or CL-PPCRE.")

(defparameter *bbcode-inline-tags* '(:b :i :u :code :kbd)
 "List of keywords denoting simple inline tags.")

(defparameter *bbcode-span-classes* '("small-caps" "menu-command" "widget")
 "List of keywords denoting an CSS class for the SPAN element.")

(defun parse-bbcode-token (token)
 "foo -> foo, [foo bar] -> (foo bar), [[foo] -> [foo "
  (let ((length (length token)))
    (cond ((or (zerop length)				; not a tag
               (not (and (char= (char token 0) #\[)
                         (char= (char token (1- length)) #\]))))
           token)
          ((char= (char token 1) #\[)			; a spurious tag
           (subseq token 1 (1- length)))
          (t			; разбить на подстроки по пробелу или знаку =
           #+ylib
           (yl:split-seq-if (lambda (char) (char-position char " ="))
                            token
                            :start 1 :end (1- length))
           #+(and (not ylib) lispworks)
           (lw:split-sequence '(#\Space #\=) token :start 1 :end (1- length))
           ;#+(and ylib lispworks)
           ;(yl:split " \\\|=" token :start 1 :end (1- length))
           #+(and (not ylib) (not lispworks) cl-ppcre)
           (ppcre:split "\\s+" token :start 1 :end (1- length))
           #-(or ylib lispworks cl-ppcre)
           (progn (warn "Do not know how to split sequence.") (list token))))))

(defun preparse-bbcode (text)
 "Find all tokens of the form [...] in the text and construct a list
 of them and intermediate text."
 ;;; LW NB:
  ;; 230 characters is a limitation of the regexp engine in LW 4.4 - 5.1
  ;;	(lw:find-regexp-in-string "\\[[^[]*\\]" "aaaaaaa bbbbbbbbbb cccccccc dd[/b]...")
  ;;	=> Error: Cannot find that; regexp is too unconstrained
  ;; The problem is that * is greedy, so even though the pattern match starts at
  ;; index 30, it has to scan to the end of the string to prove that it doesn't end
  ;; with a ] somewhere after index 33.  This is possible because [^[] matches a
  ;; newline character.
  ;;
  ;; Since you generally expect the ] to be nearby, possibly excluding it from the
  ;; pattern will do what you want without needing to scan so far, i.e. "\\[[^][]*\\]"?
  (mapcar #'parse-bbcode-token
          #+(and ylib lispworks)
          (yl:split "\\[[^][]*\\]" text :with-registers-p t)	; Emacs-like re
          #+(and (not ylib) (not lispworks) cl-ppcre)
          (ppcre:split "(\\[[^[]*])" text :with-registers-p t)
          #-(or (and ylib lispworks) cl-ppcre)
          (progn (warn "Do not know how to split sequence.") (list text))))

(defun find-closing-bbcode-tag (list)
 "Return a list of the form (((tag-name . attrs) inside-text) remaining-text)"
  (let ((tag-name (caar list)))
    (loop for (token . rest) on list
          until (and (listp token) (string-equal (car token) tag-name :start1 1))
          collect token into tag-inside
          finally (return (cons tag-inside rest)))))

(defun parse-bbcode (text)
 "Produces a parse tree out of the text"
  (labels ((parse (list)
             (cond ((endp list) nil)
                   ((consp (first list))			; DI 2014-May-07: was listp
                    (destructuring-bind ((tag . inside) . outside)
                        (find-closing-bbcode-tag list)
                      `((,tag . ,(parse inside)) . ,(parse outside))))
                   (t `(,(first list) . ,(parse (rest list)))))))
    (parse (preparse-bbcode text))))

;;; Write directly to *html-stream*

(defun %bbcode-output-node (node)
  (flet ((%wrap (key &aux (name (tag-name-string key)))
           (format *html-stream* "<~a>" name)
           (%bbcode-output-tree (cdr node))
           (format *html-stream* #2="</~a>" name))
         (%uri (inside outside)
           (cond (inside (car inside))
                 ((stringp (car outside)) (car outside))
                 (t "#"))))
    (if (stringp node) 
        ;; Split on #\Newline replacing them with <br>
        (do ((prev nil)
             (rest #+ylib
                   (yl:split-seq #\Newline node :keep-empty-subseqs t)
                   #+(and lispworks (not ylib))
                   (lw:split-sequence '(#\Newline) node :coalesce-separators nil)
                   #+(and (not ylib) (not lispworks) cl-ppcre)
                   (ppcre:split "\\n" node)
                   #-(or ylib lispworks cl-ppcre)
                   (progn (warn "Do not know how to split sequence.") (list node))
                   (rest rest)))
            ((null rest))
          (if prev
              (format *html-stream* "<~a~:[~; /~]>"
                      (tag-name-string :br) (eq *html-mode* :xml))
              (setq prev t))
          (write-string (funcall html-template:*string-modifier* (first rest))
                        *html-stream*))
        (destructuring-bind ((name . inside) . outside) node
          ;; inside is an attribute - a list of strings
          (let ((rest (member name *bbcode-inline-tags* :test #'string-equal)))
            (cond (rest
                   (%wrap (first rest)))
                  ((string-equal name "tt")
                   (%wrap :kbd))
                  ((setq rest (member name *bbcode-span-classes* :test #'string-equal))
                   (format *html-stream* #1="<~a ~a=\"~a\">"
                           (tag-name-string :span)
                           (attribute-name-string :class)
                           (first rest))
                   (%bbcode-output-tree outside)
                   (format *html-stream* #2# (tag-name-string :span)))
                  ((string-equal name "img")
                   (format *html-stream* "<~a ~a=\"~a\"~:[~; /~]>"
                           (tag-name-string :img)
                           (attribute-name-string :src)
                           (html-template:escape-string (%uri inside outside))
                           (eq *html-mode* :xml)))
                  ((string-equal name "url")
                   (format *html-stream* #1#
                           (tag-name-string :a)
                           (attribute-name-string :href)
                           (html-template:escape-string (%uri inside outside)))
                   (%bbcode-output-tree outside)
                   (format *html-stream* #2# (tag-name-string :a)))
                  (t						
                   ;; Unknown marker
                   ;; BAD: Elements in the inside list are separated with just one space!
                   (format *html-stream* "[~a~{ ~a~}]"
                           (funcall html-template:*string-modifier* name)
                           (mapcar html-template:*string-modifier* inside))
                   (%bbcode-output-tree outside))))))))
                            
(defun %bbcode-output-tree (tree)
  (dolist (token tree)
    (%bbcode-output-node token)))

;;; Convert to LHTML

(defun %bbcode-lhtml-node (node)
  (let ((acc ()))
    (flet ((%wrap (key &optional attrs (cont (%bbcode-lhtml-tree (cdr node))))
             (push (make-elem key attrs cont) acc))
           (%uri (inside outside)
             (cond (inside (car inside))
                   ((stringp (car outside)) (car outside))
                   (t "#"))))
      (if (stringp node) 
          ;; Split on #\Newline replacing them with :br
          (let ((prev nil))
            (dolist (token #+ylib
                           (yl:split-seq #\Newline node :keep-empty-subseqs t)
                           #+(and lispworks (not ylib))
                           (lw:split-sequence '(#\Newline) node :coalesce-separators nil)
                           #+(and (not ylib) (not lispworks) cl-ppcre)
                           (ppcre:split "\\n" node)
                           #-(or ylib lispworks cl-ppcre)
                           (progn (warn "Do not know how to split sequence.") (list node)))
              (if prev
                  (push :br acc)
                  (setq prev t))
              (unless (zerop (length token))			; drop empty subseq here
                (push token acc))))
          (destructuring-bind ((name . inside) . outside) node	; inside is an attribute
            (let ((rest (member name *bbcode-inline-tags* :test #'string-equal)))
              (cond (rest
                     (%wrap (first rest)))
                    ((setq rest (member name *bbcode-span-classes* :test #'string-equal))
                     (%wrap :span (list :class (first rest))))
                    ((string-equal name "tt")
                     (%wrap :kbd))
                    ((string-equal name "img")
                     (%wrap :img
                            (list :src (%uri inside outside))
                            ()))
                    ((string-equal name "url")
                     (%wrap :a
                            (list :href (%uri inside outside))
                            (%bbcode-lhtml-tree outside)))
                    (t
                     (push (format nil "[~a~{ ~a~}]" name inside)
                           acc)		; unknown marker
                     (setq acc (nreconc (%bbcode-lhtml-tree outside) acc))) )))))
    (nreverse acc)))
                            
(defun %bbcode-lhtml-tree (tree)
  (let ((acc ()))
    (dolist (token tree)
      (setq acc (nreconc (%bbcode-lhtml-node token) acc)))
    (nreverse acc)))

;;; Top-level API

(defun bbcode-output-html (text &optional (html:*html-stream* *standard-output*))
 "Convert string TEXT containing BBcode-tags to HTML and send output to the
 STREAM specified as the second parameter (*standard-output* by default)."
  (%bbcode-output-tree (parse-bbcode text)))

(defun bbcode-html-string (text)
 "Convert string TEXT containing BBcode-tags to an HTML string."
  (with-output-to-string (html:*html-stream*)
    (%bbcode-output-tree (parse-bbcode text))))

(defun bbcode-lhtml (text)
 "Convert string TEXT containing BBcode-tags to an LHTML list."
  (%bbcode-lhtml-tree (parse-bbcode text)))
