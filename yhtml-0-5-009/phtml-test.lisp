;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitry Ivanov
;;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML parser tests

(pushnew :tester *features*)

;(load #P"PROJECTS:ynet;acl-compat;defsys")
;(compile-system 'ACL-COMPAT :load t)

;(load #P"PROJECTS:ynet;uri;defsys")
;(compile-system 'YSTOK-URI :load t)

(in-package :phtml)

(defvar *test-string2*)
(defvar *test-string3*)
(defvar *expected-result2*)
(defvar *expected-result3*)


;; it uses a fake pp tag to test nesting for callbacks...
(defvar *test-string*
    "<html>
       <!-- this should be <h1>one</h1> string -->
       <head>
        <style> this should be <h1>one</h1> string </STYLE>
        <title> this is some title text </title> 
       <body> this is some body text
        <a name=\"this is an anchor\">with some text </a>
        <!-- testing allowing looser attribute parsing -->
        <a href= mailto:lmcelroy@performigence.com>lmcelroy@performigence.com
           </a>
        <br>
        this is some more text
        <bogus> tests parser 'looseness'</bogus>
        <select>
         <option>1
         <option>2 </select>
        <ul>
         <li>item 1
         <li>item 2 </ul>
        <dl>
         <dt>a term
         <dd>its definition
         <dt>another term
         <dd>another definition</dl>
        <table>
         <colgroup>
          <col align=\"right\">
          <col align=\"center\">
         <thead>
         <tr>
          <th> this cell is aligned right
          <th> this cell is centered
         <tfoot>
         <tr>
          <th> this cell is aligned right
          <th> this cell is centered
         <tbody>
         <tr>
          <td> this cell is aligned right
          <td> this cell is centered
         <tbody>
         <tr>
          <td> this cell is aligned right
          <td> this cell is centered </table>
        <pp>
         <object>
          <pp>Navigate the site:
           <map name=\"mainmap\">
            <area shape=rect coords=\"0,100,100,200\">
            <area shape=rect coords=\"100,100,100,200\"> </map> </object> </pp>
        <abbr>WWW</abbr> is an abbreviation
        <b>force</b>
        <pp>whitespace only")

(defvar *expected-result*
    '((:html
       (:comment "this should be <h1>one</h1> string")
       (:head
	(:style "this should be <h1>one</h1> string")
	(:title "this is some title text"))
       (:body 
	"this is some body text"
        ((:a :name "this is an anchor") "with some text")
	(:comment "testing allowing looser attribute parsing")
	((:a :href "mailto:lmcelroy@performigence.com")
	 "lmcelroy@performigence.com")
	:br
	"this is some more text"
	(:bogus "tests parser 'looseness'")
	(:select
	 (:option "1")
	 (:option "2"))
	(:ul
	 (:li "item 1") 
	 (:li "item 2"))
	(:dl
	 (:dt "a term")
	 (:dd "its definition")
	 (:dt "another term")
	 (:dd "another definition"))
	(:table
	 (:colgroup
	  ((:col :align "right"))
	  ((:col :align "center")))
	 (:thead
	  (:tr
	   (:th "this cell is aligned right")
	   (:th "this cell is centered")))
	 (:tfoot
	  (:tr
	   (:th "this cell is aligned right")
	   (:th "this cell is centered")))
	 (:tbody
	  (:tr
	   (:td "this cell is aligned right")
	   (:td "this cell is centered")))
	 (:tbody
	  (:tr
	   (:td "this cell is aligned right")
	   (:td "this cell is centered"))))
	(:pp
	 (:object
	  (:pp "Navigate the site:"
	      ((:map :name "mainmap")
	       ((:area :shape "rect" :coords "0,100,100,200"))
	       ((:area :shape "rect" :coords "100,100,100,200"))))))
	(:abbr "WWW")
	"is an abbreviation"
	(:b "force")
	(:pp "whitespace only")
	))))

(setf *expected-result-newline*
 '((:HTML :NEWLINE
    (:COMMENT " this should be <h1>one</h1> string ") :NEWLINE
    (:HEAD :NEWLINE
     (:STYLE " this should be <h1>one</h1> string ") :NEWLINE
     (:TITLE " this is some title text ") :NEWLINE)
    (:BODY " this is some body text" :NEWLINE
     ((:A :NAME "this is an anchor") "with some text ") :NEWLINE
     (:COMMENT " testing allowing looser attribute parsing ") :NEWLINE
     ((:A :HREF "mailto:lmcelroy@performigence.com") "lmcelroy@performigence.com" :NEWLINE)
     :NEWLINE :BR :NEWLINE
     " this is some more text" :NEWLINE
     (:BOGUS " tests parser 'looseness'") :NEWLINE
     (:SELECT :NEWLINE
      (:OPTION "1" :NEWLINE) (:OPTION "2 ")) :NEWLINE
     (:UL :NEWLINE
      (:LI "item 1" :NEWLINE)
      (:LI "item 2 ")) :NEWLINE
     (:DL :NEWLINE
      (:DT "a term" :NEWLINE)
      (:DD "its definition" :NEWLINE)
      (:DT "another term" :NEWLINE)
      (:DD "another definition")) :NEWLINE
     (:TABLE :NEWLINE
      (:COLGROUP :NEWLINE
       ((:COL :ALIGN "right")) :NEWLINE
       ((:COL :ALIGN "center")) :NEWLINE)
      (:THEAD :NEWLINE
       (:TR :NEWLINE
        (:TH " this cell is aligned right" :NEWLINE)
        (:TH " this cell is centered" :NEWLINE)))
      (:TFOOT :NEWLINE
       (:TR :NEWLINE
        (:TH " this cell is aligned right" :NEWLINE)
        (:TH " this cell is centered" :NEWLINE)))
      (:TBODY :NEWLINE
       (:TR :NEWLINE
        (:TD " this cell is aligned right" :NEWLINE)
        (:TD " this cell is centered" :NEWLINE)))
      (:TBODY :NEWLINE
       (:TR :NEWLINE
        (:TD " this cell is aligned right" :NEWLINE)
        (:TD " this cell is centered ")))) :NEWLINE
     (:PP :NEWLINE
      (:OBJECT :NEWLINE
       (:PP "Navigate the site:" :NEWLINE
        ((:MAP :NAME "mainmap") :NEWLINE
         ((:AREA :SHAPE "rect" :COORDS "0,100,100,200")) :NEWLINE
         ((:AREA :SHAPE "rect" :COORDS "100,100,100,200")))))) :NEWLINE
     (:ABBR "WWW") " is an abbreviation" :NEWLINE
     (:B "force") :NEWLINE
     (:PP "whitespace only")))))

(setf *test-string2*
  "<i><b id=1>text</i> more text</b>
   <!doctype this is some text>
   <![if xxx]>
   <i><b>text</i></b> more text
   <b>text<p>more text</b> yet more text</p>
   <ul><li><b>text<li>more text</ul></b>
   prev<b><a href=foo>bar</a>baz</b>
   <b>foo<a>bar</a>baz</b>
   <b>foo<a>bar</b>baz</a>
   <b>foo<script>bar</script><a>baz</a></b>
   <b>foo<i>bar</i>baz</b>
   <script a=b> some text if (year < 1000) year += 1900; more text </script>
   <script a=b></script>
   <frameset><frame foo><frame bar></frameset>"
  )

(setf *expected-result2*
  '((:i ((:b :id "1") "text")) ((:b :id "1") " more text")
    (:!doctype "this is some text")
    (:! "[if xxx]")
    (:i (:b "text")) (:b) " more text"
    (:b "text") (:p (:b "more text") " yet more text")
    (:ul (:li (:b "text")) (:li (:b "more text"))) (:b)
    "prev" (:b ((:a :href "foo") "bar") "baz")
    (:b "foo" (:a "bar") "baz")
    (:b "foo") (:a (:b "bar") "baz")
    (:b "foo") (:script "bar") (:b (:a "baz"))
    (:b "foo" (:i "bar") "baz")
    ((:script :a "b") " some text if (year < 1000) year += 1900; more text ")
    ((:script :a "b"))
    (:frameset ((:frame :foo "foo")) ((:frame :bar "bar")))
    ))

;;; Comparison helpers

(defgeneric white-space-p (arg)
 (:method (arg)
  nil))

(defmethod white-space-p ((arg character))
  (whitespace-char-p arg))

(defmethod white-space-p ((arg string))
  (whitespace-string-p arg))
 
(defmethod lhtml-equal ((x t) (y t))
  (equal x y))

(defmethod lhtml-equal ((x list) (y list))
  (loop
   (cond ((and (null x) (null y))
          (return t))
         ((and x (white-space-p (first x)))
          (setq x (rest x)))
         ((and y (white-space-p (first y)))
          (setq y (rest y)))
         ((and (null x) y)
          (return (loop
                   (cond ((null y) (return t))
                         ((not (white-space-p (first y))) (return nil)))
                   (setq y (rest y)))))
         ((and x (null y))
          (return (loop
                   (cond ((null x) (return t))
                         ((not (white-space-p (first x))) (return nil)))
                   (setq x (rest x)))))
         ((not (lhtml-equal (first x) (first y)))
          (return nil))
         (t
          (setq x (rest x)
                y (rest y))))))

(defmethod lhtml-equal ((x string) (y string))
  (string= (string-trim #1='(#\space #\tab #\return #\linefeed) x)
           (string-trim #1# y)))
 #|(let ((i 0) (j 0)
        (length-x (length x))
        (length-y (length y)))
    ;; skip white space in beginning
    (loop while (and (< i length-x) (white-space-p (char x i))) do (incf i))
    (loop while (and (< j length-y) (white-space-p (char y j))) do (incf j))
    (loop
     (cond ((and (= i length-x) (= j length-y))
            (return t))
           ((and (= i length-x) (/= j length-y))
            (return (loop				; skip trailing white space in y
                     (cond ((= j length-y) (return t))
                           ((not (white-space-p (char y j))) (return nil)))	;t?
                     (incf j))))
           ((and (/= i length-x) (= j length-y))	; skip trailing white space in x
            (return (loop
                     (cond ((= i (length x)) (return t))
                           ((not (white-space-p (char x i))) (return nil)))	;t?
                     (incf i))))
           ((char/= (char x i) (char y j))
            (return nil)))
     (incf i)
     (incf j))))|#

;;; Callbacks

(defvar *callback-called* 0)
(defvar *pass*)

(defun callback-test-func (arg)
    ;; incf *callback-called* so we know exactly how many times this is called
    (incf *callback-called*)
    (cond ((= *pass* 0)
           (incf *pass*)
           (test:test t (lhtml-equal arg '((:a :name "this is an anchor") 
                                           "with some text"))))
          (t
           (setf *pass* 0)
           (test:test t (lhtml-equal arg
                                     '((:a :href "mailto:lmcelroy@performigence.com")
                                       "lmcelroy@performigence.com"))))))

(defun nested-callback (arg)
    ;; incf *callback-called* so we know exactly how many times this is called
    (incf *callback-called*)
    (cond ((= *pass* 0)
           (incf *pass*)
           (test:test t
             (lhtml-equal arg '(:pp "Navigate the site:"
                                ((:map :name "mainmap")
                                 ((:area :shape "rect" :coords "0,100,100,200"))
                                 ((:area :shape "rect" :coords "100,100,100,200")))))))
          ((= *pass* 1)
           (incf *pass*)
           (test:test t
             (lhtml-equal arg '(:pp
                                (:object
                                 (:pp "Navigate the site:"
                                  ((:map :name "mainmap")
                                   ((:area :shape "rect" :coords "0,100,100,200"))
                                   ((:area :shape "rect" 
                                     :coords "100,100,100,200")))))))))
          (t
	    (setf *pass* 0)
	    (test:test t (lhtml-equal arg '(:pp "whitespace only"))))))

(defun span-promote-callback (element)
 ;;; ((:SPAN :LANG "en-us") content) -> content
  (when (equal (html:elem-attrs element) '(:lang "en-us"))
    (values (html:elem-cont element) :append)))

(defun parse-uri-callback (element)
 ;; :href "..." => :href #u"..."
  (when-let* ((plist (html:elem-attrs element))
              (desc (html:elem-desc element))
              (indicators (html:desc-uri-attrs desc)))
    (loop
     (multiple-value-bind (attr value tail) (get-properties plist indicators)
      (if attr
          (let ((uri (and (stringp value)
                          (uri:parse-uri value :errorp nil))))
                                         ;:fragment-test #'russian-alpha-char-p))))
            (when uri
              (setf (second tail) uri))
            (setq plist (cddr tail)))
          (return)))))
  element)

(setq *error-protect-tests* t)

(defun do-tests (&key (retain-whitespace t))
  (let ((test:*break-on-test-failures* nil)
        (*pass* 0))
   (test:with-tests (:name "HTML Parser")
    (test:test t
      (lhtml-equal (parse-html *test-string2* :retain-whitespace retain-whitespace)
                   *expected-result2*))

    (setq *callback-called* 0)
    (test:test t
      (lhtml-equal (parse-html *test-string* :retain-whitespace retain-whitespace)
                   *expected-result*))
    (test:test t
      (lhtml-equal (parse-html *test-string* :retain-whitespace :newline)
                   *expected-result-newline*))
    (test:test 0 *callback-called*)

    (setq *callback-called* 0)
    (test:test t
      (lhtml-equal (parse-html *test-string* :callbacks '((:a . callback-test-func))
                                :retain-whitespace retain-whitespace)
                   *expected-result*))
    (test:test 2 *callback-called*)
    (setq *callback-called* 0)
    (test:test t
      (lhtml-equal (parse-html *test-string* :retain-whitespace retain-whitespace)
                   *expected-result*))
    (test:test 0 *callback-called*)
    (setq *callback-called* 0)
    ;; make sure function is OK arg
    ;;(setf (element-callback :a) (symbol-function 'callback-test-func))
    (test:test t
      (lhtml-equal (parse-html *test-string*
                               :callbacks `((:a . ,(symbol-function 'callback-test-func)))
                               :retain-whitespace retain-whitespace)
                   *expected-result*))
    (test:test 2 *callback-called*)
    ;; try with :callback-only t
    (setq *callback-called* 0)
    (parse-html *test-string* :callbacks '((:a . callback-test-func))
                              :callback-only t		; won't return parse output
                              :retain-whitespace retain-whitespace)
    (test:test 2 *callback-called*)

    ;; try nested callback
    (setq *callback-called* 0)
    (test:test t
      (lhtml-equal (parse-html *test-string* :callbacks '((:pp . nested-callback))
                               :retain-whitespace retain-whitespace)
                   *expected-result*))
    (test:test 3 *callback-called*)
    (setq *callback-called* 0)
    (parse-html *test-string* :callbacks '((:pp . nested-callback))
                              :callback-only t
                              :retain-whitespace retain-whitespace)
    (test:test 3 *callback-called*)
    (test:test-error (parse-html "b<a"))
    (test:test t
      (lhtml-equal (multiple-value-bind (res rogues)
                       (parse-html *test-string3* :collect-rogue-tags t
                                                  :retain-whitespace retain-whitespace)
                     (declare (ignorable res))
                     (parse-html *test-string3* :no-body-tags rogues
                                                :retain-whitespace retain-whitespace))
                   *expected-result3*))

    ;; Rewriting callbacks
    (test:test t
      (lhtml-equal (parse-html
                    "<p align='center'><span lang='en-us'>normal <b>bold</b></span><br>after linebreak</p>"
                    :callbacks '((:span . span-promote-callback))
                    :retain-whitespace retain-whitespace)
                   '(((:P :ALIGN "center") "normal " (:B "bold") :BR "after linebreak"))))

    ;; Parsing Content-Type
    (test '(:mime-type "text/html" :charset "windows-1251")
          (parse-declarations "text/html; charset=windows-1251" #\= :mime-type))
    (test #+lispworks 1251
          #+allegro   :1251
          #-(or lispworks allegro) :windows-1251
          (parse-content-type-charset "text/html; charset=windows-1251"))

    ;(format t "End test: ~s,   ~d errors, ~d successes~%"
    ;	    "parse-html" test:*test-errors* test:*test-successes*)

    (and (= 0 test:*test-errors* test:*test-unexpected-failures*)
         (/= 0 test:*test-successes*)))))


#||;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(do-tests :retain-whitespace t)

(debug-on :phtml-token)
(debug-on :phtml)

(setq lhtml (parse-html *test-string* :collect-rogue-tags t))
                        :retain-whitespace nil))

(parse-html *test-string* :callbacks '((:li . print)) :collect-data :callbacks)
(parse-html *test-string2*)

;;; Entities

(setq lhtml (phtml:parse-html "<p>Wrong entities: (1) &alpha1; (2) &#xB7Z;</p>"
 :parse-entities t :retain-whitespace nil))

(setq lhtml (phtml:parse-html "<HTML><p>One &lt; two. &alpha; &#xB7; &#945; </p></h3></HTML>"
 :parse-entities t :retain-whitespace nil))

(setq lhtml (phtml:parse-html "<a title='&lt; &alpha; &gt; &sqrt; &#xB7; &#945;'>anchor</a>"))
(setq lhtml (phtml:parse-html "<a title='a &nequiv; b'>anchor</a>"))

(setq lhtml (phtml:parse-html "&alpha; <!-- &beta; --> &equiv;"	; in comments
                              :parse-entities t))


;; Pseudo-entities
(ensure-entity-table
 `(;("sqrt" . "&radic;&oline;")
   ("sqrt" . ,(first (parse-html "&radic;&oline;" :parse-entities t)))
   ;("pe-string" . "<img class=\"glyph\" src=\"../gfx/pe-string.png\">")
   ("nequiv"  . ((:img :class "glyph" :src "help:/gfx/nequivl.png"))))
 *html-entity-to-code*)

(setq lhtml (phtml:parse-html "&alpha;&beta; &sqrt; lhtml: &nequiv;"
                              :parse-entities t))

(setq lhtml (phtml:parse-html
             "<HTML><p>pe-string: &pe-string;<br>pe-lhtml: &pe-lhtml;</p></HTML>"
             :callbacks '((:entity . print))
             :parse-entities t))

#+unused
(parse-entity "ab &gt; &#81; &#x7E;" 15 19)     ;; Shall we provide this?

#+unused
(let ((string "&alphafsaffafa;")
      (i 1))
 (with-syntax-error-location ()
   (parse-entity (lambda () (when (< i (length string))
                              (prog1 (schar string i) (incf i)))))))

;;; Unknown/unmatching tag

(phtml:parse-html
 "<table><ttt><tr><td>cell 11 <td>cell 12</fsdfsd>
         <tr><td>cell 21</td><td>cell 22</tr> </table>"
  :collect-data nil) ;:unknown-tag-action :collect 

(setq lhtml (phtml:parse-html "<p><i>iiii<b>ibibib</i>bbbbb</p><em></em>"
  :collect-rogue-tags t))


(setq lhtml (phtml:parse-html "<HTML>
 <HEAD><TITLE>Example HTML input</TITLE>
 <BODY>
 <pre>Here is some text preformated
 without  nested   tags.
 </pre>
 </HTML>"
 :retain-whitespace nil))

(setq lhtml (phtml:parse-html "<HTML>
 <HEAD><TITLE>Example HTML input</TITLE>
 <BODY>
 <pre>Here is some text preformated
 with  <a href='a.html'>nested   tags</a> inside   link.
 </pre>
 </HTML>"	:parse-entities t :retain-whitespace nil))

(setq lhtml (phtml:parse-html "<HTML>
 <HEAD><TITLE>Example HTML input</TITLE>
 <BODY>
 <P>Here is some text
 </P>
 </HTML>"	:retain-whitespace nil))

(setq lhtml (phtml:parse-html
 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML \">
 <html>
 <head><title>Example title</title>
 <script>Some script &alpha;</script>
 </head>
 <BODY>
 <P>Here is some text<br>
 with  a  <B>bold</B> word
 and
 a <A HREF=\"help.html\">link</A>
 </P>
 </html>")) ;:callback-only t ;:retain-whitespace nil 


(setq lhtml (phtml:parse-html *test-string*
                              :callbacks '((:a . parse-uri-callback))
                              :collapse-whitespace t))

(setq lhtml (phtml:parse-html #P"e:/lisp/projects/ynet/examples/hello.html"
                              :parse-entities t)) ; :collapse-whitespace nil ))

(setq lhtml (phtml:parse-html #P"e:/lisp/projects/ynet/examples/hello.html"
                              :parse-entities t)) ; :collapse-whitespace nil ))
(setq lhtml (phtml:parse-html #P"e:/lisp/projects/cl-typesetting/examples/sample.html"
                              :parse-entities t :collapse-whitespace nil))

(setq lhtml (phtml:parse-html #P"PROJECTS:yhelp;doc;ru;index.html"))
(setq lhtml (phtml:parse-html #P"PROJECTS:yhelp;doc;ru;introduction.html"))

(setq lhtml (phtml:parse-html #P"E:/projects/tournament/doc/web/ug.html"
                              :callbacks '((:br . print)
                                           (:img . print)
                                           (:span . span-promote-callback))))

(setq lhtml (phtml:parse-html #P"E:/webs/ystok/feedback.html"
                              :callbacks '((:br . print)
                                           (:img . print)
                                           (:span . print))
                              :callback-only t))
(setq lhtml (phtml:parse-html #P"e:/lisp/projects/ynet/help/webs/ru/test1.html"
                              :parse-entities t)) ; :collapse-whitespace nil ))
(setq lhtml (phtml:parse-html #P"e:/lisp/projects/ynet/help/webs/ru/idx_A.html"
                              :parse-entities t)) ; :collapse-whitespace nil ))


(terpri *standard-output*)
;(let ((html::*lhtml-safe* t))
(html:html-print-list lhtml *standard-output*)
(html:html-print lhtml *standard-output*)

(collapse-whitespace " ab   cd  ")
(collapse-whitespace " 
  ")

(code-char 160)
;(dolist (x *in-line*) (when (tag-no-end x) (print x)))
(defun whitespace-token-p (string)
  (declare (optimize (speed 3) (debug 0) (safety 0) #+lispworks (hcl:fixnum-safety 0))
           (type string string))
  (every (lambda (char) (char-characteristic char char-spacechar)) string))

||#
