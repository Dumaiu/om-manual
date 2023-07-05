;;; -*- Mode: LISP; Syntax: COMMON-LISP; Encoding: Windows-1251 -*-
;(defpackage :user (:use :htmlgen :net.html.parser))

;(yl:debug-on :phtml) (yl:debug-off :phtml)
;(setq *break-on-warnings* nil)
(setf (html:html-mode) :xml)
(setq html::*default-include-pathname* #P"E:/Lisp/Test/"
      htt:*default-template-pathname* html::*default-include-pathname*)

(defun simple-table-a (&optional (p (lw:current-pathname "table-a.html")))
  (with-open-file (stream p :direction :output  :if-exists :supersede)
    (html:with-html-stream (stream)
	 (:html
          (:head (:title "Test Table"))
          (:body (:table 
                  (:tr (:td "0") ((:td :rowspan 2) (:p "0" :br "000")))
                  (:tr (:td "1")) ; (:td "1"))
                  (:tr (:td "2") (:td "4"))
                  (:tr (:td "3") (:td "9"))
                  (:tr (:td "4") (:td "16"))
			  (:tr (:td "5") (:td "25"))))))))
;(simple-table-a (merge-pathnames "table-a.html" html::*default-include-pathname*))

(defun simple-table-b (&optional (p (lw:current-pathname "table-b.html")))
  (html:with-html-file (p)
    (:html
     (:head (:title "Test Table - with-html-file"))
     (:body ((:table :border 2)
             (:tr (:td "0") ((:td :rowspan 2) (:p "0" :br "000")))
             (:tr (:td "1")) ; (:td "1"))
             (:tr (:td "2") (:td "4"))
             (:tr (:td "3") (:td "9"))
             (:tr (:td "4") (:td "16"))
             (:tr (:td "5") (:td "25")))))))

;(simple-table-b)

(defun simple-table-c (count border-width backg-color border-color
                       &optional (p (lw:current-pathname "test.html")))
  (html:with-html-file (p :mode :xml :prologue t)
    (:html
     (:head (:title (:princ-safe "Проверка на русский > Контакты"))
              ((:meta :HTTP-EQUIV "Content-Type"
                :CONTENT "text/html; charset=windows-1251"))
              ((:meta :HTTP-EQUIV "Content-Language" :CONTENT '|ru|))
              ;((:link :rel "stylesheet" :type "text/css" :href "main.css"))
       )
     (:body ((:table border border-width
              bordercolor  border-color
              bgcolor backg-color
              cellpadding 3)
             (:tr ((:td bgcolor "blue") 
                     ((:font :color "white" :size "+1")
                      (:princ-safe "число&'\"<>")))
              ((:td bgcolor "blue") 
               ((:font :color :white :size "+1")
                "квадрат"))
              )
             (dotimes (i count)
               (html:html (:tr (:td (:princ i))
                           (:td (:princ (* i i)))))))))))
;(simple-table-c 10 2 :yellow :blue)

(html:with-html-stream (*standard-output*)
  ((:table :border 2)
   (:tr ((:th :colspan 2)
         (:i "The square of four integers")))
   (dotimes (i 4)
     (html:htm
      (:tr (:td (:princ i))
           (:td (:b (:princ (* i i)))))))))


;(let ((*print-readably* t))
(html:with-html-stream (*standard-output*)
  (:html (:format "~a" 234)))
(html:with-html-stream (*standard-output*)
  (:html
   ((:A :HREF (write-to-string #u"help.html" :escape nil :readably t)
        :alt #\Y)
    "link" :nbsp "suffix" (:princ #\Z) (:escape "АБВГД-ru"))
) )

(html:with-html-stream (*standard-output*)
  (:html
   (:p "Header")
   (:include "test-included.htm")
   (:p "Footer")))
(html:with-html-stream (*standard-output*)
  (:html
   (:p "Header")
   (:include "test-included.lhtm")
   (:p "Footer")))

(html:with-html-stream (*standard-output*)
  ((:option :selected t))
  ((:option :selected nil)))

(html:with-html-stream (*standard-output*)
  (html:option (:selected t))
  (html:option (:selected nil)))

(html:with-html-stream (*standard-output*)
  (:html (:comment "#include virtual=\"/ssi/test.htm\" ")))

(html:html-stream *standard-output*
  (:B ((:FONT :NAME "ARIAL")))
  (:P ((:FONT :NAME "ARIAL") (:B "Hello")))
  (:B ((:FONT :NAME "ARIAL") "Goodbye")))

;; :optional

(let ((name "anchor")
      (uri "/a.html")
      (text "Link augmented by scheme and authority"))
 (html:with-html-stream (*standard-output*)
  ((:a :name (:optional "my-" name)) "Anchor augmented by prefix: " (:princ name))
  ((:a :href (:optional "http://my-site" uri)) (:princ text))))

;;; SUBST
;;; Если значение атрибута desc-name-attr связано в subst,
;;; то вместо целого элемента подставляется другая форма,
;;; заданная в subst в виде LHTML или ф-ции одного аргумента - потока.
;;;
;;; Если значение атрибута :iter связано в subst,
;;; то вместо печати всего целого елемента вызывается ф-ция
;;; трёх аргументов: (form subst stream), - заданная в subst.

(setq subst
 `(;nil 							; ничего не подменять
   (id-str   . "replacing string")				; подменить на строку
   (id-lhtml . ((:span :lang "ru") "replacing span element"))	; подменить на др.форму
   (id-fn    . ,(lambda (stream)
                  (format stream "<b>Replacing call of function object</b>")))
   (id-href  . "img replacement")
   (iter-span . ,(lambda (form subst stream)
                   (format stream "<span>Iterated.. span</span>")))
   (iter-a    . ,(lambda (form subst html:*html-stream*)
                   (html:a (:href (yl:string-append "http://my-host"
                                                    (getf (html:elem-attrs form) :href)))
                     (html:html-print "Iterated..")
                     (html::html-print-list-subst (html:elem-cont form)
                                                  subst html:*html-stream*))))
   (("foo" . "FOO") ("bar" . "BAR"))
) )

(html::html-print-subst 
 '(:p
   "Replace Entire Element Relying on name-attr id" :br :newline
   "String: "   ((:span :id id-str)   "..replaced element body..") #\Space
                ((:img :id id-str :title "..replaced element title..")) :br :newline
   "LHTML: "    ((:span :id id-lhtml) "..replaced element body..") :br :newline
   "Function: " ((:span :id id-fn)    "..replaced element body..") :br :newline
    
   "Replace Entire Element Relying on :iter var" :br :newline
   "span: " ((:span :iter iter-span) "..replaced span body..") :br :newline
   "a: "    ((:a :href "/my-dir/my.file" :iter iter-a)
             "link "
             ((:span :id "foo") "..replaced foo..")
             ((:span :id "bar") "..replaced bar.."))
  )
  subst *standard-output*)


(html:with-html-file ((lw:current-pathname "test.html"))
   (html:html-print lhtml))

(terpri *standard-output*)
(html:html-print-list lhtml *standard-output*)

;;;;;;;;;;;;;;;;;;;;;;;;;  LHTT Pseudo-Tags  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((html-template:*template-environment*
       '((bold . t) (style . "text-align:center") (color . :blue))))
  (html:html-print 
   '((:div :style (:optional style))
     (:if bold ((:b "Bold")) ((:i "Italics")))
     ;(:unless bold (:tt "not Bold->tt"))
     (:cond ((eq color :red) ((:tt :style "color:red") color #\Space (:eval color)))
            ((eq color :blue) ((:tt :style "color:blue") color #\Space (:eval color)))
            (t (:tt "Unknown color"))))
   *standard-output*))

(dolist (htt:*template-environment* '(((list . (1 2 3)))
                                      ()))
 (html:html-print 
  '(:table
    ((:for i list)
     ((:tr ((:td :colspan "2") "no data")))		; else-body
     (:tr (:td (:eval i)) (:td (:eval (+ i 10))))))
  *standard-output*)))

(dolist (htt:*template-environment* '(((list ((foo . 1) (bar . 2))
                                             ((foo . 10) (bar . 20))))
                                      ()))
 (html:html-print 
  '(:table
    (:loop list
     ((:tr ((:td :colspan "2") "no data")))		; else-body
     (:tr (:td (:eval foo)) (:td (:eval bar)))))
  *standard-output*))

(let ((html-template:*template-environment*
       '((count . 3) (uri . "/a.html") (text . "Link augmented by scheme and authority"))))
  (html:html-print 
   '(:table
     ((:let i 0)
      (:repeat count
       (:tr (:td (:eval (setq i (1+ i))))
            (:td ((:a :href (:optional "http://my-site" uri)) (:eval text)))))))
   *standard-output*))

(let ((html-template:*template-environment*
       '((rows . (((uri . "a.html") (text . "Link > to a"))
                  ((uri . #u"b.html") (text . "Link > to b"))
                  ((uri . nil) (text . "This is an anhcor")))))))
  (html:html-print 
   '(:table
     (:loop rows
      ((:tr ((:td :colspan "2") "no data")))		; else-body
      (:tr (:td ((:a :name (:optional name) :href (:optional uri))
                 (:eval text))))))
   *standard-output*))

		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PARSING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(phtml:parse-html "<HTML></HTML>" :collect-rogue-tags t)

(setq lhtml (phtml:parse-html
 "<HTML>
<HEAD>
<TITLE>Example HTML input</TITLE>
<BODY>
<P>Here is some text with a <B>bold</B> word<br>and a <A HREF=\"help.html\">link</P>
</HTML>" :collect-rogue-tags t))

(setq s  "<HTML>
<HEAD>
<TITLE>Пример на русском</TITLE>
<BODY>
<P>Here is some text with a <B>bold</B> word<br>and
  a <A HREF=\"http://nowhere.org/help.html\">link&nbsp;suffix</P>
</HTML>"
      lhtml (phtml:parse-html s :parse-entities t))

(setq shtt  "<HTML>
<head><title>Пример SSI</title></head>
<body>
<p>Included content is below</p>
<!--#include virtual=\"/ssi/test.htm\" -->
<p>Included content is above</p>
</body></html>"
      lhtml (phtml:parse-html shtt :parse-entities t))


(setq lhtml (phtml:parse-html #P"E:/home/www/ystok/lisp/index.html"))
(when (eq (caar lhtml) :!DOCTYPE)
  (setq lhtml (second lhtml)))

(pprint lhtml)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TEMPLATE -> LHTML  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq *template-package* (find-package :cl-user))
(setq htt:*ignore-empty-lines* t)	; affects html:parse-htt

(setq l (html::%parse-eval-substrings "abc <!-- TMPL_EVAL foo --> ghi" t))
(setq l (html::%parse-eval-substrings "abc {foo} ghi" nil))

(html::%collect-eval-substrings text l nil "--test--")

(setq htt
 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html>
<!-- TMPL_LET (foo 5) (bar 10) -->
 In let: foo <!-- TMPL_EVAL foo -->, bar {bar}.
<!-- /TMPL_LET -->
</html>")
(setq lhtml (phtml:parse-html htt))
(setq lhtt (html:parse-htt lhtml))
(html:html-print-list lhtml *standard-output*)
(html:lhtt-print-list lhtt () *standard-output*)

(setq htt
 "<html>
<pre>
 ABCD
<!-- tmpl_if foo -->{bar}
<!-- tmpl_else -->{baz}
<!-- /tmpl_if -->
</pre>
</html>")
(setq lhtml (phtml:parse-html htt :retain-whitespace :newline))
(setq lhtt (html:parse-htt lhtml))
(setq ab (first (phtml:parse-html "&alpha;&beta;" :parse-entities t))
      alpha (schar ab 0))
(html:lhtt-print-list lhtt '((foo . t) (bar . "EINS") (baz . "ONE")) *standard-output*)
(html:lhtt-print-list lhtt '((foo . t) (bar . "ОДИН") (baz . "ONE")) *standard-output*)
(html:lhtt-print-list lhtt `((foo . nil) (bar . ,ab) (baz . ,alpha)) *standard-output*)

(setq htt
 "<html lang='<!-- TMPL_EVAL lang -->'>
<!-- tmpl_if lang -->
 <!--tmpl_if bar -->1<!-- tmpl_else -->2<!--/tmpl_if -->
<!-- tmpl_else -->
 <!-- tmpl_if baz -->3<!-- tmpl_else -->4<!-- /tmpl_if -->
<!-- /tmpl_if -->
</html>")
(setq lhtml (phtml:parse-html htt :retain-whitespace :newline))
(setq lhtt (html:parse-htt lhtml))
(html:lhtt-print-list lhtt '((lang . "de") (bar . "EINS") (baz . "ONE")) *standard-output*)

(setq htt
 "<html>
<head>
<meta http-equiv=\"Content-Language\" content=\"{lang}\">
</head>
<!-- tmpl_if (eq lang :de) -->Eins, zwei - {lang}.
<!-- tmpl_elif (eq lang :ru) -->Один, два - <!-- tmpl_eval lang -->.
<!-- tmpl_else -->One, two - default.
<!-- /tmpl_if -->
</html>")
(setq lhtml (phtml:parse-html htt :retain-whitespace :newline))
(setq lhtt (html:parse-htt lhtml))
(html:lhtt-print-list lhtt '((lang . :de)) *standard-output*)

(setq htt
 "<table>
 <!-- TMPL_FOR i seq -->
<tr><td><!-- TMPL_EVAL i --></td><td>{(+ i 10)}</td></tr>
 <!-- TMPL_FORELSE else body -->
<tr><td colspan='2'>No data.</td></tr>
 <!-- /TMPL_FOR -->
</table>")
(setq lhtml (phtml:parse-html htt :retain-whitespace :newline))
(setq lhtt (html:parse-htt lhtml))
(setq env '((seq . (1 2 3))))
(setq env '((seq . #(1 2 3))))
(setq env '((seq . 3)))
(setq env ())
(setq env '((seq . 3.2)))
(html:lhtt-print-list lhtt env *standard-output*)
(with-output-to-string (htt:*default-template-output*)
  (htt:fill-and-print-template htt env))

(setq htt
  "<html>
<!---->
<ul><!-- TMPL_LOOP rows -->
   <li><a href='{foo}'><!-- TMPL_EVAL bar --></a>,
   <span class='{class1} widget {class2}'><b>Bold</b></span> {(list baz 1)}</li>
<!-- /TMPL_LOOP --></ul>
</html>")
(setq lhtml (phtml:parse-html htt :retain-whitespace :newline))
(setq lhtt (html:parse-htt lhtml))
(html:lhtt-print-list lhtt
  '((class1 . "CL1") (class2 . "CL2")
    (rows . (((foo . "a.html") (bar . "EINS") (baz . "ONE"))
             ((foo . #u"b.html") (bar . "ZWEI") (baz . "TWO")))))
  *standard-output*)

(setq htt
  "<html>
<ul>
<!-- TMPL_LOOP rows -->
 <li><a href='{foo}'>{bar}</a>,
   <span class='<!-- TMPL_EVAL class1 --> widget <!-- TMPL_EVAL class2 -->'>List: </span> 
   <!-- TMPL_EVAL (list baz 1) --></li>
<!-- TMPL_LOOPELSE -->
 <li>No data.</li>
<!-- /TMPL_LOOP -->
</ul>
</html>")
(setq lhtml (phtml:parse-html htt :retain-whitespace :newline))
(setq lhtt (html:parse-htt lhtml))
(html:lhtt-print-list lhtt
  '((class1 . "CL1") (class2 . "CL2")
    (rows . (((foo . "a.html") (bar . "EINS") (baz . "ONE"))
             ((foo . #u"b.html") (bar . "ZWEI") (baz . "TWO")))))
  *standard-output*)
(html:lhtt-print-list lhtt '((rows . ())) *standard-output*)

(macroexpand '(html:htm ((:include 's :b 'num (+ 10 5)) "test-included.htt")))

(html:with-html-stream (*standard-output*)
  (:html
   (:p "Header")
   ((:include 's :b 'num (+ 10 5)) "test-included.htt")
   ((:include :s :i :num (+ 100 50)) "test-included.htt")
   (:p "Footer")))

(setq htt
  "<html>
<p>Header</p>
<!-- TMPL_INCLUDE test-included.htt (s :b) (num (+ 10 n)) -->
<!-- TMPL_INCLUDE \"test-included.htt\" (s :i) (num (+ 100 n)) -->
<!-- TMPL_INCLUDE 'test-included.htt' (s :b) (num (+ 1000 n)) -->
<!-- MPL_INCLUDE\"test-included.htt (s :i) (num (+ 10000 n)) -->
<p>Footer</p>
</html>")
(setq lhtml (phtml:parse-html htt :retain-whitespace :newline))
(setq lhtt (html:parse-htt lhtml))
(html:lhtt-print-list lhtt '((n . 50)) *standard-output*)
(html:lhtt-print-list lhtt '((n . ())) *standard-output*)

;;; Neither TMPL_EVAL nor {} is expanded within ordinary comments

(setq htt "<html><!-- prefix {abc} suffix--></html>")
(setq lhtml (phtml:parse-html htt))
(setq lhtt (html:parse-htt lhtml))

;;; Within SSI directives {} are exapanded but TMPL_EVAL are not.
(setq shtt
 "<html><head><title>SSI test</title></head>
<p class=\"{class}\">Template within attibute is OK.</p>
<p>SSI instruction for including {file} verbatim:</p>
<!--#include virtual=\"/ssi/test1.htm\" -->
<p>Template in braces within comment SSI is acceptable!</p>
<!--#include file=\"{file}\" -->
<p>But template in comment-like markers is unacceptable!</p>
<!--#include virtual=\"<!-- TMPL_EVAL file -->\" -->
<p>Included content is above</p>
</html>")
(setq lhtml (phtml:parse-html shtt))
(setq lhtt (html:parse-htt lhtml))
(html:lhtt-print-list lhtt '((class . "CL1") (file . "/ssi/test2.htm"))
                      *standard-output*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PHP  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq php
"<?php ## Скрипт, имеющий уязвимость в защите. 
if($fname = @$_REQUEST['fname'])
{ $file = \"$type.txt\"; 
  if( $f = @fopen($file, \"r\") ) $content = fread($f, filesize($file)); 
} 
?> 
<html> 
<body> 
<form name=\"<?php echo $_ENV['SCRIPT_NAME']?>\"> 
Показать новости: 
<select name=\"fname\"> 
 <option value=\"site\">Местные</орtion> 
 <option value=\"world\">Mировые</option> 
</select><br /> 
<input type=submit value=\"Просмотреть\" /> 
</form> 
<?php if (@$content) echo '<hr />$content' ?> 
</body> 
</html>")
(setq lhtml (phtml:parse-html php :retain-whitespace nil))
(let ((html::*html-mode* :xml))
  (html:html-print-list lhtml *standard-output*))

(setq phtt
 "<html><head><title>PHP and HTML-Template test</title></head>
<p align=\"<!-- TMPL_EVAL align -->\" font-size=\"<?php echo font_size(); ?>\">
Align: <!-- TMPL_EVAL align -->, font-size: <?php echo font_size(); ?></p>
</html>")
(setq lhtml (phtml:parse-html phtt :retain-whitespace nil))
(setq lhtt (html:parse-htt lhtml))
(let ((html::*html-mode* :xml))
  (html:lhtt-print-list lhtt '((align . "center")) *standard-output*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BBCODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq r (lw:precompile-regexp "\\[[^]]*\\]"))
(lw:find-regexp-in-string "\\[[^]]*\\] " "This is [b]bold[/b] text")
(lw:split-sequence '(#\Space #\Tab) "url  address" :coalesce-separators t)

(setq s "This is a [b]bold [i]italic[/i][/b]
message")
(setq s "This is a [url]http://myserver.ru/[/url] and rest")
(setq s "This is a [url=http://myserver.ru/]link[/url] and rest")
(setq s "This is a [img]http://myserver.ru/myimg.jpg[/img] and rest")
(setq s "This is a broken [b]paragraph[/b]
[i]
with[/i] [img]http://myserver.ru/myimg.jpg[/img] image.
This is the second paragraph.
")

(html::preparse-bbcode s)
(html::parse-bbcode s)
(html:bbcode-output-html s)
(html:bbcode-html-string s)
(html:bbcode-lhtml s)

(funcall html-template:*string-modifier* s)
(htt:escape-string s)
(yl:split "\\[[^[]*\\]" "012[]56" :with-registers-p t)
(yl:split "\\[[^[]*\\]" s :with-registers-p t)
(yl:split " \\\|=" "foo=bar")

(ppcre:split "\\s+" s)
(ppcre:split "\\n" s)
(ppcre:split "(\\[[^[]*])" s :with-registers-p t)

(setq s "aaaaaaa bbbbbbbbbb cccccccc dd[/b] eeee ffffffffffffffffff ggggggggggggggggggggggggg  hhhhhhhhhhhhhh. iiiiiiiiiiiiiiiiiiiiiiiii jjjjjjjjjjjjjjjjjjjjj. kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk lllllll. mmmmmmmmmmmmmmmmmmmmmmmmm nnnnnnnnn")

(lw:find-regexp-in-string "\\[[^[]*\\]" s)				;=> 30,4
(lw:find-regexp-in-string "\\[[^[]*\\]" (lw:string-append s "1")	;=> signals!

#||;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq yl::*debug-stream* *standard-output*)
(setq html:*lhtml-xhtml* t)
(setq stm (open (lw:current-pathname "html-test" "html") :direction :output
                :element-type '(unsigned-byte 8)
                ;:external-format :unicode  ; win32:*multibyte-code-page-ef*
                ))
(setq ef (stream-external-format stm))
(setq cp (external-format::ef-coded-character-set ef))
(ef-type-type ef)			;=> simple-char
(ef:external-format-type ef)		;=> simple-char
(ef:external-format-foreign-type ef)	;=> (unsigned-byte 8) or (unsigned-byte 16)

(html::escape-char-not-ef #\Щ)
(html::escape-char-not-ef #\Щ :latin-1)
(html::escape-char-not-ef (code-char #xEB))
(html::escape-char-not-ef (code-char #xEB) :latin-1)
(html::escape-char-not-ef (code-char #xEB) :ascii)
(html::escape-char-not-ef (code-char 8334))

(write-string "abcd" stm :end nil)
(write-string "абвг" stm)
(write-char #\b stm)
(write-char #\б stm)
(write-html-string "абвг" stm)
(ef:char-external-code #\Ф :unicode) ;=> 1060 = (char-code #\Ф)
(char-code #\Ф)
(close stm)

(phtml::whitespace-string-p "
 ")
(html:tag-standard-p :nbsp)
(html:tag-standard-p :!doctype)
||#
