;;; -*- Mode: LISP; Encoding: (win32:code-page :id 1251); -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitry Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Russian localization strings for parser

(in-package :ystok.html.parser)

;;; phtml.lisp

(defnative "End of input stream after the HTML character reference marker  &~a"
	   "Неожиданный конец входного потока после маркера HTML-литеры &~a")

(defnative "No semicolon found after HTML character reference starting  &~a..."
           "Не найдена точка с запятой, завершающая HTML-литеру  &~a...")

(defnative "Replace the entire character reference by space."
           "Подставить пробел вместо HTML-литеры.")

(defnative "Non ~:[decimal~;hexadecimal~] digit '~a'~
					in HTML numeric character reference  &~a;"
     "Цифра '~a' не является ~:[10~;16~]-ричной в цифровом коде HTML-литеры  &~a;")

(defnative "Unknown HTML character entity name  &~a;"
           "Неизвестное имя HTML-литеры  &~a;")

(defnative "Completely ignore the (pseudo-)entity."
           "Пропустить (псевдо-)HTML-литеру, проигнорировав её.")

(defnative "In attrubute value, wrong pseudo-entity substituion ~s."
           "В значении атрибута вместо псевдо-HTML-литеры не допустима подстановка ~s.")

(defnative "Skip the tag."
           "Пропустить нестандартный тег.")

(defnative "Unknown HTML tag <~:[~;/~]~a>"
           "Обнаружен нестандартный HTML тег <~:[~;/~]~a>")

(defnative "Unexpected end of input stream encountered~@[ started by ~s~]."
           "Неожиданный конец входного потока~@[ начиная с ~s~].")

(defnative "Start tag is not found for </~a>"
           "Не найден начальный тег, соответствующий конечному </~a>")

(defnative "Unexpected </~a> mismathes the current tag <~a>~@[ (missing </~a>)~]."
           "Конечный тег </~a> не соответствует текущему <~a>~@[ (пропущен </~a>)~].")

;;; lhtt.lisp

(defnative "** ~?~@[~%Line: ~s~]"
           "** ~?~@[~%Строка: ~s~]")

(defnative "EOF while reading pseudo-tag expression <!-- ... ~a -->"
           "Конец входного потока при чтении выражения псевдотега <!-- ... ~a -->")

(defnative "Unexpected pseudo-tag ~:[/~;~]TMPL_~a"
           "Неожиданный псевдотег ~:[/~;~]TMPL_~a")

(defnative "Unexpected EOF - missing pseudo-tag <!-- /TMPL_~a -->"
           "Конец входного потока - не обнаружен псевдотег <!-- /TMPL_~a -->")
