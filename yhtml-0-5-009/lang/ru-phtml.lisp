;;; -*- Mode: LISP; Encoding: (win32:code-page :id 1251); -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitry Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Russian localization strings for parser

(in-package :ystok.html.parser)

;;; phtml.lisp

(defnative "End of input stream after the HTML character reference marker  &~a"
	   "����������� ����� �������� ������ ����� ������� HTML-������ &~a")

(defnative "No semicolon found after HTML character reference starting  &~a..."
           "�� ������� ����� � �������, ����������� HTML-������  &~a...")

(defnative "Replace the entire character reference by space."
           "���������� ������ ������ HTML-������.")

(defnative "Non ~:[decimal~;hexadecimal~] digit '~a'~
					in HTML numeric character reference  &~a;"
     "����� '~a' �� �������� ~:[10~;16~]-������ � �������� ���� HTML-������  &~a;")

(defnative "Unknown HTML character entity name  &~a;"
           "����������� ��� HTML-������  &~a;")

(defnative "Completely ignore the (pseudo-)entity."
           "���������� (������-)HTML-������, �������������� �.")

(defnative "In attrubute value, wrong pseudo-entity substituion ~s."
           "� �������� �������� ������ ������-HTML-������ �� ��������� ����������� ~s.")

(defnative "Skip the tag."
           "���������� ������������� ���.")

(defnative "Unknown HTML tag <~:[~;/~]~a>"
           "��������� ������������� HTML ��� <~:[~;/~]~a>")

(defnative "Unexpected end of input stream encountered~@[ started by ~s~]."
           "����������� ����� �������� ������~@[ ������� � ~s~].")

(defnative "Start tag is not found for </~a>"
           "�� ������ ��������� ���, ��������������� ��������� </~a>")

(defnative "Unexpected </~a> mismathes the current tag <~a>~@[ (missing </~a>)~]."
           "�������� ��� </~a> �� ������������� �������� <~a>~@[ (�������� </~a>)~].")

;;; lhtt.lisp

(defnative "** ~?~@[~%Line: ~s~]"
           "** ~?~@[~%������: ~s~]")

(defnative "EOF while reading pseudo-tag expression <!-- ... ~a -->"
           "����� �������� ������ ��� ������ ��������� ���������� <!-- ... ~a -->")

(defnative "Unexpected pseudo-tag ~:[/~;~]TMPL_~a"
           "����������� ��������� ~:[/~;~]TMPL_~a")

(defnative "Unexpected EOF - missing pseudo-tag <!-- /TMPL_~a -->"
           "����� �������� ������ - �� ��������� ��������� <!-- /TMPL_~a -->")
