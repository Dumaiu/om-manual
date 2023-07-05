;;; -*- Mode: LISP; Encoding: (win32:code-page :id 1251); -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Russian localization strings for generator

(in-package :html)

(defnative "The external format or charset ~s is illegal or not supported."
	   "Спецификация кодировки ~s некорректна или не поддерживается.")
(defnative "Charset ~s is not supported as a file external format designator."
	   "Кодировка ~s не поддерживается в качестве спецификатора формата файла.")

;;; htmlgen.lisp

(defnative "Unknown HTML tag keyword ~s~@[ in form ~s~]"
           "Неизвестный ключ HTML-тега ~s ~@[ в Лисп-форме ~s~]")



