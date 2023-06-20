;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local-Time output

(in-package :html)

;; Keep the functions for the symbols oftenly used in templates
(yl:keep-symbol-names '(lt:current-year lt:current-date lt:localtimestamp
                        lt:d-format-to-string lt:d-long-princ-to-string
                        lt:ts-format-to-string
                        lt:time-component
                        lt:d-interval-princ-to-string)
                      :function)

(defmethod html-princ-to-string ((object lt:date))
  (lt:d-format-to-string object :format :dmy :separator #\. :century-digits 4))
(defmethod html-princ-to-string ((object lt:local-time))
  (lt:ts-format-to-string object :date-format :dmy :date-separator #\. :century-digits 4
                          :time-elements 2))

;(html-princ-to-string (lt:current-date))
