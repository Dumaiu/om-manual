;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Ystok-URI (RFC3986) - ASDlite/ASDF system definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2002-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;; Based on:	uri.lisp 1.0.1	(c) 2001 by Jochen Schmidt
;;;      	uri.cl 1.1	(c) 1999-2001 Franz Inc, Berkeley, CA
;;; RFC: http://www.ietf.org/rfc/rfc3986 (and obsolete rfc2986, rfc2396)

(in-package :cl-user)

(asdf:defsystem ystok-uri
  :version "2.0.007"
  :description "Portable Universal Resource Indentifier library"
  :maintainer "Dmitry Ivanov"
  :licence "LLGPL"
  :depends-on (ystok-library)
  #+asdlite #+asdlite
  :output-pathname (asd:current-location "bin/")
  :serial t
  :components
  ((:file "package")
   (:file "uri")
   (:file "utils")))


#|
;;; Test system definition borrowed from puri
;;; copyright (c) 2003-2006 Kevin Rosenberg

(asdf:defsystem ystok-uri-test
  :depends-on (ystok-uri :acl-compat)
  :in-order-to ((asdf:compile-op (asdf:feature :tester)))
  :components
  ((:file "test")))

(defmethod asdf:perform ((o asdf:test-op) (s (eql (asdf:find-system 'ystok-uri-test))))
  (or (funcall (intern (symbol-name '#:do-tests) (find-package :ystok.uri.test)))
      (error "test-op failed")))

(defmethod asdf:perform ((o asdf:test-op) (s (eql (asdf:find-system 'ystok-uri))))
  (asdf:operate 'asdf:load-op 'ystok-uri-test)
  (asdf:operate 'asdf:test-op 'ystok-uri-test))

;(asdf:operate 'asdf:load-op 'ystok-uri)
;(asdf:operate 'asdf:test-op 'ystok-uri)
|#
