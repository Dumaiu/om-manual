;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; Ystok-URI (RFC3986) - Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2002-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;; copyright (c) 1999-2001 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; Original version from ACL 6.1:
;; t-uri.cl,v 1.3.6.3.2.1 2001/08/09 17:42:43 layer

;;; Notes:
;;; 1. According to RFC3986, more flexibility is allowed than with RFC2396.
;;;    Some old "known failures" do not signal now.
;;; 2. Unsupported features: URN, interning URI.

;(pushnew :tester *features*)
;(load #P"PROJECTS:ynet;acl-compat;defsys")
;(compile-system 'ACL-COMPAT :load t)

(defpackage :ystok.uri.test
 (:use :common-lisp :excl :uri)	;:test
 (:export #:do-tests))

(in-package :ystok.uri.test)

(defmacro gen-test-forms ()
  (let ((res ())
	(base-uri "http://a/b/c/d;p?q"))

    (dolist (x `(;; tuple ::= (relative-uri result base-uri)
;;;; RFC Appendix C.1 (normal examples)
		 ("g:h" "g:h" ,base-uri)
		 ("g" "http://a/b/c/g" ,base-uri)
		 ("./g" "http://a/b/c/g" ,base-uri)
		 ("g/" "http://a/b/c/g/" ,base-uri)
		 ("/g" "http://a/g" ,base-uri) 
		 ("//g" "http://g" ,base-uri) 
		 ("?y" "http://a/b/c/d;p?y" ,base-uri) 
		 ("g?y" "http://a/b/c/g?y" ,base-uri)
		 ("#s" "http://a/b/c/d;p?q#s" ,base-uri) 
		 ("g#s" "http://a/b/c/g#s" ,base-uri) 
		 ("g?y#s" "http://a/b/c/g?y#s" ,base-uri)
		 (";x" "http://a/b/c/;x" ,base-uri) 
		 ("g;x" "http://a/b/c/g;x" ,base-uri) 
		 ("g;x?y#s" "http://a/b/c/g;x?y#s" ,base-uri)
		 ("." "http://a/b/c/" ,base-uri) 
		 ("./" "http://a/b/c/" ,base-uri) 
		 (".." "http://a/b/" ,base-uri) 
		 ("../" "http://a/b/" ,base-uri)
		 ("../g" "http://a/b/g" ,base-uri) 
		 ("../.." "http://a/" ,base-uri) 
		 ("../../" "http://a/" ,base-uri)
		 ("../../g" "http://a/g" ,base-uri)
;;;; RFC Appendix C.2 (abnormal examples)
		 ("" "http://a/b/c/d;p?q" ,base-uri) 
		 ("../../../g" "http://a/../g" ,base-uri)
		 ("../../../../g" "http://a/../../g" ,base-uri) 
		 ("/./g" "http://a/./g" ,base-uri) 
		 ("/../g" "http://a/../g" ,base-uri)
		 ("g." "http://a/b/c/g." ,base-uri) 
		 (".g" "http://a/b/c/.g" ,base-uri) 
		 ("g.." "http://a/b/c/g.." ,base-uri)
		 ("..g" "http://a/b/c/..g" ,base-uri) 
		 ("./../g" "http://a/b/g" ,base-uri) 
		 ("./g/." "http://a/b/c/g/" ,base-uri)
		 ("g/./h" "http://a/b/c/g/h" ,base-uri) 
		 ("g/../h" "http://a/b/c/h" ,base-uri) 
		 ("g;x=1/./y" "http://a/b/c/g;x=1/y" ,base-uri)
		 ("g;x=1/../y" "http://a/b/c/y" ,base-uri) 
		 ("g?y/./x" "http://a/b/c/g?y/./x" ,base-uri)
		 ("g?y/../x" "http://a/b/c/g?y/../x" ,base-uri) 
		 ("g#s/./x" "http://a/b/c/g#s/./x" ,base-uri)
		 ("g#s/../x" "http://a/b/c/g#s/../x" ,base-uri) 
		 ("http:g" "http:g" ,base-uri)

		 ("foo/bar/baz.htm#foo"
		  "http://a/b/foo/bar/baz.htm#foo"
		  "http://a/b/c.htm")
		 ("foo/bar/baz.htm#foo"
		  "http://a/b/foo/bar/baz.htm#foo"
		  "http://a/b/")
		 ("foo/bar/baz.htm#foo"
		  "http://a/foo/bar/baz.htm#foo"
		  "http://a/b")
		 ("foo/bar;x;y/bam.htm"
		  "http://a/b/c/foo/bar;x;y/bam.htm"
		  "http://a/b/c/")))
      (push `(test:test (parse-uri ,(second x))
                        (merge-uris ,(first x) ,(third x))
                        :test 'uri=)
	    res))

;;;; Parsing and equivalence tests
    (dolist (x '(;; uri-string
                 "ftp://parcftp.xerox.com/pub/pcl/mop/"
                 "http://www.verada.com:8010/kapow?name=foo%3Dbar%25"))
      (push `(test:test ,x (render-uri (parse-uri ,x))
                        :test 'string=)
            res))
    (push '(test:test (parse-uri "http://www.foo.com")
		      (parse-uri "http://www.foo.com?")		; allowed '?' at end
                      :test 'uri=)
	  res)
    (push `(test:test nil (uri= (parse-uri "foo")		; Franz's URI returns T 
                                (parse-uri "foo#")))		; but we adhere to 6.2.3
	  res)
   #|(push `(test:test
	    (parse-uri "http://foo+bar?baz=b%26lob+bof")
	    (parse-uri (parse-uri "http://foo+bar?baz=b%26lob+bof"))
	    :test 'uri=)
	  res)|#

;;; Authority
    (push `(test:test "usr"
                      (uri-user (parse-uri "http://usr:pwd@a/b/c/d;p?q"))
	              :test 'string=)
          res)
    (push `(test:test "pwd"
                      (uri-password (parse-uri "http://usr:pwd@a/b/c/d;p?q"))
	              :test 'string=)
          res)
    (push `(test:test nil
                      (uri-password (parse-uri "http://u%26sr@a/b/c/d;p?q")))
          res)
    (push `(test:test "u'sr"
                      (uri-user (parse-uri "http://u%27sr@a/b/c/d;p?q"))
	              :test 'string=)
          res)

;;;; Path
    (push `(test:test "%20" (render-uri (parse-uri "%20"))
                      :test 'string=)
	  res)
    (push `(test:test "&" (render-uri (parse-uri "%26" :decode t))
                      :test 'string=)
	  res)
    (push `(test:test "foo%23bar" (render-uri (parse-uri "foo%23bar"))
                      :test 'string=)
          res)
    (push `(test:test "http://foo/bAr;3/baz?baf=3"
                      (princ-to-string (parse-uri "http://foo/b%41r;3/baz?baf=3"))
                      :test 'string=)
	  res)
    (push `(test:test '(:absolute ("/bAr" "3") "baz")
                      (uri-parsed-path (parse-uri "http://foo/%2fb%41r;3/baz?baf=3")))
	  res)
    (push `(test:test "/%2fbAr;3/baz"
             (let ((u (parse-uri "http://foo/%2fb%41r;3/baz?baf=3")))
               (setf (uri-parsed-path u) '(:absolute ("/bAr" "3") "baz"))
               (uri-path u))
             :test 'string=
             :known-failure t
             :fail-info "We assume '/' cannot be encountered in path data even encoded.")
	  res)

;;;; Query
    (push `(test:test '(("baz" . "b&lob+bof"))
                      (uri-parsed-query (parse-uri "http://foo+bar?baz=b%26lob+bof")))
	  res)
    (push `(test:test "baz=b%26lob+bof"
                      (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof" :decode nil))
	              :test 'string=)
	  res)
    (push `(test:test "baz=b%26lob+bof"
                      (uri::render-query
                       (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof" :decode t)))
	              :test 'string=)
	  res)
    (push `(test:test "baz=b%26lob+bof%3D"
             (uri::render-query
              (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof%3d" :decode t)))
             :test 'string=)
	  res)
    (push `(test:test (parse-uri "xxx?%41") (parse-uri "xxx?A")	; parameter names
                      :test 'uri=)				; also must be decoded
     res)

    (push `(test:test "A" (uri::render-query (uri-query (parse-uri "xxx?%41" :decode t)))
                      :test 'string=)
     res)
    (push `(test:test '(("A")) (uri-query (parse-uri "xxx?%41" :decode t)))
     res)

    (push `(test:test-error (uri:parse-uri "http://foo.com/bar?a=zip|zop")
             :condition-type 'parse-error  :include-subtypes t)
     res)
    (push `(test:test-no-error (uri:parse-uri "http://foo.com/bar?a=zip|zop"
                                              :query-char-p (lambda (c) (char= c #\|))))
     res)
    ;; Allowed by RFC3986
    (push `(test:test-no-error (parse-uri "foobar??"))
                            ;:condition-type 'parse-error  :include-subtypes t)
	  res)
    (push `(test:test-no-error (parse-uri "foobar?foo?"))
                            ;:condition-type 'parse-error  :include-subtypes t)
	  res)
    (push `(test:test "foobar??"	;"foobar?%3F"
                      (princ-to-string (parse-uri "foobar?%3f"))
                      :test 'string=)
	  res)

;;;; Fragment
    (push `(test:test "foo%23bar#foobar" (render-uri (parse-uri "foo%23bar#foobar"))
                      :test 'string=)
     res)
    (push `(test:test-error (parse-uri "foo%23bar#foobar#baz")
                            :condition-type 'parse-error  :include-subtypes t)
     res)
    (push `(test:test "foo%23bar#foobar%23baz"
                      (princ-to-string (parse-uri "foo%23bar#foobar%23baz"))
                      :test 'string=)
     res)
    (push `(test:test "foo%23bar#foobar%23baz"
                      (princ-to-string (parse-uri "foo%23bar#foobar#baz"
                                         :fragment-char-p (lambda (c) (char= c #\#))))
                      :test 'string=)
     res)
    (push `(test:test "foo%23bar#foobar/baz"
                      (princ-to-string (parse-uri "foo%23bar#foobar%2fbaz" :decode t))
                      :test 'string=)
     res)

;;;; enough-uri tests
    (dolist (x `(("http://www.franz.com/foo/bar/baz.htm"
		  "http://www.franz.com/foo/bar/"
		  "baz.htm")
		 ("http://www.franz.com/foo/bar/baz.htm"
		  "http://www.franz.com/foo/bar"
		  "baz.htm")
		 ("http://www.franz.com:80/foo/bar/baz.htm"
		  "http://www.franz.com:80/foo/bar"
		  "baz.htm")
		 ("http:/foo/bar/baz.htm" "http:/foo/bar"  "baz.htm")
		 ("http:/foo/bar/baz.htm" "http:/foo/bar/" "baz.htm")
		 ("/foo/bar/baz.htm" "/foo/bar"  "baz.htm")
		 ("/foo/bar/baz.htm" "/foo/bar/" "baz.htm")
		 ("/foo/bar/baz.htm#foo" "/foo/bar/" "baz.htm#foo")
		 ("/foo/bar/baz.htm?bar#foo" "/foo/bar/" "baz.htm?bar#foo")
		 
		 ("http://www.dnai.com/~layer/foo.htm"
		  "http://www.known.net"
		  "http://www.dnai.com/~layer/foo.htm")
		 ("http://www.dnai.com/~layer/foo.htm"
		  "http://www.dnai.com:8000/~layer/"
		  "http://www.dnai.com/~layer/foo.htm")
		 ("http://www.dnai.com:8000/~layer/foo.htm"
		  "http://www.dnai.com/~layer/"
		  "http://www.dnai.com:8000/~layer/foo.htm")
		 ("http://www.franz.com"
		  "http://www.franz.com"		; DI 2009-Apr-05: 
		  "#")					;  Different from Franz's "/"
                 ("http://www.franz.com/a.html#foo"
                  "http://www.franz.com/a.html"
                  "#foo")))
      (push `(test:test (parse-uri ,(third x))
                        (enough-uri (parse-uri ,(first x))
                                    (parse-uri ,(second x)))
                        :test 'uri=)
	    res))

;;;; Errors
    (push `(test:test-error (parse-uri " ")
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-error (parse-uri "foo ")
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-error (parse-uri " foo ")
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-error (parse-uri "<foo")
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-error (parse-uri "foo>")
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-error (parse-uri "<foo>")
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-error (parse-uri "%" :decode t)
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-error (parse-uri "foo%xyr" :decode t)
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-error (parse-uri "\"foo\"")
                            :condition-type 'parse-error :include-subtypes t)
	  res)
    (push `(test:test-no-error
            (uri:parse-uri		; collect only two first elements separated by =
             "http://arc3.msn.com/ADSAdClient31.dll?GetAd?PG=NBCSBU?SC=D2?AN=1.0586041"))
     res)
    #|(push
       '(let ((uri::*strict-parse* t))
	 (test:test-error
	   (uri:parse-uri
	     "http://arc3.msn.com/ADSAdClient31.dll?GetAd?PG=NBCSBU?SC=D2?AN=1.0586041")
	   :condition-type 'parse-error :include-subtypes t)
     res)|#
    
    (push
     '(test:test-error
       (uri:parse-uri "http://scbc.booksonline.com/cgi-bin/ndCGI.exe/Develop/pagClubHome.hrfTIOLI_onWebEvent(hrfTIOLI)?selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&selGetClubOffer.TB_OFFER_ID_ITEM=34487%2e0&selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&^CSpCommand.currRowNumber=5&hrfTIOLI=The+Visual+Basic+6+Programmer%27s+Toolkit&SPIDERSESSION=%3f%3f%3f%3f%3f%5f%3f%3f%3f%40%5b%3f%3f%3f%3fBOs%5cH%3f%3f%3f%3f%3f%3f%3f%3f%3fMMpXO%5f%40JG%7d%40%5c%5f%3f%3f%3fECK%5dt%3fLDT%3fTBD%3fDDTxPEToBS%40%5f%5dBDgXVoKBSDs%7cDT%3fK%3fd%3fTIb%7ceHbkeMfh%60LRpO%5cact%5eUC%7bMu%5fyWUGzLUhP%5ebpdWRka%5dFO%3f%5dBopW%3f%40HMrxbMRd%60LOpuMVga%3fv%3fTS%3fpODT%40O&%5euniqueValue=977933764843")
       :condition-type 'parse-error  :include-subtypes t)
     res)
   #|(push
     '(let (uri::*strict-parse* nil))
       (test:test-no-error
	(uri:parse-uri "http://scbc.booksonline.com/cgi-bin/ndCGI.exe/Develop/pagClubHome.hrfTIOLI_onWebEvent(hrfTIOLI)?selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&selGetClubOffer.TB_OFFER_ID_ITEM=34487%2e0&selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&^CSpCommand.currRowNumber=5&hrfTIOLI=The+Visual+Basic+6+Programmer%27s+Toolkit&SPIDERSESSION=%3f%3f%3f%3f%3f%5f%3f%3f%3f%40%5b%3f%3f%3f%3fBOs%5cH%3f%3f%3f%3f%3f%3f%3f%3f%3fMMpXO%5f%40JG%7d%40%5c%5f%3f%3f%3fECK%5dt%3fLDT%3fTBD%3fDDTxPEToBS%40%5f%5dBDgXVoKBSDs%7cDT%3fK%3fd%3fTIb%7ceHbkeMfh%60LRpO%5cact%5eUC%7bMu%5fyWUGzLUhP%5ebpdWRka%5dFO%3f%5dBopW%3f%40HMrxbMRd%60LOpuMVga%3fv%3fTS%3fpODT%40O&%5euniqueValue=977933764843")))
     res)|#

;;;; uri-utils
    (push `(test:test nil (uri:uri-type (parse-uri "http://lisp.ystok.ru/my-dir")))
	  res)
    (push `(test:test "pdf"
                      (uri:uri-type (parse-uri "http://lisp.ystok.ru/my-doc.pdf")))
	  res)
    (push `(test:test "pdf"
                      (uri:uri-type (parse-uri "http://lisp.ystok.ru/my-doc.PDF"))
                      :test 'string-equal)
	  res)
    
    (let ((ef :utf-8))
      (push `(test:test #90="%D0%BC%D0%BE%D0%B9.%D1%80%D1%84"
                        (uri:percent-encode
                         (uri:percent-decode #90# :external-format ,ef)
                         :external-format ,ef))
            res)
      (push `(test:test #91="http://my%20name@%D0%BC%D0%BE%D0%B9.%D1%80%D1%84/my-path"
                        (uri:render-uri (uri:parse-uri #91# :decode ,ef)
                                        nil ,ef t))
            res)
    )

  #|;;;; intern tests
    (push `(test:test (intern-uri ,(second x))
                        (intern-uri (merge-uris (intern-uri ,(first x))
                                                (intern-uri ,(third x))))
                        :test 'uri=)
	    res))
    (dolist (x '(;; default port and specifying the default port are
		 ;; supposed to compare the same:
		 ("http://www.franz.com:80" "http://www.franz.com")
		 ("http://www.franz.com:80" "http://www.franz.com" eq)
		 ;; make sure they're `eq':
		 ("http://www.franz.com:80" "http://www.franz.com" eq)
		 ("http://www.franz.com" "http://www.franz.com" eq)
		 ("http://www.franz.com/foo" "http://www.franz.com/foo" eq)
		 ("http://www.franz.com/foo?bar"
		  "http://www.franz.com/foo?bar" eq)
		 ("http://www.franz.com/foo?bar#baz"
		  "http://www.franz.com/foo?bar#baz" eq)
		 ("http://WWW.FRANZ.COM" "http://www.franz.com" eq)
		 ("http://www.FRANZ.com" "http://www.franz.com" eq)
		 ("http://www.franz.com" "http://www.franz.com/" eq)
		 (;; %72 is "r", %2f is "/", %3b is ";"
		  "http://www.franz.com/ba%72%2f%3b;x;y;z/baz/"
		  "http://www.franz.com/bar%2f%3b;x;y;z/baz/" eq)))
      (push `(test:test (uri:intern-uri ,(second x))
			     (uri:ntern-uri ,(first x))
			     :test ',(if* (third x)
					then (third x)
					else 'uri=))
	    res))

    ;;;; urn tests, ideas of which are from rfc2141
    (let ((urn "urn:com:foo-the-bar"))
      (push `(test:test "com" (urn-nid (parse-uri ,urn))
			     :test #'string=)
	    res)
      (push `(test:test "foo-the-bar" (urn-nss (parse-uri ,urn))
			     :test #'string=)
	    res))
    (push `(test:test-error (parse-uri "urn:")
				 :condition-type 'parse-error)
	  res)
    (push `(test:test-error (parse-uri "urn:foo")
				 :condition-type 'parse-error)
	  res)
    (push `(test:test-error (parse-uri "urn:foo$")
				 :condition-type 'parse-error)
	  res)
    (push `(test:test-error (parse-uri "urn:foo_")
				 :condition-type 'parse-error)
	  res)
    (push `(test:test-error (parse-uri "urn:foo:foo&bar")
				 :condition-type 'parse-error)
	  res)
    (push `(test:test (parse-uri "URN:foo:a123,456")
			   (parse-uri "urn:foo:a123,456")
			   :test #'uri=)
	  res)
    (push `(test:test (parse-uri "URN:foo:a123,456")
			   (parse-uri "urn:FOO:a123,456")
			   :test #'uri=)
	  res)
    (push `(test:test (parse-uri "urn:foo:a123,456")
			   (parse-uri "urn:FOO:a123,456")
			   :test #'uri=)
	  res)
    (push `(test:test (parse-uri "URN:FOO:a123%2c456")
			   (parse-uri "urn:foo:a123%2C456")
			   :test #'uri=)
	  res)
    (push `(test:test
	    nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "urn:FOO:a123,456")))
	  res)
    (push `(test:test
	    nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    (push `(test:test
	    nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "URN:foo:a123,456")))
	  res)
    (push `(test:test
	    nil
	    (uri= (parse-uri "urn:foo:a123%2C456")
		  (parse-uri "urn:FOO:a123,456")))
	  res)
    (push `(test:test
	    nil
	    (uri= (parse-uri "urn:foo:a123%2C456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    (push `(test:test
	    nil
	    (uri= (parse-uri "URN:FOO:a123%2c456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    (push `(test:test
	    nil
	    (uri= (parse-uri "urn:FOO:a123%2c456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    (push `(test:test nil
	    (uri= (parse-uri "urn:foo:a123%2c456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    ;;;; end of intern/urn |#    
    
    `(progn ,@(nreverse res)))
)

;(setq test:*error-protect-tests* t)

(defun do-tests ()
  (let ((test:*break-on-test-failures* nil))
    (test:with-tests (:name :URI)
      (gen-test-forms)
      (and (= 0 test:*test-errors* test:*test-unexpected-failures*)
           (/= 0 test:*test-successes*)))))


#||;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(do-tests)
(setq b (parse-uri "http://a/b/c/d;p?q"))
(setq b (parse-uri "http://u%27sr@a/b/c/d;p?q"))
(render-uri b)
(setq u (merge-uris "" b))
(setq u (merge-uris "#s" b))	;"http://a/b/c/d;p?q#s"
(setq r #u"http://a/b/c/d;p?q#s")
(setq e (parse-uri "foobar?foo?"))
(setq b (parse-uri "http://a/"))
(setq e (uri:parse-uri "http://foo.bar/baz?a&b=zip|zop"))	; -> error
(setq e (uri:parse-uri "http://foo.bar/baz?a&b=zip|zop"
                       :query-char-p (lambda (char) (yl:char-position char "|"))))

(parse-uri ";x")
(uri-parsed-query e)
(setq u (uri:parse-uri
	 "http://arc3.msn.com/ADSAdClient31.dll?GetAd?PG=NBCSBU?SC=D2?AN=1.0586041"))

(setq b (parse-uri "http://my.ru/a/b/c/d.html"))
(setq b (parse-uri "http://my.ru/a/b/c/"))
(merge-uris (parse-uri "../ss") b)

(enough-uri (parse-uri "http://my.ru/ss.v") b)		;=> #<URI ../../../ss.v>
(enough-uri (parse-uri "/ss.v") b t)			;=>#<URI http://my.ru../../../ss.v>
(enough-uri (parse-uri "http://my.ru/../ss.v") b)	;=> #<URI ../../../../ss.v>
(enough-uri (parse-uri "http://my.ru/a/b/c/ss.v") b)	;=> #<URI ss.v> - OK
(setq e (enough-uri (parse-uri "/a/b/c/ss.v") b))
(setq e (enough-uri (parse-uri "/a/b/c/e/ss.v") b))

(setq u (uri:enough-uri "http://www.franz.com" "http://www.franz.com"))
(setq u (uri:enough-uri "http://www.franz.com/a.html#foo" "http://www.franz.com/a.html"))
;=> path="" instead of NIL
(setq u (parse-uri "foo#"))
(setq u (uri:enough-uri "http://www.franz.com/a.html#foo" "http://www.franz.com/a.html"))

(uri::charbitp #\/ uri::*pchar-bitmask*)
(uri::percent-decode "%26")
(uri::percent-decode "%2fb%41r;3/baz")
(setq u (parse-uri "http://foo/%2fb%41r;3/baz?baf=3" :decode t))
(setq u (PARSE-URI "http://foo+bar?baz=b%26lob+bof" :DECODE T))

(setq u2 #u"#foo")

(uri::%relative-path "/ss.v"	   (uri-path b))	;=> "../../../ss.v" - GOOD!
(uri::%relative-path "../ss.v"	   (uri-path b))	;=> error
(uri::%relative-path "/a/b/c/ss.v" (uri-path b))	;=> "./ss.v" - WHY NOT "ss.v"?
(uri::%relative-path "/a/b/c/e/ss.v" (uri-path b))	;=> "./e/ss.v"

(uri-parsed-path b)                     ;=> (:ABSOLUTE "a" "b" "c" "d.html")
(uri-parsed-path (parse-uri "/ss.vv"))	;=> (:ABSOLUTE "ss.vv")
(uri-parsed-path (parse-uri "../ss.vv"));=> (:RELATIVE ".." "ss.vv")
(uri-parsed-path (parse-uri "./ss.vv"))	;=> (:RELATIVE "." "ss.vv")
(uri-parsed-path (parse-uri "ss.vv"))	;=> (:RELATIVE "ss.vv")
(uri-parsed-path (parse-uri "foo;10/bar:x;y;z/baz.htm"))

(test:test-error (parse-uri "<foo")
                 :condition-type 'parse-error :include-subtypes t)

;;; More internal
(multiple-value-list (%meta-parse-uri "http://www.lisp.ru/test.sf#?sfsd"))
(setq s1 (copy-seq s))
(setf u (parse-uri "http://www.lisp.ru/test.sf?2423"))
(setf u (parse-uri "ftp://ftp.lisp.ru/testdir/"))
(setf u (parse-uri "ftp.lisp.ru"))
(setf u (parse-uri "file:e:/lisp/test.sf"))
(setf u #u"file:e:/lisp/test.sf")
(princ-to-string u)

(setq p (uri-parsed-path u)) ;=> (:RELATIVE "e:" "" "lisp" "test.sf") 
(setq pathname (parse-namestring (uri-path u))) ;=> :absolute !
(setf u (parse-uri "/"))
(setq  (uri-parsed-path (parse-uri "http://www.lisp.ru/test.sf")))
(%render-parsed-path nil)
(%render-parsed-path (uri-parsed-path (uri "http://www.lisp.ru/test.sf")))
(%render-parsed-path (uri-parsed-path #u"file:e:/lisp/test.sf/"))

(parse-path "/lisp/test.sf/")

(setf b (parse-uri "ftp://ftp.lisp.ru:221/dir/file.typ"))
(uri-authority b)
(uri-parsed-path #u"ftp://ftp.lisp.ru/fsdf")
(%render-parsed-path (uri-parsed-path #u"ftp://ftp.lisp.ru/fsdf"))
(setq r (parse-uri "test1.txt"))
(setq r (make-instance 'uri :path "test.sf"))
(uri-parsed-path m3)
(setq m (merge-uris r b))
(setq m2 (merge-uris "/test2.txt" b))
(setq m3 (merge-uris "../test3.txt" b))
(uri-parsed-path (enough-uri m3 b))

(uri-parsed-path #u"abcd.e")
(setq e (enough-uri "abcd.e" b))
(enough-uri b b)
(parse-uri " foo ")

(setq p #P"/Program Files/My app 1.2/")
(setq p #P"D:/Program Files/My app 1.2/")
(setq p #P"D:/Program Files/My app 1.2/my file.lisp")
(setq p #P"../My dir/my file.lisp")
(setq p #P"//comp/share/My app 1.2/my file.lisp/")	; Windows UNC
(setq p (make-pathname :host "comp" :device "D" :directory "Program Files"))
(setq p (yl:current-pathname))
(file-namestring p)

(setq u (uri:uri p))
(uri:render-uri u)
(setq p #P"D:/Program Files/Русская папка/")

(setq u (uri:parse-uri "mailto:nobody@nowhere.org"))	; not considered to be an authority
(setq u (uri:parse-uri "mailto://nobody@nowhere.org"))

(setq u (uri:parse-uri "file:///D:/Program%20Files/My%20app%201.2/" :decode t))
(uri::percent-decode (uri:uri-path u))
(uri:merge-uris "dir/file.html" u)
(setq octets (ef:encode-lisp-string "Русская папка" :utf-8))
(ef:decode-external-string octets :utf-8)

(setq *fragment-char-p* 'help::russian-alpha-char-p)
(setq u #u"help:book_id/chapter_id#anchor_имя")

(uri:percent-decode "+") => "+"
(hunchentoot:url-decode "+") => " "
(uri:percent-encode " ") => "%20"
(hunchentoot:url-encode " ") => "%20"
(drakma:url-encode " " :utf-8) -> "+"
(uri:percent-encode "+") = (hunchentoot:url-encode "+") => "%2B"
||#
