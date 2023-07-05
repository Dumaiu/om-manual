;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HTML-TEMPLATE.TEST -*-
;;; YHTML-Template
;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2014, Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests relied on ACL-COMPAT lite tester.

;(pushnew :tester *features*)
;(load #P"PROJECTS:ynet;acl-compat;defsys")
;(lw:compile-system 'ACL-COMPAT :load t)

(defpackage :html-template.test
  (:use :common-lisp :html-template)
  (:export #:do-tests))

(in-package :html-template.test)

(defparameter *tmp-dir*
  (pathname (concatenate 'string
             (or #+(and win32 lispworks)
                 (or (lw:environment-variable "TEMP") (lw:environment-variable "TMP"))
                 #+(and win32 sbcl)
                 (or (sb-ext:posix-getenv "TEMP") (sb-ext:posix-getenv "TMP"))
                 #-(and win32 (or lispworks sbcl))
                 "/tmp"
                 "")
             "/")))

(setq *attributes-are-lisp-forms* t
      *template-package* (setq *template-symbol-package*
                               (find-package :html-template.test)))

(setq test:*error-protect-tests* t)

(defmacro test= (result tmpl environment &rest args)
  `(test:test ,result
     (with-output-to-string (*default-template-output*)
       (fill-and-print-template ,tmpl ,environment)) ; :stream stream))
     ;:test 'string=
     ,@args))

(defmacro test-err (tmpl environment &rest args)
  `(test:test-error (fill-and-print-template ,tmpl ,environment)
     ,@args))

(defun do-tests ()
 (let ((test:*break-on-test-failures* nil))
       ;(test::*announce-test* nil))
  (test:with-tests (:name :HTML-TEMPLATE)

  ;; Symbols (attributes) and forms (expressions)
    (test= "abc" "<!-- TMPL_VAR foo -->" '((foo . "abc")))
    (test= "abc" "<!-- TMPL_VAR 'foo' -->" '((foo . "abc")))
    (test= "abc" "<!-- TMPL_VAR \"foo\" -->" '((foo . "abc")))
    (test-err "<!-- TMPL_VAR foo-->" '((foo . "abc"))
              :condition-type 'template-syntax-error)
    (test= "<!-- tmpl_vaar foo -->" "<!-- tmpl_vaar foo -->" '((foo . "abc")))

    (test= "abcdef" "<!-- TMPL_EVAL (concatenate 'string foo bar) -->"
           '((foo . "abc") (bar . "def")))
    (test-err "<!-- TMPL_EVAL (concatenate 'string foo bar -->" ()
              :condition-type 'template-syntax-error)

    (test= "xabcy" "x<!-- TMPL_EVAL foo -->y" '((foo . "abc")))

    (test= "" "<!-- TMPL_EVAL foo -->" ())
    (test= "" "<!-- TMPL_EVAL foo -->" '((cl-user::foo . "abc")))
    (test= "" "<!-- TMPL_EVAL foo -->" '((bar . "abc")))
    (test= "FOO" "<!-- TMPL_EVAL 'foo -->" '((foo . "abc")))

    (test= "abc" "<!-- TMPL_VAR 'foo' -->" '((foo . "abc")))
    (test= "abc" "<!-- TMPL_VAR \"foo\" -->" '((foo . "abc")))
    (test-err "<!-- TMPL_VAR foo-->" '((foo . "abc"))
              :condition-type 'template-syntax-error)

    (let ((*accept-braces-markers* t))
      (test= "" "{foo}" ())
      (test= "abc" "{foo}" '((foo . "abc")))
      (test= "abcdef" "{(concatenate 'string foo bar)}" '((foo . "abc") (bar . "def")))
      (test-err "{foo" '((foo . "abc"))
              :condition-type 'template-syntax-error))

    (let ((*upcase-attribute-strings* nil))
      (test= "The slow brown fox"
             "The <!-- TMPL_VAR speed --> brown fox"
             '((speed . "quick") (|speed| . "slow")))
      ;; Does not apply to TMPL_EVAL tag
      (test= "The quick brown fox"
             "The <!-- TMPL_EVAL speed --> brown fox"
             '((speed . "quick") (|speed| . "slow"))))

    (let ((tp (create-template-printer "The <!-- TMPL_EVAL speed --> brown fox"))
          #+ylib (yl:*tiny-symbol-value-function* #'gethash)
          #-ylib (*value-access-function* #'gethash)
          (hash (make-hash-table :test #'eq)))
      (setf (gethash 'speed hash) "fast")
      (test= "The fast brown fox" tp hash))

    ;; Boolean
    (test= "" "<!-- TMPL_EVAL (and \"def\" foo)-->"  '((foo . nil)))
    (test= "abc" "<!-- TMPL_EVAL (and \"def\" foo) -->" '((foo . "abc")))

    (test= "abc" "<!-- TMPL_EVAL (or foo \"def\") -->" '((foo . "abc")))
    (test= "def" "<!-- TMPL_EVAL (or foo \"def\")-->"  '((foo . nil)))

  ;; TMPL_IF
    (test= "" "<!-- TMPL_IF foo -->abc<!-- /TMPL_IF -->" ())
    (test= "" "<!-- TMPL_IF foo -->abc<!-- /TMPL_IF -->" '((foo . nil)))
    (test= "abc" "<!-- TMPL_IF foo -->abc<!-- /TMPL_IF -->" '((foo . t)))
    (test= "abc" "<!-- TMPL_IF foo -->abc<!-- /TMPL_IF -->" '((foo . t) (bar . 42)))

    (test-err "<!-- TMPL_IF -->abc<!-- /TMPL_IF -->" ()
              :condition-type 'template-syntax-error)
    (test= "def" "<!-- TMPL_IF foo -->abc<!-- TMPL_ELSE-->def<!-- /TMPL_IF -->" ())
    (test-err "<!-- TMPL_IF foo -->abc<!-- TMPL_ELSE-->def<!-- /TMPL_UNLESS -->" ()
              :condition-type 'template-syntax-error)
    (test= "def" "<!-- TMPL_IF foo -->abc<!-- TMPL_ELSE-->def<!-- /TMPL_IF -->"
           '((foo . nil)))
    (test= "abc" "<!-- TMPL_IF foo -->abc<!-- TMPL_ELSE-->def<!-- /TMPL_IF -->"
           '((foo . t)))
    (test= "abc" "<!-- TMPL_UNLESS foo -->abc<!-- TMPL_ELSE-->def<!-- /TMPL_UNLESS -->"
           '((foo . nil)))
    (test= "def" "<!-- TMPL_UNLESS foo -->abc<!-- TMPL_ELSE-->def<!-- /TMPL_UNLESS -->"
           '((foo . t)))
    (test= "abc" "<!-- TMPL_IF foo --><!-- TMPL_EVAL foo --><!-- TMPL_ELSE-->def<!-- /TMPL_IF -->"
           '((foo . "abc")))
    (test= "def" "<!-- TMPL_IF foo --><!-- TMPL_EVAL foo --><!-- TMPL_ELSE-->def<!-- /TMPL_IF -->"
           '((foo . nil)))
    (test= "abcabcabc" "<!-- TMPL_IF foo --><!-- TMPL_EVAL foo -->abc<!-- TMPL_EVAL foo --><!-- TMPL_ELSE-->def<!-- /TMPL_IF -->"
           '((foo . "abc")))
    (test= "defdefdef" "<!-- TMPL_IF foo --><!-- TMPL_EVAL foo -->abc<!-- TMPL_EVAL foo --><!-- TMPL_ELSE--><!-- TMPL_EVAL bar -->def<!-- TMPL_EVAL bar --><!-- /TMPL_IF -->"
           '((bar . "def")))

    (test= "<!-- /TMPL_ELSE -->" "<!-- /TMPL_ELSE -->" ())

    (test= "1"
           "<!-- TMPL_IF foo --><!-- TMPL_IF bar -->1<!-- TMPL_ELSE -->2<!-- /TMPL_IF --><!-- TMPL_ELSE --><!-- TMPL_IF baz -->3<!-- TMPL_ELSE -->4<!-- /TMPL_IF --><!-- /TMPL_IF -->"
           '((foo . t) (bar . t)))
    (test= "2"
           "<!-- TMPL_IF foo --><!-- TMPL_IF bar -->1<!-- TMPL_ELSE -->2<!-- /TMPL_IF --><!-- TMPL_ELSE --><!-- TMPL_IF baz -->3<!-- TMPL_ELSE -->4<!-- /TMPL_IF --><!-- /TMPL_IF -->"
           '((foo . t) (bar . nil)))
    (test= "3"
           "<!-- TMPL_IF foo --><!-- TMPL_IF bar -->1<!-- TMPL_ELSE -->2<!-- /TMPL_IF --><!-- TMPL_ELSE --><!-- TMPL_IF baz -->3<!-- TMPL_ELSE -->4<!-- /TMPL_IF --><!-- /TMPL_IF -->"
           '((foo . nil) (baz t)))
    (test= "4"
           "<!-- TMPL_IF foo --><!-- TMPL_IF bar -->1<!-- TMPL_ELSE -->2<!-- /TMPL_IF --><!-- TMPL_ELSE --><!-- TMPL_IF baz -->3<!-- TMPL_ELSE -->4<!-- /TMPL_IF --><!-- /TMPL_IF -->"
           '((foo . nil) (baz . nil)))
    ;; TMPL_ELIF
    (test-err "<!-- TMPL_UNLESS one -->xone<!-- TMPL_ELIF bar -->xbar<!-- /TMPL_UNLESS -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_IF one -->xone<!-- TMPL_ELIF bar -->xbar<!-- /TMPL_UNLESS -->" ()
              :condition-type 'template-syntax-error)
    (test= "xoney"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- /TMPL_IF -->y"
           '((one . t) (two . nil)))
    (test= "xy"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- /TMPL_IF -->y"
           ())
    (test= "xtwoy"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- /TMPL_IF -->y"
           '((one . nil) (two . t)))
    (test= "xthreey"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- TMPL_ELSE three -->three<!-- /TMPL_IF -->y"
           ())
    (test= "xy"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- TMPL_IF three -->three<!-- /TMPL_IF --><!-- /TMPL_IF -->y"
           ())
    (test= "xtwoy"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- TMPL_IF three -->three<!-- /TMPL_IF --><!-- /TMPL_IF -->y"
           '((two . t)))
    (test= "xtwothreey"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- TMPL_IF three -->three<!-- /TMPL_IF --><!-- /TMPL_IF -->y"
           '((two . t) (three . t)))
    (test= "xelsey"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- TMPL_ELIF three -->three<!-- TMPL_ELSE -->else<!-- /TMPL_IF -->y"
           ())
    (test= "xthreey"
           "x<!-- TMPL_IF one -->one<!-- TMPL_ELIF two -->two<!-- TMPL_ELIF three -->three<!-- TMPL_ELSE -->else<!-- /TMPL_IF -->y"
           '((three . t)))

  ;; TMPL_FOR
    (test= "abcdefghi"
           "<!-- TMPL_FOR foo '(\"abc\" \"def\" \"ghi\")--><!-- TMPL_EVAL foo --><!-- /TMPL_FOR -->"
           ())
    (test= "abcdefghi"
           "<!-- TMPL_FOR foo #(\"abc\" \"def\" \"ghi\")--><!-- TMPL_EVAL foo --><!-- /TMPL_FOR -->"
           ())
    (test= "012"
           "<!-- TMPL_FOR foo 3 --><!-- TMPL_EVAL foo --><!-- /TMPL_FOR -->"
           ())

    (dolist (pair '((foos . (0 1 2)) (foos . #(0 1 2)) (foos . 3)))
      (test= "[a0][a1][a2]"
             "<!-- TMPL_FOR foo foos -->[<!-- TMPL_EVAL bar --><!-- TMPL_EVAL foo -->]<!-- /TMPL_FOR -->"
             (cons pair '((bar . "a")))))
    ;; TMPL_FORELSE - loop over an empty domain
    (dolist (pair '((foos . ()) (foos . #()) (foos . 0)))
      (test= "xyz"
             "<!-- TMPL_FOR foo foos --><!-- TMPL_EVAL foo --><!-- TMPL_FORELSE -->xyz<!-- /TMPL_FOR -->"
             (list pair)))
    (test= "<!-- /TMPL_FORELSE -->" "<!-- /TMPL_FORELSE -->" ())

  ;; TMPL_LOOP, TMPL_REPEAT
    (test= "[[1][2][3]]"
           "[<!-- TMPL_LOOP foo -->[<!-- TMPL_EVAL bar -->]<!-- /TMPL_LOOP -->]"
           '((foo . (((bar . "1")) ((bar . "2")) ((bar . "3"))))))
    (test= "[[][][]]"
           "[<!-- TMPL_LOOP foo -->[<!-- TMPL_EVAL bar -->]<!-- /TMPL_LOOP -->]"
           '((foo . (() () ()))))
    (test= "[[1][2][3]]"
           "[<!-- TMPL_LOOP foo -->[<!-- TMPL_EVAL bar -->]<!-- /TMPL_LOOP -->]"
           '((foo . (((bar . "1")) ((bar . "2")) ((bar . "3"))))))
    (test= "[[1][][3]]"
           "[<!-- TMPL_LOOP foo -->[<!-- TMPL_EVAL bar -->]<!-- /TMPL_LOOP -->]"
           '((foo . (((bar . "1")) () ((bar . "3"))))))
    (test= "[[123][456][789]]"
           "[<!-- TMPL_LOOP foo -->[<!-- TMPL_LOOP bar --><!-- TMPL_EVAL bar --><!-- /TMPL_LOOP bar -->]<!-- /TMPL_LOOP foo -->]"
           '((foo . (((bar . (((bar . "1")) ((bar . "2")) ((bar . "3")))))
                     ((bar . (((bar . "4")) ((bar . "5")) ((bar . "6")))))
                     ((bar . (((bar . "7")) ((bar . "8")) ((bar . "9")))))))))
    (test= "[[123][baz][789]]"
           "[<!-- TMPL_LOOP foo -->[<!-- TMPL_IF baz --><!-- TMPL_LOOP baz --><!-- TMPL_EVAL bar --><!-- /TMPL_LOOP --><!-- TMPL_ELSE -->baz<!-- /TMPL_IF -->]<!-- /TMPL_LOOP -->]"
           '((foo . (((baz . (((bar . "1")) ((bar . "2")) ((bar . "3")))))
                     ()
                     ((baz . (((bar . "7")) ((bar . "8")) ((bar . "9")))))))))

    (test= "[]" "[<!-- TMPL_LOOP foo -->[x]<!-- /TMPL_LOOP -->]" '((foo . nil)))
    (test= "[xxx]" "[<!-- TMPL_REPEAT foo -->x<!-- /TMPL_REPEAT -->]" '((foo . 3)))
    (test= "[]" "[<!-- TMPL_REPEAT foo -->x<!-- /TMPL_REPEAT -->]" '((foo . 0)))
    (test= "[]" "[<!-- TMPL_REPEAT foo -->x<!-- /TMPL_REPEAT -->]" '((foo . "foo")))
    ;; Vector allowed
    (test= "[1][2][3]"
           "<!-- TMPL_LOOP vector -->[<!-- TMPL_EVAL item -->]<!-- /TMPL_LOOP -->"
           '((vector . #(((item . "1")) ((item . "2")) ((item . "3"))))))
    ;; TMPL_LOOPELSE
    (test= "[[4]]"
           "[<!-- TMPL_LOOP foo -->foo<!-- TMPL_LOOPELSE -->[<!-- TMPL_EVAL bar -->]<!-- /TMPL_LOOP -->]"
           '((bar . 4)))
    (test= "[[4]]"
           "[<!-- TMPL_LOOP foo -->foo<!-- TMPL_LOOPELSE -->[<!-- TMPL_EVAL bar -->]<!-- /TMPL_LOOP -->]"
           '((foo . #()) (bar . 4)))
    (test= "<!-- /TMPL_LOOPELSE -->" "<!-- /TMPL_LOOPELSE -->" ())

    (test-err "[<!-- TMPL_REPEAT foo -->x<!-- /TMPL_LOOP -->]" '((foo . 3))
              :condition-type 'template-syntax-error)
    (test-err "[<!-- TMPL_LOOP foo -->x<!-- /TMPL_REPEAT -->]" '((foo . 3))
              :condition-type 'template-syntax-error)
    ;; TMPL_LOOP needs a sequence of environment extensions, i.e. lists, not atoms
    (test-err "<!-- TMPL_LOOP foo -->x<!-- /TMPL_LOOP -->" '((foo . (1 2 3)))
              :condition-type 'type-error :include-subtypes t)

  ;; TMPL_LET, progn, setq
    (test= "[1][12]"
           "<!-- TMPL_LET (foo 1) (bar (+ foo 10)) -->[<!-- TMPL_EVAL foo -->][<!-- TMPL_EVAL bar -->]<!-- /TMPL_LET -->"
           '((foo . 2)))
    (test= "[1][10][11]"
           "<!-- TMPL_LET (foo 1) -->[<!-- TMPL_EVAL foo -->][<!-- TMPL_EVAL (progn (setq foo (1+ bar)) bar) -->][<!-- TMPL_EVAL foo -->]<!-- /TMPL_LET -->"
           '((bar . 10)))

    (let ((temp-name (make-pathname :name (format nil "template-test-~A" (random 1000000))
                                    :defaults *tmp-dir*)))
      (with-open-file (stream temp-name :direction :output :if-exists :error)
        (write-string "<!-- TMPL_EVAL foo -->" stream))
      (let ((*warn-on-creation* nil))
        (test= "abc" temp-name '((foo . "abc"))))
      (with-open-file (stream temp-name :direction :input)
        (test= "def" stream '((foo . "def"))))
        ;(test:test "def"
        ;  (fill-and-print-template stream '((foo . "def")))))
      (with-open-file (stream temp-name :direction :input)
        (let ((tp (create-template-printer stream)))
          (test= "ghi" tp '((foo . "ghi")))))
      (let ((tp (create-template-printer temp-name)))
        (test= "jkl" tp '((foo . "jkl"))))
      (let ((tp (create-template-printer "<!-- TMPL_EVAL foo -->")))
        (test= "mno" tp '((foo . "mno"))))
      (delete-file temp-name)
      (sleep 2)                             ; sleep because of FILE-WRITE-DATE
      ;; Warnings
      (with-open-file (stream temp-name :direction :output :if-exists :error)
        (write-string "<!-- TMPL_EVAL bar -->" stream))
      (test:test-warning (create-template-printer temp-name))
      (test:test-no-warning (create-template-printer temp-name))
      (test:test-warning (create-template-printer temp-name :force t))
      (delete-from-template-cache temp-name)
      (test:test-warning (create-template-printer temp-name))
      (clear-template-cache)
      (test:test-warning (create-template-printer temp-name))
      (delete-file temp-name))

  ;; TMPL_INCLUDE
    (let* ((random-string (format nil "template-test-~A" (random 1000000)))
           (temp-name (merge-pathnames random-string *tmp-dir*))
           (*default-template-pathname* *tmp-dir*)
           (*warn-on-creation* nil)
           (src "The <!-- TMPL_EVAL speed --> <!-- TMPL_EVAL color --> fox")
           (result "The very fast brown fox"))
      (with-open-file (stream temp-name :direction :output :if-exists :error)
        (write-string src stream))
      (test= result
             (make-pathname :name random-string)                     ; pathname
             '((speed . "very fast") (color . "brown")))
      (delete-file temp-name)

      (sleep 2)                           ; sleep because of FILE-WRITE-DATE
      (with-open-file (stream temp-name :direction :output :if-exists :error)
        (write-string src stream))
      (test= result
             (format nil "<!-- TMPL_INCLUDE '~A' -->" random-string)	; namestring
             '((speed . "very fast") (color . "brown")))
      (delete-file temp-name)

      (sleep 2)                           ; sleep because of FILE-WRITE-DATE
      (with-open-file (stream temp-name :direction :output :if-exists :error)
        (write-string src stream))
      (let ((*warn-on-creation* t))
        (test:test-warning
          (with-output-to-string (stream)
            (fill-and-print-template
             (format nil "<!-- TMPL_INCLUDE '~A' -->" random-string)
             '((speed . "very fast") (color . "brown")))
             :stream stream)))
      
      ;; TMPL_INCLUDE with bindings inside the tag
      (test= "The very fast dark brown fox"
       (format nil "<!-- TMPL_INCLUDE '~A' (speed \"very fast\")
                      (color (concatenate 'string \"dark \" color)) -->"
               random-string)
       '((color . "brown")))
      (delete-file temp-name))

    ;; TMPL_INCLUDE: passing arguments into inner template
    (let* ((outer-string (format nil "outer-~A" (random 1000000)))
           (outer-pathname (merge-pathnames outer-string *tmp-dir*))
           (inner-string (format nil "inner-~A" (random 1000000)))
           (inner-pathname (merge-pathnames inner-string *tmp-dir*))
           (*warn-on-creation* nil)
           (*default-template-pathname* *tmp-dir*))
      ;; Invocation: outer -> inner.
      (with-open-file (stream outer-pathname :direction :output :if-exists :error)
        (format stream "<!-- TMPL_INCLUDE '~A' (color (concatenate 'string \"dark \" (or color \"\")))-->"
                inner-string))
      (with-open-file (stream inner-pathname :direction :output :if-exists :error)
        (write-string "The <!-- TMPL_EVAL speed --> <!-- TMPL_EVAL color --> fox" stream))
      (test= "The very fast dark  fox"
             (format nil "<!-- TMPL_INCLUDE '~A' -->" outer-string)	; invoke outer
             '((speed . "very fast")))
      (test= "The slow dark red fox"
             (format nil "<!-- TMPL_INCLUDE '~A'  -->" outer-string)	; invoke outer
             '((speed . "slow") (color . "red")))
      (delete-file inner-pathname)
      (delete-file outer-pathname)
      (sleep 2)

      ;; Error: Recursive invocation: outer -> inner -> outer
      (with-open-file (stream outer-pathname :direction :output :if-exists :error)
        (format stream "<!-- TMPL_INCLUDE '~A' -->" inner-string))
      (with-open-file (stream inner-pathname :direction :output :if-exists :error)
        (format stream "<!-- TMPL_INCLUDE '~A' -->" outer-string))
      (test:test-error (fill-and-print-template
                        (format nil "<!-- TMPL_INCLUDE '~A' -->" outer-string) nil)
        :condition-type 'template-error :include-subtypes t)
      (delete-file inner-pathname)
      (delete-file outer-pathname))

    ;; TMPL_CALL
    (test= "X" "<!-- TMPL_CALL foo -->"
           '((foo . (("X")))))
    (test= "" "<!-- TMPL_IF foo --><!-- TMPL_CALL bar --><!-- /TMPL_IF -->"
           '((foo . (("---")))))
    (test= "QUUX" "<!-- TMPL_EVAL baz --><!-- TMPL_CALL foo -->"
           '((baz . "Q")
             (foo . (("<!-- TMPL_EVAL bar -->" (bar . "U"))
                     ("<!-- TMPL_EVAL bar -->X" (bar . "U"))))))
    (test= "QUUX"
           "<!-- TMPL_EVAL baz --><!-- TMPL_CALL foo -->"
           '((baz . "Q")
             (foo . #(("<!-- TMPL_EVAL bar -->"  (bar . "U"))
                      ("<!-- TMPL_EVAL bar -->X" (bar . "U"))))))
    ;; Error: Recursive invocation
    (test:test-error (fill-and-print-template
                      #1="<!-- TMPL_EVAL baz --><!-- TMPL_CALL foo -->X"
                      '((baz . "Q")
                        (foo . (("<!-- TMPL_EVAL bar -->" (bar . "U"))
                                (#1# (baz . "u"))))))
        :condition-type 'template-invocation-error)

  ;; Large output: TMPL_FOR vs. TMPL_LOOP
    (let ((result "<table>
  <tr>
    <td>1</td>
    <td>2</td>
    <td>3</td>
    <td>4</td>
  </tr>
  <tr>
    <td>2</td>
    <td>3</td>
    <td>4</td>
    <td>5</td>
  </tr>
  <tr>
    <td>3</td>
    <td>4</td>
    <td>5</td>
    <td>6</td>
  </tr>
</table>")
          (*ignore-empty-lines* t))
      (let ((template "<table>
  <!-- TMPL_FOR row '((1 2 3 4) (2 3 4 5) (3 4 5 6)) -->
  <tr>
    <!-- TMPL_FOR col row -->
    <td><!-- TMPL_EVAL col --></td>
    <!-- /TMPL_FOR -->
  </tr>
  <!-- /TMPL_FOR -->
</table>"))
        (test= result template ()))

      (let ((template "<table>
  <!-- TMPL_LOOP row-loop -->
  <tr>
    <!-- TMPL_LOOP col-loop -->
    <td><!-- TMPL_EVAL item --></td>
    <!-- /TMPL_LOOP -->
  </tr>
  <!-- /TMPL_LOOP -->
</table>")
            (environment                      ; complex environment is needed
             `((row-loop .
                ,(loop for row in '((1 2 3 4) (2 3 4 5) (3 4 5 6))
                       collect `((col-loop .
                                  ,(loop for col in row
                                         collect `((item . ,(format nil "~A" col)))))))))))
        (test= result template environment)))

    ;; Syntax sugar
    (let ((*template-start-marker* "<")
          (*template-end-marker* ">"))
      (test= "The quick <brown> fox" "The <TMPL_VAR 'speed'> <brown> fox"
             '((speed . "quick")))
      (test= "The quick <brown> fox" "The <TMPL_EVAL speed> <brown> fox"
             '((speed . "quick"))))

  ;; Errors
    (test-err "<!-- TMPL_ELIF -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_ELSE -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- /TMPL_IF -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- /TMPL_UNLESS -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_IF foo --><!-- TMPL_ELSE -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_UNLESS foo --><!-- TMPL_ELSE -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_UNLESS foo -->abc<!-- TMPL_ELSE-->def<!-- /TMPL_IF -->" '((foo . t))
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_IF foo --><!-- TMPL_IF bar -->1<!-- TMPL_ELSE -->2<!-- /TMPL_IF --><!-- TMPL_ELSE --><!-- TMPL_IF baz -->3<!-- TMPL_ELSE -->4<!-- /TMPL_IF -->" ()
              :condition-type 'template-syntax-error)

    (test-err "<!-- TMPL_FORELSE -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- /TMPL_FOR -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_FOR foo --><!-- TMPL_FORELSE -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_FOR foo --><!-- TMPL_EVAL foo --><!-- /TMPL_FOR -->" ()
              :condition-type 'template-syntax-error)		; EOF

    (test-err "<!-- TMPL_LOOPELSE -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- /TMPL_LOOP -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_LOOP foo --><!-- TMPL_ELSE -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_LOOP foo --><!-- TMPL_ELSE --><!-- /TMPL_LOOP -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_IF bar --><!-- TMPL_LOOP foo --><!-- TMPL_ELSE --><!-- /TMPL_LOOP -->" ()
              :condition-type 'template-syntax-error)

    (test-err "<!-- TMPL_LET () --><!-- TMPL_EVAL foo --><!-- /TMPL_LET -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_LET 5 --><!-- TMPL_EVAL foo --><!-- /TMPL_LET -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_LET (5) --><!-- TMPL_EVAL foo --><!-- /TMPL_LET -->" ()
              :condition-type 'template-syntax-error)

    (test-err "<!-- TMPL_INCLUDE /tmp/dummy var -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_INCLUDE /tmp/dummy 5 -->" ()
              :condition-type 'template-syntax-error)
    (test-err "<!-- TMPL_INCLUDE /tmp/dummy (5) -->" ()
              :condition-type 'template-syntax-error)

    ;; Non-sequence signal
    (test-err "<!-- TMPL_FOR var foo --><!-- TMPL_EVAL var --><!-- /TMPL_FOR -->"
              '((foo . abc))
              :condition-type 'template-invocation-error)
    (test-err "<!-- TMPL_LOOP foo --><!-- TMPL_EVAL var --><!-- /TMPL_LOOP -->"
              '((foo . 57))
              :condition-type 'template-invocation-error)
    (test-err "<!-- TMPL_CALL foo -->"
              '((foo . 57))
              :condition-type 'template-invocation-error)

    ;; Null value signal
    (let ((*format-null-forms* nil))
      (test-err "<!-- TMPL_EVAL foo -->" ()
                :condition-type 'template-missing-value-error)
      (test-err "<!-- TMPL_EVAL foo -->" ()
                :condition-type 'template-missing-value-error)
      (let ((tp (create-template-printer "The <!-- TMPL_EVAL speed --> brown fox"))
            (environment '((foo . "bar"))))
        (with-output-to-string (*default-template-output*)
          (test-err tp environment
                    :condition-type 'template-missing-value-error))
        (test:test "The slow brown fox"
          (handler-bind ((template-missing-value-error
                          (lambda (condition) (declare (ignore condition))
                            (use-value "slow"))))
            (with-output-to-string (*default-template-output*)
              (fill-and-print-template tp environment)))) ))

    (let ((*format-null-forms* "{~S}"))
      (test= "The {SPEED} brown fox"
             "The <!-- TMPL_EVAL speed --> brown fox"
             ()))

    ;; Non-string value signal
    (let ((*format-non-strings* nil)
          (result "A square has four corners")
          (environment '((number . 4)))
          (tp (create-template-printer "A square has <!-- TMPL_EVAL number --> corners")))
      (test:test result
        (handler-bind ((template-not-a-string-error
                        (lambda (condition)
                          (use-value
                           (format nil "~R"
                                   (template-not-a-string-error-value condition))))))
          (with-output-to-string (*default-template-output*)
            (fill-and-print-template tp environment))))

      (setq tp (create-template-printer "A square has <!-- TMPL_EVAL number --> corners"))
      (test:test result
        (handler-bind ((template-not-a-string-error
                        (lambda (condition)
                          (use-value
                           (format nil "~R"
                                   (template-not-a-string-error-value condition))))))
          (with-output-to-string (*default-template-output*)
            (fill-and-print-template tp environment)))))

    ;; Comment at tag rest
    (let ((*ignore-tag-rest* t))
      (test= "abc" "<!-- TMPL_EVAL foo ; Good style comment  -->"
             '((foo . "abc")))
      (test= "abc" "<!-- TMPL_IF foo Not a good style -->abc<!-- /TMPL_IF end of foo -->"
             '((foo . t)))
      (test= "abc" "<!-- TMPL_IF foo -->abc<!-- /TMPL_IF foo ; Good style comment -->"   
             '((foo . t)))
      (test= "1" "<!-- TMPL_IF 'bar' --><!-- TMPL_EVAL bar --><!-- /TMPL_IF -->"
             '((bar . "1")))
      (test= "123" "<!-- TMPL_FOR foo '(1 2 3) ; Good style comment --><!-- TMPL_EVAL foo --><!-- /TMPL_FOR foo -->" ()))

    (let ((*ignore-tag-rest* nil))
      (test-err "<!-- TMPL_EVAL foo ; prohibited everywhere -->" '((foo . "abc"))
                :condition-type 'template-syntax-error)
      (test-err "<!-- TMPL_IF foo ; prohibited everywhere -->abc<!-- /TMPL_IF -->" ()
                :condition-type 'template-syntax-error)
      (test-err "<!-- TMPL_IF foo -->abc<!-- /TMPL_IF foo -->" ()
                :condition-type 'template-syntax-error)
      (test-err "<!-- TMPL_FOR foo list ; prohibited everywhere -->abc<!-- /TMPL_FOR -->" ()
                :condition-type 'template-syntax-error)
      ;; A form should follow TMPL_IF and the like, not an old-style attribute
      (test-err "<!-- TMPL_IF 'bar' --><!-- TMPL_EVAL bar --><!-- /TMPL_IF -->"
                '((bar . "1"))
                :condition-type 'template-syntax-error) )

    ;; TOTALS
    (and (= 0 test:*test-errors* test:*test-unexpected-failures*) ; total successp
         (/= 0 test:*test-successes*)))))                         ; total tests passed


#||;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(do-tests)

(with-output-to-string (*default-template-output*)
  (fill-and-print-template "<!-- TMPL_VAR foo -->" '((foo . "abc"))))
(with-output-to-string (*default-template-output*)
  (fill-and-print-template "<!-- TMPL_EVAL foo -->" '((foo . "abc"))))

(with-input-from-string (*standard-input* " foo comment -->")
  (htt::read-tag-rest :kind :attribute :skip t))
(with-input-from-string (*standard-input* " /tmp/tpl (a 1) (b 2)comment -->")
  (htt::read-tag-rest :kind :attribute :intern nil :skip most-positive-fixnum))
(with-input-from-string (*standard-input* " (cons a b) (a 1) (b 2)comment -->")
  (htt::read-tag-rest :kind :forms :skip nil))        ; or t - does not matter
(with-input-from-string (*standard-input* " (cons a b) (a 1) (b 2)comment -->")
  (htt::read-tag-rest :kind :forms :skip most-positive-fixnum))
(with-input-from-string (*standard-input* " (cons a b) (a 1) (b  -->")
  (htt::read-tag-rest :kind :forms :skip nil))

(with-input-from-string (*standard-input* "before {foo}")
  (htt::read-until *template-start-marker* :skip nil :braces t))
(with-input-from-string (*standard-input* "before <!--TMPL_EVAL foo -->")
  (htt::read-until *template-start-marker* :skip nil :braces t))

(with-input-from-string (*standard-input* " foo}")
  (htt::read-tag-rest :kind :forms :skip t :end-marker "}"))

(with-output-to-string (*default-template-output*)
  (fill-and-print-template "before { foo}" '((foo . "abc"))))

(with-input-from-string (*standard-input* " foo (list 1 2) ; comment -->")
  (htt::read-tag-rest :kind :forms :skip 2))

(with-output-to-string (*default-template-output*)
  (fill-and-print-template "<!-- TMPL_FOR foo foos --><!-- TMPL_EVAL foo --><!-- /TMPL_FOR -->"
                           '((foos . (1 2 3)))))
(with-output-to-string (*default-template-output*)
  (fill-and-print-template
   "<!-- TMPL_FOR foo ()--><!-- TMPL_EVAL foo --><!-- TMPL_FORELSE -->xyz<!-- /TMPL_FOR -->" ()))
(with-output-to-string (*default-template-output*)
  (fill-and-print-template
   "<!-- TMPL_FOR foo --><!-- TMPL_EVAL foo --><!-- /TMPL_FOR -->" ()))

(let ((*format-null-forms* "{~S}"))
  (with-output-to-string (*default-template-output*)
    (fill-and-print-template "The <!-- TMPL_EVAL speed --> brown fox" ())))

(with-output-to-string (*default-template-output*)
  (fill-and-print-template "<!-- TMPL_FOR var foo --><!-- TMPL_EVAL var --><!-- /TMPL_FOR -->"
                           '((foo . abc))))

(with-output-to-string (*default-template-output*)
  (fill-and-print-template #1="<!-- TMPL_EVAL baz --><!-- TMPL_CALL foo -->X"
           '((baz . "Q")
             (foo . (("<!-- TMPL_EVAL bar -->" (bar . "U"))
                     (#1# (baz . "u")))))))

(let ((*ignore-tag-rest* nil))
  (with-input-from-string (*standard-input* " 'bar' -->")
    (htt::read-tag-rest :kind :forms :skip 1)))
(with-output-to-string (*default-template-output*)
  (fill-and-print-template
    "<!-- TMPL_IF 'bar' --><!-- TMPL_EVAL bar --><!-- /TMPL_IF -->"
    '((bar . "1"))))

(with-output-to-string (*default-template-output*)
  (fill-and-print-template "<!-- TMPL_INCLUDE /tmp/dummy (5) -->" ()))

(let* ((inner-filename (format nil "inner-~A" (random 1000000)))
       (inner-pathname (merge-pathnames inner-filename *tmp-dir*))
       (outer-string (format nil "<!-- TMPL_FOR foo '(1 2 3) --><!-- TMPL_INCLUDE '~A' --><!-- /TMPL_FOR -->"
                             inner-filename))
       (outer-pathname (merge-pathnames (format nil "outer-~A" (random 1000000))
                                        *tmp-dir*))
       (*warn-on-creation* t)
       (*default-template-pathname* *tmp-dir*))
  ;(trace file-write-date)
  ;; Include: the inner template file is compiled only once
  (with-open-file (stream inner-pathname :direction :output :if-exists :error)
    (write-line "Value of foo: {foo}" stream))
  (fill-and-print-template outer-string nil)
  (sleep 2)
  ;; file-write-date is called only once
  (with-open-file (stream outer-pathname :direction :output :if-exists :error)
    (write-string outer-string stream))
  (fill-and-print-template outer-pathname nil)
  
  (delete-file inner-pathname)
  (delete-file outer-pathname))

(let* ((outer-string (format nil "outer-~A" (random 1000000)))
       (outer-pathname (merge-pathnames outer-string *tmp-dir*))
       (inner-string (format nil "inner-~A" (random 1000000)))
       (inner-pathname (merge-pathnames inner-string *tmp-dir*))
       (*warn-on-creation* nil)
       (*default-template-pathname* *tmp-dir*))
  ;; Error: Recursive invocation: outer -> inner -> outer
  (with-open-file (stream outer-pathname :direction :output :if-exists :error)
    (format stream "<!-- TMPL_INCLUDE '~A' -->" inner-string))
  (with-open-file (stream inner-pathname :direction :output :if-exists :error)
    (format stream "<!-- TMPL_INCLUDE '~A' -->" outer-string))
  (fill-and-print-template (format nil "<!-- TMPL_INCLUDE '~A' -->" outer-string) nil)
  ;(test:test-error (fill-and-print-template
  ;                      (format nil "<!-- TMPL_INCLUDE '~A' -->" outer-string) nil)
  ;      :condition-type 'template-error :include-subtypes t)
  (delete-file inner-pathname)
  (delete-file outer-pathname))
||#
