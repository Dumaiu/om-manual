;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HTML-TEMPLATE; Base: 10 -*-
;;; YHTML-Template
;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2014, Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template compilation and fill-in functions

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; SUMMARY OF DIFFERENCES FROM EDI WEITZ'S HTML-TEMPLATE
;;;
;;; 1. A Lisp form in place of "plane attribute" is allowed.
;;;    Forms are read by the standard read function.
;;;    All starting tags (except TMPL_INCLUDE) accept forms
;;;    instead of "old style" attributes.
;;;
;;;    The *attributes-are-lisp-forms* special variable was introduced.
;;;    Bind or set it to NIL to treating those tags in "compatibility" mode.
;;;
;;; 2. Symbols follow standard Lisp syntax: a package specifier is allowed
;;;    in front of a symbol name. By default, symbols are interned into the
;;;    package that is the value of *template-package*, a new special variable.
;;;    The *template-symbol-package* is used only for TMPL_VAR and in
;;;    in "compatibility" mode.
;;;
;;; 3. To interpret the forms in run-time, the template-eval function was introduced.
;;;    It is a simple evaluator akin to the standard eval function except for:
;;;    - A limited number of special-forms is supported, namely:
;;;      IF WHEN UNLESS AND OR NOT QUOTE PROGN SETQ.
;;;    - The symbol with a name like *var* is treated as a dynamic variable
;;;      and is retrieved via symbol-value.
;;;    - The values of other symbols are looked up via *value-access-function*.
;;;
;;; 4. The TMPL_EVAL tag and curly braces {} are supported as a replacement
;;;    of the TMPL_VAR tag.
;;;    - The create-eval-printer helper function added;
;;;    - The *accept-braces-markers* special added; if its value is true,
;;;      the following pseudo-tags are equivalent:
;;;	   {var}
;;;	   <!-- TMPL_EVAL var -->
;;;	   <!-- TMPL_VAR var -->
;;;
;;; 5. The value of *format-non-strings* has got an additional meaning.
;;;    If eq to T, the result is produced by means of princ-to-string,
;;;    i.e. (format nil "~A" ...).
;;;    If it is true but not eq to T, the value must be a single-parameter
;;;    function, which returns two values:
;;;    (1) a string resulted from its argument, and optionally
;;;    (2) do-not-modify flag controlling whether *string-modifier* is
;;;        applied afterwards.
;;;    The truth as second value can prevent the result of converting
;;;    from predefined format, e.g. LHTML, from further escaping.
;;;
;;;  6. The TMPL_ELIF tag was introduced to allow a more concise code.
;;;     In full, now the "if" pattern looks like:
;;;	   <!-- TMPL_IF condition_1 --> body_1
;;;	   <!-- TMPL_ELIF condition_2 --> body_2 ...
;;;	   <!-- TMPL_ELSE --> else_body
;;;	   <!-- /TMPL_IF -->
;;;
;;;  7. The TMPL_LOOPELSE tag was introduced for specifying the body that is
;;;     executed when the length of seguence argument is zero.
;;;     In full, now the "loop" pattern looks like:
;;;	   <!-- TMPL_LOOP sequence -->
;;         body_1
;;;	   [ <!-- TMPL_LOOPELSE --> else_body ]
;;;	   <!-- /TMPL_LOOP -->
;;;
;;;  8. The TMPL_FOR tag was introduced as a generic loop. It provides functionality
;;;     of dolist, loop for across, and dotimes Lisp operators.
;;;     The "for" pattern looks like:
;;;        <!-- TMPL_FOR var sequence-or-integer -->
;;;        body
;;;        [ <!-- TMPL_FORELSE --> else_body ]
;;;	   <!-- /TMPL_FOR -->
;;;
;;;  9. The TMPL_LET tag was introduced:
;;;        <!-- TMPL_LET { var | (var expr) } ... -->
;;;        body
;;;        <!-- /TMPL_LET -->
;;;
;;; 10. The TMPL_INCLUDE tag syntax extended and behavior changed.
;;;     - Full syntax is:
;;;        <!-- TMPL_INCLUDE filename [(var expr)] ... -->
;;;       So one can easily pass arguments to the template invoked.
;;;
;;;     - We call (create-template-printer pathname) in run-time, within the
;;;       closure created by the outer printer. Thus any change of file included
;;;       passes the up-to-date check against the cache using file-write-date.
;;;
;;;     Edi's version just returned the closure created during the compilation
;;;     of the caller template and circumvented the up-to-date check.
;;;     Modifying the template file included was not cause for recompiling it
;;;     actually.
;;;
;;; 11. Introduced *ignore-tag-rest* parameter.
;;;     When it is true (default):
;;;     - TMPL_EVAL, TMPL_VAR, TMPL_IF, TMPL_ELIF, TMPL_UNLESS,
;;;       TMPL_LOOP, TMPL_REPEAT, TMPL_CALL tags
;;;       can embed an optional text after the first form or atribute
;;;       up to the closing marker "-->".
;;;	  For example:
;;;	   <!-- TMPL_EVAL (+ row-number 1) Increase by one -->
;;;     - TMPL_FOR can embed an optional text after the second form
;;;       up to the closing marker "-->".
;;;	  For example:
;;;	   <!-- TMPL_FOR var '(1 2 3) Loop over items -->
;;;     - TMPL_ELSE, TMPL_LOOPELSE, and TMPL_FORELSE tags as well as all
;;;	  ending tags /TMPL_... can embed an optional text between
;;;       the tag name and the closing marker "-->".
;;;	  For example:
;;;	   <!-- /TMPL_LOOP rows -->
;;;     This text is intended for readability only and completely ignored by the parser.
;;;     It is a matter of style to start the text it with ; (semicolon).
;;;
;;; 12. Added special *format-null-forms* instead of *convert-nil-to-empty-string*.
;;;     When its value is a string, e.g. "{~S}", the form resulting in NIL is output
;;;     into the HTML stream according to this format control string.
;;;
;;; 13. Cache control changed.
;;;     - The force parameter of the #<method create-template-printer (pathname)>
;;;       and *force-default* special can be on of:
;;;       :no-check, T, NIL, or :no-cache (was :do-not-cache in Edi's version).
;;;     - The *no-cache-check* special variable is deprecated,
;;;
;;; 14. The purpose of the template-invocation-error condition has changed.
;;;     Now it is signaled by several printer functions:
;;;     - when an argument of a wrong type is passed.
;;;     - when infinite recursion is detected by a TMPL_CALL printer.
;;;     In Edi's version, it was signaled by API functions during keyword arguments
;;;     check.
;;;
;;; 15. Dependency on Ystok-Library is obligatory.
;;;     + Added lang/ru.lisp, the file with localization strings.
;;;
;;; DEPRECATED STUFF
;;;  1. The third parameter of a *value-access-function* is unused but retained
;;;     for compatibility (none of the callers supplies it any longer).
;;;
;;;  2. *sequences-are-lists* is no longer used.
;;;     The TMPL_LOOP and TMPL_CALL printers do the following:
;;;     - dispatch in "run-time" depending on whether the type of the sequence
;;;       supplied as an attribute value is a list or a vector;
;;;     - before filling the "body, always retain outer environment by extending
;;;       it with all elements of the sequence.
;;;
;;;  3. *convert-nil-to-empty-string* and *no-cache-check* are no logner used.
;;;
;;; CAUTION:
;;;  When delivering with LispWorks, do not forget to keep
;;;  - all the symbol names and definitions referred by templates,
;;;  - all the packages mentioned.
;;;
;;; CONCEPTUAL NOTE:
;;;  The default value of *template-symbol-package* is the keyword package.
;;;  Interning symbols there is not a good idea from general Lisp concepts
;;;  because keywords are actually constants in Lisp.

(in-package :html-template)

(defun create-simple-printer (string-list &optional (next-fn #'no-values))
  "Used internally to create template printers for strings that do not
include template tags. Arguments:
STRING-LIST List of strings in reverse order to be printed first.
NEXT-FN     Next function to be called in the chain of closures."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (funcall next-fn environment))))

(defun create-eval-printer (string-list form next-fn)
  "Used internally to create template printers for TMPL_EVAL.
Arguments:
 STRING-LIST List of strings in reverse order to be printed first.
 FORM        Lisp form associated with the tag.
 NEXT-FN     Next function to be called in the chain of closures."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (let ((value (template-eval form environment))
            string
            (do-not-modify nil))
        (cond ((null value)
               (setq string (cond ((eq *format-null-forms* t)
                                   "")
                                  ((stringp *format-null-forms*)
                                   (format nil *format-null-forms* form))
                                  (t
                                   (with-use-value-restart (form)
                                     (signal-template-missing-value-error
                                      #L"The value of form ~S is NIL" form))))))
              ((stringp value)
               (setq string value))
              ((eq *format-non-strings* t)
               (setq string (princ-to-string value)))
              (*format-non-strings*
               (multiple-value-setq (string do-not-modify)
                   (funcall *format-non-strings* value)))
              (t
               (setq string (with-use-value-restart (form)
                              (error 'template-not-a-string-error
                               :value value
                               :format-control #L"The value ~S of form ~S is not a string"
                               :format-arguments (list value form))))))
        (write-string (if do-not-modify
                          string
                          (funcall *string-modifier* string))
                      *template-output*))
      (funcall next-fn environment))))

(defun create-var-printer (string-list symbol next-fn)
  "Used internally to create template printers for TMPL_VAR. SYMBOL is
the symbol associated with the tag. NEXT-FN is the next function to be
called in the chain of closures. STRING-LIST is a list of strings in
reverse order to be printed first."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (let* ((value (funcall *value-access-function* symbol environment))
             (string (typecase value
                       (null
                        (cond ((eq *format-null-forms* t)
                               "")
                              ((stringp *format-null-forms*)
                               (format nil *format-null-forms* symbol))
                              (t
                               (with-use-value-restart (symbol)
                                 (signal-template-missing-value-error 
                                  #L"Value for symbol ~S is NIL" symbol)))))
                       (string value)                            
                       (otherwise
                        (cond ((eq *format-non-strings* t)
                               (princ-to-string value))
                              (*format-non-strings*
                               (funcall *format-non-strings* value))
                              (t
                               (with-use-value-restart (symbol)
                                 (error 'template-not-a-string-error
                                  :value value
                                  :format-control #L"Value ~S for symbol ~S is not a string"
                                  :format-arguments (list value symbol)))))))))
        (write-string (funcall *string-modifier* string) *template-output*))
      (funcall next-fn environment))))

(defun create-if-printer (string-list form if-fn else-fn next-fn unlessp)
  "Used internally to create template printers for TMPL_IF and TMPL_UNLESS tags.
Arguments:
 STRING-LIST List of strings in reverse order to be printed first.
 FORM        Lisp form embedded into the tag.
 IF-FN       Template printer for the IF branch,
 ELSE-FN     Template printer for the ELSE branch.
 NEXT-FN     Next function to be called in the chain of closures.
 UNLESSP     Boolean, if it is true, IF-FN and ELSE-FN are switched."
  (let ((string (list-to-string string-list)))
    (when unlessp
      (rotatef if-fn else-fn))
    (lambda (environment)
      (write-string string *template-output*)
      (if (if *attributes-are-lisp-forms*
              (template-eval form environment)
              (funcall *value-access-function* form environment))
          (funcall if-fn environment)
          (funcall else-fn environment))
      (funcall next-fn environment))))

(defun create-for-printer (string-list symbol form body-fn else-fn next-fn)
  "Used internally to create template printers for TMPL_FOR tags.
Arguments:
 STRING-LIST List of strings in reverse order to be printed first.
 SYMBOL      Loop variable. 
 FORM        Lisp form evaluated to either a sequence or interger.
 BODY-FN     Template printer representing the loop body.
 ELSE-FN     Template printer for the FORELSE branch.
 NEXT-FN     Next function to be called in the chain of closures."
 ;; CAUTION: Used only with alist-like enviroments for now
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (let* ((sequence (template-eval form environment))
             (pair (cons symbol nil))
             (new-environment (cons pair environment)))
        (cond ((vectorp sequence)                          ; can even be a string!
               (if (plusp (length sequence))
                   (loop for val across sequence
                         do (rplacd pair val)
                            (funcall body-fn new-environment))
                   (funcall else-fn environment)))
              ((consp sequence)
               (dolist (val sequence)
                 (rplacd pair val)
                 (funcall body-fn new-environment)))
              ((null sequence)
               (funcall else-fn environment))
              ((integerp sequence)
               (if (plusp sequence)
                   (dotimes (val sequence)
                     (rplacd pair val)
                     (funcall body-fn new-environment))
                   (funcall else-fn environment)))
              (t
               (signal-template-invocation-error
                #L"For tag TMPL_FOR ~S ~S, the value is~
		- neither a sequence~
		- nor an integer:~%~S."
                symbol form sequence)))
       (funcall next-fn environment)))))

(defun create-loop-printer (string-list form body-fn else-fn next-fn)
  "Used internally to create template printers for TMPL_LOOP tags.
Arguments:
 STRING-LIST List of strings in reverse order to be printed first.
 FORM        Symbol or arbitrary Lisp form embedded in the tag.
 BODY-FN     Template printer for the body of the loop.
 ELSE-FN     Template printer for the LOOPELSE branch.
 NEXT-FN     Next function to be called in the chain of closures."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (let ((sequence (if *attributes-are-lisp-forms*
                          (template-eval form environment)
                          (funcall *value-access-function* form environment))))
        (cond ((vectorp sequence)
               (if (plusp (length sequence))
                   (loop for env across sequence
                         do (funcall body-fn (append env environment)))
                   (funcall else-fn environment)))
              ((consp sequence)
               (dolist (env sequence)
                 (funcall body-fn (append env environment))))
              ((null sequence)
               (funcall else-fn environment))
              (t
               (signal-template-invocation-error
                #L"For tag ~A ~S, the value is not a sequence:~%~S."
                "TMPL_LOOP" form sequence)))
        (funcall next-fn environment)))))

(defun create-repeat-printer (string-list form body-fn next-fn)
  "Used internally to create template printers for TMPL_REPEAT tags.
Arguments:
 STRING-LIST List of strings in reverse order to be printed first.
 FORM        Symbol or Lisp form embedded into the tag.
 BODY-FN     Template printer for the body of the loop.
 NEXT-FN     Next function to be called in the chain of closures."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (let ((factor (if *attributes-are-lisp-forms*
                        (template-eval form environment)
                        (funcall *value-access-function* form environment))))
        (when (and (integerp factor) (plusp factor))
          (loop repeat factor
                do (funcall body-fn environment))))
      (funcall next-fn environment))))

(defun create-let-printer (string-list bingings body-fn next-fn)
  "Used internally to create template printers for TMPL_LET.
Arguments:
 STRING-LIST List of strings in reverse order to be printed first.
 BINGINGS    List of \(var form) pairs that extends the environment
             bindings on calling the function of the template included.
 BODY-FN     Template printer for the let body.
 NEXT-FN     Next function to be called in the chain of closures."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      ;; Evaluate binding forms in parallel
      ;; and cons a fresh dotted pairs to the environment
      (let ((new environment))
        (dolist (pair bingings)
          (setq new (acons (if (consp pair) (first pair) pair)
                           (if (consp pair) (template-eval (second pair) environment) nil)
                           new)))
        (funcall body-fn new))
      (funcall next-fn environment))))

(defun create-include-printer (string-list pathname next-fn &optional bingings)
  "Used internally to create template printers for TMPL_INCLUDE.
Arguments:
 PATHNAME    The pathname resulted from mergingassociated with the tag.
 NEXT-FN     The next function to be called in the chain of closures.
 STRING-LIST A list of strings in reverse order to be printed first.
 BINGINGS    A list of \(var form) pairs that extends the environment
             bindings on calling the function of the template included."
  ;; We check for recursive inclusion in run-time (fill-in time) rather than
  ;; in creation time in order not to circumvent cache.
  (let ((string (list-to-string string-list))
        (merged-pathname (merge-pathnames pathname *default-template-pathname*))
        (syntax-error-location (make-syntax-error-location :line *current-line*
                                                           :col *current-column*
                                                           :stream *standard-input*)))
    (lambda (environment)
      (when (member merged-pathname %source-stack% :test #'equal)
        ;; Raise an error if this file has been included before
        ;; Extract the error location from the variable we are closed over.
        (let ((*syntax-error-location* syntax-error-location))
          (signal-template-syntax-error
           "Infinite recursion in TMPL_INCLUDE - file is already in stack: ~S."
           merged-pathname)))
      (write-string string *template-output*)
      ;; Push this pathname onto stack of included files (so to say)
      ;; to make sure a file can't include itself recursively
      (let* ((printer (create-template-printer merged-pathname))
             (%source-stack% (cons merged-pathname %source-stack%)))
        (funcall printer
                 (if (and bingings *attributes-are-lisp-forms*)
                     ;; Evaluate binding forms in parallel and
                     ;; append fresh dotted pairs to the environment
                     (let ((new environment))
                       (dolist (pair bingings)
                         (setq new (acons (first pair)
                                          ;; Bind in parallel a la let
                                          (template-eval (second pair) environment)
                                          ;; Bind sequentially a la let*?
                                          ;(template-eval (second pair) new)
                                          new)))
                       new)
                     environment)))
      (funcall next-fn environment))))

(defun create-call-printer (string-list form next-fn)
  "Used internally to create template printers for TMPL_CALL tags.
Arguments:
 FORM    is a symbol or Lisp form embedded into the tag.
 BODY-FN is the template printer for the body of the loop.
 NEXT-FN is the next function to be called in the chain of closures.
 STRING-LIST is a list of strings in reverse order to be printed first."
  ;; The called templates is pushed onto %source-stack% by the corresponding
  ;; fill-and-print-template method.
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (let ((sequence (if *attributes-are-lisp-forms*
                          (template-eval form environment)
                          (funcall *value-access-function* form environment))))
        (flet ((%call (call)
                 (let ((source (funcall *call-template-access-function* call)))
                   (when (pathnamep source)
                     (setq source (merge-pathnames source *default-template-pathname*)))
                   (when (member source %source-stack% :test #'equal)
                     ;; Raise an error if this file has been included before
                     ;; Extract the error location from the variable we are closed over.
                     (signal-template-invocation-error
                      #L"Infinite recursion in TMPL_CALL - template is already in stack:~%~S."
                      source))
                   (let* ((printer (create-template-printer source))
                          (%source-stack% (cons source %source-stack%)))
                     (funcall printer (append (funcall *call-value-access-function* call)
                                              environment))))))
          (cond ((vectorp sequence)
                 (loop for call across sequence
                       do (%call call)))
                ((listp sequence)
                 (dolist (call sequence)
                   (%call call)))
                 (t
                  (signal-template-invocation-error
                   #L"For tag ~A ~S, the value is not a sequence:~%~S."
                   "TMPL_CALL" form sequence))))
        (funcall next-fn environment)))))

(defun create-template-printer-aux (string-stack end-token)
  "Reads from *STANDARD-INPUT* and returns a template printer from
what it reads.  When this function is entered the stream pointer must
not be inside a template tag.
Arguments:
 STRING-STACK is a list of strings (in reverse order) read so far
     which haven't been used to build a template printer.
 END-TOKEN is either NIL or one of :LOOP, :REPEAT, :IF,
    :IF-ELSE, or :UNLESS-ELSE denoting that we expect certain tags to
    close open TMPL_LOOP, TMPL_REPEAT, TMPL_IF, or TMPL_UNLESS tags.
Values:
 Return the second value
  - either a function object if after TMPL_IF a TMPL_ELIF was seen,
  - or generalized boolean true if, after reading TMPL_IF or TMPL_UNLESS, 
    a corresponding TMPL_ELSE was seen."
  (multiple-value-bind (string braces)
      ;; read text up to the next template start marker
      (read-until *template-start-marker*
                  :braces *accept-braces-markers*
                  :skip nil                         ; don't skip it, return it
                  :eof-action (lambda (collector)
                                (when end-token
                                  ;; make sure we don't accept EOF if there are
                                  ;; still tags waiting to be closed
                                  (signal-unexpected :eof
                                   (format nil #L", end ~A tag is missing"
                                     (case end-token
                                       ((:if :if-else) "<!-- /TMPL_IF -->")
                                       ((:unless :unless-else) "<!-- /TMPL_UNLESS -->")
                                       ((:for :for-else) "<!-- /TMPL_FOR -->")
                                       ((:loop :loop-else) "<!-- /TMPL_LOOP -->")
                                       ((:repeat) "<!-- /TMPL_REPEAT -->")))))
                                ;; otherwise (EOF before another start marker was
                                ;; seen) just return a template printer which
                                ;; unconditionally prints the rest of the stream
                                (return-from create-template-printer-aux
                                  (create-simple-printer
                                   (cons collector string-stack)))))
   (let* ((whitespace
           (if braces
               ""
               ;; skip whitespace but keep it in case this turns out not
               ;; to be a template tag
               (skip-whitespace :skip nil)))
          (token
           (if braces
               "TMPL_EVAL"
               ;; read what could be a template token's name
               (with-syntax-error-location ()
                 (read-while (lambda (c) (or (alpha-char-p c)
                                             (char= c #\_)
                                             (char= c #\/)))
                             :skip nil
                             :eof-action (lambda (collector)
                                           (declare (ignore collector))
                                           ;; complain about tags that haven't been closed
                                           (signal-unexpected :eof
                                            (format nil
                                              #L" while inside of tag starting with ~S"
                                              *template-start-marker*))))))))
    (cond ((string-equal token "TMPL_EVAL")
            ;; First, read the form that must follow with "usual" Lisp reader
            (let ((form (read-tag-rest :kind :forms :skip 1
                                       :end-marker (if braces "}" *template-end-marker*))))
              (multiple-value-bind (next-fn else-follows)
                  ;; Recursively create the template printer for the rest of the stream
                  (create-template-printer-aux nil end-token)
                ;; Combine it with the strings before the tag
                ;; NB: we don't skip leading and trailing whitespace here
                (values (create-eval-printer (cons string string-stack) form next-fn)
                        else-follows))))
          ((string-equal token "TMPL_VAR")
            ;; First, read the symbol that has to follow and intern it
            (let ((symbol (read-tag-rest :kind :attribute)))
              (multiple-value-bind (next-fn else-follows)
                  ;; first we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux nil end-token)
                (values
                 ;; then we combine it with the strings before the tag
                 ;; to create a template printer for TMPL_VAR - note
                 ;; that we don't skip leading and trailing whitespace
                 ;; here
                 (create-var-printer (cons string string-stack)
                                     symbol
                                     next-fn)
                 else-follows))))

          ((or (string-equal token "TMPL_IF")
               (string-equal token "TMPL_UNLESS"))
            ;; First, read the conditional expression that must follow
            (let ((symbol (if *attributes-are-lisp-forms*
                              (read-tag-rest :kind :forms :skip 1)
                              (read-tag-rest :kind :attribute)))
                  (unlessp (string-equal token "TMPL_UNLESS")))
              (multiple-value-bind (if-fn else-follows)
                  (with-syntax-error-location ()
                    ;; then read the stream up to the corresponding
                    ;; TMPL_ELSE, /TMPL_IF, or /TMPL_UNLESS and create
                    ;; a template printer for the "if" (or "unless") part
                    (create-template-printer-aux
                     (skip-trailing-whitespace)
                     ;; this argument denotes that we expect to see
                     ;; TMPL_ELSE _or_ one of /TMPL_IF, /TMPL_UNLESS and,
                     ;; in the second case, want to stop there
                     (if unlessp :unless-else :if-else)))
                (let ((else-fn (cond ((functionp else-follows)
                                      ;; TMPL_ELIF has been was encountered -
                                      ;; it returns the off-the-shelf function
                                      else-follows)
                                     (else-follows
                                      ;; if we encountered TMPL_ELSE read
                                      ;; the stream up to the corresponding
                                      ;; /TMPL_IF or /TMPL_UNLESS and
                                      ;; create a template printer for the "else" part
                                      (with-syntax-error-location ()
                                        (create-template-printer-aux
                                         (skip-trailing-whitespace)
                                         ;; From now on, we expect to see /TMPL_IF
                                         ;; or /TMPL_UNLESS (but not TMPL_ELSE) 
                                         ;; and want to stop there
                                         (if unlessp :unless :if))))
                                     (t ;; use a dummy printer for the "else"
                                        ;; part if we didn't see TMPL_ELSE
                                        #'no-values))))
                  (multiple-value-bind (next-fn else-follows)
                      ;; now we recursively create the template printer
                      ;; for the rest of the stream
                      (create-template-printer-aux (skip-trailing-whitespace)
                                                   end-token)
                    (values
                     ;; then we combine it with the strings before the
                     ;; tag and the "if" and "else" parts to create a
                     ;; template printer for TMPL_IF or TMPL_UNLESS
                     (create-if-printer (cons (skip-leading-whitespace string)
                                              string-stack)
                                        symbol
                                        if-fn
                                        else-fn
                                        next-fn
                                        unlessp)
                     else-follows))))))
          ((string-equal token "TMPL_ELIF")
            (unless (eq end-token :if-else)
              ;; Check if we expected /TMPL_ELIF here, i.e. if an open
              ;; TMPL_IF was pending and we haven't seen TMPL_ELSE before
              (with-syntax-error-location ()
                (signal-unexpected token)))
            ;; Next read the conditional expression which has to follow
            (let* ((symbol (if *attributes-are-lisp-forms*
                               (read-tag-rest :kind :forms :skip 1)
                               (read-tag-rest :kind :attribute)))
                   ;; The value to be returned and assigned to if-fn of the caller
                   (fn (create-simple-printer (cons (skip-leading-whitespace string)
                                                    string-stack))))
              ;; Then read the stream up to the following TMPL_ELIF, TMPL_ELSE
              ;; or /TMPL_IF and create a template printer for the "elif" part.
              (multiple-value-bind (if-fn else-follows)
                  (with-syntax-error-location ()
                    (create-template-printer-aux (skip-trailing-whitespace) end-token))
                (let ((else-fn (cond ((functionp else-follows)
                                      ;; Another TMPL_ELIF was encountered -
                                      ;; it returns the off-the-shelf function
                                      else-follows)
                                     (else-follows
                                      ;; As we encountered TMPL_ELSE, read
                                      ;; the stream up to the closing /TMPL_IF
                                      ;; and create a template printer for the "else" part
                                      (with-syntax-error-location ()
                                        ;; From now on, we expect /TMPL_IF, not TMPL_ELSE
                                        (create-template-printer-aux
                                         (skip-trailing-whitespace) :if)))
                                     (t ;; use a dummy printer for the "else"
                                        ;; part if we didn't see TMPL_ELSE
                                        #'no-values))))
                  (values
                   fn
                   ;; then we combine it with the strings before the
                   ;; tag and the "if" and "else" parts to create a
                   ;; template printer for TMPL_IF or TMPL_UNLESS
                   (create-if-printer nil                 ;(skip-trailing-whitespace)
                                      symbol
                                      if-fn
                                      else-fn
                                      #'no-values         ; no next-fn
                                      nil))))))
          ((string-equal token "TMPL_ELSE")
            (unless (or (eq end-token :if-else) (eq end-token :unless-else))
              ;; check if we expected /TMPL_ELSE here, i.e. if an open
              ;; TMPL_IF or TMPL_UNLESS was pending and we haven't seen TMPL_ELSE before
              (with-syntax-error-location ()
                (signal-unexpected token)))
            ;; read the rest of the tag but ignore it - no attributes expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "if" or "elif" part
            ;; The actual "else" printer will be created by the TMPL_IF, 
            ;; TMPL_ELIF, or TMPL_UNLESS branch.
            (values
             (create-simple-printer (cons (skip-leading-whitespace string)
                                          string-stack))
             ;; return a true second value to denote that we've seen TMPL_ELSE
             t))
          ((string-equal token "/TMPL_IF")
            (unless (or (eq end-token :if) (eq end-token :if-else))
              ;; check if we expected /TMPL_IF here, i.e. if an open
              ;; TMPL_IF was pending
              (with-syntax-error-location ()
                (signal-unexpected token)))
            ;; read the rest of the tag but ignore it - no attributes expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "if" or "else" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((string-equal token "/TMPL_UNLESS")
            (unless (or (eq end-token :unless) (eq end-token :unless-else))
              ;; check if we expected /TMPL_UNLESS here, i.e. if an open
              ;; TMPL_UNLESS was pending
              (with-syntax-error-location ()
                (signal-unexpected token)))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "unless" or "else" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))

          ((string-equal token "TMPL_FOR")
           (multiple-value-bind (symbol forms) (read-tag-rest :kind :forms :skip 2)
             (unless (and (setq symbol (pop forms))
                          (symbolp symbol))
               (with-syntax-error-location ()
                 (signal-template-syntax-error
                  #L"In tag TMPL_FOR, incorrect syntax of variable: ~S" symbol)))
             ;; Create a template printer for the loop body foreseeing both
             ;; TMPL_FORELSE and /TMPL_FOR
             (multiple-value-bind (body-fn else-follows)
                 (with-syntax-error-location ()
                   (create-template-printer-aux (skip-trailing-whitespace) :for-else))
               (let ((else-fn (if else-follows
                                  ;; As we have encountered TMPL_FORELSE, read
                                  ;; the stream up to the corresponding /TMPL_FOR
                                  ;; and create a template printer for the "else" part
                                  (with-syntax-error-location ()
                                    (create-template-printer-aux (skip-trailing-whitespace)
                                                                 :for))
                                  #'no-values)))              ; dummy printer
                  (multiple-value-bind (next-fn else-follows)
                      ;; Recursively create the template printer for the rest of the stream
                      (create-template-printer-aux (skip-trailing-whitespace)
                                                   end-token)
                    ;; Combine the strings before the tag and the "for" and "forelse" parts
                    (values (create-for-printer (cons (skip-leading-whitespace string)
                                                      string-stack)
                                                symbol (first forms)
                                                body-fn else-fn next-fn)
                            else-follows))))))
          ((string-equal token "TMPL_FORELSE")
            (unless (eq end-token :for-else)
              (with-syntax-error-location ()
                (signal-unexpected token)))
            (read-tag-rest)
            ;; Make a simple printer of the strings - this is the end of for body
            ;; The actual "else" printer will be created by the TMPL_FOR branch
            (values (create-simple-printer (cons (skip-leading-whitespace string)
                                                 string-stack))
                    t))                            ; denotes that we've seen TMPL_FORELSE
          ((string-equal token "/TMPL_FOR")
            (unless (or (eq end-token :for) (eq end-token :for-else))
              (with-syntax-error-location ()
                (signal-unexpected token)))
            (read-tag-rest)
            ;; Make a simple printer of the strings -
            ;; this is the end of either for body or "forelse" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))

          ((or (string-equal token "TMPL_LOOP")
               (string-equal token "TMPL_REPEAT"))
           ;; First, read the form or symbol that must follow
           (let ((kind (if (string-equal token "TMPL_LOOP") :loop-else :repeat))
                 (form (if *attributes-are-lisp-forms*
                           (read-tag-rest :kind :forms :skip 1)
                           (read-tag-rest :kind :attribute))))      ; intern the symbol
             ;; Read the stream up to the corresponding "else" part or end tag 
             ;; and create a template printer for the loop body
             (multiple-value-bind (body-fn else-follows)
                 (with-syntax-error-location ()
                   (create-template-printer-aux (skip-trailing-whitespace) kind))
               (let ((string-list (cons (skip-leading-whitespace string) string-stack))
                     (else-fn (if else-follows
                                  ;; As we have encountered TMPL_LOOPELSE, read
                                  ;; the stream up to the corresponding /TMPL_LOOP
                                  ;; and create a template printer for the "else" part
                                  (with-syntax-error-location ()
                                    (create-template-printer-aux (skip-trailing-whitespace)
                                                                 :loop))
                                  #'no-values)))              ; dummy printer
                 ;; Create the template printer for the rest of the stream
                 (multiple-value-bind (next-fn else-follows)
                     (create-template-printer-aux (skip-trailing-whitespace) end-token)
                   ;; Combine it with the strings before the tag, "else" part,
                   ;; and the body printer
                   (values (if (eq kind :loop-else)
                               (create-loop-printer string-list
                                                    form body-fn else-fn next-fn)
                               (create-repeat-printer string-list
                                                      form body-fn next-fn)) ; no else-fn
                           else-follows))))))
          ((string-equal token "TMPL_LOOPELSE")
            (unless (eq end-token :loop-else)
              (with-syntax-error-location ()
                (signal-unexpected token)))
            (read-tag-rest)
            ;; Make a simple printer of the strings - this is the end of for body
            ;; The actual "else" printer will be created by the TMPL_LOOP branch
            (values (create-simple-printer (cons (skip-leading-whitespace string)
                                                 string-stack))
                    t))                           ; denotes that we've seen TMPL_LOOPELSE
          ((string-equal token "/TMPL_LOOP")
            (unless (or (eq end-token :loop-else) (eq end-token :loop))
              ;; check if we expected /TMPL_LOOP here, i.e. an open TMPL_LOOP was pending
              (with-syntax-error-location ()
                (signal-unexpected token)))
            ;; read the rest of the tag but ignore it - no attributes  expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some TMPL_LOOP body
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((string-equal token "/TMPL_REPEAT")
            (unless (eq end-token :repeat)
              ;; check if we expected /TMPL_REPEAT here, i.e. if an open
              ;; TMPL_REPEAT was pending
              (with-syntax-error-location ()
                (signal-unexpected token)))
            ;; read the rest of the tag but ignore it - no attributes expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some TMPL_REPEAT body
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))

          ((string-equal token "TMPL_LET")
           ;; Read the binding pairs
           (let ((bindings (nth-value 1 (read-tag-rest :kind :forms
                                                       :skip most-positive-fixnum))))
             (dolist (pair bindings)
               (unless (and pair
                            (or (symbolp pair)
                                (and (consp pair) (first pair) (symbolp (first pair)))))
                 (with-syntax-error-location ()
                   (signal-template-syntax-error #L"In ~A, incorrect syntax of binding: ~S"
                                                 token pair))))
             (let ((body-fn (with-syntax-error-location ()
                              (create-template-printer-aux (skip-trailing-whitespace)
                                                           :let))))
               ;; Create the template printer for the rest of the stream
               (multiple-value-bind (next-fn else-follows)
                   (create-template-printer-aux (skip-trailing-whitespace) end-token)
                 ;; Combine it with the strings before the tag
                 (values (create-let-printer (cons (skip-leading-whitespace string)
                                                   string-stack)
                                             bindings body-fn next-fn)
                         else-follows)))))
          ((string-equal token "/TMPL_LET")
            (unless (eq end-token :let)
              (with-syntax-error-location ()
                (signal-unexpected token)))
            ;; Read the rest of the tag but ignore it - no attributes expected
            (read-tag-rest)
            (create-simple-printer (cons (skip-leading-whitespace string) string-stack)))

          ((string-equal token "TMPL_INCLUDE")
           ;; Read the pathname which has to follow
           (multiple-value-bind (pathname bindings)
               (read-tag-rest :kind :attribute :intern nil
                              :skip (if *attributes-are-lisp-forms*
                                        most-positive-fixnum
                                        nil))
             (dolist (pair bindings)
               (unless (and (consp pair) (first pair) (symbolp (first pair)))
                 (with-syntax-error-location ()
                   (signal-template-syntax-error #L"In ~A, incorrect syntax of binding: ~S"
                                                 token pair))))
             ;; Create and cache in create-include-printer rather than here
             (multiple-value-bind (next-fn else-follows)
                 ;; first we recursively create the template printer
                 ;; for the rest of the stream
                 (create-template-printer-aux (skip-trailing-whitespace)
                                              end-token)
               ;; then we combine it with the strings before the tag
               ;; to create a template printer for TMPL_INCLUDE
               (values
                (create-include-printer (cons (skip-leading-whitespace string)
                                              string-stack)
                                        pathname
                                        next-fn
                                        bindings)
                else-follows))))

          ((string-equal token "TMPL_CALL")
            ;; Read the form or symbol which has to follow
           (let ((symbol (if *attributes-are-lisp-forms*
                             (read-tag-rest :kind :forms :skip 1)
                             (read-tag-rest :kind :attribute))))
             (multiple-value-bind (next-fn else-follows)
                  ;; recursively create the template printer for the rest of the stream
                 (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
               ;; create the printer that will output the strings
               ;; before this tag and call the templates stored under  SYMBOL
               (values (funcall #'create-call-printer
                                (cons (skip-leading-whitespace string)
                                      string-stack)
                                symbol
                                next-fn)
                       else-follows))))
          (t
            ;; We have failed to identify a valid tag, so we treat
            ;; everything we've read so far as a literal string and
            ;; carry on - if we're lucky our CL implementation will
            ;; optimize this tail call into an iterative loop
            (create-template-printer-aux (list* token whitespace *template-start-marker*
                                                string
                                                string-stack)
                                         end-token))))))

(defun %create-template-printer-aux (&rest args)
  "Wrapper for CREATE-TEMPLATE-PRINTER-AUX to initialize
*CURRENT-COLUMN* and *CURRENT-LINE*."
  (let ((*current-column* 0)
        (*current-line* 1))
    (apply #'create-template-printer-aux args)))
