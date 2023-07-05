;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HTML-TEMPLATE; Base: 10 -*-
;;; YHTML-Template
;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2014, Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

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

(in-package :html-template)

(defun no-values (&rest rest)
  "A function which does not return any values. This is always the
last function in a chain of template printer closures."
  (declare (ignore rest))
  (values))

(defun list-to-string (string-list)
  "Concatenates a list of strings to one string in reverse order. The
list is destructively modified."
  ;; note that we can't use APPLY with CONCATENATE here because of
  ;; CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (dolist (string string-list)
      (incf total-size (length string)))
    (let ((result-string (make-string total-size
                                      #+lispworks #+lispworks
                                      :element-type 'lw:simple-char))
          (curr-pos 0))
      (dolist (string (nreverse string-list))
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun %read-char ()
  "Like READ-CHAR but updates the line and column counters."
  (let ((char (read-char)))
    (cond ((char= char #\Newline)
            (setf *current-column* 0)
            (incf *current-line*))
          (t (incf *current-column*)))
    char))

(declaim (inline whitespacep))
(defun whitespacep (char)
  "Checks whether CHAR is whitespace."
  #+lispworks
  (lw:whitespace-char-p char)
  #-lispworks
  (find char '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page)))

(defun read-while (predicate &key (skip t) (eof-action t))
  "Reads characters from *STANDARD-INPUT* while PREDICATE returns a
true value for each character.
Returns the string which was read unless SKIP is true.
On reading EOF 
- either an error is signaled if EOF-ACTION is T,
- or NIL is silently returned if EOF-ACTION is NIL,
- or the function EOF-ACTION is called with one argument - the string
  read so far."
  ;; Q: Can the EOF-ACTION function return multiple values?
  (let ((collector (or skip
                       (make-array 0
                                   :element-type 'character
                                   :fill-pointer t
                                   :adjustable t))))
    (handler-case
      (loop for c = (peek-char)
            while (funcall predicate c)
            if skip do (%read-char)
            else do (vector-push-extend (%read-char) collector)
            end
            finally (return collector))
      (end-of-file ()
        (cond ((eq eof-action t)
               (signal-unexpected :eof))
              ((null eof-action)
               nil)
              (t (funcall eof-action collector)))))))

(defun read-delimited-string (&key (eof-action t))
  "Reads and returns a string from *STANDARD-INPUT*. The string is
either delimited by ' or \" in which case the delimiters aren't
returned or it is assumed to extend to the next whitespace
character. See READ-WHILE's docstring for EOF-ACTION."
  (handler-case
    (let* ((peek-char (peek-char))
           (delimiter (find peek-char '(#\' #\"))))
      (when delimiter
        (%read-char))
      (prog1
        (read-while (if delimiter
                        (lambda (c) (char/= c delimiter))
                        (complement #'whitespacep))       ;(not (whitespacep c))))
                    :skip nil
                    :eof-action eof-action)
        (when delimiter
          (%read-char))))
    (end-of-file ()
      (cond ((eq eof-action t)
             (signal-unexpected :eof #L" while reading (delimited) string"))
            ((null eof-action)
              nil)
            (t (funcall eof-action ""))))))

(defun skip-whitespace (&key assert (skip t) (eof-action t))
  "Read characters from *STANDARD-INPUT* as long as they are
whitespace. Signals an error if the first character read isn't
whitespace and ASSERT is true. Return the string which was read unless
SKIP is true. See READ-WHILE's docstring for EOF-ACTION."
  (handler-case
    (progn
      (when assert
        (with-syntax-error-location ()
          (unless (whitespacep (peek-char))
            (signal-unexpected (peek-char) " where whitespace was expected"))))
      (read-while #'whitespacep
                  :skip skip
                  :eof-action eof-action))
    (end-of-file ()
      (cond ((eq eof-action t)
             (signal-unexpected :eof #L" while skipping whitespace"))
            ((null eof-action)
             nil)
            (t (funcall eof-action ""))))))

(defun skip-trailing-whitespace ()
  "Reads and skips whitespace from *STANDARD-INPUT* until a #\Newline
characters is seen if *IGNORE-EMPTY-LINES* is true. If there is no
#\Newline before the first non-whitespace character the string read so
far is returned \(wrapped in a list)."
  (cond (*ignore-empty-lines*
          (let ((string (read-while (lambda (c)
                                      (and (whitespacep c)
                                           (char/= #\Newline c)))
                                    :skip nil
                                    :eof-action nil)))
            (case (peek-char nil nil nil nil)
              ((#\Newline)
                nil)
              (otherwise
                (list string)))))
        (t nil)))
    
(defun read-until (string &key (skip t) (eof-action t) braces)
  "Reads characters from *STANDARD-INPUT* up to and including STRING.
Arguments:
 EOF-ACTION See READ-WHILE's docstring.
 BRACES     If true, expect opening curly brace { allong with the STRING.
Values:
 1) The string which was read \(excluding STRING) unless SKIP is true.
 2) The end marker corresponding to a start marker found (only when brace
    is true); NIL usually means *template-end-marker*."
  (let* ((length (length string))
         (offsets
           ;; we first check whether some substring which starts
           ;; STRING can be found again later in STRING - this is
           ;; necessary because we only peek one character ahead
           (cond ((gethash string *find-string-hash*))
                 (t (setf (gethash string *find-string-hash*)
                            ;; the resulting array of offsets is
                            ;; cached in *FIND-STRING-HASH* so we can
                            ;; use it again in case READ-UNTIL is
                            ;; called with the same STRING argument
                            (loop with offsets = (make-array length
                                                             :initial-element nil)
                                  for i from 1 below length
                                  ;; check if STRING starting from 0
                                  ;; has something in common with
                                  ;; STRING starting from I
                                  for mismatch = (mismatch string string
                                                           :start1 i :test #'char=)
                                  when (> mismatch i)
                                  ;; if this is the case remember the
                                  ;; length of the match plus the
                                  ;; character which must follow in
                                  ;; OFFSETS
                                  do (push (cons (char string (- mismatch i))
                                                 (1+ (- mismatch i)))
                                           (svref offsets i))
                                  finally (return offsets))))))
         (collector (or skip
                        (make-array 0
                                    :element-type 'character
                                    :fill-pointer t
                                    :adjustable t))))
    (handler-case
      (loop for i = 0 then (cond (match (1+ i))
                                 ;; if there is an offset (see above)
                                 ;; we don't have to start from the
                                 ;; beginning of STRING
                                 ((cdr (assoc c (svref offsets i))))
                                 (t 0))
            for c = (peek-char)
            for match = (cond ((and braces (char= c #\{))
                               (%read-char)
                               (return (values collector t)))
                              (t (char= c (char string i))))
            while (or (not match)
                      (< (1+ i) length))
            do (cond (skip (%read-char))
                     (t (vector-push-extend (%read-char) collector)))
            finally (%read-char)
            (unless skip
              ;; decrement the fill pointer because collector now also
              ;; contains STRING itself
              (decf (fill-pointer collector) (1- length)))
            (return collector))
      (end-of-file ()
        (cond ((eq eof-action t)
               (signal-unexpected :eof))
              ((null eof-action)
                nil)
              (t (funcall eof-action collector)))))))

(defun skip-leading-whitespace (string)
  "Removes whitespace from the end of STRING up to and including a
#\Newline. If there is no #\Newline before the first non-whitespace
character is seen nothing is removed. STRING must have a fill
pointer."
  (when *ignore-empty-lines*
    (let ((old-fill-pointer (fill-pointer string)))
      (loop for fill-pointer = (fill-pointer string)
            for char = (and (plusp fill-pointer)
                            (char string (1- fill-pointer)))
            while (and char
                       (whitespacep char)
                       (char/= #\Newline char))
            do (decf (fill-pointer string)))
      (cond ((let ((fill-pointer (fill-pointer string)))
               (and (plusp fill-pointer)
                    (char= #\Newline (char string (1- fill-pointer)))))
              (decf (fill-pointer string)))
            (t
              (setf (fill-pointer string)
                      old-fill-pointer)))))
  string)

(defun read-tag-rest (&key kind (intern t) (eof-action t) skip
                           (end-marker *template-end-marker*))
  "Reads the rest of a template tag from *STANDARD-INPUT* after the
name of the tag has been read.
Arguments:
KIND   One of
  :forms     - Read forms, either a single or the number specified by skip.
  :attribute - The tag attribute in the old syntax (as if READ-ATTRIBUTE is true);
               Also interns the attribute string if INTERN is true.
  NIL        - Nothing except comment is allowed. 
SKIP   Specifies what to do when some text appears between the tag name
       or the following attribute/form and the ending marker -->. One of:
  n   - Integer: read n forms from this string, collect into a list,
        and return it as the second value.
        Equals to most-positive-fixnum if no limit.
  true - Skip everything without error.
  false- Signal if not empty and *IGNORE-TAG-REST* is false.
EOF-ACTION
       See READ-WHILE docstring."
  (with-syntax-error-location ()
    (let (rest)
      (handler-case
          (let ((attribute nil)
                (forms ()))
            (flet ((%read-forms (&optional (eof-error-p
                                            (and (integerp skip)
                                                 (not (eql skip most-positive-fixnum)))))
                     ;; a la with-standard-io-syntax
                     (let ((*package* *template-package*)
                           (*read-eval* nil)
                           (*read-suppress* nil)
                           (*readtable* (or *template-readtable*
                                            (setq *template-readtable*
                                                  (copy-readtable nil))))
                           (start 0))
                       (when (eq eof-action t)
                         (setq eof-action :forms))     ; for proper error message
                       (dotimes (i (if (integerp skip) skip most-positive-fixnum))
                         (multiple-value-bind (form end)
                             (read-from-string rest eof-error-p :eof
                                               :start start :preserve-whitespace nil)
                           (when (eq form :eof)         ; error is not surpressed
                             (return))                  ; if the form is incomplete
                           (push form forms)
                           (setq start end)))
                       (setq forms (nreverse forms)
                             rest (subseq rest start)))))
              (ecase kind
                (:attribute
                 (skip-whitespace :assert t)
                 (let ((string
                        (read-delimited-string
                         :eof-action (lambda (collector)
                                       (declare (ignore collector))
                                       #1=(signal-unexpected :oef
                                           #L" while reading tag attribute")))))
                   (setq attribute (if intern
                                       (intern (if *upcase-attribute-strings*
                                                   (string-upcase string)
                                                   string)
                                               *template-symbol-package*)
                                       string)))
                 (skip-whitespace)
                 (setq rest (read-until end-marker
                                        :skip (if (integerp skip) nil skip)
                                        :eof-action eof-action))
                 (when (integerp skip)
                   (%read-forms)))
                (:forms
                 ;; Further forms can immediately follow without whitespace
                 (setq rest (read-until end-marker
                                        :skip nil :eof-action eof-action))
                 (%read-forms)
                 (when (eql skip 1)
                   (setq attribute (pop forms))))
                ((nil)
                 (skip-whitespace)
                 (setq rest (read-until end-marker
                                        :skip skip :eof-action eof-action)))) )
            (when (and (not *ignore-tag-rest*)
                       (not (eq rest skip))
                       (plusp (length rest)))
              (signal-template-syntax-error #L"Expected ~S but read ~S"
                                            end-marker rest))
            (values attribute forms))
        (end-of-file ()
          (cond ((eq eof-action t)
                 (signal-unexpected :eof))
                ((eq eof-action :forms)
                 #1#)
                ((null eof-action)
                 nil)
                (t (funcall eof-action rest))))))))

(defun escape-string (string &key (test *escape-char-p*))
  (declare (optimize speed)
           (type string string))
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (let ((first-pos (position-if test string)))
    (if (not first-pos)
      ;; nothing to do, just return STRING
      string
      (with-output-to-string (s)
        (loop with len = (length string)
              for old-pos = 0 then (1+ pos)
              for pos = first-pos
                  then (position-if test string :start old-pos)
              ;; now the characters from OLD-POS to (excluding) POS
              ;; don't have to be escaped while the next character has to
              for char = (and pos (char string pos))
              while pos
              do (write-sequence string s :start old-pos :end pos)
                 (case char
                   ((#\<)
                     (write-sequence "&lt;" s))
                   ((#\>)
                     (write-sequence "&gt;" s))
                   ((#\&)
                     (write-sequence "&amp;" s))
                   ((#\')
                     (write-sequence "&#039;" s))
                   ((#\")
                     (write-sequence "&quot;" s))
                   (otherwise
                     (format s "&#~d;" (char-code char))))
              while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))

(defun escape-string-minimal (string)
  "Escape only #\<, #\>, and #\& in STRING."
  (escape-string string :test (lambda (char) (find char "<>&"))))

(defun escape-string-minimal-plus-quotes (string)
  "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
  (escape-string string :test (lambda (char) (find char "<>&'\""))))

(defun escape-string-iso-8859-1 (string)
  "Escapes all characters in STRING which aren't defined in ISO-8859-1."
  (escape-string string :test (lambda (char) (or (find char "<>&'\"")
                                                 (> (char-code char) 255)))))

(defun escape-string-all (string)
  "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
  (escape-string string :test (lambda (char) (or (find char "<>&'\"")
                                                 (> (char-code char) 127)))))

(defun template-eval (form &optional (environment *template-environment*))
  "Template evaluator combining both eval and apply.
Arguments:
 FORM
   If it is list, the evaluator treats specially the functions that are standard
   macros or special operators in Lisp. For now, the following are recognized:
     IF WHEN UNLESS AND OR NOT QUOTE PROGN SETQ.
   If it is a symbol, the evaluator looks at its name first;
   when the name is like *var*, it tries to interpret the symbol as
   dynamic variable and retreive its value via symbol-value;
   otherwise the look-up proceeds via *value-access-function*.
 ENVIRONMENT
   Binds template variables with values (aka template structure)."
  (yl:tiny-eval form environment))
