;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; YstokHTML - (X)HTML generation and parser library
;;; Copyright (c) 2004-2014 Dr. Dmitriy Ivanov
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LHTML and LHTT-based generation

;;; Lisp Hyper-Text Template (LHTT) is an LHTML extension.
;;; In brief, a document can contain additional pseudo-elements:
;;;   :eval, :if, :for, :loop, and the like.
;;; The names assume corresponding functionality but they act during print-time.
;;; While printing, these elements perform evaluation of pseudo-expressions,
;;; simplified S-expressions, by virtue of the template-eval function.
;;;
;;; Limitations
;;; 1) Tags cannot be computed via templates, e.g. the follwing is illegal:
;;;	 h<!-- TMPL_EVAL toc-level -->
;;;
;;; 2) Template expansion cannot spread across element borders.
;;;    Wrong code example:
;;;      <tr><td>One<!-- TMPL_IF foo --></td><td>Two<!-- /TMPL_IF --></td></tr>
;;;    The result of expansion must be enclosed by some parent element
;;;    and must represent plain content, an element tree, or list of trees.
;;;
;;; 3) Only TMPL_EVAL pseudo-tags and embraced {form} are allowed within
;;;    an attribute value.
;;;    Those tags must be embedded into a string, which is replaced by the list
;;;      (:optional string-or-form [string-or-form]...) 
;;;
;;; 4) Starting and ending tag markers must be as for comment, i.e. <!-- and -->.
;;;
;;;
;;; YHTML-Template and LHTT notes
;;; 1) Ending tag can contain optional text, which is ignored as comment, e.g.
;;;	<!-- /TMPL_LOOP rows -->
;;;
;;; 2) As per YHTML-Template, tags names can be uppercased, lowercased, or of mixed case.
;;;    For example, the following three tags are equavalent:
;;;	<!-- TMPL_IF expr -->
;;;     <!-- tmpl_if expr -->
;;;     <!-- Tmpl_If expr -->
;;;
;;; 3) Templates embraced by {} are expanded even within ordinary (non-pseudo-tag)
;;;    comments.

(in-package :html)

(defconstant* +spec-bad-arg-type-format-string+
  "In LHTML pseudo-tag ~a, bad type of argument~@[ ~s~]")

;;; Pseudo-tags definition
;;; :eval is used only for content generation, not for attribute values - see :optional.
;;; If the primary value returned by htt::template-eval is list of the form
;;;	(:content . content)
;;; then the content must be a list of LHTML forms, and we keep on walking over them
;;; with the HTMLGen printer rather than HTML-TEMPLATE procedures.
;;;
;;; Args: desc    Instance of descriptor
;;:       attrs  Attribute plist. Shall we use it to augment *template-environment*?

(def-special-html :eval 
  t						; for tag-standard-p => nil
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore attrs desc))
    (let (length)
      (if (and (eq cmd :full) (= (setq length (length form)) 2))
          (multiple-value-bind (value do-not-modify)
              (htt:template-eval (second form))
            (if (and (consp value) (eq (first value) :content))	; LHTML forms
                  ;(let ((*lhtml-safe* do-not-modify))  ; do-not-modify == safep?
                (html-print-list-subst (rest value) subst stream)
                (let (string)
                  (cond ((null value)
                         (setq string
                               (cond ((eq htt:*format-null-forms* t)
                                      +null-string+)
                                     ((stringp htt:*format-null-forms*)
                                      (format nil htt:*format-null-forms* (second form)))
                                     (t
                                      (htt::with-use-value-restart (form)
                                        (htt::signal-template-missing-value-error
                                         "The value of form ~S is NIL"
                                         form))))))
                        ((stringp value)
                         (setq string value))
                        ((eq htt:*format-non-strings* t)
                         (setq string (princ-to-string value)))
                        (htt:*format-non-strings*
                         (multiple-value-setq (string do-not-modify)
                             (funcall htt:*format-non-strings* value)))
                        (t
                         (htt::with-use-value-restart (form)
                          (error 'htt::template-not-a-string-error
                                 :value value
                                 :format-control
                                 "The value ~S of form ~S is not a string"
                                 :format-arguments (list value form)))))
                  (write-html-string (if do-not-modify
                                         string
                                         (funcall htt:*string-modifier* string))
                                     stream))))
          (error +spec-bad-args-format-string+ :eval length)))))

(def-special-html :if
  ;; (:if condition true-body false-body)
  t						; for tag-standard-p => nil
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore attrs desc))
    (let (val)
      (if (and (eq cmd :full) (<= 3 (setq val (length form)) 4))
          (cond ((htt:template-eval (second form))
                 (html-print-list-subst (third form) subst stream))
                ((setq val (fourth form))
                 (html-print-list-subst val subst stream)))
          (error +spec-bad-args-format-string+ :if val))))) 

(def-special-html :cond
  ;; (:cond (condition1 . body1) ...)
  t						; for tag-standard-p => nil
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore attrs desc))
    (let (val)
      (if (and (eq cmd :full) (<= 2 (setq val (length form))))
          (dolist (tuple (rest form))
            (when (htt:template-eval (first tuple))
              (html-print-list-subst (rest tuple) subst stream)
              (return)))
          (error +spec-bad-args-format-string+ :cond val))))) 

(def-special-html :for
  ;; ((:for variable sequence-or-integer) else-body . body)
  t						; for tag-standard-p => nil
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore desc))
    (if (and (eq cmd :full) (cddr form))
        (let ((var (first attrs))
              (sequence (htt:template-eval (second attrs)))
              (ff (cddr form)))
          (cond ;((not (and var (symbolp var)))		; checked during transformation
                ; (error +spec-bad-arg-type-format-string+ :for var))
                ((vectorp sequence)
                 (if (plusp (length sequence))
                     (loop with pair = (cons var nil)
                           with *template-environment* = (cons pair *template-environment*)
                           for val across sequence
                           do (rplacd pair val)
                              #1=(html-print-list-subst ff subst stream))
                     #2=(html-print-list-subst (second form) subst stream)))
                ((consp sequence)
                 (let* #3=((pair (cons var nil))
                           (*template-environment* (cons pair *template-environment*)))
                   (dolist (val sequence)
                     (rplacd pair val)
                     #1#)))
                ((null sequence)
                 #2#)
                ((integerp sequence)
                 (if (plusp sequence)
                     (let* #3#
                       (dotimes (val sequence)
                         (rplacd pair val)
                         #1#))
                     #2#))
                (t
                 (error +spec-bad-arg-type-format-string+ :for sequence))))
        (error +spec-bad-args-format-string+ :for nil))))

(def-special-html :loop
  ;; (:loop condition else-body . body)
  t						; for tag-standard-p => nil
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore attrs desc))
    (if (and (eq cmd :full) (cddr form))
        (let ((sequence (htt:template-eval (second form)))
              (ff (cdddr form)))
          (cond ((vectorp sequence)
                 (if (plusp (length sequence))
                     (loop for env across sequence
                           do #1=(let ((*template-environment*
                                        (append env *template-environment*)))
                                   (html-print-list-subst ff subst stream)))
                     #2=(html-print-list-subst (third form) subst stream)))
                ((consp sequence)
                 (dolist (env sequence)
                   #1#))
                ((null sequence)
                 #2#)
                (t
                 (error +spec-bad-arg-type-format-string+ :loop sequence))))
        (error +spec-bad-args-format-string+ :loop nil))))

(def-special-html :repeat
  t						; for tag-standard-p => nil
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore attrs desc))
    (if (and (eq cmd :full) (cdr form))
        (let ((count (htt:template-eval (second form))))
          (when (iplusp count)
            (loop with ff = (cddr form)
                  repeat count
                  do (html-print-list-subst ff subst stream))))
        (error +spec-bad-args-format-string+ :repeat))))

(def-special-html :let
  ;; ((:let var1 form1...) . body)
  t						; for tag-standard-p => nil
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore desc))
    (if (and (eq cmd :full) (cdr form))
        (let ((new *template-environment*))
          (loop for rest on attrs by #'cddr
                for var = (first rest)
                if (and var (symbolp var))
                do (setq new (acons var
                                    (htt:template-eval (second rest)
                                                                 *template-environment*)
                                    new))
                else do (error +bad-variable-format-string+ :let var))
          (let ((*template-environment* new))
            (html-print-list-subst (cdr form) subst stream)))
        (error +spec-bad-args-format-string+ :let nil))))

(def-special-html :call
  t						; for tag-standard-p => nil
  (lambda (desc cmd attrs form subst stream)
    (declare (ignore desc cmd attrs form subst stream))
    (error ":call not implemented")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LHTML -> LHTT  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transform YHTML-Template patterns to LHTML pseudo-tags
;;; Convert comments and embraced forms to elements and attribute values,
;;; read from inner strings and intern symbols.
;;;
;;; CAUTION:
;;;  If LHTML is a result of
;;;	(phtml:parse-html ... :retain-whitespace :newline) - default
;;;  every embraced {form} within content must reside on a single line
;;;  in the source file!
;;;  (But can spread multiple lines if within an attribute value.)
;;;
;;; NB: As determing an exact line number is not possible as a rule,
;;;     content of an LHTML element is passed as the line argument.
;;; NB: The compactness of the result depends on htt:*ignore-empty-lines*

(defun signal-template-error (format-control &rest args &key format-arguments line
                                                        &allow-other-keys)
  ;(apply #'error 'htt::template-syntax-error :format-control format-control args))
  (warn #L"** ~?~@[~%Line: ~s~]" format-control
        (append format-arguments (remove-properties args '(:format-arguments :line)))
        line))

(define-file-format :htt (:ascii) "HTT")

(defmethod include-file-using-format ((format (eql :htt)) (pathname pathname)
                                      &key (defaults htt:*default-template-pathname*)
                                           (element-type #-lispworks 'character
                                                         #+lispworks 'lw:simple-char)
                                           (external-format htt::*external-format*))
  (declare (ignore format))
  (template:fill-and-print-template (if defaults
                                        (merge-pathnames pathname defaults)
                                        pathname)
                                    *template-environment*
                                    :stream html:*html-stream*	; output stream
                                    :element-type element-type
                                    :external-format external-format))

(defun template-read-from-string (string
                                  &key (start 0) end line (eof-action t)
                                       (kind :form)
                                       ((:package *package*) htt:*template-package*))
 ;;; Args: line Usually LTHML element content.
  ;: Values: 1) Form, string, or NIL (the latter may be due to EOF).
  ;;         2) Integer position or :eof in case of EOF
  (ecase kind
    (:form
     (handler-case (let ((*read-eval* nil)
                         (*read-suppress* nil)
                         (*readtable* (orf htt:*template-readtable*
                                           (copy-readtable nil))))
                     (read-from-string string t nil :start start :end end))
       (end-of-file (c)
         (cond ((eq eof-action t)
                #1=(signal-template-error
                    #L"EOF while reading pseudo-tag expression <!-- ... ~a -->"
                    :format-arguments (list (subseq string start end))
                    :line line)
                #2=(values nil :eof))
               ((null eof-action)
                #2#)
               (t (funcall eof-action string))))))
    (:attribute
     (let* ((htt::*current-column* start)
            (eof-action (lambda (c) (declare (ignore c)) #1# nil))
            (attr (with-input-from-string (*standard-input* string :start start :end end)
                    (when (htt::skip-whitespace :eof-action eof-action) ;:assert
                      (htt::read-delimited-string :eof-action eof-action)))))
       (if attr
           (values attr htt::*current-column*)
           #2#))) ))

(defun transform-elem (transformer tree &optional parents)
 ;;; Recursively transform document tree by calling the transformer function on each
  ;; node in depth-first manner: the function is called on children before root.
  ;; Args: transformer
  ;;               Signature: (key attrs content &optional parents) => result
  ;;		   where attrs = T means that element is an atom, i.e. a tree leaf.
  ;;                     result= element or NIL if the tree should be ignored.
  ;;	   tree	   A cons or atom.
  ;;	   parents A route, stack of nodes not including the tree itself.
  ;;		::= (parent grandparent ... top-level-node) -
  ;; Value: The result of applying the function to the root with children converted.
  ;; NB: This function makes a copy of any (sub)element but itself does not copy the
  ;;     attrs list. The transformer should also cautiosly make a fresh copy,
  ;;     including (copy-list attrs) if needed.
  (declare (function transformer))
  (if (consp tree)
      (funcall transformer (elem-key tree) (elem-attrs tree)
               (loop with parents = (cons tree parents)		; augment parents
                     for child in (elem-cont tree)
                     for elem = (transform-elem transformer child parents)
                     when elem collect elem)
               parents)
      (funcall transformer tree t () parents)))

(eval-when (:compile-toplevel :load-toplevel :execute) (meta:enable-meta-syntax))

;;; TWO HELPERS: Process TMPL_EVAL and embraced expressions within attribute 
;;;              and content strings

(defun %parse-eval-substrings (text comment &optional (braces htt:*accept-braces-markers*))
 ;;; Args: comment True = allow comment-style markers
  ;;       braces  True = allow braces markers
  ;; Value: List of triples ((tag-start start . end)...) in direct order
  (let ((acc ())
        tag-start		; tag starting position
        start end)		; form starting and ending positions
    (meta:with-string-meta (text :test #'char-equal)
      (macrolet ((backtrack (&body body)
                   `(progn ,@body (setq meta:%index% tag-start) nil)))
        (flet ((%form ()
                 (setq tag-start meta:%index%
                       end nil)
                 (meta:match
                   {[ "{" !braces
                      !(setq start meta:%index%)
                      ${ [ "}" !(setq end (1- meta:%index%)) !(return-from %form t) ]
                         @(characterp) }
                    ]
                    [ "<!--" !comment {#\Space []} "TMPL_EVAL"
                      !(setq start meta:%index%)
                      ${ [ "-->" !(setq end (- meta:%index% 3)) !(return-from %form t) ]
                         @(characterp) }
                    ]} )
                 (backtrack)))	; does not match if we get here
          (meta:match ${ [ !(%form) !(push (list* tag-start start end) acc) ]
                         @(characterp) } ))))
    (nreverse acc)))

(defun %collect-eval-substrings (text acc make-elem &optional line)
  (do ((i 0)
       (list ())			; list interleaving strings and forms
       (rest acc (rest rest)))
      ((null rest)
       (when (< i (length text))
         (push (subseq text i) list))
       (nreverse list))
    (destructuring-bind (tag-start start . end) 
        (first rest)
      (when (< i tag-start)
        (push (subseq text i tag-start) list))
      (let ((form (template-read-from-string text
                                             :start start :end end
                                             :line line)))
        (push (if make-elem (make-elem :eval nil (list form)) form)
              list))
      (setq i (+ end (if (char= (char text end) #\}) 1 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  %TEMPLATE-TRANSFORMER  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %template-transformer (key attrs cont &optional parents)
 ;;; Transform: LHTML document tree in the two ways:
  ;; (1) Replace (:comment " TMPL_tag") with corresponding pseudo-elements
  ;;	 by walking through cont.
  ;; (2) Replace attrs values "<-- TMPL_EVAL form -->" -> (:optional form)
  ;;	 Any text after closing "-->" is ignored, so the entire value is substituted.
  ;; Args: key   Starting tag of the element
  ;;       attr	 Plist or T (meaning no attribute),
  ;;       cont  Transformed children
  ;; NB: Setting htt:*ignore-empty-lines* to True eliminates Newline
  ;;     that follows any pseudo-tag except TMPL_EVAL.
  ;;     Whitespace should be already removed by phtml.
  ;; NB: Within "ordinary" comments, i.e. not in pseudo-tags, braces are only allowed
  ;;     for SSI directives, e.g. <--# include file="{file}" -->
  #-debug (declare (ignore parents))
  (debug-format :phtml "%tt-r >> key ~s, attrs ~s, cont ~s, parent ~s"
                key attrs cont (first parents))
  (if (eq attrs t)
      key
    (let ((guts ())			; FIFO ((collection [else-collection]
					;       [. {form | var | test-form }])...)
          (pending ())			; FIFO (:key1 ...) of pending block-level tag
          (acc ())			; element's top-level accumulator
          kind
          (braces-in-strings (case key			; true - expand {} in cont strings
                               (:comment		; expand only in SSI directives
                                (let ((child (first cont)))
                                  (and (stringp child)
                                       (plusp (length child))
                                       (char= (char child 0) #\#))))
                               ((:style :script)	; do not expand in raw elements
                                nil)
                               (otherwise		; expand within PCDATA
                                 t)))
          #|(whitespace-ignorable	; if true, remove newline before/after some tags
           (and cont ;htt:*ignore-empty-lines*
                (loop for parent in parents
                    never (phtml:tag-retain-whitespace (elem-key parent)))))|#
         )
     (flet ((%collect (elem)
              (if pending
                  (let ((triple (car guts)))
                    (if (memq (car pending)
                              #1='(:if-else :unless-else :for-else :loop-else))
                        (push elem (second triple))
                        (push elem (first triple))))
                  (push elem acc)))
            (%ignore-prev (list) 
               ;; Skip newline immediately preceeding TMPL_tagELSE or /TMPL_tag
               ;(if (and whitespace-ignorable
               ;         htt:*ignore-empty-lines*
               ;         (eq (first list) :newline))
               ;    (rest list)
              list)
            (%ignore-next (rest)
	       ;; Skip newline immediately following /TMPL_tag by additional move on rest
               ;; NB: (first rest) is the element processed on the current iteration
               (cond ;((not whitespace-ignorable)
                     ; rest)
                     ((eq (second rest) :newline)
                      (if htt:*ignore-empty-lines*
                          (cdr rest)
                          rest))
                     ((phtml:whitespace-string-p (second rest))
                      (if (and htt:*ignore-empty-lines*
                               (eq (third rest) :newline))
                          (cddr rest)
                          (cdr rest)))
                     (rest)))
            (%signal-unexpected (key)
              (signal-template-error #L"Unexpected pseudo-tag ~:[/~;~]TMPL_~a"
                                     :format-arguments (list (eq kind :start-tag) key)
                                     :line cont))
           )
      (do ((elem)
           (rest cont (rest rest)))
          ((null rest))
       (setq elem (first rest))
       (cond ((and (consp elem) (eq (elem-key elem) :comment))
              ;; CAUTION: Comment body comes already transformed, e.g. from embraced
              (let* ((text (first (elem-cont elem)))
                     #|(start (position-if #'pseudo-tag-charp text))
                     (end (when start
                            (position-if-not #'pseudo-tag-charp text :start (1+ start))))
                     (token (when end
                              (subseq text start end))))|#
                     (name nil)
                     (start
                      (when (stringp text)
                       (meta:with-string-meta (text :test #'char-equal)
                        (when (meta:match
                               [ $#\Space
                                 { ["TMPL_" {[{"EVAL" "VAR"} !(setq name :eval)]
                                             ["IF"           !(setq name :if)]
                                             ["ELIF"         !(setq name :elif)]
                                             ["ELSE"         !(setq name :else)]
                                             ["UNLESS"       !(setq name :unless)]
                                             ["FORELSE"      !(setq name :for-else)]
                                             ["FOR"          !(setq name :for)]
                                             ["LOOPELSE"     !(setq name :loop-else)]
                                             ["LOOP"         !(setq name :loop)]
                                             ["REPEAT"       !(setq name :repeat)]
                                             ["LET"          !(setq name :let)]
                                             ["INCLUDE"      !(setq name :include)]
                                             ["CALL"         !(setq name :call)]}
                                    !(setq kind :start-tag)]
                                   ["/TMPL_" {["IF"           !(setq name :if)]
                                              ["UNLESS"       !(setq name :unless)]
                                              ["FOR"          !(setq name :for)]
                                              ["LOOP"         !(setq name :loop)]
                                              ["REPEAT"       !(setq name :repeat)]
                                              ["LET"          !(setq name :let)]}
                                    !(setq kind :end-tag)] }])
                          meta:%index%)))))
                (cond ((null name)
		       ;; Unrecognized comment cannot be eliminated as it can contain
                       ;; transformed children and represent an SSI instruction.
                       (%collect elem))
                      ((eq kind :start-tag)
                       (debug-format :phtml "Starting text ~s start ~a" text start) 
                       (flet ((read-form (&optional (eof-action t) (start start))
                                ;; Values: 1) form or NIL 2) position or :eof
                                (template-read-from-string text :start start :line cont
                                                           :eof-action eof-action)))
                         (case name
                           ((:eval :var)
                            (%collect (make-elem :eval nil (list (read-form)))))
                           ((:if :unless)
                            (push name pending)
                            (push `(nil nil . ,(read-form)) guts))
                           (:else
                            (case (car pending)
                              (:if
                               (rplaca pending :if-else))
                              (:unless
                               (rplaca pending :unless-else))
                              (:cond
                               (let ((triple (car guts)))
                                 (push (cons (cddr triple) (nreverse (%ignore-prev
                                                                      (first triple))))
                                       (second triple))
                                 (setf (cddr triple) t)		; true branch
                                 (rplaca triple nil)))
                              (otherwise (%signal-unexpected name))))
                           (:elif
                            (case (car pending)
                              (:if
                               ;; Replace tripple by (nil list-of-tuples . elif-condition)
                               ;; where tupleI ::= (conditionI formI1 formI2 ...)
                               (rplaca pending :cond)
                               (rplaca guts `(nil (,(cons (cddar guts)
                                                          (nreverse (%ignore-prev
                                                                     (caar guts)))))
                                                  . ,(read-form))))
                              (:cond
                               (let ((triple (car guts)))
                                 (push (cons (cddr triple) (nreverse (%ignore-prev
                                                                      (first triple))))
                                       (second triple))
                                 (setf (cddr triple) (read-form))
                                 (rplaca triple nil)))
                              (otherwise
                               (%signal-unexpected name))))
                           (:for
                            (push name pending)
                            (multiple-value-bind (var position) (read-form)
                              (cond ((eq position :eof))
                                    ((and var (symbolp var))
                                     (push `(nil nil ,var . ,(read-form t position)) guts))
                                    (t
                                     (signal-template-error
                                      +bad-variable-format-string+
                                      :format-arguments (list "TMPL_FOR" var)))))
                            (debug-format :phtml #10="For triple: ~s" (car guts)))
                           (:for-else
                            (unless (eq (car pending) :for)
                              (%signal-unexpected name))
                            (debug-format :phtml #10# (car guts))
                            (rplaca pending name))
                           (:loop
                            (push name pending)
                            (push `(nil nil . ,(read-form)) guts))
                           (:loop-else
                            (unless (eq (car pending) :loop)
                              (%signal-unexpected name))
                            (rplaca pending name))
                           (:repeat
                            (push name pending)
                            (push `(nil . ,(read-form)) guts))
                           (:let
                            (push name pending)
                            (let ((bindings ())
                                  (position start)
                                  pair)
                              (while (multiple-value-setq (pair position)
                                         (read-form nil position))
                                (push pair bindings))
                              (push `(nil . ,bindings) guts)))
                           (:include
                            (let ((plist ())
                                  position file pair)
                              (multiple-value-setq (file position) ;(read-form)
                                (template-read-from-string text :kind :attribute
                                                           :start start :line cont))
                              (unless (eq position :eof)
                                (while (multiple-value-setq (pair position)
                                           (read-form nil position))
                                  (setq plist (list* (second pair) (first pair) plist)))
                                (%collect (make-elem name (nreverse plist) (list file))))))
                           (:call
                            (%collect (make-elem name nil (list (read-form)))))
                      )) )
                      (t
                       (debug-format :phtml "Ending text ~s start ~a" text start)
                       (case name
                         (:if
                          (case (car pending)
                            ((:if :if-else)
                             (let ((triple (pop guts)))
                               (pop pending)
                               (%collect (make-elem :if nil
                                                   (list (cddr triple)
                                                         (nreverse (%ignore-prev
                                                                    (first triple)))
                                                         (nreverse (%ignore-prev
                                                                    (second triple))))))))
                            (:cond
                             (let ((triple (pop guts)))
                               (pop pending)
                               (%collect (make-elem :cond nil
                                                   (nreverse
                                                    (cons (cons (cddr triple)
                                                                (nreverse (%ignore-prev
                                                                           (first triple))))
                                                          (second triple)))))))
                            (otherwise
                             (%signal-unexpected name)))
                          (setq rest (%ignore-next rest)))	; look forward
                         (:unless
                          (unless (memq (pop pending) '(:unless :unless-else))
                            (%signal-unexpected name))
                          (let ((triple (pop guts)))
                            ;; Replace by if but swap args
                            (%collect (make-elem :if nil
                                                 (list (cddr triple)
                                                       (nreverse (%ignore-prev
                                                                  (second triple)))
                                                       (nreverse (%ignore-prev
                                                                  (first triple)))))))
                           (setq rest (%ignore-next rest)))	; look forward
                         (:for
                          (unless (memq (pop pending) '(:for :for-else))
                            (%signal-unexpected name))
                          (let ((triple (pop guts)))
                            (debug-format :phtml #10# triple)
                            (%collect (make-elem name		; save in attrs
                                                (list (caddr triple) (cdddr triple))
                                                (cons (nreverse (%ignore-prev
                                                                 (second triple)))
                                                      (nreverse (%ignore-prev
                                                                 (first triple)))))))
                          (setq rest (%ignore-next rest)))	; look forward
                         (:loop
                          (unless (memq (pop pending) '(:loop :loop-else))
                            (%signal-unexpected name))
                          (let ((triple (pop guts)))
                            (%collect (make-elem name nil
                                                (list* (cddr triple)
                                                       (nreverse (%ignore-prev
                                                                  (second triple)))
                                                       (nreverse (%ignore-prev
                                                                  (first triple)))))))
                          (setq rest (%ignore-next rest)))	; look forward
                         (:repeat
                          (unless (eq (pop pending) name)
                            (%signal-unexpected name))
                          (let ((pair (pop guts)))
                            (%collect (make-elem name nil
                                                (list* (cdr pair)
                                                       (nreverse (car pair))))))
                          (setq rest (%ignore-next rest)))	; look forward
                         (:let
                          (unless (eq (pop pending) name)
                            (%signal-unexpected name))
                          (let ((pair (pop guts)))
                            (%collect (make-elem name
                                                ;; Convert bindings to plist-like attrs
                                                (mapcan (lambda (pair)
                                                          (list (first-or-self pair)
                                                                (if (consp pair)
                                                                    (second pair)
                                                                    nil)))
                                                        (cdr pair))
                                                (nreverse (car pair)))))
                          (setq rest (%ignore-next rest)))	; look forward
             ) ) )    ))
             ((and (eq elem :newline)
                   htt:*ignore-empty-lines*
		   ;; This Newline follows some TMPL_tag immediately or after whitespace
                   (when-bind (cons (cond ((memq (car pending) #1#)
                                           (cdar guts))	; = (second triple)
                                          ((car pending)
                                           (car guts))))  	; = (first triple)
                     (let ((body (car cons)))
                       (or (null body)
                           (and (null (rest body))		; sigleton
                                ;whitespace-ignorable
                                (phtml::whitespace-string-p (first body))
                                (rplaca cons nil)))))		; remove whitespace as well
             ))
             ((and (stringp elem)
                   braces-in-strings
                   htt:*accept-braces-markers*
                   (when-let (list (%parse-eval-substrings elem nil))	; no comment inside
                     (mapc #'%collect (%collect-eval-substrings elem list t cont))
                     t)))
             (t (%collect elem)) ))				; collect element as is
      ;; Test for EOF
      (when-bind (name (first pending))
        (signal-template-error #L"Unexpected EOF - missing pseudo-tag <!-- /TMPL_~a -->"
         :format-arguments (list (case name (:if-else :if)
                                            (:unless-else :unless)
                                            (:for-else :for)
                                            (:loop-else :loop)
                                            (otherwise name)))
         :line cont))
      ;; Transform attributes values that can contain TMPL_EVAL or embraced patterns
      (do ((rest attrs (cddr rest)))
          ((null rest))
        (let ((text (cadr rest)))
          (when (stringp text)
            (when-let (list (%parse-eval-substrings text t))	; comment can occur inside
              (rplaca (cdr rest)
                      (cons :optional (%collect-eval-substrings text list nil cont)))))))
      ;; Finally, recreate element
      (debug-format :phtml "%tt-r << ~s" (make-elem key attrs (reverse acc)))
      (make-elem key attrs (nreverse acc)) ))))

(eval-when (:compile-toplevel :load-toplevel :execute) (meta:disable-meta-syntax))

(defun template-lhtt (elem htt:*template-package* htt:*accept-braces-markers*)
 ;;; Helper
  (let ((htt:*template-symbol-package* htt:*template-package*))
    (transform-elem #'%template-transformer elem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  API  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse-htt (arg &key package braces &allow-other-keys)
 (:method ((list cons) &key (package htt:*template-package*)
                            (braces htt:*accept-braces-markers*))
  ;; Args: list List of LHTML forms
  ;; Make auxiliary top-level element (:start-parse . list) and invoke transformer on it.
  ;; In such a way, we handle correctly the template pseudo-tags that appears on
  ;; top level (just among the elements of the list).
  (elem-cont (template-lhtt (make-elem :start-parse nil list) package braces)))

 (:method ((pathname pathname) &rest args &key (package htt:*template-package*)
                                               (braces htt:*accept-braces-markers*))
  ;; Args: All except for package and braces are passed to phtml:parse-html
  ;; ASSUMPITON: No template pseudo-tags appear at top-level.
  (mapcar (lambda (elem) (template-lhtt elem package braces))
          (apply #'phtml:parse-html pathname
                 (remove-properties args '(:package :braces))))) )

;;; Fill templates, possibly by evaluating inner forms, and output HTML to the stream given

(defun lhtt-print (lhtt *template-environment* &optional (stream *html-stream*))
  (html:html-print lhtt stream))

(defun lhtt-print-list (list *template-environment* &optional (stream *html-stream*))
  (html:html-print-list list stream))
