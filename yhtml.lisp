(in-package :xcae8963/om-manual-conversion)

(eval-always
  (shadowing-import '(ystok.html.generator:dir
					  ystok.html.generator:input))
  (use-package '(ystok.html.parser
				 ystok.html.generator
				 trivia))
  (import 'trivia:plist)
  (import 'sb-kernel:pathname-designator)
  )

#|
TODO: (htm) should throw more specific exceptions

TODO: (htm) shouldn't create `</img>` or `</meta>` closers

TODO: How do you get `(:!doctype "html")` to translate correctly?  [Dumaiu/om-manual#5]


TODO: ystok: [I don't think it's possible to distinguish between an element with attributes vs. a list of elements without attributes.]

|#

(deftype html-element-sexp ()
  '(or keyword							; single-tag element without attributes
	(cons keyword cons)					; single-tag element w/ attributes or double-tag element w/o attributes
	(cons (cons keyword cons) list)		; double-tag element w/ attributes
	))

;; (assert (not (typep *orig-manual.md* 'html-element-sexp)))
;; (assert (not (typep *manual.md.ystok* 'html-element-sexp)))
;; But:
;; (assert (every (λ x (typep x 'html-element-sexp)) *manual.md.ystok*))


(defun list-to-html (list)
  (declare (list list))
  (let-1 res (reduce #'strcat (mapcar #'to-html list))
	(declare (string res))
	res))

(defun to-html (form)
  "Convert FORM to HTML and return as a string."
  (etypecase form
	(html-element-sexp
	 (let-1 form~ `(with-output-to-string (strm)
					 (with-html-stream (strm)
					   (htm ,form)))
	   ;; (break "~A" form~)
	   (let-1 res (eval form~)
		 (declare (string res))
		 res)))
	(list
	 (list-to-html form))))

(defun print-html (form &optional (stream *standard-output*))
  "Convert FORM to HTML and print to STREAM."
  (print (to-html form) stream))

''(
   ;;; Works:
   (eval `(with-html-stream (*standard-output*)
			(htm ,(car *orig-manual.md*))))


   ;;; Also works:
   (eval `(with-html-stream (*standard-output*)
			(htm ,(car *manual.md.ystok*))))

   (print-html :newline)

   (to-html *orig-manual.md*)

   (print-html *manual.md.ystok*)

   (list-to-html *manual.md.ystok*)

   (to-html *manual.md.ystok*)

   (with-html-stream (*standard-output*)
	 (htm :newline))

   (with-output-to-string (strm)
	 (with-html-stream (strm)
	   (htm :newline)))

   ;;; [Dumaiu/om-manual#5]
   (htm '(:!doctype "html")) ; XXX
   (to-html '(:!doctype "html")) ; XXX
   (to-html '((:!doctype "html"))) ; XXX

   )

(defun modify-html (html &key
					 callbacks
					 (output :string)
					 &aux
					 input
					 output-file)
  "Act on the Lisp representation of ``html``, which can be a pathname or HTML string.  "
  (declare (type (or string pathname) html))

  (cond
	((pathnamep html)
	 (setq input html))
	((handler-case
		 (file-exists-p (pathname html)) ; If it can't be parsed as a pathname, convert to NIL
	   (parse-error () nil))
	 (setq input (pathname html)))
	(t
	 (check-type html string)
	 (setq input html)))


  (let-1 sexp (parse-html input :callbacks callbacks)
	(let-1 res (to-html sexp)
	  (etypecase output
		((or null (eql :string))
		 res)
		((or (eql :file)
			 pathname-designator)
		 (case output
		   (:file
			(assert (pathnamep input))
			(setq output-file input))
		   (t
			(assert (typep output 'pathname-designator))
			(setq output-file (pathname output))))
		 (with-output-file (f output-file
							  :if-exists :supersede
							  :if-does-not-exist :create)
		   (princ res f)))))))



;; ''(defparameter *manual.md.ystok.new*
;;    (to-html `(;; (:!doctype "html") XXX
;; 			  ((:html :lang "en")
;; 			   (:head (:title "TODO title")
;; 				 ((:meta  :charset "utf-8")))
;; 			   (:body ,@*manual.md.ystok*))))
;;    "Like *manual.md.ystok*, but adds completing elements.")

;; (princ *manual.md.ystok.new*)

;; (parse-html *manual.md.ystok.new*)

;; ''(defparameter *manual.md.ystok.new.html*
;;   (with-output-to-string (f)
;; 	(format f "<!DOCTYPE ~A>~%" "html")	; KLUDGE: I don't know how to do this with :ystok.html.generator (see [Dumaiu/om-manual#5])
;; 	(princ *manual.md.ystok.new* f)))

;; ''(let-1 file (merge-pathnames* "OM-User-Manual.ystok.html" *default-directory*)
;;   (with-output-file (f file :if-exists :supersede
;; 							:if-does-not-exist :create)
;; 	(princ *manual.md.ystok.new.html* f))
;;   )


;; ''(
;;    (parse-html *manual.md.ystok.new.html*)
;;    (file-exists-p *manual.md.html*)

;;    (html-file->md *manual.md.html*  )

;;    (html-string->md *manual.md.ystok.new.html*  )
;;    )
