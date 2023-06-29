(in-package :om-manual-conversion)

(eval-always
  (shadowing-import '(ystok.html.generator:dir
					  ystok.html.generator:input))
  (use-package '(ystok.html.parser
				 ystok.html.generator
				 trivia))
  (import 'trivia:plist))

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

(declaim (special *orig-manual.md*
				  *manual.md.ystok*))
(let-1 file (merge-pathnames* "OM-User-Manual.md.html" *default-directory*)
  (defparameter *manual.md.html* file)

  (defparameter *orig-manual.md* (parse-html file))


  (defparameter *manual.md.ystok*
	(parse-html file :callbacks `((:div . ,(λ div-form
											 "Remove 'googleSearchFrom' `div` element."
											 ;; (break "Arg: ~A" div-form)
											 (match div-form
											   ((list* (list* :div (plist :class  "googleSearchFrom")) _)
												;; (break "class: ~A" class)
												;; (break "Arg: ~A" div-form)
												(values nil t) ; delete element
												))))
								  (:a . ,(λ anchor-form
										   "Remove 'Scenari' `<a>` at end of page."
										   (match anchor-form
											 ((list* (list* :a (plist :href "http://scenari-platform.org")) _)
											  (values nil t)))))  ; delete element
								  (:img . ,(λ img-form
											 "Add alt-text to `<img>` tags."
											 (match img-form
											   ((guard (list* (list* :img (and plist-form
																			   (plist :src src :alt alt-text)))
															  _)
													   (null alt-text))
												;; (break "Null-text img: ~A" img-form)
												;; TODO: Plist setter; no-copy lens:
												(let-1 alt-text~ (namestring
																  (make-pathname :name (pathname-name src)
																				 :type (pathname-type src)))
												  (rplacd (last plist-form) (list :alt alt-text~)))
												;; (break "Null-text img: ~S" plist-form)
												(values img-form t)))))
								  )))

  ;; (setf *manual.md.ystok* `(:body ,*manual.md.ystok*))
  )

(assert (not (typep *orig-manual.md* 'html-element-sexp)))
(assert (not (typep *manual.md.ystok* 'html-element-sexp)))
;; But:
(assert (every (λ x (typep x 'html-element-sexp)) *manual.md.ystok*))


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

(defparameter *manual.md.ystok.new*
  (to-html `(;; (:!doctype "html") XXX
			 ((:html :lang "en")
			  (:head (:title "TODO title")
				((:meta  :charset "utf-8")))
			  (:body ,@*manual.md.ystok*))))
  "Like *manual.md.ystok*, but adds completing elements.")

;; (princ *manual.md.ystok.new*)

										; (parse-html *manual.md.ystok.new*) XXX

(defparameter *manual.md.ystok.new.html*
  (with-output-to-string (f)
	(format f "<!DOCTYPE ~A>~%" "html")	; KLUDGE: I don't know how to do this with :ystok.html.generator (see [Dumaiu/om-manual#5])
	(princ *manual.md.ystok.new* f)))

(let-1 file (merge-pathnames* "OM-User-Manual.ystok.html" *default-directory*)
  (with-output-file (f file :if-exists :supersede
							:if-does-not-exist :create)
	(princ *manual.md.ystok.new.html* f))
  )


(parse-html *manual.md.ystok.new.html*)
(file-exists-p *manual.md.html*)  

(html-file->md *manual.md.html*  )

(html-string->md *manual.md.ystok.new.html*  )
