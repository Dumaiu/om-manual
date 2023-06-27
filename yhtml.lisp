(in-package :om-manual-conversion)

(eval-always
  (shadowing-import '(ystok.html.generator:dir
					  ystok.html.generator:input))
  (use-package '(ystok.html.parser
				 ystok.html.generator
				 trivia))
  (import 'trivia:plist))


(let-1 file (merge-pathnames* "OM-User-Manual.md.html" *default-directory*)
  (defparameter *manual* (parse-html file))


  (defparameter *manual-new*
	(parse-html file :callbacks `((:div . ,(λ div-form
											 ;; (break "Arg: ~A" div-form)
											 (match div-form
											   ((list* (list* :div (plist :class  "googleSearchFrom")) _)
												;; (break "class: ~A" class)
												;; (break "Arg: ~A" div-form)
												(values nil t) ; delete tag
												))))))))

(defun print-html (form)
  (html:with-html-stream (*standard-output*)
	(html:htm form)))

''(
   (print-html *manual-new*)

   (with-html-stream (*standard-output*)
	 (html:htm
	  :newline))

   (html:htm
	*manual*)

   (with-html-stream (*standard-output*)
	 *manual*)

   (defparameter *manual-new-html*
	 (with-html-stream (*standard-output*)
	   *manual-new*))

   (equalp *manual-new* *manual*)

   (eval `(html:with-html-stream (*standard-output*)
			(html:htm ,(car *manual-new*))))



   )
