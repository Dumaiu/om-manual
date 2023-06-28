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
	(car (parse-html file :callbacks `((:div . ,(λ div-form
											  ;; (break "Arg: ~A" div-form)
											  (match div-form
												((list* (list* :div (plist :class  "googleSearchFrom")) _)
												 ;; (break "class: ~A" class)
												 ;; (break "Arg: ~A" div-form)
												 (values nil t) ; delete tag
												 ))))))))
  "NOTE: Strips off trailing newling.")

(defun to-html (form)
  "Convert FORM to HTML and return as a string."
  (let-1 form~ `(with-output-to-string (strm)
				  (with-html-stream (strm)
					(htm ,form)))
	;; (break "~A" form~)
	(let-1 res (eval form~)
	  (declare (string res))
	  res)))

(defun print-html (form &optional (stream *standard-output*))
  "Convert FORM to HTML and print to STREAM."
  (print (to-html form) stream))

''(
   ;;; Works:
   (eval `(with-html-stream (*standard-output*)
			(htm ,(car *manual*))))


   ;;; Also works:
   (eval `(with-html-stream (*standard-output*)
			(htm ,*manual-new*)))

   (print-html :newline)

   (print-html *manual-new*)


   (eval
	`(with-html-stream (*standard-output*)
	   (htm ',(first *manual*))))

   (with-html-stream (*standard-output*)
	 (htm :newline))

   (with-output-to-string (strm)
	 (with-html-stream (strm)
	   (htm :newline)))

   ;; (reduce 'strcat (mapcar 'print-html *manual-new*))

   ;; (to-html *manual*) XXX

   (defparameter *manual-new-html*
	 (with-html-stream (*standard-output*)
	   *manual-new*))

   (equalp *manual-new* *manual*)


   )
