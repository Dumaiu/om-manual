(in-package :om-manual-conversion)

(eval-always
  (use-package '(ystok.html.parser
				 ystok.html.generator
				 trivia))
  (import 'trivia:plist))


(let-1 file (merge-pathnames* "OM-User-Manual.md.html" *default-directory*)
  (defparameter *manual* (parse-html file))
  

  (parse-html file :callbacks `((:div . ,(λ div-form
										   ;; (break "Arg: ~A" div-form)
										   (match div-form
											 ((list* (list* :div (plist :class  "googleSearchFrom")) _)
											  ;; (break "class: ~A" class)
											  (break "Arg: ~A" div-form))))))))


