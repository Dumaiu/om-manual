(in-package :om-manual-conversion)

;; (defparameter *manual.md.html* file)

;; (defparameter *orig-manual.md.ystok* (parse-html *manual*))

''(

   (parse-html
	(with-input-file (f *manual*)
	  (slurp-stream-string f)))

   ;; (slurp-stream-string *manual.md*) XXX

   slurp-input-stream
   )

;; (parse-html *manual*)

;; 1.
(defparameter *manual.ystok*
  (parse-html *manual* :callbacks `((:div . ,(λ div-form
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
														 (or (null alt-text)
															 (string-equal alt-text "")))
												  ;; (break "Null-text img: ~A" img-form)
												  ;; TODO: Plist setter; no-copy lens:
												  (let-1 alt-text~ (namestring
																	(make-pathname :name (pathname-name src)
																				   :type (pathname-type src)))
													(setf (getf plist-form :alt) alt-text~)
													;; (rplacd (last plist-form) (list :alt alt-text~))
													)
												  ;; (break "Null-text img: ~S" plist-form)
												  (values img-form t)))))
									))
  "Parse HTML to Lisp, making modifications.")



;; 2.
(defparameter *manual.ystok.html*
  (to-html *manual.ystok*)
  "Convert back to HTML.")

;; 3.
(html-string->md *manual.ystok.html* :output *manual.md*) ; *side-effect*

;; 4.
(md-file->html *manual.md* :output (make-pathname :name "OM-User-Manual.md" :type "html" :defaults *manual*)) ; *side-effect*

''(

   (html-string->md *manual.ystok.html*)

   (html-file->md *manual*) ; ✓

   (html-file->md *manual* :output :file) ; ✓

   (md-string->html (md-file->html "00-Sommaire.md"))


   (md-file->html *manual.md*) ; ✓


   (md-file->html *manual.md* :output (make-pathname :name "OM-User-Manual.md" :type "html" :defaults *manual*)) ; ✓ 

   ;; (md-file->html "01-Presentation.md" :output :file)


   (pandoc-command :input-file *manual.md*
				   :in-fmt "gfm"
				   :out-fmt "html"
				   :standalone t)

   )

