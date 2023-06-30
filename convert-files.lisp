(in-package :om-manual-conversion)

;; (defparameter *manual.md.html* file)

;; (defparameter *orig-manual.md.ystok* (parse-html *manual*))


(defun html->sexp+ (file)
  "Convert with modifications."
  (let-1 sexp (parse-html file :callbacks `((:div . ,(λ div-form
													   "Remove 'googleSearchFrom' `div` element."
													   ;; (break "Arg: ~A" div-form)
													   (match div-form
														 ((list* (list* :div (plist :class  "googleSearchFrom")) _)
														  ;; (break "class: ~A" class)
														  ;; (break "Arg: ~A" div-form)
														  (values nil t) ; delete element
														  ))))
											(:a . ,(λ anchor-form
													 "* Remove 'Scenari' `<a>` at end of page.
 * Change .html extensions to .md.
"
													 (match anchor-form
													   ((list* (list* :a (plist :href "http://scenari-platform.org")) _)
														(values nil t))  ; delete element
													   ((guard (list* (list* :a (and plist
																					 (plist :href href))) _)
															   (string-equal "html"
																			 (pathname-type href)))
														(let-1 md-anchor (namestring (make-pathname :type "md"
																									:defaults href))
														  (setf (getf plist :href) md-anchor)
														  ;; (break "HTML anchor: ~S" anchor-form)
														  (values anchor-form t))))))
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
	sexp))

''(

   (parse-html
	(with-input-file (f *manual*)
	  (slurp-stream-string f)))

   ;; (slurp-stream-string *manual.md*) XXX

   slurp-input-stream
   )

;; (parse-html *manual*)

''(
   ;; 1.
   (defparameter *manual.ystok* (html->sexp+ *manual*)
	 "Parse HTML to Lisp, making modifications.")



   ;; 2.
   (defparameter *manual.ystok.html*
	 (to-html *manual.ystok*)
	 "Convert back to HTML.  NB: the URLs still point to Markdown files!")

   ;; 3. Generate Markdown file
   (html-string->md *manual.ystok.html* :output *manual.md*) ; *side-effect*

   )

(defun html-file->md+-file (file &key verbose
								   &allow-other-keys)
  "Write to new file.

  * [2023-06-29 Thu] Added `skip-file` restart.
"
  (let+ (((&values result skipped?) (with-simple-restart (skip-file "Skip file ~S" file)
									  (let* ((sexp (html->sexp+ file))
											 (html+ (to-html sexp)))
										(let-1 newfile (make-pathname :type "md" :defaults file)
										  (html-string->md html+ :output newfile)
										  (when verbose
											(format t "~&Converted ~S -> ~S.~%" file newfile))
										  newfile)))))
	(if skipped?
		(values)
		result)))

(defun html-directory->md+ (dir &rest *keys
							 &key (verbose t)
							 &allow-other-keys
							 &aux (pattern "*.html"))
  (setq dir (ensure-directory-pathname dir))
  (assert (directory-exists-p dir))
  ;; (collect-sub*directories dir )
  (let-1 files (directory-files dir pattern)
	(loop for f in files
		  do (apply #'html-file->md+-file f :verbose verbose *keys)))
  dir)
;; TODO: (export-from 'html-directory->md+ *project-pkg*)


''(
   (html-file->md+-file *manual*) ; * side-effect*--convert one file


   ;; Bad files:
   (html-file->md+-file #p"/mnt/c/Users/Jonathan/Documents/openmusic/support.ircam.fr/docs/om/om6-manual/co/OM-Documentation_3.html")

   (html-directory->md+ *default-directory*)
   )

;; 4. Convert back to HTML:
(let* ((html (md-file->html *manual.md*))
	   ;; 5. Convert to Lisp, editing anchors:
	   (sexp (parse-html html :callbacks `((:a . ,(λ anchor-form
													" * Change '.md' extensions to '.html'.
"
													(match anchor-form
													  ((guard (list* (list* :a (and plist
																					(plist :href href))) _)
															  (string-equal "md"
																			(pathname-type href)))
													   (let-1 new-href (namestring (make-pathname :type "html"
																								  :defaults href))
														 (setf (getf plist :href) new-href)
														 ;; (break "HTML anchor: ~S" anchor-form)
														 (values anchor-form t)))))))))
	   (html~ (to-html sexp)))

  ;; 6. Convert Lisp to HTML.
  (with-output-file (f (make-pathname :name "OM-User-Manual.md" :type "html" :defaults *manual*)
					   :if-exists :supersede
					   :if-does-not-exist :create)
	(princ html~ f)))


;; (md-file->html *manual.md* :output (make-pathname :name "OM-User-Manual.md" :type "html" :defaults *manual*)) ; *side-effect*

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
