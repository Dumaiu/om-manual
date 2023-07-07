(in-package :om-manual-conversion)

(export '(skip-file

		  ;; Phase 1: HTML4 -> GFM
		  html->sexp+
		  html-file->md+-file
		  html-directory->md+

		  ;; Phase 2: GFM -> HTML5
		  generate-html-from-markdown-file
		  generate-html-from-markdown
		  ))

;; (defparameter *manual.md.html* file)

;; (defparameter *orig-manual.md.ystok* (parse-html *manual*))

(defun html->sexp+ (file)
  "Convert a single file, with modifications.

  * TODO: Refactor with `(modify-html)`.
  "
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
													 "Multiple edits:
  * Remove 'Scenari' `<a>` at end of page.
  * Change .html extensions to .md [Dumaiu/om-manual#7].
"
													 (match anchor-form
													   ((list* (list* :a (plist :href "http://scenari-platform.org")) _)
														(values nil t))  ; delete element
													   ((guard (list* (list* :a (and plist!
																					 (plist :href href))) _)
															   (string-equal "html"
																			 (pathname-type href)))
														(let-1 md-anchor (namestring (make-pathname :type "md"
																									:defaults href))
														  (setf (getf plist! :href) md-anchor)
														  ;; (break "HTML anchor: ~S" anchor-form)
														  (values anchor-form t))))))
											(:img . ,(λ img-form
													   "Add alt-text to `<img>` tags [Dumaiu/om-manual#2]."
													   (match img-form
														 ((guard (list* (list* :img (and plist!
																						 (plist :src src :alt alt-text)))
																		_)
																 (or (null alt-text)
																	 (string-equal alt-text "")))
														  ;; (break "Null-text img: ~A" img-form)
														  ;; TODO: Plist setter; no-copy lens:
														  (let-1 alt-text~ (namestring
																			(make-pathname :name (pathname-name src)
																						   :type (pathname-type src)))
															(setf (getf plist! :alt) alt-text~)
															;; (rplacd (last plist!) (list :alt alt-text~))
															)
														  ;; (break "Null-text img: ~S" plist!)
														  (values img-form t)))))
											))
	sexp))

;; ''(

;;    (parse-html
;;	(with-input-file (f *manual*)
;;	  (slurp-stream-string f)))

;;    ;; (slurp-stream-string *manual.md*) XXX

;;    slurp-input-stream
;;    )

;; (parse-html *manual*)

;; ''(
;;    ;; 1.
;;    (defparameter *manual.ystok* (html->sexp+ *manual*)
;;	 "Parse HTML to Lisp, making modifications.")



;;    ;; 2.
;;    (defparameter *manual.ystok.html*
;;	 (to-html *manual.ystok*)
;;	 "Convert back to HTML.  NB: the URLs still point to Markdown files!")

;;    ;; 3. Generate Markdown file
;;    (html-string->md *manual.ystok.html* :output *manual.md*) ; *side-effect*

;;    )

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

(defun generate-html-from-markdown-file (pathname &key
													(verbose t)
													(output-directory *default-directory*)
										 &allow-other-keys)
  "
  If the `skip-file` restart is used, the primary retval will be NIL, with the failed input file as a second val.

  * TODO: Factor out callbacks.
  * TODO: Use :log4cl to handle `verbose`.
"
  (let ((html (md-file->html pathname))
		(output-pathname (make-pathname :type "html" :defaults pathname
										:directory (pathname-directory output-directory)))
		#|(sexp (parse-html html))
		(html~ (to-html sexp))|#)

	(let-1 success? (with-simple-restart (skip-file "Skip file ~S" pathname)
					  (modify-html html :callbacks `((:a . ,(λ anchor-form
															  " * Change '.md' extensions to '.html' [Dumaiu/om-manual#7].
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
																   (values anchor-form t)))))))
										:output output-pathname)
					  (assert (file-exists-p output-pathname))
					  (when verbose
						(format t "~&Converted ~S -> ~S.~%" pathname output-pathname))
					  t)
	  (if success?
		  output-pathname
		  (progn
			;; (break)
			(values nil pathname))))))

(defun generate-html-from-markdown (&optional (pathname *default-directory*) &rest *keys
									&key
									  output-directory
									  (verbose t)
									&allow-other-keys
									&aux
									  (md-wildcard "*.md"))
  "Convert a file or a whole directory from the GFM files previously generated.

Returns a list of files written.  Secondary retval is a list of files which couldn't be handled and therefore were skipped; (only when the `skip-file' restart is used).

  Args:
	- pathname: If a directory, iterate over its member Markdown files.

"
  (declare (type pathname-designator pathname))
  (let-1 pathname (ensure-pathname pathname)
	(cond
	  ((directory-pathname-p pathname)
	   (assert (directory-exists-p pathname))
	   (unless output-directory
		 (setq output-directory pathname))
	   (loop with files = (directory-files pathname md-wildcard)
			 with failures! = ()
			 with successes! = ()
			 with n = (length files)
			 for i from 1
			 for f in files
			 ;; Recurse:
			 do (when verbose
				  (format t "~&~D/~D~%" i n))
			 do (assert (not (directory-pathname-p f)))
			 do (let+ (((&values result skipped) (apply #'generate-html-from-markdown f
														:output-directory output-directory
														:verbose verbose
														*keys)))
				  (declare (type (or pathname null) result skipped))
				  (nconcf successes! (ensure-list result))
				  (nconcf failures! (ensure-list skipped)))
			 finally (progn
					   (when verbose
						 (format t "~2& Failures: ~S~%" failures!))
					   (return (values successes! `(:failures ,failures!))))
			 ))
	  (t
	   ;; (break)
	   (assert (file-exists-p pathname))
	   (assert (string-equal "md" (pathname-type pathname)))
	   (unless output-directory
		 (setq output-directory *default-directory*))
	   (apply #'generate-html-from-markdown-file pathname
			  :verbose verbose
			  :output-directory output-directory
			  *keys)))))

''(
   (html-file->md+-file *manual*) ; * side-effect*--convert one file


   ;; Dealing with [Dumaiu/om-manual#9]:
   (html-file->md+-file #p"/mnt/c/Users/Jonathan/Documents/openmusic/support.ircam.fr/docs/om/om6-manual/co/OM-Documentation_3.html")

   (html-directory->md+ *default-directory*)

   )

(generate-html-from-markdown *manual.md*) ; *side-effect*


''(
   ;; 4,5,6. Convert to Lisp, editing anchors; then return to HTML.
   (generate-html-from-markdown *default-directory*)

   )

;; (let* ((html (md-file->html *manual.md*))

;;		 #|(sexp (parse-html html))
;;		 (html~ (to-html sexp))|#)

;;	(modify-html html :callbacks `((:a . ,(λ anchor-form
;;											" * Change '.md' extensions to '.html'.
;; "
;;											(match anchor-form
;;											  ((guard (list* (list* :a (and plist
;;																			(plist :href href))) _)
;;													  (string-equal "md"
;;																	(pathname-type href)))
;;											   (let-1 new-href (namestring (make-pathname :type "html"
;;																						  :defaults href))
;;												 (setf (getf plist :href) new-href)
;;												 ;; (break "HTML anchor: ~S" anchor-form)
;;												 (values anchor-form t)))))))
;;					  :output (make-pathname :name "OM-User-Manual.md" :type "html" :defaults *manual*
;;											 :directory (pathname-directory *default-directory*))))


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
