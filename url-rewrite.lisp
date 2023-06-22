(in-package :om-manual-conversion)

(eval-always
  (use-package :url-rewrite)
  (shadowing-import* (list (find-symbol* '#:*url-rewrite-tags* :url-rewrite)
						   (find-symbol* '#:*url-rewrite-fill-tags* :url-rewrite))
					 *package*))


(defun add-alt-attributes (filepath &key (output t))
  "Add `alt` fields to `img` tags that don't have them.

  `:output :file`: Overwrite FILEPATH.
"
  (assert (string-equal "html" (pathname-type filepath)))
  (let ((*url-rewrite-tags* nil)
		(*url-rewrite-fill-tags* '(("img" . "alt"))))

	(flet ((doit ()
			 (with-input-file (*standard-input* filepath)
			   (rewrite-urls (λ _
							   ;; (break)
							   "TODO: alt-text for image"))
			   )))
	  ;; (break)
	  (ecase output
		((nil :string)
		 (with-output-to-string (*standard-output*)
		   (doit)))
		((t)
		 (doit))
		(:file
		 (prog1
			 (let-1 str (with-output-to-string (*standard-output*)
						  (doit))
			   (with-output-file (f filepath
									:if-does-not-exist :error
									:if-exists :supersede
									)
				 (princ str f)))
		   (format t "~&Overwrote ~A.~%" filepath)))))))

''(

   (add-alt-attributes  "docs/om/om6-manual/co/OM-User-Manual.md.html")

   (add-alt-attributes  "docs/om/om6-manual/co/OM-User-Manual.md.html" :output nil)

   (add-alt-attributes  "docs/om/om6-manual/co/OM-User-Manual.html" :output :file)

   (html-string->md (add-alt-attributes  "docs/om/om6-manual/co/OM-User-Manual.html" :output :string)
	:output "/mnt/c/Users/Jonathan/Documents/openmusic/support.ircam.fr/docs/om/om6-manual/co/OM-User-Manual.md")

   (md-file->html  "/mnt/c/Users/Jonathan/Documents/openmusic/support.ircam.fr/docs/om/om6-manual/co/OM-User-Manual.md"
	:output t)

   (md-file->html  "/mnt/c/Users/Jonathan/Documents/openmusic/support.ircam.fr/docs/om/om6-manual/co/OM-User-Manual.md"
	:output t)

   )
