(in-package :om-manual-conversion)

(eval-always
  (use-package :url-rewrite))


(defun add-alt-attributes (filepath &key (output t))
  "Add `alt` fields to `img` tags that don't have them."
  (assert (string-equal "html" (pathname-type filepath)))
  (let ((*url-rewrite-tags* nil)
		(*url-rewrite-fill-tags* '(("img" . "alt"))))

	(flet ((doit ()
			 (with-input-file (*standard-input* filepath)
			   (rewrite-urls (λ _
							   ;; (break)
							   "TODO: alt-text for image"))
			   )))
	  (ecase output
		((nil :string)
		 (with-output-to-string (*standard-output*)
		   (doit)))
		((t)
		 (doit))))))

''(

   (add-alt-attributes  "docs/om/om6-manual/co/OM-User-Manual.md.html")

   (add-alt-attributes  "docs/om/om6-manual/co/OM-User-Manual.md.html" :output nil)

   (html-string->md (add-alt-attributes  "docs/om/om6-manual/co/OM-User-Manual.html" :output :string)
	:output :string)

   )
