(in-package :asdf-user)

'#:foo

(defun html-file->md (input-file~
					  &key (output :string)
					  &aux
						(in-fmt "html")
						(out-fmt "gfm")
						(input-file
						 (probe-file (ensure-pathname (pathname-name input-file~)
													  :type in-fmt))))
  "By default, returns output as a string.  To write to a file, use `:output`, e.g.::
	  (html-file->md \"00-Sommaire.html\" :output :file)

  This creates a file with the same base name, substituting 'md' for the extension.  Pass a pathname-designator to `:output` to override the naming.
"
  (when (eq :file output)
	(setq output (merge-pathnames* (make-pathname* :type "md") input-file)))
  (assert (file-exists-p input-file))
  (run-program (list "pandoc"
					 "-f" in-fmt
					 "-t" out-fmt
					 (namestring input-file))
			   :output output))

(defun md-file->html (file~
					  &key (output :string)
					  &aux (file
							(probe-file
							 (ensure-pathname (pathname-name file~)
											  :type "md"))))
  "By default, returns output as a string.  To write to a file, e.g.::
	  (md-file->html \"00-Sommaire.md\" :output :file)
"
  (when (eq :file output)
	(setq output (merge-pathnames* (make-pathname* :type "html")
								   file))
	;; (break "Modified output: ~A" output)
	)
  (assert (file-exists-p file))
  ;; (break "File: ~A" file)
  (run-program (list "pandoc"
					 "-f" "gfm"
					 "-t" "html"
					 (namestring file))
			   :output output))

(defun md-string->html (input
						&key (output :string))
  (with-input (*standard-input* input)
	(run-program (list "pandoc"
					   "-f" "gfm"
					   "-t" "html")
				 :input t
				 :output output)))

''(
   (md-string->html (md-file->html "00-Sommaire.md"))

   (html-file->md "OM-User-Manual.html" :output :file)

   (md-file->html "OM-User-Manual.md" :output "OM-User-Manual.md.html")

   (md-file->html "01-Presentation.md" :output :file)


   )
