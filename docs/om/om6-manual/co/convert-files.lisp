(in-package :asdf-user)

(define-package :om-manual-conversion
	(:mix
	 ;; :alexandria
	 :uiop
	 :cl)
  (:intern file-error
		   run-program))

(in-package :om-manual-conversion)

'#:foo

(defun pandoc-command
	(input-file &key
				  in-fmt
				  out-fmt
				  in-type
				  (out-type out-fmt)
				  (output :string))
  "Returns a list used by (pandoc), q.v."
  (check-type input-file (or string pathname))
  (assert out-fmt (out-fmt) "The :out-fmt key is required.")
  (check-type out-fmt string)

  (unless in-type
	(setq in-type (or in-fmt
					  (pathname-type input-file))))
  (check-type in-type string)

  (check-type out-fmt string)
  (check-type out-type string)

  (when (eq :file output)
	(setq output (merge-pathnames* (make-pathname* :type out-type)
								   input-file))
	;; (break "Modified output: ~A" output)
	)

  (let* ((input-file-pathname (ensure-pathname (pathname-name input-file)
											   :type in-type))
		 (input-file~ (probe-file input-file-pathname))
		 #|(expr (format nil "pandoc XXX")|#)
	(unless (file-exists-p input-file~)
	  (error 'file-error :pathname input-file-pathname))
	(let ((command `("pandoc"
					 ,@(when in-fmt
						 `("-f" ,in-fmt))
					 "-t" ,out-fmt
					 ,(namestring input-file~))))
	  command)))

(defun pandoc (input-file &key
							in-fmt
							out-fmt
							in-type
							(output :string)
			   &aux
				 )
  XXX
  "Thin wrapper around the `pandoc` executable, which must be installed separately.

  `:out-fmt` is required.
  `:output` is inherited from (run-program).

By default, returns output as a string::
	(pandoc \"00-Sommaire.html\" :out-fmt \"html\")
  To write to a file, use `:output`, e.g.::
	  (pandoc \"00-Sommaire.html\" :out-fmt \"html\" :output :file)

  This creates a file with the same base name, substituting :out-fmt for the extension.  Pass a pathname-designator to `:output` to override the naming.
"
  (check-type input-file (or string pathname))
  (assert out-fmt (out-fmt) "The :out-fmt key is required.")

  (unless in-type
	(setq in-type (or in-fmt
					  (pathname-type input-file))))
  (check-type in-type string)

  (check-type out-fmt string)
  (check-type out-type string)

  (when (eq :file output)
	(setq output (merge-pathnames* (make-pathname* :type out-type)
								   input-file))
	;; (break "Modified output: ~A" output)
	)

  (let* ((input-file-pathname (ensure-pathname (pathname-name input-file)
											   :type in-type))
		 (input-file~ (probe-file input-file-pathname))
		 #|(expr (format nil "pandoc XXX")|#)
	(unless (file-exists-p input-file~)
	  (error 'file-error :pathname input-file-pathname))
	(let ((command `("pandoc"
					 ,@(when in-fmt
						 `("-f" ,in-fmt))
					 "-t" ,out-fmt
					 ,(namestring input-file~))))
	  (format t "~&Calling ~A.~%" command)
	  (run-program command :output output))))

(defun html-file->md (input-file &rest *rest
					  &key
						(in-fmt "html")
						(out-fmt "gfm")
					  &allow-other-keys)
  "Convert HTML to GitHub-flavored Markdown using Pandoc.

  See (pandoc)."
  (apply #'pandoc input-file
		   :in-fmt in-fmt
		   :out-fmt out-fmt
		   *rest))

(defun md-file->html

	(defun html-file->md (input-file &rest *rest
						  &key
							(in-fmt "gfm")
							(out-fmt "html")
							(in-type "md")
						  &allow-other-keys)
	  "Convert GitHub-flavored Markdown to HTML using (pandoc), q.v.
"
	  (apply #'pandoc input-file
			   :in-fmt in-fmt
			   :out-fmt out-fmt
			   :in-type in-type
			   *rest)))

(defun md-string->html (input &rest *keys)
  (with-input (*standard-input* input)
	(apply #'pandoc input :input t *keys)))

''(
   (pandoc-command  "00-Sommaire.md" :out-fmt "html")

   (md-string->html (md-file->html "00-Sommaire.md"))

   (html-file->md "OM-User-Manual.html" :output :file)

   (md-file->html "OM-User-Manual.md" :output "OM-User-Manual.md.html")

   (md-file->html "01-Presentation.md" :output :file)


   )
