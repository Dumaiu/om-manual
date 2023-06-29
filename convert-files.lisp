(in-package :asdf-user)

(define-package :om-manual-conversion
	(:mix
	 ;; :alexandria
	 :let-plus
	 :asdf :uiop
	 :cl
	 :asdf-user)
  (:intern
   file-error
   run-program)
  (:export
   *default-directory*
   pandoc-command
   ))

(in-package :om-manual-conversion)



(defvar +root-directory+ (let ((dir
								   (pathname-directory-pathname (system-definition-pathname (find-system :om-manual-conversion)))))
						   (assert (directory-exists-p dir))
						   dir))

(defparameter *default-directory*
  (merge-pathnames* "docs/om/om6-manual/co/" +root-directory+))

(assert (directory-exists-p *default-directory*))

(defparameter *manual* (merge-pathnames* "OM-User-Manual.html" *default-directory*))
(assert (file-exists-p *manual*))

(defparameter *manual.md* (make-pathname :type "md" :defaults *manual*))

										; '#99 ; XXX
'#:foo ; XXX
':foo

(defun pandoc-command (&key input
						 input-file
						 in-fmt
						 out-fmt
						 in-type
						 (out-type out-fmt)
						 (output :string))
  "Returns a list used by (pandoc), q.v."
  (cond
	(input-file
	 (when input (assert (equalp input-file input)))
	 (check-type input-file (or string pathname)))
	(input
		(assert (null input-file))
	  (when (pathnamep input)
		(setf input-file input))))

  (assert out-fmt (out-fmt) "The :out-fmt key is required.")
  (check-type out-fmt string)

  (unless in-type
	(setq in-type (or in-fmt
					  (pathname-type input-file))))
  (check-type in-type string)

  (check-type out-fmt string)
  (check-type out-type string)

  (when (eq :file output)
	(setq output (make-pathname :type out-type :defaults input-file))
	;; (break "Modified output: ~A" output)
	)

  (let ((command `("pandoc"
				   ,@(when in-fmt
					   `("-f" ,in-fmt))
				   "-t" ,out-fmt
				   ,(ensure-list
					 (when input-file
					   (let* ((input-file-pathname (ensure-pathname input-file))
							  (input-file~ (probe-file input-file-pathname))
							  #|(expr (format nil "pandoc XXX")|#)
						 (unless (file-exists-p input-file~)
						   (error 'file-error :pathname input-file-pathname))
						 (namestring input-file~)))))))

	command))

(defun html-string->md (input
						   &key (output :string)
						 (in-fmt "html")
						 (out-fmt "gfm")
						 )
  (declare (string input))
  (with-input (*standard-input* input)
	(run-program (list "pandoc"
					   "-f" in-fmt
					   "-t" out-fmt)
				 :input t
				 :output output)))

(defun html-file->md (input-file~
					  &key (output :string)
					  &aux
						(in-fmt "html")
						(out-fmt "gfm")
						(input-file
						 (or (file-exists-p input-file~)
							 (probe-file (ensure-pathname (pathname-name input-file~)
														  :type in-fmt)))))
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
							 (make-pathname :name (pathname-name file~)
											:directory (pathname-directory file~)
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


   (pandoc-command :input-file *manual.md*
				   :in-fmt "gfm"
				   :out-fmt "html")

   )
