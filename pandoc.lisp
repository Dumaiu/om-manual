(in-package :xcae8963/om-manual-conversion)

(defun pandoc-command (&key input
						 input-file
						 in-fmt
						 out-fmt
						 in-type
						 standalone
						 (lang "en")
						 (out-type out-fmt)
						 (output :string))
  "Returns a list for use by (pandoc), which doesn't yet exist.

 * [2023-06-29 Thu] TODO: Use Screamer to unify parameters per a consistency rubric.  Make sense?
 "
  (declare (boolean standalone)
		   (string lang))

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
  (when (string-equal out-fmt "gfm")
	(assert (string-equal out-type "md")))

  (when (eq :file output)
	(setq output (make-pathname :type out-type :defaults input-file))
	;; (break "Modified output: ~A" output)
	)


  (when standalone
	(assert (equalp out-fmt "html")))

  (let ((command `("pandoc"
				   ,@(when lang
					   `("-V" ,(format nil "lang=~A" lang)))
				   ,@(when standalone
					   `("-s"))
				   ,@(when in-fmt
					   `("-f" ,in-fmt))
				   "-t" ,out-fmt
				   ,@(ensure-list
					  (when input-file
						(let* ((input-file-pathname (ensure-pathname input-file))
							   (input-file~ (probe-file input-file-pathname))
							   #|(expr (format nil "pandoc XXX")|#)
						  (unless (file-exists-p input-file~)
							(error 'file-error :pathname input-file-pathname))
						  (namestring input-file~)))))))

	(values command
			(list :output output))))

(defun html-string->md (input &rest *keys
						 &key (output :string)
						 (in-fmt "html")
						 (out-fmt "gfm")
						 (out-type "md")
						 &allow-other-keys)
  (declare (string input))
  "TODO [parameter-subtyping]: Inherit from (pandoc-command).
"
  (let-1 command (apply #'pandoc-command :input t :output output
										 :in-fmt in-fmt :out-fmt out-fmt
										 :out-type out-type
										 *keys)
	(with-input (*standard-input* input)
	  (run-program command :input t :output output))))

(defun html-file->md (input-file &rest *keys
					  &key (output :string)
						(in-fmt "html")
						(out-fmt "gfm")
						(out-type "md")
						&allow-other-keys)
  "By default, returns output as a string.  To write to a file, use `:output`, e.g.::
	  (html-file->md \"00-Sommaire.html\" :output :file)

  This creates a file with the same base name, substituting 'md' for the extension.  Pass a pathname-designator to `:output` to override the naming.
"
  (let+ (((&values command plist) (apply #'pandoc-command :input-file input-file :output output
										  :in-fmt in-fmt :out-fmt out-fmt
										  :out-type out-type
										  *keys))
		 ((&plist-r/o (output~ :output)) plist))
	(run-program command :output output~)))

(defun md-file->html (file
					  &key (output :string)
						(standalone t)
					  &aux
						(in-fmt "gfm")
						(in-type "md")
						(out-fmt "html")
						;; (file
						;;	(probe-file
						;;	 (make-pathname :name (pathname-name file~)
						;;					:directory (pathname-directory file~)
						;;					:type "md")))
						)
  "By default, returns output as a string.  To write to a file, e.g.::
	  (md-file->html \"00-Sommaire.md\" :output :file)
"
  (let+ (((&values command plist) (pandoc-command :input-file file :output output
												  :in-fmt in-fmt :out-fmt out-fmt
												  :in-type in-type
												  :standalone standalone))
		 ((&plist-r/o (output~ :output)) plist))
	(run-program command :output output~)))

(defun md-string->html (input
						   &key (output :string))
  (with-input (*standard-input* input)
	(run-program (list "pandoc"
					   "-f" "gfm"
					   "-t" "html")
				 :input t
				 :output output)))

