(in-package :om-manual-conversion)

(export '(validate-html
		  html-validation-failure
		  ))

(define-condition html-validation-failure (simple-error)
  ((file :initarg :file)))

(defun validate-html (filename &key ((:directory dir) *default-directory*)
								 (output t)
					  &aux
						(file-pathname (merge-pathnames* filename dir))
						(filepath-relative (enough-pathname file-pathname
											   +root-directory+))
						(vnu "vnu"))
  "Calls `vnu`--shellscript for the Nu Validator--which should be on $PATH."
  (format output "~2&~A...~%" filepath-relative)
  (assert (file-exists-p filepath-relative))
  (catch :some-errors
   (handler-bind
	   ((subprocess-error (lambda (xc)
							(let+ (((&accessors-r/o subprocess-error-command) xc)
								   ((command-name . _) subprocess-error-command))
							  (declare (ignore _))
							  (when (string-equal vnu command-name))
							  (cerror "Ignore errors" 'html-validation-failure
									  :format-control "Nu Validator noticed errors in ~A; see REPL"
									  :format-arguments (list filepath-relative)
									  :file file-pathname)
							  (throw :some-errors nil)))))
	 (run-program (list vnu (namestring filepath-relative))
				  :output output
				  :error-output :output))))


''(
   (validate-html "OM-User-Manual.html")
   (validate-html "OM-User-Manual.md.html")
   (validate-html "OM-User-Manual.test.html")
   )
