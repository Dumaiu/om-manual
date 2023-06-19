(in-package :om-manual-conversion)

(defun validate-html (filename &key ((:directory dir) *default-directory*)
                          (output t)
                          &aux
                            (file (enough-pathname (merge-pathnames* filename dir)
												   +root-directory+))
                            (vnu "vnu"))
  "Calls `vnu`--shellscript for the Nu Validator--which should be on $PATH."
  (format output "~2&~A...~%" file)
  (assert (file-exists-p file))
  (run-program (list vnu (namestring file))
               :output output
			   :error-output :output))


''(
   (validate-html "OM-User-Manual.html")
   (validate-html "OM-User-Manual.md.html")
   (validate-html "OM-User-Manual.test.html")
   )
