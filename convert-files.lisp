(in-package :om-manual-conversion)


''(

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

