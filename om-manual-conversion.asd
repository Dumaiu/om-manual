(in-package :asdf-user)

(defsystem om-manual-conversion
  :depends-on (:let-plus
			   :url-rewrite
			   :trivia
			   :yhtml)
  :components
  ((:module base :pathname ""
	:serial t
	:components
	((:file "pkg")
	 (:file "test-vars")))
   (:file "pandoc"
	:depends-on (base))
   (:file "yhtml"
	:depends-on (base))
   (:file "convert-files"
	:depends-on (base pandoc yhtml))
   (:file "validate-html"
	:depends-on (base))
   (:file "url-rewrite" ; requires :url-rewrite
	:depends-on (base))

   ))
