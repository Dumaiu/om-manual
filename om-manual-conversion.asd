(in-package :asdf-user)

(defsystem om-manual-conversion
  :depends-on (:let-plus
			   :url-rewrite
			   :trivia
			   :yhtml)
  :components
  ((:module base :pathname ""
	:components
	((:file "convert-files")))
   (:file "validate-html"
	:depends-on (base))
   (:file "yhtml"
	:depends-on (base))
   (:file "url-rewrite" ; requires :url-rewrite
	:depends-on (base))
   ))
