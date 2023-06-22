(in-package :asdf-user)

(defsystem om-manual-conversion
  :depends-on (:let-plus
			   :url-rewrite
			   :yhtml)
  :components
  ((:module base :pathname ""
	:components
	((:file "convert-files")))
   (:file "validate-html"
	:depends-on (base))
   (:file "url-rewrite" ; requires :url-rewrite
	:depends-on (base))
   ))
