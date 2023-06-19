(in-package :asdf-user)

(defsystem om-manual-conversion
  :depends-on (:let-plus)
  :serial t
  :components
  ((:file "convert-files")
   (:file "validate-html")
   ))
