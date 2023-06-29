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
