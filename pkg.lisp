(in-package :asdf-user)

(define-package :xcae8963/om-manual-conversion
	(:mix
	 :trivia
	 :alexandria						; (nconcf)
	 :let-plus
	 :asdf :uiop
	 :cl
	 :asdf-user)
  (:intern
   file-error
   run-program
   *project-pkg*)
  (:export
   *default-directory*
   pandoc-command
   ))

(in-package :xcae8963/om-manual-conversion)

(defparameter *project-pkg* *package*
  "TODO: [lattice].")
