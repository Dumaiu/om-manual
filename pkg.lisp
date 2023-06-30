(in-package :asdf-user)

(define-package :om-manual-conversion
	(:mix
	 :trivia
	 :alexandria
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

(in-package :om-manual-conversion)

(defparameter *project-pkg* *package*
  "TODO: [lattice].")
