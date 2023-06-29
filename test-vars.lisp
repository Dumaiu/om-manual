(in-package :om-manual-conversion)

(defvar +root-directory+ (let ((dir
								   (pathname-directory-pathname (system-definition-pathname (find-system :om-manual-conversion)))))
						   (assert (directory-exists-p dir))
						   dir))

(defparameter *default-directory*
  (merge-pathnames* "docs/om/om6-manual/co/" +root-directory+))

(assert (directory-exists-p *default-directory*))

(defparameter *manual* (merge-pathnames* "OM-User-Manual.html" *default-directory*))
(assert (file-exists-p *manual*))

(defparameter *manual.md* (make-pathname :type "md" :defaults *manual*))

										; '#99 ; XXX
'#:foo ; XXX
':foo

(declaim (special *orig-manual.md*
				  *manual.md.ystok*))
