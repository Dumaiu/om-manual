(in-package :om-manual-conversion)

(defvar +root-directory+ (let ((dir
								   (pathname-directory-pathname (system-source-file (find-system :om-manual-conversion)))))
						   (assert (directory-exists-p dir))
						   dir))

(defparameter +original-html-files-directory+ (merge-pathnames "docs/om/om6-manual/v6_6-original/" +root-directory+)
  "HTML files for the v6.6 manual.")

(defparameter *default-directory*
  (merge-pathnames "docs/om/om6-manual/co/" +root-directory+))

(assert (directory-exists-p *default-directory*))

(defparameter *manual* (merge-pathnames "OM-User-Manual.html" +original-html-files-directory+))
(declaim (type pathname *manual*
			   *manual.md*))
(assert (file-exists-p *manual*))


(defparameter *manual.md* (make-pathname :type "md" :defaults *manual*
										 :directory (pathname-directory *default-directory*)))

										; '#99 ; XXX
'#:foo ; XXX
':foo

(declaim (special *orig-manual.md*
				  *manual.md.ystok*))
