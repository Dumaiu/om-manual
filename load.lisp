(in-package :asdf-user)

(use-package :quicklisp)

(defvar +om-manual-dir+ (let* ((dir (ensure-directory-pathname (pathname-directory-pathname (current-lisp-file-pathname))))
							   )
						  (assert (directory-exists-p dir))
						  dir))

(progn

  (progn
	(load-asd (merge-pathnames* "yuri-2-0-008/ystok-uri.asd" +om-manual-dir+))
	(load-asd (merge-pathnames* "ylib-1-4-025/ystok-library.asd" +om-manual-dir+))
	(load-asd (merge-pathnames* "yhtml-template-0-10-3/html-template.asd" +om-manual-dir+))
	(load-asd (merge-pathnames* "yhtml-0-5-009/yhtml.asd" +om-manual-dir+)))

  (quickload '(;:html-template
			   :acl-compat
			   :trivia
			   :cl-ppcre))

  ;; (make :yhtml-template)
  ;; (make :ystok-library)
  ;; (make :ystok-uri)
  (make :yhtml))

(let-1 om-manual-conversion.asdf (merge-pathnames* "xcae8963.om-manual-conversion.asd" +om-manual-dir+)
  (assert (file-exists-p om-manual-conversion.asdf))
  (load-asd om-manual-conversion.asdf))


(make :xcae8963.om-manual-conversion)
