(in-package :asdf-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((dir (ensure-directory-pathname (pathname-directory-pathname (current-lisp-file-pathname))))
         (asdf (make-pathname* :directory (pathname-directory dir) :name "om-manual-conversion" :type "asd")))
    (assert (file-exists-p asdf))
    (load-asd asdf))

  (make :om-manual-conversion))
