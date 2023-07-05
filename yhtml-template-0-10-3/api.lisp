;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HTML-TEMPLATE; Base: 10 -*-
;;; YHTML-Template - Top-level API functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2012, Dr. Dmitriy Ivanov. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :html-template)

;; Variable bound to current universal-time in all methods on fill-and-print-template.
;; Then it is used within #<method create-template-printer (pathname)> and ensures all
;; the printers created during "one transaction" to be marked with the same timestamp.
;;
;; The further optimization trick avoids calling the file-write-date repeatedly
;; over the same template file, which can be included in a loop.
;; On next "transaction", the file-write-date can be called no more than
;; once per each cached printer.
(defvar %top-level-invocation-time% nil)

(defgeneric create-template-printer (template &key)
  (:documentation "Creates a template printer from TEMPLATE, which
is an open input stream, a string, or a pathname. For a pathname,
additional keyword arguments allowed:
 FORCE - see docstring on the *FORCE-DEFAULT* special,
 other - passed to WITH-OPEN-FILE."))

(defmethod create-template-printer ((input-stream stream) &key)
  (let ((*standard-input* input-stream))
    (%create-template-printer-aux nil nil)))

(defmethod create-template-printer ((string string) &key)
  (with-input-from-string (*standard-input* string)
    (%create-template-printer-aux nil nil)))

(defmethod create-template-printer ((pathname pathname)
                                    &key (force *force-default*)
                                         (element-type #-:lispworks 'character
                                                       #+:lispworks 'lw:simple-char)
                                         (if-does-not-exist :error)
                                         (external-format *external-format*))
 ;;; Args: pathname  In fact, merged against *default-template-pathname* already
  ;;                 when is called from an TMPL_INCLUDE or TMPL_CALL printer.
  (let* ((merged-pathname (merge-pathnames pathname *default-template-pathname*))
         (pair (gethash merged-pathname *printer-hash*))
         (%top-level-invocation-time% (or %top-level-invocation-time%
                                          (get-universal-time)))
         creation-time)
    ;; Check whether there is a printer for this pathname the cache already
    (if (and pair                                              ; yes, there is
             (or (eq force :no-check)                          ; we do not have to check
                 (and (not force)                              ; or not forced to recreate
                      (or (<= %top-level-invocation-time%      ; it has created during
                              (setq creation-time (cdr pair))) ; the same transaction
                          (and (<= (file-write-date            ; or file is not too old
                                    merged-pathname)
                                   creation-time)
                               ;; TRICK: As the cached printer is still actual,
                               ;;        this allows us to "update" the creation time
                               ;;        as of a later moment
                               (rplacd pair %top-level-invocation-time%))))))
        (car pair)                                             ; return the cached printer
        (let ((new-printer
               (let ((*external-format* external-format))
                 (with-open-file (*standard-input* merged-pathname
                                  :direction :input
                                  :if-does-not-exist if-does-not-exist
                                  :element-type element-type
                                  :external-format external-format)
                   (%create-template-printer-aux nil nil)))))
          ;; Cache the newly created printer along with top-level time or current time
          (unless (eq force :no-cache)
            (setf (gethash merged-pathname *printer-hash*)
                  (cons new-printer %top-level-invocation-time%)))
          ;; Optionally issue a warning
          (cond ((eq *warn-on-creation* 'yl:logg)
                 (yl:logg :html-template
                          #1="New template printer created for ~A" merged-pathname))
                (*warn-on-creation*
                 (warn #1# merged-pathname)))
          new-printer))))

(defgeneric fill-and-print-template (template/printer environment &key stream)
  (:documentation "Fills the template denoted by TEMPLATE/PRINTER with
ENVIRONMENT and print it to STREAM. If TEMPLATE/PRINTER is a function uses
it as if it were a template printer, otherwise creates a printer \(or
pull one out of the cache) with CREATE-TEMPLATE-PRINTER. Optional
keyword arguments are given to CREATE-TEMPLATE printer and can only be
used if TEMPLATE/PRINTER is a pathname."))

(defmethod fill-and-print-template ((function function) environment
                                    &key (stream *default-template-output*))
  (let ((%top-level-invocation-time% (get-universal-time))
        (*template-output* stream))
    (funcall function environment)))

(defmethod fill-and-print-template ((string string) environment
                                    &key (stream *default-template-output*))
  (let* ((%top-level-invocation-time% (get-universal-time))
         (printer (create-template-printer string))
         (%source-stack% (cons string %source-stack%))
         (*template-output* stream))
    (funcall printer environment)))

(defmethod fill-and-print-template ((input-stream stream) environment
                                    &key (stream *default-template-output*))
  (let ((%top-level-invocation-time% (get-universal-time))
        (*template-output* stream))
    (funcall (create-template-printer input-stream) environment)))

(defmethod fill-and-print-template ((pathname pathname) environment &rest rest
                                    &key (stream *default-template-output*)
                                    &allow-other-keys)
  ;; As this method cannot be called by a TMPL_CALL printer in YHTML-Template,
  ;; initialize %source-stack% from scratch.
  (remf rest :stream)
  (let* ((%top-level-invocation-time% (get-universal-time))
         (printer (apply #'create-template-printer pathname rest))
         (%source-stack% (list pathname))
         (*template-output* stream))
    (funcall printer environment)))

(defun clear-template-cache ()
  "Complete clears all template printers from the cache."
  (clrhash *printer-hash*)
  (values))

(defun delete-from-template-cache (pathname)
  "Deletes the template printer denoted by PATHNAME from the
cache. Returns true if such a printer existed, false otherwise."
  (remhash (merge-pathnames pathname *default-template-pathname*)
           *printer-hash*))
