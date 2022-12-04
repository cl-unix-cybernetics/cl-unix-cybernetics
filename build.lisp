;; cl-unix-cybernetics
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>
;;
;; Permission is hereby granted to use this software granted
;; the above copyright notice and this permission paragraph
;; are included in all copies and substantial portions of this
;; software.
;;
;; THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
;; PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
;; AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
;; THIS SOFTWARE.

(in-package :common-lisp-user)

(declaim (optimize (speed 1)
                   (space 1)
                   (safety 3)
                   (debug 3)
                   (compilation-speed 0)))

(defun compile-lisp (path)
  (let* ((fasl (make-pathname :type "fasl" :defaults path))
         (fasl (merge-pathnames fasl)))
    (print fasl)
    (unless (and (probe-file fasl)
                 (<= (file-write-date path)
                     (file-write-date fasl)))
      (print path)
      (compile-file path :output-file fasl))
    (load fasl)))

(defun load* (path)
  (format t "~&Loading ~S" path)
  (load path))

(load* "config.lisp")
(load* "build/systems.lisp")
(load* "toplevel.lisp")
