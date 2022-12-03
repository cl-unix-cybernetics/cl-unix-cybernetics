;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(defun adams-toplevel ()
  (let ((*package* (find-package :adams-user)))
    (sb-impl::toplevel-init)))

(defun build (path)
  (sb-ext:save-lisp-and-die path
                            :toplevel #'cl-unix-cybernetics-toplevel
                            :executable t))
