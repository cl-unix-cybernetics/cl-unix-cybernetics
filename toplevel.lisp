;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(defun adams-toplevel ()
  (let ((*package* (find-package :adams-user)))
    (sb-impl::toplevel-init)))

(sb-ext:save-lisp-and-die #P"~/common-lisp/cl-adams/adams/build/adams"
                          :toplevel #'adams-toplevel
                          :executable t)
