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

(in-package :cl-user)

(flet ((cl-unix-cybernetics-toplevel ()
         (let ((*package* (find-package :cl-unix-cybernetics-user)))
           (sb-impl::toplevel-init))))
  (sb-ext:save-lisp-and-die #P"build/cl-unix-cybernetics"
                            :toplevel #'cl-unix-cybernetics-toplevel
                            :executable t)))
