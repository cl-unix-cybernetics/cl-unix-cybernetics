;;
;;  adams  -  Remote system administration tools
;;
;;  Copyright 2013,2014 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :adams)

(in-re-readtable)

(defun uname ()
  (let ((uname-a (first (run "uname -a"))))
    (flet ((try-re (re)
	     (re-bind re (os-name node-name os-release os-version machine) uname-a)))
      (try-re #~"^(\S+) (\S+) (\S+) (.+) (\S+)$"))))

(defun grep (pattern &rest files)
  (run "grep ~A~{ ~A~}" (sh-quote pattern) (mapcar #'sh-quote files)))

(defun egrep (pattern &rest files)
  (run "egrep ~A~{ ~A~}" (sh-quote pattern) (mapcar #'sh-quote files)))

(defun stat (options &rest files)
  (run "stat ~A~{ ~A~}" options (mapcar #'sh-quote files)))

(defun ls (options &rest files)
  (run "ls ~A~{ ~A~}" options (mapcar #'sh-quote files)))
