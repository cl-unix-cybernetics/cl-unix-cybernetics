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

(in-package :cl-user)

(require :adams)

(in-package :adams-user)

;; TEST

(setf (debug-p :shell) t)

(with-host "root@dc"
  (describe-probed (make-instance 'user :id "root")))

(with-manifest "root@dc"
  (make-instance 'user :id "thodg"))

  (with-host "root@dc"
    (adams::apply-manifest "root@dc")

(manifest-resources (manifest "h"))

(apply-manifest "h")

(remove-manifest "h")
