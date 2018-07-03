;;
;;  adams  -  Remote system administration tools
;;
;;  Copyright 2018 Thomas de Grivel <thomas@lowh.net>
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

(define-resource-class debian-pkg (pkg)
  ()
  ((probe-apt-pkg :properties (:versions))))

(define-syntax apt<8>-list (name release version arch tags)
  #~|([^/\s]+)(?:/([^\s]*))?\s+([^\s]+)\s+([^\s]+)(?:\s+\[([^\]]+)\])?|
  "Syntax for apt(8) list on Linux Debian"
  (values name release version arch (cl-ppcre:split "," tags)))

(defmethod probe-host-packages ((host host) (os os-linux-debian))
  (with-host host
    (let ((packages))
      (with-apt<8>-list (name release version arch tags)
          (run "apt list | grep installed")
        (let ((pkg (resource 'debian-pkg name)))
          (add-probed-properties pkg (properties* name release version
                                                  arch tags))
          (push pkg packages)))
      (list :packages (nreverse packages)))))

#+nil
(clear-resources)

#+nil
(describe-probed (resource 'openbsd-pkg "emacs"))

#+nil
(probe-installed-packages)

#+nil
(map nil #'describe-probed (probe-installed-packages))

#+nil
(run "pkg_info -q | grep emacs-")
